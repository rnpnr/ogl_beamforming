/* See LICENSE for license details. */
#include "ogl_beamformer_lib.h"

typedef struct {
	BeamformerParameters raw;
	enum compute_shaders compute_stages[16];
	u32                  compute_stages_count;
	b32                  upload;
	b32                  export_next_frame;
	c8                   export_pipe_name[1024];
} BeamformerParametersFull;

typedef struct {
	iptr  file;
	char *name;
} Pipe;

#define INVALID_FILE (-1)

static volatile BeamformerParametersFull *g_bp;
static Pipe g_pipe = {.file = INVALID_FILE};

#define ARRAY_COUNT(a) (sizeof(a) / sizeof(*a))

#define MS_TO_S (1000ULL)
#define NS_TO_S (1000ULL * 1000ULL)

#define PIPE_RETRY_PERIOD_MS   (100ULL)
#define PIPE_RETRY_TIMEOUT_MS  (10ULL * 1000ULL)
#define VOLUME_WAIT_TIMEOUT_MS (600ULL * 1000ULL)

#if defined(__unix__)
#include <fcntl.h>
#include <poll.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#define OS_EXPORT_PIPE_NAME "/tmp/beamformer_output_pipe"

#elif defined(_WIN32)

#define OS_EXPORT_PIPE_NAME "\\\\.\\pipe\\beamformer_output_fifo"

#define OPEN_EXISTING        3
#define GENERIC_WRITE        0x40000000
#define FILE_MAP_ALL_ACCESS  0x000F001F

#define PIPE_TYPE_BYTE      0x00
#define PIPE_ACCESS_INBOUND 0x01

#define PIPE_WAIT   0x00
#define PIPE_NOWAIT 0x01

#define ERROR_NO_DATA            232L
#define ERROR_PIPE_NOT_CONNECTED 233L
#define ERROR_PIPE_LISTENING     536L

#define W32(r) __declspec(dllimport) r __stdcall
W32(b32)  CloseHandle(iptr);
W32(iptr) CreateFileA(c8 *, u32, u32, void *, u32, u32, void *);
W32(iptr) CreateNamedPipeA(c8 *, u32, u32, u32, u32, u32, u32, void *);
W32(b32)  DisconnectNamedPipe(iptr);
W32(i32)  GetLastError(void);
W32(iptr) MapViewOfFile(iptr, u32, u32, u32, u64);
W32(iptr) OpenFileMappingA(u32, b32, c8 *);
W32(b32)  ReadFile(iptr, u8 *, i32, i32 *, void *);
W32(void) Sleep(u32);
W32(b32)  WriteFile(iptr, u8 *, i32, i32 *, void *);

#else
#error Unsupported Platform
#endif

#if defined(MATLAB_CONSOLE)
#define mexErrMsgIdAndTxt  mexErrMsgIdAndTxt_800
#define mexWarnMsgIdAndTxt mexWarnMsgIdAndTxt_800
void mexErrMsgIdAndTxt(const c8*, c8*, ...);
void mexWarnMsgIdAndTxt(const c8*, c8*, ...);
#define error_tag "ogl_beamformer_lib:error"
#define error_msg(...)   mexErrMsgIdAndTxt(error_tag, __VA_ARGS__)
#define warning_msg(...) mexWarnMsgIdAndTxt(error_tag, __VA_ARGS__)
#else
#define error_msg(...)
#define warning_msg(...)
#endif

#if defined(__unix__)
static Pipe
os_open_named_pipe(char *name)
{
	return (Pipe){.file = open(name, O_WRONLY), .name = name};
}

static Pipe
os_open_read_pipe(char *name)
{
	mkfifo(name, 0660);
	return (Pipe){.file = open(name, O_RDONLY|O_NONBLOCK), .name = name};
}

static void
os_disconnect_pipe(Pipe p)
{
}

static void
os_close_pipe(Pipe* p)
{
	close(p->file);
	unlink(p->name);
	p->file = INVALID_FILE;
}

static b32
os_wait_read_pipe(Pipe p, void *buf, size read_size, u32 timeout_ms)
{
	struct pollfd pfd = {.fd = p.file, .events = POLLIN};
	size total_read = 0;
	if (poll(&pfd, 1, timeout_ms) > 0) {
		size r;
		do {
			 r = read(p.file, buf + total_read, read_size - total_read);
			 if (r > 0) total_read += r;
		} while (r != 0);
	}
	return total_read == read_size;
}

static size
os_write_with_timeout(iptr f, void *data, size data_size, u32 timeout_ms)
{
	size written = 0, start_clock = clock();
	do {
		size w = write(f, data + written, data_size - written);
		if (w != -1) written += w;
	} while (written != data_size && ((clock() - start_clock) * 1000 / CLOCKS_PER_SEC) < timeout_ms);
	return written;
}

static BeamformerParametersFull *
os_open_shared_memory_area(char *name)
{
	i32 fd = shm_open(name, O_RDWR, S_IRUSR|S_IWUSR);
	if (fd == -1)
		return NULL;

	BeamformerParametersFull *new;
	new = mmap(NULL, sizeof(*new), PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
	close(fd);

	if (new == MAP_FAILED)
		return NULL;

	return new;
}

#elif defined(_WIN32)

static Pipe
os_open_named_pipe(char *name)
{
	iptr pipe = CreateFileA(name, GENERIC_WRITE, 0, 0, OPEN_EXISTING, 0, 0);
	return (Pipe){.file = pipe, .name = name};
}

static Pipe
os_open_read_pipe(char *name)
{
	iptr file = CreateNamedPipeA(name, PIPE_ACCESS_INBOUND, PIPE_TYPE_BYTE|PIPE_NOWAIT, 1,
	                             0, 1024UL * 1024UL, 0, 0);
	return (Pipe){.file = file, .name = name};
}

static void
os_disconnect_pipe(Pipe p)
{
	DisconnectNamedPipe(p.file);
}

static void
os_close_pipe(Pipe *p)
{
	CloseHandle(p->file);
	p->file = INVALID_FILE;
}

static b32
os_wait_read_pipe(Pipe p, void *buf, size read_size, u32 timeout_ms)
{
	size elapsed_ms = 0, total_read = 0;
	while (elapsed_ms <= timeout_ms && read_size != total_read) {
		u8 data;
		i32 read;
		b32 result = ReadFile(p.file, &data, 0, &read, 0);
		if (!result) {
			i32 error = GetLastError();
			if (error != ERROR_NO_DATA &&
			    error != ERROR_PIPE_LISTENING &&
			    error != ERROR_PIPE_NOT_CONNECTED)
			{
				/* NOTE: pipe is in a bad state; we will never read anything */
				break;
			}
			Sleep(PIPE_RETRY_PERIOD_MS);
			elapsed_ms += PIPE_RETRY_PERIOD_MS;
		} else {
			ReadFile(p.file, buf + total_read, read_size - total_read, &read, 0);
			total_read += read;
		}
	}
	return total_read == read_size;
}

static size
os_write_with_timeout(iptr f, void *data, size data_size, u32 timeout_ms)
{
	size elapsed_ms = 0, total_written = 0;
	while (elapsed_ms <= timeout_ms && total_written != data_size) {
		i32 written;
		b32 result = WriteFile(f, data + total_written, data_size - total_written, &written, 0);
		total_written += written;

		if (total_written == data_size)
			break;
		if (!result && GetLastError() != ERROR_PIPE_NOT_CONNECTED)
			break;

		warning_msg("waiting...");
		Sleep(PIPE_RETRY_PERIOD_MS);
		elapsed_ms += PIPE_RETRY_PERIOD_MS;
	}
	return total_written;
}

static BeamformerParametersFull *
os_open_shared_memory_area(char *name)
{
	iptr h = OpenFileMappingA(FILE_MAP_ALL_ACCESS, 0, name);
	if (h == INVALID_FILE)
		return 0;

	BeamformerParametersFull *new;
	iptr view = MapViewOfFile(h, FILE_MAP_ALL_ACCESS, 0, 0, sizeof(*new));
	new = (BeamformerParametersFull *)view;
	CloseHandle(h);

	return new;
}

#endif

static b32
check_shared_memory(char *name)
{
	if (!g_bp) {
		g_bp = os_open_shared_memory_area(name);
		if (!g_bp) {
			error_msg("failed to open shared memory area");
			return 0;
		}
	}
	return 1;
}

b32
set_beamformer_pipeline(char *shm_name, i32 *stages, i32 stages_count)
{
	if (stages_count > ARRAY_COUNT(g_bp->compute_stages)) {
		error_msg("maximum stage count is %u", ARRAY_COUNT(g_bp->compute_stages));
		return 0;
	}

	if (!check_shared_memory(shm_name))
		return 0;

	for (i32 i = 0; i < stages_count; i++) {
		switch (stages[i]) {
		case CS_CUDA_DECODE:
		case CS_CUDA_HILBERT:
		case CS_DAS:
		case CS_DEMOD:
		case CS_HADAMARD:
		case CS_MIN_MAX:
		case CS_SUM:
			g_bp->compute_stages[i] = stages[i];
			break;
		default:
			error_msg("invalid shader stage: %d", stages[i]);
			return 0;
		}
	}
	g_bp->compute_stages_count = stages_count;

	return 1;
}

b32
send_data(char *pipe_name, char *shm_name, i16 *data, uv2 data_dim)
{
	if (g_pipe.file == INVALID_FILE) {
		g_pipe = os_open_named_pipe(pipe_name);
		if (g_pipe.file == INVALID_FILE) {
			error_msg("failed to open pipe");
			return 0;
		}
	}

	if (!check_shared_memory(shm_name))
		return 0;

	/* TODO: this probably needs a mutex around it if we want to change it here */
	g_bp->raw.rf_raw_dim = data_dim;
	size data_size       = data_dim.x * data_dim.y * sizeof(i16);
	size written         = os_write_with_timeout(g_pipe.file, data, data_size,
	                                             PIPE_RETRY_TIMEOUT_MS);
	if (written != data_size) {
		warning_msg("failed to write full data to pipe: wrote: %ld/%ld",
		            written, data_size);
	}

	g_bp->upload = 1;

	/* TODO(rnp): this is really wasteful/slow is it necessary? */
	os_close_pipe(&g_pipe);

	return 1;
}

b32
set_beamformer_parameters(char *shm_name, BeamformerParameters *new_bp)
{
	if (!check_shared_memory(shm_name))
		return 0;

	u8 *src = (u8 *)new_bp, *dest = (u8 *)&g_bp->raw;
	for (size i = 0; i < sizeof(BeamformerParameters); i++)
		dest[i] = src[i];
	g_bp->upload = 1;

	return 1;
}

void
beamform_data_synchronized(char *pipe_name, char *shm_name, i16 *data, uv2 data_dim,
                           uv4 output_points, f32 *out_data)
{
	if (!check_shared_memory(shm_name))
		return;

	if (output_points.x == 0) output_points.x = 1;
	if (output_points.y == 0) output_points.y = 1;
	if (output_points.z == 0) output_points.z = 1;
	output_points.w = 1;

	g_bp->raw.rf_raw_dim      = data_dim;
	g_bp->raw.output_points.x = output_points.x;
	g_bp->raw.output_points.y = output_points.y;
	g_bp->raw.output_points.z = output_points.z;
	g_bp->export_next_frame   = 1;

	s8 export_name = s8(OS_EXPORT_PIPE_NAME);
	if (export_name.len > ARRAY_COUNT(g_bp->export_pipe_name)) {
		error_msg("export pipe name too long");
		return;
	}

	Pipe volume_pipe = os_open_read_pipe(OS_EXPORT_PIPE_NAME);
	if (volume_pipe.file == INVALID_FILE) {
		error_msg("failed to open volume pipe");
		return;
	}

	for (u32 i = 0; i < export_name.len; i++)
		g_bp->export_pipe_name[i] = export_name.data[i];

	g_bp->upload = 1;

	if (g_pipe.file == INVALID_FILE) {
		g_pipe = os_open_named_pipe(pipe_name);
		if (g_pipe.file == INVALID_FILE) {
			os_close_pipe(&volume_pipe);
			error_msg("failed to open data pipe");
			return;
		}
	}

	size data_size     = data_dim.x * data_dim.y * sizeof(i16);
	size bytes_written = os_write_with_timeout(g_pipe.file, data, data_size, PIPE_RETRY_TIMEOUT_MS);
	if (bytes_written != data_size) {
		os_disconnect_pipe(volume_pipe);
		os_close_pipe(&volume_pipe);
		error_msg("failed to write full data to pipe wrote: %ld/%ld",
		          bytes_written, data_size);
		return;
	}

	/* TODO(rnp): is this necessary? */
	os_close_pipe(&g_pipe);

	size output_size = output_points.x * output_points.y * output_points.z * sizeof(f32) * 2;
	b32  success     = os_wait_read_pipe(volume_pipe, out_data, output_size, VOLUME_WAIT_TIMEOUT_MS);

	os_disconnect_pipe(volume_pipe);
	os_close_pipe(&volume_pipe);

	if (!success)
		error_msg("failed to read full export data from pipe");
}
