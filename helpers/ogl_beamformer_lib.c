/* See LICENSE for license details. */
#include "ogl_beamformer_lib.h"

typedef struct {
	BeamformerParameters raw;
	enum compute_shaders compute_stages[16];
	u32                  compute_stages_count;
	b32                  upload;
	u32                  raw_data_size;
	b32                  export_next_frame;
	c8                   export_pipe_name[1024];
} BeamformerParametersFull;

typedef struct {
	iptr  file;
	char *name;
} Pipe;

typedef struct { size len; u8 *data; } s8;
#define s8(s) (s8){.len = ARRAY_COUNT(s) - 1, .data = (u8 *)s}

#define ARRAY_COUNT(a) (sizeof(a) / sizeof(*a))

#define U32_MAX (0xFFFFFFFFUL)

#define INVALID_FILE (-1)

#define PIPE_RETRY_PERIOD_MS (100ULL)

static volatile BeamformerParametersFull *g_bp;
static Pipe g_pipe = {.file = INVALID_FILE};

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
W32(void) UnmapViewOfFile(iptr);
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
os_close_pipe(iptr *file, char *name)
{
	if (file) close(*file);
	if (name) unlink(name);
	*file = INVALID_FILE;
}

static b32
os_wait_read_pipe(Pipe p, void *buf, size read_size, u32 timeout_ms)
{
	struct pollfd pfd = {.fd = p.file, .events = POLLIN};
	size total_read = 0;
	if (poll(&pfd, 1, timeout_ms) > 0) {
		size r;
		do {
			 r = read(p.file, (u8 *)buf + total_read, read_size - total_read);
			 if (r > 0) total_read += r;
		} while (r != 0);
	}
	return total_read == read_size;
}

static size
os_write(iptr f, void *data, size data_size)
{
	size written = 0, w = 0;
	do {
		w = write(f, (u8 *)data + written, data_size - written);
		if (w != -1) written += w;
	} while (written != data_size && w != 0);
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

static void
os_release_shared_memory(iptr memory, u64 size)
{
	munmap((void *)memory, size);
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
os_close_pipe(iptr *file, char *name)
{
	if (file) CloseHandle(*file);
	*file = INVALID_FILE;
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
			ReadFile(p.file, (u8 *)buf + total_read, read_size - total_read, &read, 0);
			total_read += read;
		}
	}
	return total_read == read_size;
}

static size
os_write(iptr f, void *data, size data_size)
{
	i32 written = 0;
	b32 result = WriteFile(f, (u8 *)data, data_size, &written, 0);
	if (!result) {
		i32 error = GetLastError();
		warning_msg("os_write(data_size = %td): error: %d", data_size, error);
	}
	return written;
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

static void
os_release_shared_memory(iptr memory, u64 size)
{
	UnmapViewOfFile(memory);
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
		error_msg("maximum stage count is %lu", ARRAY_COUNT(g_bp->compute_stages));
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
		case CS_DECODE:
		case CS_DECODE_FLOAT:
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

static b32
send_raw_data(char *pipe_name, char *shm_name, void *data, u32 data_size)
{
	b32 result = g_pipe.file != INVALID_FILE;
	if (!result) {
		g_pipe = os_open_named_pipe(pipe_name);
		result = g_pipe.file != INVALID_FILE;
		if (!result)
			error_msg("failed to open pipe");
	}
	result &= check_shared_memory(shm_name);

	if (result) {
		g_bp->raw_data_size = data_size;
		g_bp->upload        = 1;

		size written = os_write(g_pipe.file, data, data_size);
		result = written == data_size;
		if (!result) {
			warning_msg("failed to write data to pipe: retrying...");
			os_close_pipe(&g_pipe.file, 0);
			os_release_shared_memory((iptr)g_bp, sizeof(*g_bp));
			g_bp   = 0;
			g_pipe = os_open_named_pipe(pipe_name);
			result = g_pipe.file != INVALID_FILE && check_shared_memory(shm_name);
			if (result)
				written = os_write(g_pipe.file, data, data_size);
			result = written == data_size;
			if (!result)
				warning_msg("failed again, wrote %ld/%u\ngiving up",
				            written, data_size);
		}
	}

	return result;
}

b32
send_data(char *pipe_name, char *shm_name, i16 *data, uv2 data_dim)
{
	b32 result = 0;
	if (check_shared_memory(shm_name)) {
		u64 data_size = data_dim.x * data_dim.y * sizeof(i16);
		if (data_size <= U32_MAX) {
			g_bp->raw.rf_raw_dim = data_dim;
			result = send_raw_data(pipe_name, shm_name, data, data_size);
		}
	}
	return result;
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

static b32
beamform_data_synchronized(char *pipe_name, char *shm_name, void *data, uv2 data_dim,
                           u32 data_size, uv4 output_points, f32 *out_data, i32 timeout_ms)
{
	if (!check_shared_memory(shm_name))
		return 0;

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
		return 0;
	}

	Pipe export_pipe = os_open_read_pipe(OS_EXPORT_PIPE_NAME);
	if (export_pipe.file == INVALID_FILE) {
		error_msg("failed to open export pipe");
		return 0;
	}

	for (u32 i = 0; i < export_name.len; i++)
		g_bp->export_pipe_name[i] = export_name.data[i];

	b32 result = send_raw_data(pipe_name, shm_name, data, data_size);
	if (result) {
		size output_size = output_points.x * output_points.y * output_points.z * sizeof(f32) * 2;
		result = os_wait_read_pipe(export_pipe, out_data, output_size, timeout_ms);
		if (!result)
			warning_msg("failed to read full export data from pipe");
	}

	os_disconnect_pipe(export_pipe);
	os_close_pipe(&export_pipe.file, export_pipe.name);
	os_close_pipe(&g_pipe.file, 0);

	return result;
}

b32
beamform_data_synchronized_i16(char *pipe_name, char *shm_name, i16 *data, uv2 data_dim,
                               uv4 output_points, f32 *out_data, i32 timeout_ms)
{
	b32 result    = 0;
	u64 data_size = data_dim.x * data_dim.y * sizeof(i16);
	if (data_size <= U32_MAX) {
		g_bp->raw.rf_raw_dim = data_dim;
		result = beamform_data_synchronized(pipe_name, shm_name, data, data_dim, data_size,
		                                    output_points, out_data, timeout_ms);
	}
	return result;
}

b32
beamform_data_synchronized_f32(char *pipe_name, char *shm_name, f32 *data, uv2 data_dim,
                               uv4 output_points, f32 *out_data, i32 timeout_ms)
{
	b32 result    = 0;
	u64 data_size = data_dim.x * data_dim.y * sizeof(f32);
	if (data_size <= U32_MAX) {
		g_bp->raw.rf_raw_dim = data_dim;
		result = beamform_data_synchronized(pipe_name, shm_name, data, data_dim, data_size,
		                                    output_points, out_data, timeout_ms);
	}
	return result;
}
