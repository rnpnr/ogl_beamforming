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

#define POLL_PERIOD 100  // 100s
#define POLL_TIMEOUT 1200 * 1000 // 20 minutes

#if defined(__unix__)
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include <poll.h>

#define OS_EXPORT_PIPE_NAME "/tmp/beamformer_output_pipe"

#elif defined(_WIN32)

#define OS_EXPORT_PIPE_NAME "\\\\.\\pipe\\beamformer_output_fifo"

#define OPEN_EXISTING        3
#define GENERIC_WRITE        0x40000000
#define FILE_MAP_ALL_ACCESS  0x000F001F

#define WIN_PIPE_TYPE_BYTE				0x00
#define WIN_PIPE_ACCESS_INBOUND			0x01

#define WIN_PIPE_WAIT					0x00
#define WIN_PIPE_NOWAIT					0x01

#define WIN_ERROR_NO_DATA				232L
#define WIN_ERROR_PIPE_NOT_CONNECTED	233L
#define WIN_ERROR_PIPE_LISTENING		536L

#define W32(r) __declspec(dllimport) r __stdcall
W32(b32)  CloseHandle(iptr);
W32(iptr) CreateFileA(c8 *, u32, u32, void *, u32, u32, void *);
W32(iptr) CreateNamedPipeA(const c8 *, u32, u32, u32, u32, u32, u32, void *);
W32(iptr) MapViewOfFile(iptr, u32, u32, u32, u64);
W32(iptr) OpenFileMappingA(u32, b32, c8 *);
W32(b32)  ReadFile(iptr, u8 *, i32, i32 *, void *);
W32(b32)  WriteFile(iptr, u8 *, i32, i32 *, void *);
W32(void) Sleep(i32);
W32(i32)  GetLastError();
W32(b32)  PeekNamedPipe(iptr, void*, i32, i32*, i32*, i32*);
W32(b32)  DisconnectNamedPipe(iptr);

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
os_open_read_pipe(char *name)
{
	mkfifo(name, 0660);
	return (Pipe){.file = open(name, O_RDONLY|O_NONBLOCK), .name = name};
}

static void
os_close_pipe(Pipe* p)
{
	close(p->file);
	unlink(p->name);
	p->file = INVALID_FILE;
}

static void
os_disconnect_pipe_server(Pipe p)
{
	// Windows only
}

static b32
os_read_pipe(Pipe p, void *buf, size read_size)
{
	size r = 0, total_read = 0;
	do {
		if (r != -1)
			total_read += r;
		r = read(p.file, buf + total_read, read_size - total_read);
	} while (r);
	return total_read == read_size;
}

static Pipe
os_open_named_pipe(char *name)
{
	return (Pipe){.file = open(name, O_WRONLY), .name = name};
}

static size
os_write_to_pipe(Pipe p, void *data, size len)
{
	size written = 0, w = 0;
	do {
		if (w != -1)
			written += w;
		w = write(p.file, data + written, len - written);
	} while(written != len && w != 0);
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

// Return true if the pipe has an unrecoverable error,
// false if we just need to wait for a client to connect
static b32 
os_write_pipe_failed()
{
	// STUB
	return 1;
}

static void
os_sleep_ms(u32 duration)
{
	usleep(duration);
}

static b32
os_poll_pipe(Pipe* p)
{
	struct pollfd pfd = { .fd = p->file, .events = POLLIN };
	poll(&pfd, 1, 0);
	return !!(pfd.revents & POLLIN);
}

#elif defined(_WIN32)

static Pipe
os_open_read_pipe(const char* name)
{
	iptr file = CreateNamedPipeA(name, WIN_PIPE_ACCESS_INBOUND, WIN_PIPE_TYPE_BYTE | WIN_PIPE_NOWAIT, 1,
		0, 1024UL * 1024UL, 0, 0);

	return (Pipe) { .file = file, .name = name };
}


static void
os_close_read_pipe(Pipe p)
{
	CloseHandle(p.file);
}

static b32
os_read_pipe(Pipe p, void* buf, size read_size)
{
	i32 total_read = 0;
	b32 result = ReadFile(p.file, buf, read_size, &total_read, 0);

	if (!result)
	{
		i32 error_code = GetLastError();
		// Use warning_msg, error_msg exits MEX early preventing cleanup
		warning_msg("Read pipe error, Data size: %i, Total read: %i, Error code: %i\n", read_size, total_read, error_code);
	}

	return result;
}

static Pipe
os_open_named_pipe(char *name)
{
	iptr pipe = CreateFileA(name, GENERIC_WRITE, 0, 0, OPEN_EXISTING, 0, 0);
	return (Pipe){.file = pipe, .name = name};
}

static i32
os_write_to_pipe(Pipe p, void *data, size len)
{
	i32 bytes_written;
	WriteFile(p.file, data, len, &bytes_written, 0);
	return (size)bytes_written;
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
os_disconnect_pipe_server(Pipe p)
{
	b32 result = DisconnectNamedPipe(p.file);

	if (!result)
	{
		warning_msg("Failed to disconnect from pipe '%s' with error: %i", p.name, GetLastError());
	}
}

static void
os_close_pipe(Pipe* p)
{
	b32 result = CloseHandle(p->file);

	if (!result)
	{
		warning_msg("Failed to close pipe '%s' with error: %i", p->name, GetLastError());
	}
	else
	{
		p->file = INVALID_FILE;
	}
}


// Return true if the pipe state requires restart
static b32
os_write_pipe_failed()
{
	return GetLastError() != WIN_ERROR_PIPE_NOT_CONNECTED;
}

static void
os_sleep_ms(u32 duration)
{
	Sleep((i32)duration);
}

static b32 
os_poll_pipe(Pipe* p)
{
	// Try and read 0 bytes, this will give more pipe status information than PeakNamedPipe
	u8 data = 0;
	i32 total_read = 0;
	b32 result = ReadFile(p->file, &data, 0, &total_read, NULL);

	i32 error = GetLastError();
	if (result)
	{
		//warning_msg("Poll success, Error: %i", error);
		return 1;
	}

	// These three errors just mean nothing's been sent yet, otherwise the pipe is in a bad state
	// and needs to be recreated.
	if (error != WIN_ERROR_NO_DATA && error != WIN_ERROR_PIPE_LISTENING && error != WIN_ERROR_PIPE_NOT_CONNECTED)
	{
		warning_msg("os_poll_pipe: Data pipe poll failed, Windows error '%i'.\n", error);
		result = DisconnectNamedPipe(p->file);
		result = CloseHandle(p->file);
		*p = os_open_named_pipe(p->name);

		if (p->file == INVALID_FILE)
		{
			error = GetLastError();
			error_msg("os_poll_pipe: Failed to reopen volumne pipe after error, "
				"Windows error '%i'.\n", error);
		}
	}
	return 0;
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
			error_msg("failed to open pipe with error: '%i'", GetLastError());
			return 0;
		}
	}

	if (!check_shared_memory(shm_name))
		return 0;

	/* TODO: this probably needs a mutex around it if we want to change it here */
	g_bp->raw.rf_raw_dim = data_dim;
	size data_size       = data_dim.x * data_dim.y * sizeof(i16);
	size written         = os_write_to_pipe(g_pipe, data, data_size);
	if (written != data_size)
		warning_msg("failed to write full data to pipe: wrote: %ld", written);
	g_bp->upload = 1;

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
	else
	{
		warning_msg("Opened export pipe '%s', file '%p'", volume_pipe.name, volume_pipe.file);
	}

	g_bp->raw.rf_raw_dim = data_dim;
	g_bp->raw.output_points.x = output_points.x;
	g_bp->raw.output_points.y = output_points.y;
	g_bp->raw.output_points.z = output_points.z;
	g_bp->export_next_frame = 1;

	for (u32 i = 0; i < export_name.len; i++)
		g_bp->export_pipe_name[i] = export_name.data[i];

	g_bp->upload = 1;

	if (g_pipe.file == INVALID_FILE || g_pipe.file == NULL) {
		g_pipe = os_open_named_pipe(pipe_name);
		if (g_pipe.file == INVALID_FILE) {
			error_msg("failed to open data pipe");
			os_disconnect_pipe_server(volume_pipe);
			os_close_pipe(&volume_pipe);
			return;
		}
		warning_msg("Opened pipe '%s', file '%p'", g_pipe.name, g_pipe.file);
	}
	else
	{
		warning_msg("Data pipe healthy, handle: %p", g_pipe.file);
	}

	size data_size = data_dim.x * data_dim.y * sizeof(i16);

	size bytes_written = 0;
	u32 elapsed = 0;

	while (elapsed <= POLL_TIMEOUT)
	{
		bytes_written = os_write_to_pipe(g_pipe, data, data_size);
		if (bytes_written != data_size)
		{
			if (os_write_pipe_failed())
			{
				os_disconnect_pipe_server(volume_pipe);
				os_close_pipe(&volume_pipe);
				error_msg("Failed to write full data to pipe: Total: %ld, Wrote: %i",
					data_size, bytes_written);
				return;
			}
			else
			{
				// Client just not connected, wait
			}
		}
		else
		{
			// Success
			break;
		}

		warning_msg("Waiting\n");
		os_sleep_ms(POLL_PERIOD);
		elapsed += POLL_PERIOD;
	}

	os_close_pipe(&g_pipe);

	b32 pipe_ready = 0;
	b32 success = 0;

	size output_size = output_points.x * output_points.y * output_points.z * sizeof(f32);
	while (elapsed <= POLL_TIMEOUT)
	{

		if (os_poll_pipe(&volume_pipe))
		{
			success = os_read_pipe(volume_pipe, out_data, output_size);
			break;
		}
		else
		{
			os_sleep_ms(POLL_PERIOD);
			elapsed += POLL_PERIOD;
		}
	}

	warning_msg("Closing pipe");
	os_disconnect_pipe_server(volume_pipe);
	os_close_pipe(&volume_pipe);

	if (!success)
	{
		error_msg("failed to read full export data from pipe\n");
	}	
}

