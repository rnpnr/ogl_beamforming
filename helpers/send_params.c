#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef uint8_t   u8;
typedef int16_t   i16;
typedef int32_t   i32;
typedef uint32_t  u32;
typedef uint32_t  b32;
typedef float     f32;
typedef double    f64;
typedef ptrdiff_t size;

typedef struct { f32 x, y; }       v2;
typedef struct { u32 x, y; }       uv2;
typedef struct { u32 x, y, z, w; } uv4;

#include "../beamformer_parameters.h"

typedef struct {
	BeamformerParameters raw;
	b32 upload;
} BeamformerParametersFull;

#include "/tmp/downloads/240723_ATS539_Resolution_uFORCES-32-TxRow_bp_inc.h"
//#include "/tmp/downloads/240723_ATS539_Contrast_FORCES-TxRow_bp_inc.h"
//#include "/tmp/downloads/240723_ATS539_Resolution_FORCES-TxRow_bp_inc.h"

#define GIGABYTE (1024UL * 1024UL * 1024UL)

static void __attribute__((noreturn))
die(char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	exit(1);
}


#if defined(__unix__)
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#define OS_PIPE_NAME "/tmp/beamformer_data_fifo"
#define OS_SMEM_NAME "/ogl_beamformer_parameters"

#define OS_INVALID_FILE (-1)
typedef i32 os_file;
typedef struct {
	os_file  file;
	char    *name;
} os_pipe;
#elif defined(_WIN32)
#include <windows.h>

#define OS_PIPE_NAME "\\\\.\\pipe\\beamformer_data_fifo"
#define OS_SMEM_NAME "Local\\ogl_beamformer_parameters"

#define OS_INVALID_FILE (INVALID_HANDLE_VALUE)
typedef HANDLE os_file;
typedef struct {
	os_file  file;
	char    *name;
} os_pipe;

#else
#error Unsupported Platform
#endif

static volatile BeamformerParametersFull *g_bp;
static os_pipe g_pipe = {.file = OS_INVALID_FILE};

#if defined(__unix__)
static void *
os_read_file(char *fname)
{
	i32 fd = open(fname, O_RDONLY);
	if (fd < 0)
		die("os_read_file: couldn't open file: %s\n", fname);

	struct stat st;
	if (stat(fname, &st) < 0)
		die("os_read_file: couldn't stat file\n");

	void *out = malloc(st.st_size);
	if (!out)
		die("os_read_file: couldn't alloc space for reading\n");

	size rlen = read(fd, out, st.st_size);
	close(fd);

	if (rlen != st.st_size)
		die("os_read_file: couldn't read file: %s\n", fname);

	return out;
}

static os_pipe
os_open_named_pipe(char *name)
{
	return (os_pipe){.file = open(name, O_WRONLY), .name = name};
}

static size
os_write_to_pipe(os_pipe p, void *data, size len)
{
	size written = 0, w = 0;
	do {
		written += w;
		w = write(p.file, data, len);
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

#elif defined(_WIN32)

static void *
os_read_file(char *fname)
{
	HANDLE h = CreateFileA(fname, GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);
	if (h == INVALID_HANDLE_VALUE)
		die("os_read_file: couldn't open file: %s\n", fname);

	BY_HANDLE_FILE_INFORMATION fileinfo;
	if (!GetFileInformationByHandle(h, &fileinfo))
		die("os_get_file_stats: couldn't get file info\n", stderr);

	void *out = malloc(fileinfo.nFileSizeLow);
	if (!out)
		die("os_read_file: couldn't alloc space for reading\n");

	DWORD rlen = 0;
	if (!ReadFile(h, out, fileinfo.nFileSizeLow, &rlen, 0) && rlen != fileinfo.nFileSizeLow)
		die("os_read_file: couldn't read file: %s\n", fname);
	CloseHandle(h);

	return out;
}

static os_pipe
os_open_named_pipe(char *name)
{
	HANDLE pipe = CreateFileA(name, GENERIC_WRITE, 0, 0, OPEN_EXISTING, 0, 0);
	return (os_pipe){.file = pipe, .name = name};
}

static size
os_write_to_pipe(os_pipe p, void *data, size len)
{
	DWORD bytes_written;
	WriteFile(p.file, data, len, &bytes_written, 0);
	return bytes_written;
}

static BeamformerParametersFull *
os_open_shared_memory_area(char *name)
{
	HANDLE h = OpenFileMappingA(FILE_MAP_ALL_ACCESS, FALSE, name);
	if (h == OS_INVALID_FILE)
		return NULL;

	BeamformerParametersFull *new;
	new = MapViewOfFile(h, FILE_MAP_ALL_ACCESS, 0, 0, sizeof(*new));
	CloseHandle(h);

	return new;
}
#endif

static void
check_shared_memory(char *name)
{
	if (g_bp)
		return;
	g_bp = os_open_shared_memory_area(name);
	if (g_bp == NULL)
		die("failed to open shared memory\n");
}

static size
send_data(char *pipe_name, char *shm_name, i16 *data, uv2 data_dim)
{
	if (g_pipe.file == OS_INVALID_FILE) {
		g_pipe = os_open_named_pipe(pipe_name);
		if (g_pipe.file == OS_INVALID_FILE) {
			printf("Failed to open named pipe: %s\n", pipe_name);
			exit(1);
		}
	}

	check_shared_memory(shm_name);
	/* TODO: this probably needs a mutex around it if we want to change it here */
	g_bp->raw.rf_raw_dim = data_dim;
	size data_size       = data_dim.x * data_dim.y * sizeof(i16);
	size written         = os_write_to_pipe(g_pipe, data, data_size);
	if (written != data_size)
		printf("Failed to write full data to pipe: wrote: %ld/%ld\n", written, data_size);
	g_bp->upload = 1;

	return written;
}


static void
set_beamformer_parameters(char *shm_name, BeamformerParameters *new_bp)
{
	check_shared_memory(shm_name);

	if (!g_bp)
		return;

	u8 *src = (u8 *)new_bp, *dest = (u8 *)&g_bp->raw;
	for (size i = 0; i < sizeof(BeamformerParameters); i++)
		dest[i] = src[i];
	g_bp->upload = 1;
}

int
main(i32 argc, char *argv[])
{
	if (!strcmp(argv[1], "params")) {
		set_beamformer_parameters(OS_SMEM_NAME, &bp);
	} else if (!strcmp(argv[1], "data")) {
		if (argc != 3)
			die("usage: %s data file_name\n", argv[0]);
		i16 *data = os_read_file(argv[2]);
		if (!data)
			die("failed to read data: %s!\n", argv[2]);
		size written    = 0;
		i32  fcount     = 0;
		f32  frame_time = 0;
		clock_t timestamp;
		while (1) {
			if (fcount == 5) {
				printf("Last Frame Time: %0.03f [ms]; Throughput: %0.03f [GB/s]\n",
				       frame_time * 1e3,
				       (double)(written)/(frame_time * (double)(GIGABYTE)));
				fcount = 0;
			}
			timestamp  = clock();
			written    = send_data(OS_PIPE_NAME, OS_SMEM_NAME, data, bp.rf_raw_dim);
			frame_time = (f32)(clock() - timestamp)/(f32)CLOCKS_PER_SEC;
			fcount++;
		}
	}

	return 0;
}
