#include <mex.h>

#include <stddef.h>
#include <stdint.h>
typedef int16_t   i16;
typedef int32_t   i32;
typedef ptrdiff_t size;

#if defined(__unix__)
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#define OS_INVALID_FILE (-1)
typedef i32 os_file;
typedef struct {
	os_file  file;
	char    *name;
} os_pipe;
#elif defined(_WIN32)
#include <windows.h>

#define OS_INVALID_FILE (INVALID_HANDLE_VALUE)
typedef HANDLE os_file;
typedef struct {
	os_file  file;
	char    *name;
} os_pipe;

#else
#error Unsupported Platform
#endif

/* NOTE: the mexAtExit function is poorly designed and doesn't
 * take a context pointer so this must be a global */
static os_pipe g_pipe = {.file = OS_INVALID_FILE};

#if defined(__unix__)
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

static void
os_close_pipe(void)
{
	close(g_pipe.file);
	mxFree(g_pipe.name);
}

#elif defined(_WIN32)

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

static void
os_close_pipe(void)
{
	CloseHandle(g_pipe.file);
	mxFree(g_pipe.name);
}
#endif

/* NOTE: usage: pipe_data_to_beamformer(pipe_name, data) */
void
mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
	if (nrhs != 2) {
		mexErrMsgIdAndTxt("ogl_beamformer:wrong_input",
		                  "usage: ogl_beamformer_pipe(pipe_name, data)");
		return;
	}

	if (g_pipe.file == OS_INVALID_FILE) {
		char *pipe_name = mxArrayToString(prhs[0]);
		g_pipe = os_open_named_pipe(pipe_name);
		if (g_pipe.file == OS_INVALID_FILE) {
			mexErrMsgIdAndTxt("ogl_beamformer:pipe_error", "failed to open pipe");
			mxFree(pipe_name);
			return;
		}
		mexAtExit(os_close_pipe);
	}

	const mxArray *mxdata = prhs[1];
	if (!mxIsInt16(mxdata)) {
		mexErrMsgIdAndTxt("ogl_beamformer:invalid_type",
		                  "invalid data type; only int16 is supported");
		return;
	}

	void *data     = mxGetPr(mxdata);
	size data_size = mxGetNumberOfElements(mxdata) * sizeof(i16);
	size written   = os_write_to_pipe(g_pipe, data, data_size);
	if (written != data_size)
		mexWarnMsgIdAndTxt("ogl_beamformer:write_error",
		                   "failed to write full data to pipe: wrote: %ld", written);
}
