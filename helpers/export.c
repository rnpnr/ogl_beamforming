#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <zstd.h>

#include "ogl_beamformer_lib.c"

typedef u32 b32;

#include "/tmp/downloads/extracted/241210_ATS539_Resolution_FORCES-TxRow_bp_inc.h"

#define die(...) die_((char *)__func__, __VA_ARGS__)
static void __attribute__((noreturn))
die_(char *function, char *fmt, ...)
{
	if (function)
		fprintf(stderr, "%s: ", function);

	va_list ap;

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	exit(1);
}

static s8
s8_cut_head(s8 s, size cut)
{
	s8 result = s;
	if (cut > 0) {
		result.data += cut;
		result.len -= cut;
	}
	return result;
}

#if defined(__unix__)

#define OS_PIPE_NAME "/tmp/beamformer_data_fifo"
#define OS_SMEM_NAME "/ogl_beamformer_parameters"

static b32
os_write_file(iptr file, s8 raw)
{
	while (raw.len) {
		size r = write(file, raw.data, raw.len);
		if (r < 0) return 0;
		raw = s8_cut_head(raw, r);
	}
	return 1;
}

static b32
os_write_new_file(char *fname, s8 raw)
{
	iptr fd = open(fname, O_WRONLY|O_TRUNC|O_CREAT, 0600);
	if (fd == INVALID_FILE)
		return 0;
	b32 ret = os_write_file(fd, raw);
	close(fd);
	return ret;
}

static s8
os_read_file(char *fname)
{
	s8 result;
	i32 fd = open(fname, O_RDONLY);
	if (fd < 0)
		die("couldn't open file: %s\n", fname);

	struct stat st;
	if (stat(fname, &st) < 0)
		die("couldn't stat file\n");

	result.len  = st.st_size;
	result.data = malloc(st.st_size);
	if (!result.data)
		die("couldn't alloc space for reading\n");

	size rlen = read(fd, result.data, st.st_size);
	close(fd);

	if (rlen != st.st_size)
		die("couldn't read file: %s\n", fname);

	return result;
}

#elif defined(_WIN32)

#define OS_PIPE_NAME "\\\\.\\pipe\\beamformer_data_fifo"
#define OS_SMEM_NAME "Local\\ogl_beamformer_parameters"

#define GENERIC_READ   0x80000000
#define OPEN_EXISTING  3
#define INVALID_HANDLE_VALUE (void *)-1

typedef struct __attribute__((packed)) {
	u32 dwFileAttributes;
	u64 ftCreationTime;
	u64 ftLastAccessTime;
	u64 ftLastWriteTime;
	u32 dwVolumeSerialNumber;
	u32 nFileSizeHigh;
	u32 nFileSizeLow;
	u32 nNumberOfLinks;
	u32 nFileIndexHigh;
	u32 nFileIndexLow;
} w32_file_info;

#define W32(r) __declspec(dllimport) r __stdcall
W32(b32)    CloseHandle(void *);
W32(void *) CreateFileA(c8 *, u32, u32, void *, u32, u32, void *);
W32(b32)    GetFileInformationByHandle(void *, void *);
W32(b32)    ReadFile(void *, u8 *, i32, i32 *, void *);
W32(b32)    WriteFile(iptr, u8 *, i32, i32 *, void *);

static b32
os_write_file(iptr file, s8 raw)
{
	i32 wlen;
	WriteFile(file, raw.data, raw.len, &wlen, 0);
	return raw.len == wlen;
}

static ib32
os_write_new_file(char *fname, s8 raw)
{
	if (raw.len > (size)U32_MAX) {
		os_write_err_msg(s8("os_write_file: files >4GB are not yet handled on win32\n"));
		return 0;
	}

	iptr h = CreateFileA(fname, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, 0, 0);
	if (h == INVALID_FILE)
		return  0;

	b32 ret = os_write_file(h, raw);
	CloseHandle(h);

	return ret;
}

static s8
os_read_file(char *fname)
{
	s8 result;
	void *h = CreateFileA(fname, GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);
	if (h == INVALID_HANDLE_VALUE)
		die("couldn't open file: %s\n", fname);

	w32_file_info fileinfo;
	if (!GetFileInformationByHandle(h, &fileinfo))
		die("couldn't get file info\n", stderr);

	result.len  = fileinfo.nFileSizeLow;
	result.data = malloc(fileinfo.nFileSizeLow);
	if (!result.data)
		die("couldn't alloc space for reading\n");

	i32 rlen = 0;
	if (!ReadFile(h, result.data, fileinfo.nFileSizeLow, &rlen, 0) && rlen != fileinfo.nFileSizeLow)
		die("couldn't read file: %s\n", fname);
	CloseHandle(h);

	return result;
}

#else
#error Unsupported Platform
#endif

static void *
decompress_zstd_data(s8 raw)
{
	clock_t timestamp = clock();
	size requested_size = ZSTD_getFrameContentSize(raw.data, raw.len);
	fprintf(stderr, "Compressed Size: %zd\nDecompressed Size: %zd\n", raw.len, requested_size);

	void *out = malloc(requested_size);
	if (!out)
		die("couldn't alloc space for decompressing data\n");

	size decompressed = ZSTD_decompress(out, requested_size, raw.data, raw.len);
	if (decompressed != requested_size)
		die("decompressed size %zd != requested size %zd\n", decompressed, requested_size);

	fprintf(stderr, "Time to Decompress: %0.03f [ms]\n",
	        (f32)(clock() - timestamp) * 1e3 / (f32)CLOCKS_PER_SEC);

	return out;
}

int
main(i32 argc, char *argv[])
{
	if (argc < 3)
		die_(NULL, "usage: %s raw.zstd out_file_name\n", argv[0]);

	set_beamformer_parameters(OS_SMEM_NAME, &bp);
	set_beamformer_pipeline(OS_SMEM_NAME, shader_stages, ARRAY_COUNT(shader_stages));

	s8 compressed_data = os_read_file(argv[1]);
	i16 *data = decompress_zstd_data(compressed_data);
	if (!data) die("failed to read data: %s!\n", argv[1]);

	uv3 output_points    = {.x = 2048, .y = 1, .z = 2048};
	size beamformed_size = output_points.x * output_points.z * 2 * sizeof(f32);
	void *out = malloc(beamformed_size);
	if (!out) die("couldn't alloc space for beamformed data\n");

	beamform_data_synchronized(OS_PIPE_NAME, OS_SMEM_NAME, data, bp.rf_raw_dim,
	                           output_points, out);
	s8 raw = {.len = beamformed_size, .data = out};
	os_write_new_file(argv[2], raw);

	return 0;
}
