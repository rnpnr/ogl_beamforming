#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <zstd.h>

#include "../util.h"
#include "../beamformer_parameters.h"

b32 set_beamformer_parameters(char *shm_name, BeamformerParameters *);
b32 set_beamformer_pipeline(char *shm_name, i32 *stages, i32 stages_count);
b32 send_data(char *pipe_name, char *shm_name, i16 *data, uv2 data_dim);
void beamform_data_synchronized(char *pipe_name, char *shm_name, i16 *data, uv2 data_dim,
                                       uv4 output_points, f32 *out_data);

//#include "data/240905_ATS539_Cyst_FORCES-TxRow_bp_inc.h"
//#include "data/240905_ATS539_Resolution_uFORCES-16-TxRow_bp_inc.h"
//#include "data/240905_ATS539_Resolution_FORCES-TxRow_bp_inc.h"
//#include "data/240905_ATS539_Resolution_uFORCES-8-TxRow_bp_inc.h"
#include "/tmp/downloads/250114_A06_ATS539_Cyst_2mm_FORCES-TxRow_bp_inc.h"
//#include "/tmp/downloads/250114_C3_ATS539_Cyst_2mm_FORCES-TxColumn_bp_inc.h"

//#define EXPORT_DATA

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

#if defined(__unix__)

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#define OS_PIPE_NAME "/tmp/beamformer_data_fifo"
#define OS_SMEM_NAME "/ogl_beamformer_parameters"

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

static void
output_base_name_from_argv(Stream *s, char *input)
{
	s8 in = cstr_to_s8(input);

	for (i32 i = 0; i < 2; i++) {
		while (in.len && in.data[in.len - 1] != '.') in.len--;
		in.len--;
	}

	stream_append_s8(s, in);
}

static void
write_output_data(char *input_name, f32 *data, uv4 output_points, v4 min_coord, v4 max_coord)
{
	u8 buf[2048];
	Stream path = {.data = buf, .cap = sizeof(buf)};

	output_base_name_from_argv(&path, input_name);
	size sidx = path.widx;

	stream_append_s8(&path, s8("_beamformed.bin"));
	stream_append_byte(&path, 0);

	size out_size = output_points.x * output_points.y * output_points.z * 2 * sizeof(f32);

	i32 fd = open((char *)path.data, O_CREAT|O_WRONLY|O_TRUNC, 0660);
	if (fd == -1) printf("fuck: %s\n", (char *)path.data);
	else          printf("writing data to:   %s\n", (char *)path.data);
	write(fd, data, out_size);
	close(fd);

	path.widx = sidx - 3; /* NOTE: _XX */
	stream_append_s8(&path, s8("_params.csv"));
	stream_append_byte(&path, 0);

	fd = open((char *)path.data, O_CREAT|O_WRONLY|O_TRUNC, 0660);
	if (fd == -1) printf("fuck: %s\n", (char *)path.data);
	else          printf("writing params to: %s\n", (char *)path.data);
	dprintf(fd, "min_coord,max_coord,size\n");
	dprintf(fd, "%f,%f,%u\n", min_coord.x, max_coord.x, output_points.x);
	dprintf(fd, "%f,%f,%u\n", min_coord.y, max_coord.y, output_points.y);
	dprintf(fd, "%f,%f,%u\n", min_coord.z, max_coord.z, output_points.z);
	close(fd);
}

int
main(i32 argc, char *argv[])
{
	if (argc < 2)
		die_(NULL, "usage: %s [params|pipeline|data raw.zstd]\n", argv[0]);

	if (!strcmp(argv[1], "params")) {
		set_beamformer_parameters(OS_SMEM_NAME, &bp);
	} else if (!strcmp(argv[1], "pipeline")) {
		set_beamformer_pipeline(OS_SMEM_NAME, shader_stages, ARRAY_COUNT(shader_stages));
	} else if (!strcmp(argv[1], "data")) {
		if (argc < 3)
			die_(NULL, "usage: %s data raw.zstd\n", argv[0]);

		#if 0
		s8 compressed_data = os_read_file(argv[2]);
		i16 *data = decompress_zstd_data(compressed_data);
		if (!data)
			die("failed to read data: %s!\n", argv[2]);

		i32  fcount     = 0;
		f32  frame_time = 0;
		clock_t timestamp;
		while (1) {
			if (fcount == 5) {
				size data_size = bp.rf_raw_dim.x * bp.rf_raw_dim.y * sizeof(i16);
				printf("Last Pipe Write Time: %0.03f [ms]; Throughput: %0.03f [GB/s]\n",
				       frame_time * 1e3,
				       (double)(data_size)/(frame_time * (double)(GIGABYTE)));
				fcount = 0;
			}
			timestamp  = clock();
			send_data(OS_PIPE_NAME, OS_SMEM_NAME, data, bp.rf_raw_dim);
			frame_time = (f32)(clock() - timestamp)/(f32)CLOCKS_PER_SEC;
			fcount++;
		}
		#else
		for (i32 i = 2; i < argc; i++) {
			fprintf(stderr, "loading: %s\n", argv[i]);
			s8 compressed_data = os_read_file(argv[i]);
			i16 *data = decompress_zstd_data(compressed_data);
			if (!data)
				die("failed to read data: %s!\n", argv[i]);

			#ifdef EXPORT_DATA
			size out_size = bp.output_points.x * bp.output_points.y * bp.output_points.z * 2 * sizeof(f32);
			f32 *out_data = malloc(out_size);
			beamform_data_synchronized(OS_PIPE_NAME, OS_SMEM_NAME, data, bp.rf_raw_dim,
			                           bp.output_points, out_data);

			write_output_data(argv[i], out_data, bp.output_points,
			                  bp.output_min_coordinate, bp.output_max_coordinate);
			#else
			send_data(OS_PIPE_NAME, OS_SMEM_NAME, data, bp.rf_raw_dim);
			#endif
		}
		#endif
	}

	return 0;
}
