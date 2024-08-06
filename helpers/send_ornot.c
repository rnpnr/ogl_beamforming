#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <zstd.h>

#include "../util.h"
#include "../beamformer_parameters.h"

//#define LOOP
//#define EXPORT_DATA

typedef struct {
	b32 hilbert;
	b32 floating_point_input;
	v2  axial_extent;
	v2  lateral_extent;

	char **remaining;
	i32    remaining_count;
} Options;

static uv4 g_output_points = {.x = 512, .y = 1, .z = 1024, .w = 1};

b32 set_beamformer_parameters(char *shm_name, BeamformerParameters *);
b32 set_beamformer_pipeline(char *shm_name, i32 *stages, i32 stages_count);
b32 send_data(char *pipe_name, char *shm_name, i16 *data, uv2 data_dim);
b32 beamform_data_synchronized_i16(char *pipe_name, char *shm_name, i16 *data, uv2 data_dim,
                                   uv4 output_points, f32 *out_data, i32 timeout_ms);
b32 beamform_data_synchronized_f32(char *pipe_name, char *shm_name, f32 *data, uv2 data_dim,
                                   uv4 output_points, f32 *out_data, i32 timeout_ms);

#define ZEMP_BP_MAGIC (uint64_t)0x5042504D455AFECAull
typedef struct {
	u64 magic;
	u32 version;
	u16 decode_mode;
	u16 beamform_mode;
	u32 raw_data_dim[4];
	u32 decoded_data_dim[4];
	f32 xdc_element_pitch[2];
	f32 xdc_transform[16]; /* NOTE: column major order */
	i16 channel_mapping[256];
	f32 transmit_angles[256];
	f32 focal_depths[256];
	i16 sparse_elements[256];
	i16 hadamard_rows[256];
	f32 speed_of_sound;
	f32 center_frequency;
	f32 sampling_frequency;
	f32 time_offset;
	u32 transmit_mode;
} zemp_bp_v1;

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

#define OS_PATH_SEPERATOR '/'

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

	iz rlen = read(fd, result.data, st.st_size);
	close(fd);

	if (rlen != st.st_size)
		die("couldn't read file: %s\n", fname);

	return result;
}

static b32
os_write_new_file(char *file, void *data, iz data_size)
{
	b32 result = 0;
	i32 fd = open(file, O_WRONLY|O_CREAT|O_TRUNC, 0660);
	if (fd != -1) {
		iz written = 0;
		while (written != data_size) {
			iz w = write(fd, (u8 *)data + written, data_size - written);
			if (w < 0) break;
			written += w;
		}
		result = written == data_size;
	}
	return result;
}

#elif defined(_WIN32)

#define OS_PIPE_NAME "\\\\.\\pipe\\beamformer_data_fifo"
#define OS_SMEM_NAME "Local\\ogl_beamformer_parameters"

#define OS_PATH_SEPERATOR '\\'

#define GENERIC_WRITE  0x40000000
#define GENERIC_READ   0x80000000
#define CREATE_ALWAYS  2
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
W32(b32)    WriteFile(void *, u8 *, i32, i32 *, void *);

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

static b32
os_write_new_file(char *file, void *data, size data_size)
{
	if (data_size > (size)U32_MAX)
		return 0;

	void *h = CreateFileA(file, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, 0, 0);
	if (h == INVALID_HANDLE_VALUE)
		return  0;

	i32 written = 0;
	WriteFile(h, data, data_size, &written, 0);
	CloseHandle(h);

	return written == data_size;
}

#else
#error Unsupported Platform
#endif

static void *
decompress_zstd_data(s8 raw)
{
	clock_t timestamp = clock();
	iz requested_size = ZSTD_getFrameContentSize(raw.data, raw.len);
	fprintf(stderr, "Compressed Size: %zd\nDecompressed Size: %zd\n", raw.len, requested_size);

	void *out = malloc(requested_size);
	if (!out)
		die("couldn't alloc space for decompressing data\n");

	iz decompressed = ZSTD_decompress(out, requested_size, raw.data, raw.len);
	if (decompressed != requested_size)
		die("decompressed size %zd != requested size %zd\n", decompressed, requested_size);

	fprintf(stderr, "Time to Decompress: %0.03f [ms]\n",
	        (f32)(clock() - timestamp) * 1e3 / (f32)CLOCKS_PER_SEC);

	return out;
}

static void
output_base_name_from_argv(Stream *s, char *input)
{
	s8 in = c_str_to_s8(input);

	#if 0
	/* NOTE: .bin.zst */
	for (i32 i = 0; i < 2; i++) {
		while (in.len && in.data[in.len - 1] != '.') in.len--;
		in.len--;
	}
	#else
	/* NOTE: .zst */
	while (in.len && in.data[in.len - 1] != '.') in.len--;
	in.len--;
	#endif

	stream_append_s8(s, in);
}

static void
write_output_data(char *input_name, f32 *data, uv4 output_points, v4 min_coord, v4 max_coord)
{
	u8 buf[2048];
	Stream path = {.data = buf, .cap = sizeof(buf)};

	output_base_name_from_argv(&path, input_name);
	iz sidx = path.widx;

	stream_append_s8(&path, s8("_beamformed.bin"));
	stream_append_byte(&path, 0);

	iz out_size = output_points.x * output_points.y * output_points.z * 2 * sizeof(f32);
	if (!os_write_new_file((c8 *)path.data, data, out_size)) {
		printf("failed to write output data: %s\n", (char *)path.data);
	} else {
		printf("wrote data to:   %s\n", (c8 *)path.data);
	}

	stream_reset(&path, sidx - 3); /* NOTE: _XX */
	stream_append_s8(&path, s8("_params.csv"));
	stream_append_byte(&path, 0);

	u8 buf2[2048];
	Stream o = {.data = buf2, .cap = sizeof(buf2)};
	stream_append_s8(&o, s8("min_coord,max_coord,size\n"));
	o.widx += snprintf((c8 *)o.data + o.widx, o.cap - o.widx, "%f,%f,%u\n", min_coord.x, max_coord.x, output_points.x);
	o.widx += snprintf((c8 *)o.data + o.widx, o.cap - o.widx, "%f,%f,%u\n", min_coord.y, max_coord.y, output_points.y);
	o.widx += snprintf((c8 *)o.data + o.widx, o.cap - o.widx, "%f,%f,%u\n", min_coord.z, max_coord.z, output_points.z);
	if (!os_write_new_file((c8 *)path.data, o.data, o.widx)) {
		printf("failed to write output parameters: %s\n", (c8 *)path.data);
	} else {
		printf("wrote params to: %s\n", (c8 *)path.data);
	}
}

static zemp_bp_v1 *
read_zemp_bp_v1(u8 *path)
{
	s8 raw = os_read_file((char *)path);
	zemp_bp_v1 *result = 0;
	if (raw.len == sizeof(zemp_bp_v1) && *(u64 *)raw.data == ZEMP_BP_MAGIC) {
		if (((zemp_bp_v1 *)raw.data)->version == 1)
			result = (zemp_bp_v1 *)raw.data;
	}
	return result;
}

static void
fill_beamformer_parameters_from_zemp_bp_v1(zemp_bp_v1 *zbp, BeamformerParameters *out)
{
	memcpy(out->channel_mapping,   zbp->channel_mapping,   sizeof(out->channel_mapping));
	memcpy(out->focal_depths,      zbp->focal_depths,      sizeof(out->focal_depths));
	memcpy(out->transmit_angles,   zbp->transmit_angles,   sizeof(out->transmit_angles));
	memcpy(out->xdc_transform,     zbp->xdc_transform,     sizeof(out->xdc_transform));
	memcpy(out->dec_data_dim.E,    zbp->decoded_data_dim,  sizeof(out->dec_data_dim));
	memcpy(out->xdc_element_pitch, zbp->xdc_element_pitch, sizeof(out->xdc_element_pitch));
	memcpy(out->rf_raw_dim.E,      zbp->raw_data_dim,      sizeof(out->rf_raw_dim));

	if (zbp->sparse_elements[0] == -1) {
		for (u32 i = 0; i < zbp->decoded_data_dim[2]; i++)
			out->uforces_channels[i] = i;
	} else {
		memcpy(out->uforces_channels, zbp->sparse_elements, sizeof(out->uforces_channels));
	}

	out->transmit_mode      = zbp->transmit_mode;
	out->decode             = zbp->decode_mode;
	out->das_shader_id      = zbp->beamform_mode;
	out->time_offset        = zbp->time_offset;
	out->sampling_frequency = zbp->sampling_frequency;
	out->center_frequency   = zbp->center_frequency;
	out->speed_of_sound     = zbp->speed_of_sound;
}

#define shift_n(v, c, n) v += n, c -= n
#define shift(v, c) shift_n(v, c, 1)

static b32
s8_equal(s8 a, s8 b)
{
	b32 result = a.len == b.len;
	while (result && a.len) {
		result &= a.data[0] == b.data[0];
		shift(a.data, a.len);
		shift(b.data, b.len);
	}
	return result;
}

static void
s8_split(s8 s, s8 *left, s8 *right, u8 byte)
{
	iz index = 0;
	while (index < s.len && s.data[index] != byte) index++;

	if (left) *left = (s8){.len = index, .data = s.data};

	if (right) {
		right->data = s.data + index + 1;
		right->len  = s.data + s.len - right->data;
	}
}

static s8
s8_trim_left(s8 s)
{
	while (s.len > 0 && s.data[0] == ' ')
		shift(s.data, s.len);
	return s;
}

static v2
parse_v2(char *argv[], i32 argc)
{
	v2 result = {0};

	if (argc > 0) {
		s8 arg = c_str_to_s8(*argv);

		s8 x, y;
		s8_split(arg, &x, &y, ' ');

		if (x.len > 0 && x.data[0] == '{')
			shift(x.data, x.len);
		x = s8_trim_left(x);
		if (x.len > 0 && x.data[x.len - 1] == ',') x.len--;
		if (x.len > 0) {
			char *tmp = strndup((char *)x.data, x.len);
			result.x  = strtod(tmp, 0);
			free(tmp);
		}

		y = s8_trim_left(y);
		if (y.len > 0 && y.data[0] == ',') {
			shift(y.data, y.len);
			y = s8_trim_left(y);
		}
		if (y.len > 0 && y.data[y.len - 1] == ',') y.len--;
		if (y.len > 0) {
			char *tmp = strndup((char *)y.data, y.len);
			result.y  = strtod(tmp, 0);
			free(tmp);
		}
	}

	return result;
}

static void
usage(char *argv0)
{
	die("%s [optional args] basename data_directory data_file\n"
	    "Optional Arguments:\n"
	    "    --float:   input data is floating point\n"
	    "    --hilbert: use CUDA Hilbert shader after decoding\n"
	    "    --axial   '{min, max}': use min and max for image's axial extent\n"
	    "    --lateral '{min, max}': use min and max for image's lateral extent\n",
	    argv0);
}

static Options
parse_argv(i32 argc, char *argv[])
{
	Options result = {
		.axial_extent   = {.y = 100e-3},
		.lateral_extent = {.x = -25e-3, .y = 25e-3}
	};

	char *argv0 = argv[0];
	shift(argv, argc);

	while (argc > 0) {
		s8 arg = c_str_to_s8(*argv);

		if (s8_equal(arg, s8("--hilbert"))) {
			shift(argv, argc);
			result.hilbert = 1;
		} else if (s8_equal(arg, s8("--float"))) {
			shift(argv, argc);
			result.floating_point_input = 1;
		} else if (s8_equal(arg, s8("--axial"))) {
			shift(argv, argc);
			result.axial_extent = parse_v2(argv, argc);
			shift(argv, argc);
		} else if (s8_equal(arg, s8("--lateral"))) {
			shift(argv, argc);
			result.lateral_extent = parse_v2(argv, argc);
			shift(argv, argc);
		} else if (arg.len > 0 && arg.data[0] == '-') {
			usage(argv0);
		} else {
			break;
		}

	}

	result.remaining       = argv;
	result.remaining_count = argc;

	return result;
}

int
main(i32 argc, char *argv[])
{
	Options options = parse_argv(argc, argv);

	if (options.remaining_count < 3)
		usage(argv[0]);

	char *base_name      = options.remaining[0];
	char *data_directory = options.remaining[1];

	u8 path_buffer[2048];
	Stream path = {.data = path_buffer, .cap = sizeof(path_buffer)};
	stream_append_s8(&path, c_str_to_s8(data_directory));
	stream_append_byte(&path, OS_PATH_SEPERATOR);
	iz path_base_widx = path.widx;

	stream_append_s8(&path, c_str_to_s8(base_name));
	stream_append_s8(&path, s8(".bp"));
	stream_append_byte(&path, 0);

	zemp_bp_v1 *zbp = read_zemp_bp_v1(path.data);
	if (!zbp) die("failed to unpack parameters file\n");

	stream_reset(&path, path_base_widx);

	BeamformerParameters bp = {0};
	fill_beamformer_parameters_from_zemp_bp_v1(zbp, &bp);
	bp.output_points         = g_output_points;
	bp.output_min_coordinate = (v4){.x = options.lateral_extent.x, .z = options.axial_extent.x};
	bp.output_max_coordinate = (v4){.x = options.lateral_extent.y, .z = options.axial_extent.y};
	bp.f_number              = 1;

	i32 shader_stages[16];
	i32 shader_stage_count = 0;

	if (options.floating_point_input) shader_stages[shader_stage_count++] = CS_DECODE_FLOAT;
	else                              shader_stages[shader_stage_count++] = CS_DECODE;

	if (options.hilbert) shader_stages[shader_stage_count++] = CS_CUDA_HILBERT;

	shader_stages[shader_stage_count++] = CS_DAS;

	set_beamformer_parameters(OS_SMEM_NAME, &bp);
	set_beamformer_pipeline(OS_SMEM_NAME, shader_stages, shader_stage_count);

	/* TODO(rnp): file iterator, limit based on atoi(argv[2]) */
	#ifdef LOOP
	s8 compressed_data = os_read_file(options.remaining[2]);
	i16 *data = decompress_zstd_data(compressed_data);
	if (!data)
		die("failed to read data: %s!\n", options.remaining[2]);

	i32  fcount     = 0;
	f32  frame_time = 0;
	clock_t timestamp;
	while (1) {
		if (fcount == 5) {
			size data_size = bp.rf_raw_dim.x * bp.rf_raw_dim.y * sizeof(i16);
			printf("Last Pipe Write Time: %0.03f [ms]; Throughput: %0.03f [GB/s]\n",
			       frame_time * 1e3,
			       (double)(data_size)/(frame_time * (double)GB(1)));
			fcount = 0;
		}
		timestamp  = clock();
		send_data(OS_PIPE_NAME, OS_SMEM_NAME, data, bp.rf_raw_dim);
		frame_time = (f32)(clock() - timestamp)/(f32)CLOCKS_PER_SEC;
		fcount++;
	}
	#else
	for (i32 i = 2; i < options.remaining_count; i++) {
		fprintf(stderr, "loading: %s\n", options.remaining[i]);
		s8 compressed_data = os_read_file(options.remaining[i]);
		i16 *data = decompress_zstd_data(compressed_data);
		if (!data)
			die("failed to read data: %s!\n", options.remaining[i]);

		#ifdef EXPORT_DATA
		size out_size = bp.output_points.x * bp.output_points.y * bp.output_points.z * 2 * sizeof(f32);
		f32 *out_data = malloc(out_size);
		beamform_data_synchronized_i16(OS_PIPE_NAME, OS_SMEM_NAME, data, bp.rf_raw_dim,
		                               bp.output_points, out_data);

		write_output_data(options.remaining[i], out_data, bp.output_points,
		                  bp.output_min_coordinate, bp.output_max_coordinate);
		#else
		send_data(OS_PIPE_NAME, OS_SMEM_NAME, data, bp.rf_raw_dim);
		#endif
	}
	#endif

	return 0;
}
