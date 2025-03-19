#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <zstd.h>

#include "../util.h"
#include "../beamformer_parameters.h"

static uv4 g_output_points = {.x = 512, .y = 1, .z = 1024, .w = 1};

typedef struct {
	s8  output_path;
	v2  axial_extent;
	v2  lateral_extent;
	i32 frame_count;
	f32 f_number;
} WorkGroupSettings;

typedef struct {
	s8 study;
	WorkGroupSettings settings;
} WorkFrame;

#define low_freq_cyst_settings (WorkGroupSettings){ \
	.output_path    = s8("low_freq_cysts"),       \
	.axial_extent   = {.x =  10e-3, .y = 165e-3}, \
	.lateral_extent = {.x = -60e-3, .y = 60e-3},  \
	.frame_count    = 8,                          \
	.f_number       = 1,                          \
}

#define low_freq_cyst_xplane (WorkGroupSettings){ \
	.output_path    = s8("low_freq_cyst_xplane"), \
	.axial_extent   = {.x =  10e-3, .y = 165e-3}, \
	.lateral_extent = {.x = -60e-3, .y = 60e-3},  \
	.frame_count    = 8,                          \
	.f_number       = 0.5,                        \
}

#define high_freq_cyst_settings (WorkGroupSettings){ \
	.output_path    = s8("high_freq_cysts"),         \
	.axial_extent   = {.x =  5e-3,  .y = 55e-3},     \
	.lateral_extent = {.x = -18.5e-3, .y = 18.5e-3}, \
	.frame_count    = 8,                             \
	.f_number       = 1,                             \
}

#define high_freq_cyst_xplane (WorkGroupSettings){ \
	.output_path    = s8("high_freq_cyst_xplane"),   \
	.axial_extent   = {.x =  5e-3,  .y = 55e-3},     \
	.lateral_extent = {.x = -18.5e-3, .y = 18.5e-3}, \
	.frame_count    = 8,                             \
	.f_number       = 1,                             \
}

static WorkFrame work[] = {
	/* MN45-1 - 3.3MHz */
	{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_FORCES-TxRow"),     low_freq_cyst_xplane},
	{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_FORCES-TxColumn"),  low_freq_cyst_xplane},
	{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_FORCES-TxRow"),     low_freq_cyst_settings},
	{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_TPW-128-TxColumn"), low_freq_cyst_settings},
	{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_VLS-128-TxColumn"), low_freq_cyst_settings},
	/* A06 - 7.8MHz */
	{s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxRow"),        high_freq_cyst_xplane},
	{s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxColumn"),     high_freq_cyst_xplane},
	{s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxRow"),        high_freq_cyst_settings},
	{s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxColumn"),     high_freq_cyst_settings},
	{s8("250319_A06_7.80MHz_ATS539_Cyst_TPW-128-TxColumn"),    high_freq_cyst_settings},
	{s8("250319_A06_7.80MHz_ATS539_Cyst_VLS-128-TxColumn"),    high_freq_cyst_settings},
};

typedef struct {
	b32 export;
	b32 hilbert;
	b32 beamform_plane;

	s8  output_path_prefix;

	char **remaining;
	i32    remaining_count;
} Options;

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

static void
os_make_directory(char *name)
{
	mkdir(name, 0770);
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

static b32
os_write_new_file(char *file, void *data, size data_size)
{
	b32 result = 0;
	i32 fd = open(file, O_WRONLY|O_CREAT|O_TRUNC, 0660);
	if (fd != -1) {
		size written = 0;
		while (written != data_size) {
			size w = write(fd, (u8 *)data + written, data_size - written);
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
W32(b32)    CreateDirectoryA(c8 *, void *);
W32(void *) CreateFileA(c8 *, u32, u32, void *, u32, u32, void *);
W32(b32)    GetFileInformationByHandle(void *, void *);
W32(b32)    ReadFile(void *, u8 *, i32, i32 *, void *);
W32(b32)    WriteFile(void *, u8 *, i32, i32 *, void *);

static void
os_make_directory(char *name)
{
	CreateDirectoryA(name, 0);
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

static Stream
stream_malloc(size capacity)
{
	Stream result = {0};
	result.cap    = capacity;
	result.data   = malloc(capacity);
	if (!result.data)
		die("failed to allocate stream with capacity: %zd\n", capacity);
	return result;
}

static void
stream_ensure_termination(Stream *s, u8 byte)
{
	b32 found = 0;
	if (!s->errors && s->widx > 0)
		found = s->data[s->widx - 1] == byte;
	if (!found) {
		s->errors |= s->cap - 1 < s->widx;
		if (!s->errors)
			s->data[s->widx++] = byte;
	}
}

static void
stream_append_u64_width(Stream *s, u64 n, u64 min_width)
{
	u8 tmp[64];
	u8 *end = tmp + sizeof(tmp);
	u8 *beg = end;
	min_width = MIN(sizeof(tmp), min_width);

	do { *--beg = '0' + (n % 10); } while (n /= 10);
	while (end - beg > 0 &&  end - beg < min_width)
		*--beg = '0';

	stream_append(s, beg, end - beg);
}

static void *
decompress_zstd_data(s8 raw)
{
	size requested_size = ZSTD_getFrameContentSize(raw.data, raw.len);
	void *out           = malloc(requested_size);
	if (out) {
		size decompressed = ZSTD_decompress(out, requested_size, raw.data, raw.len);
		if (decompressed != requested_size) {
			free(out);
			out = 0;
		}
	}
	return out;
}

static void
write_output_data(Options *options, s8 output_path, s8 study, f32 *data, uv4 points,
                  v4 min_coord, v4 max_coord)
{
	u8 buf[2048];
	Stream path = {.data = buf, .cap = sizeof(buf)};

	stream_append_s8(&path, options->output_path_prefix);
	stream_ensure_termination(&path, OS_PATH_SEPERATOR);
	stream_append_s8(&path, output_path);
	stream_append_byte(&path, 0);
	os_make_directory((c8 *)path.data);
	stream_commit(&path, -1);

	stream_ensure_termination(&path, OS_PATH_SEPERATOR);
	stream_append_s8(&path, study);
	size sidx = path.widx;

	stream_append_s8(&path, s8("_beamformed.bin"));
	stream_append_byte(&path, 0);

	size out_size = points.x * points.y * points.z * 2 * sizeof(f32);
	if (path.errors || !os_write_new_file((c8 *)path.data, data, out_size)) {
		printf("failed to write output data: %s\n", (char *)path.data);
	} else {
		printf("wrote data to:   %s\n", (c8 *)path.data);
	}

	stream_reset(&path, sidx);
	stream_append_s8(&path, s8("_params.csv"));
	stream_append_byte(&path, 0);

	u8 buf2[2048];
	Stream o = {.data = buf2, .cap = sizeof(buf2)};
	stream_append_s8(&o, s8("min_coord,max_coord,size\n"));
	stream_commit(&o, snprintf((c8 *)o.data + o.widx, o.cap - o.widx, "%f,%f,%u\n", min_coord.x, max_coord.x, points.x));
	stream_commit(&o, snprintf((c8 *)o.data + o.widx, o.cap - o.widx, "%f,%f,%u\n", min_coord.y, max_coord.y, points.y));
	stream_commit(&o, snprintf((c8 *)o.data + o.widx, o.cap - o.widx, "%f,%f,%u\n", min_coord.z, max_coord.z, points.z));
	if (o.errors || !os_write_new_file((c8 *)path.data, o.data, o.widx)) {
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

static void
usage(char *argv0)
{
	die("%s [--hilbert] [--export path] [--swap-plane] base_path\n"
	    "    --export  path: export data to path\n"
	    "    --hilbert:      use CUDA Hilbert shader after decoding\n"
	    "    --swap-plane:   beamform the YZ plane\n",
	    argv0);
}

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

static Options
parse_argv(i32 argc, char *argv[])
{
	Options result = {0};

	char *argv0 = argv[0];
	shift(argv, argc);

	while (argc > 0) {
		s8 arg = c_str_to_s8(*argv);

		if (s8_equal(arg, s8("--hilbert"))) {
			shift(argv, argc);
			result.hilbert = 1;
		} else if (s8_equal(arg, s8("--swap-plane"))) {
			shift(argv, argc);
			result.beamform_plane = 1;
		} else if (s8_equal(arg, s8("--export"))) {
			shift(argv, argc);
			result.export = 1;
			result.output_path_prefix = c_str_to_s8(*argv);
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

static i16 *
decompress_data_at_work_index(Stream *path_base, u32 index)
{
	stream_append_byte(path_base, '_');
	stream_append_u64_width(path_base, index, 2);
	stream_append_s8(path_base, s8(".zst"));
	stream_ensure_termination(path_base, 0);

	s8 compressed_data = os_read_file((char *)path_base->data);
	i16 *result = decompress_zstd_data(compressed_data);
	if (!result)
		die("failed to decompress data: %s\n", path_base->data);
	free(compressed_data.data);

	return result;
}

static void
work_loop(Options *options, s8 path_base)
{
	Stream path = stream_malloc(KB(4));
	stream_append_s8(&path, path_base);
	stream_ensure_termination(&path, OS_PATH_SEPERATOR);
	size path_base_index = path.widx;

	f32 *out_data;
	if (options->export) {
		size out_size = g_output_points.x * g_output_points.y * g_output_points.z * 2 * sizeof(f32);
		out_data      = malloc(out_size);
		if (!out_data)
			die("failed to allocate space for exported frame\n");
	}

	i32 shader_stages[16];
	i32 shader_stage_count;

	b32 exit = 1;
	do {
	for (u32 i = 0; i < ARRAY_COUNT(work); i++) {
		WorkFrame *w = work + i;

		fprintf(stderr, "showing: %.*s\n", (i32)w->study.len, w->study.data);

		stream_reset(&path, path_base_index);
		stream_append_s8(&path, w->study);
		stream_ensure_termination(&path, OS_PATH_SEPERATOR);
		stream_append_s8(&path, w->study);
		size path_work_index = path.widx;

		stream_append_s8(&path, s8(".bp"));
		stream_ensure_termination(&path, 0);

		zemp_bp_v1 *zbp = read_zemp_bp_v1(path.data);
		if (!zbp) die("failed to unpack parameters file\n");

		BeamformerParameters bp = {0};
		fill_beamformer_parameters_from_zemp_bp_v1(zbp, &bp);
		free(zbp);

		bp.output_points         = g_output_points;
		bp.output_points.w       = w->settings.frame_count;
		bp.output_min_coordinate = (v4){.x = w->settings.lateral_extent.x, .z = w->settings.axial_extent.x};
		bp.output_max_coordinate = (v4){.x = w->settings.lateral_extent.y, .z = w->settings.axial_extent.y};
		bp.f_number              = w->settings.f_number;
		bp.beamform_plane        = options->beamform_plane;

		set_beamformer_parameters(OS_SMEM_NAME, &bp);

		shader_stage_count = 0;
		shader_stages[shader_stage_count++] = CS_DECODE;
		if (options->hilbert) shader_stages[shader_stage_count++] = CS_CUDA_HILBERT;
		shader_stages[shader_stage_count++] = CS_DAS;
		if (w->settings.frame_count > 1) shader_stages[shader_stage_count++] = CS_SUM;

		set_beamformer_pipeline(OS_SMEM_NAME, shader_stages, shader_stage_count);

		for (i32 frame = 0; frame < w->settings.frame_count - 1; frame++) {
			stream_reset(&path, path_work_index);
			i16 *data = decompress_data_at_work_index(&path, (u32)frame);
			send_data(OS_PIPE_NAME, OS_SMEM_NAME, data, bp.rf_raw_dim);
			free(data);
		}

		stream_reset(&path, path_work_index);
		i16 *data = decompress_data_at_work_index(&path, w->settings.frame_count - 1);
		if (options->export) {
			b32 result = beamform_data_synchronized_i16(OS_PIPE_NAME, OS_SMEM_NAME, data,
			                                            bp.rf_raw_dim, bp.output_points,
			                                            out_data, 100000);
			if (result) {
				write_output_data(options, w->settings.output_path, w->study,
				                  out_data, bp.output_points,
				                  bp.output_min_coordinate, bp.output_max_coordinate);
			}
		} else {
			send_data(OS_PIPE_NAME, OS_SMEM_NAME, data, bp.rf_raw_dim);
			fprintf(stderr, "press key to continue...");
			exit = fgetc(stdin) == EOF;
		}
		free(data);
	}
	} while(!exit);
}

int
main(i32 argc, char *argv[])
{
	Options options = parse_argv(argc, argv);

	if (options.remaining_count > 1)
		usage(argv[0]);

	work_loop(&options, c_str_to_s8(options.remaining[0]));

	return 0;
}
