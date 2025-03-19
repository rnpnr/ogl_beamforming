#include "../util.h"
#include "../beamformer_parameters.h"

/* NOTE(rnp): I want these activated here even in release mode */
#ifndef ASSERT
#define ASSERT(c) do { if (!(c)) debugbreak(); } while (0);
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <fftw3.h>
#include <zstd.h>

//#define TIME_FFT
/* NOTE: main thread will also do work */
#define THREAD_COUNT (1 - 1)
#if THREAD_COUNT
#include <pthread.h>
#endif

#define PI   3.14159265358979323846f
#define cosf __builtin_cosf
#define sinf __builtin_sinf

global uv4 g_output_points = {.x = 512, .y = 1, .z = 1024, .w = 1};

typedef struct {
	s8  output_path;
	uv4 output_points;
	v2  axial_extent;
	v2  lateral_extent;
	i32 frame_count;
	f32 f_number;
	f32 center_frequency;
	f32 low_pass_cutoff_frequency;
	u32 low_pass_filter_order;
} WorkGroupSettings;

typedef struct {
	s8 study;
	WorkGroupSettings settings;
} WorkFrame;

/* TODO(rnp): file dir iterator */
#define LAST_SAVED_FRAME_NUMBER 8

#define low_freq_res_settings (WorkGroupSettings){ \
	.output_path    = s8("low_freq_res"),         \
	.axial_extent   = {.x =  5e-3,  .y = 170e-3}, \
	.lateral_extent = {.x = -50e-3, .y = 50e-3},  \
	.frame_count    = 1,                          \
	.f_number       = 0.5,                        \
}

#define low_freq_cyst_settings (WorkGroupSettings){ \
	.output_path    = s8("low_freq_cysts"),       \
	.axial_extent   = {.x =  10e-3, .y = 165e-3}, \
	.lateral_extent = {.x = -60e-3, .y = 60e-3},  \
	.frame_count    = 8,                          \
	.f_number       = 0.5,                        \
	.low_pass_cutoff_frequency = 3.5e6,           \
	.low_pass_filter_order     = 101,             \
}

#define low_freq_contrast_settings (WorkGroupSettings){ \
	.output_points  = {{512, 1, 512, 1}},           \
	.output_path    = s8("low_freq_contrast"),      \
	.axial_extent   = {.x = 91.0e-3, .y = 108e-3},  \
	.lateral_extent = {.x =  2.7e-3, .y = 20.7e-3}, \
	.frame_count    = 8,                            \
	.f_number       = 1,                            \
	.low_pass_cutoff_frequency = 4e6,               \
	.low_pass_filter_order     = 101,               \
}

#define low_freq_cyst_xplane (WorkGroupSettings){ \
	.output_path    = s8("low_freq_cyst_xplane"), \
	.axial_extent   = {.x =  10e-3, .y = 165e-3}, \
	.lateral_extent = {.x = -60e-3, .y = 60e-3},  \
	.frame_count    = 8,                          \
	.f_number       = 0.5,                        \
}

#define high_freq_res_settings (WorkGroupSettings){ \
	.output_path    = s8("high_freq_res"),           \
	.axial_extent   = {.x =  5e-3,  .y = 55e-3},     \
	.lateral_extent = {.x = -18.5e-3, .y = 18.5e-3}, \
	.frame_count    = 8,                             \
	.f_number       = 0.75,                          \
}

#define high_freq_cyst_settings (WorkGroupSettings){ \
	.output_path    = s8("high_freq_cysts"),         \
	.axial_extent   = {.x =  5e-3,    .y = 55e-3},   \
	.lateral_extent = {.x = -18.5e-3, .y = 18.5e-3}, \
	.frame_count    = 8,                             \
	.f_number       = 1,                             \
}

#define high_freq_contrast_settings (WorkGroupSettings){ \
	.output_points  = {{512, 1, 512, 1}},         \
	.output_path    = s8("high_freq_contrast"),   \
	.axial_extent   = {.x =  28e-3, .y =  36e-3}, \
	.lateral_extent = {.x = -13e-3, .y = -2e-3},  \
	.frame_count    = 8,                          \
	.f_number       = 1,                          \
}

#define high_freq_cyst_xplane (WorkGroupSettings){ \
	.output_path    = s8("high_freq_cyst_xplane"),   \
	.axial_extent   = {.x =  5e-3,  .y = 60e-3},     \
	.lateral_extent = {.x = -18.5e-3, .y = 18.5e-3}, \
	.frame_count    = 8,                             \
	.f_number       = 1,                             \
}

global WorkFrame work[] = {
	/* MN45-1 - 3.3MHz */
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_FORCES-TxRow"),     low_freq_cyst_xplane},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_FORCES-TxColumn"),  low_freq_cyst_xplane},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_FORCES-TxRow"),           low_freq_cyst_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_TPW-128-TxColumn"),       low_freq_cyst_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_VLS-128-TxColumn"),       low_freq_cyst_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Resolution_FORCES-TxRow"),     low_freq_res_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Resolution_TPW-128-TxColumn"), low_freq_res_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Resolution_VLS-128-TxColumn"), low_freq_res_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Resolution_uFORCES-32-TxRow"), low_freq_res_settings},

	/* A06 - 7.8MHz */
	//{s8("250327_A06_7.80MHz_35mm_ATS539_Cyst_FORCES-TxColumn"), high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_ATS539_Cyst_uFORCES-32-TxRow"),        high_freq_cyst_xplane},
	//{s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxColumn"),     high_freq_cyst_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxRow"),        high_freq_cyst_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Cyst_TPW-128-TxColumn"),    high_freq_cyst_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Cyst_VLS-128-TxColumn"),    high_freq_cyst_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Resolution_FORCES-TxRow"),     high_freq_res_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Resolution_VLS-128-TxColumn"), high_freq_res_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Resolution_TPW-128-TxColumn"), high_freq_res_settings},

	/* Cyst X-Plane */
	//{s8("250327_A06_7.80MHz_15mm_ATS539_Cyst_FORCES-TxColumn"), high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_25mm_ATS539_Cyst_FORCES-TxColumn"), high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_35mm_ATS539_Cyst_FORCES-TxColumn"), high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_45mm_ATS539_Cyst_FORCES-TxColumn"), high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_15mm_ATS539_Cyst_FORCES-TxRow"),    high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_25mm_ATS539_Cyst_FORCES-TxRow"),    high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_35mm_ATS539_Cyst_FORCES-TxRow"),    high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_45mm_ATS539_Cyst_FORCES-TxRow"),    high_freq_cyst_xplane},

	/* Contrast */
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_FORCES-TxRow"),     low_freq_contrast_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_TPW-128-TxColumn"), low_freq_contrast_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_VLS-128-TxColumn"), low_freq_contrast_settings},

	//{s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxRow"),        high_freq_contrast_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Cyst_TPW-128-TxColumn"),    high_freq_contrast_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Cyst_VLS-128-TxColumn"),    high_freq_contrast_settings},
};

typedef struct {
	b32 export;
	b32 analytic;
	b32 low_pass;
	b32 matched_filter;
	b32 beamform_plane;

	s8  output_path_prefix;

	char **remaining;
	i32    remaining_count;
} Options;

b32 set_beamformer_parameters(char *shm_name, BeamformerParametersV0 *);
b32 set_beamformer_pipeline(char *shm_name, i32 *stages, i32 stages_count);
b32 send_data(char *pipe_name, char *shm_name, void *data, u32 data_size);
b32 beamform_data_synchronized(char *pipe_name, char *shm_name, void *data, u32 data_size,
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
function void __attribute__((noreturn))
die_(char *function_name, char *format, ...)
{
	if (function_name)
		fprintf(stderr, "%s: ", function_name);

	va_list ap;

	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);

	exit(1);
}

#if defined(__linux__)

#include "../os_linux.c"

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#define OS_SMEM_NAME "/ogl_beamformer_shared_memory"

#define OS_PATH_SEPERATOR '/'

function void
os_make_directory(char *name)
{
	mkdir(name, 0770);
}

function s8
os_read_file_simp(char *fname)
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

#elif defined(_WIN32)

typedef struct {
	iptr io_completion_handle;
} w32_context;

enum w32_io_events {
	W32_IO_FILE_WATCH,
	W32_IO_PIPE,
};

typedef struct {
	u64  tag;
	iptr context;
} w32_io_completion_event;

#include "../os_win32.c"

#define OS_SMEM_NAME "Local\\ogl_beamformer_parameters"

#define OS_PATH_SEPERATOR '\\'

W32(b32) CreateDirectoryA(c8 *, void *);

function void
os_make_directory(char *name)
{
	CreateDirectoryA(name, 0);
}

function s8
os_read_file_simp(char *fname)
{
	s8 result;
	iptr h = CreateFileA(fname, GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);
	if (h == INVALID_FILE)
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

function void
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

function void
stream_push_u64_width(Stream *s, u64 n, u64 min_width)
{
	u8 tmp[64];
	u8 *end = tmp + sizeof(tmp);
	u8 *beg = end;
	min_width = MIN(sizeof(tmp), min_width);

	do { *--beg = '0' + (n % 10); } while (n /= 10);
	while (end - beg > 0 && end - beg < min_width)
		*--beg = '0';

	stream_append(s, beg, end - beg);
}

function void
stream_push_file_end_at_index(Stream *s, u32 index)
{
	stream_append_byte(s, '_');
	stream_push_u64_width(s, index, 2);
	stream_append_s8(s, s8(".zst"));
	stream_ensure_termination(s, 0);
}

function void *
decompress_zstd_data(s8 raw)
{
	iz requested_size = ZSTD_getFrameContentSize(raw.data, raw.len);
	void *out         = malloc(requested_size);
	if (out) {
		iz decompressed = ZSTD_decompress(out, requested_size, raw.data, raw.len);
		if (decompressed != requested_size) {
			free(out);
			out = 0;
		}
	}
	return out;
}

function void
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
	iz sidx = path.widx;

	stream_append_s8(&path, s8("_beamformed.bin"));
	stream_append_byte(&path, 0);

	s8 raw_data = {.len = points.x * points.y * points.z * 2 * sizeof(f32), .data = (u8 *)data};
	if (path.errors || !os_write_new_file((c8 *)path.data, raw_data)) {
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
	if (o.errors || !os_write_new_file((c8 *)path.data, stream_to_s8(&o))) {
		printf("failed to write output parameters: %s\n", (c8 *)path.data);
	} else {
		printf("wrote params to: %s\n", (c8 *)path.data);
	}
}

function zemp_bp_v1 *
read_zemp_bp_v1(u8 *path)
{
	s8 raw = os_read_file_simp((char *)path);
	zemp_bp_v1 *result = 0;
	if (raw.len == sizeof(zemp_bp_v1) && *(u64 *)raw.data == ZEMP_BP_MAGIC) {
		if (((zemp_bp_v1 *)raw.data)->version == 1)
			result = (zemp_bp_v1 *)raw.data;
	}
	return result;
}

function void
fill_beamformer_parameters_from_zemp_bp_v1(zemp_bp_v1 *zbp, BeamformerParametersV0 *out)
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

function void
usage(char *argv0)
{
	die("%s [--analytic] [--low-pass] [--export path] [--swap-plane] base_path\n"
	    "    --export  path:   export data to path\n"
	    "    --analytic:       use analytic signal for beamforming\n"
	    "    --low-pass:       apply low pass filter to input data\n"
	    "    --matched-filter: apply matched filtering to input data\n"
	    "    --swap-plane:     beamform the YZ plane\n",
	    argv0);
}

function b32
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

function Options
parse_argv(i32 argc, char *argv[])
{
	Options result = {0};

	char *argv0 = argv[0];
	shift(argv, argc);

	while (argc > 0) {
		s8 arg = c_str_to_s8(*argv);

		if (s8_equal(arg, s8("--analytic"))) {
			shift(argv, argc);
			result.analytic = 1;
		} else if (s8_equal(arg, s8("--low-pass"))) {
			shift(argv, argc);
			result.low_pass = 1;
		} else if (s8_equal(arg, s8("--matched-filter"))) {
			shift(argv, argc);
			result.matched_filter = 1;
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

function i16 *
decompress_data_at_work_index(Stream path_base, u32 index)
{
	stream_push_file_end_at_index(&path_base, index);
	s8 compressed_data = os_read_file_simp((c8 *)path_base.data);
	i16 *result = decompress_zstd_data(compressed_data);
	if (!result)
		die("failed to decompress data: %s\n", path_base.data);
	free(compressed_data.data);

	return result;
}

/* NOTE(rnp): I was going to write something fancy here but the function is so simple
 * that the compiler will generate the fast version with march=native and O >= 2 */
function void
convert_i16_to_f32_fast(i16 *restrict in, f32 *restrict out, uz elements)
{
	for (uz i = 0; i < elements; i++)
		out[i] = in[i];
}

global u32 channel_index;

typedef struct {
	f32 *input;
	fftwf_complex *fft, *ifft, *filter;
	fftwf_plan fplan, bplan;
	u32 filter_length;
} fftw_context;

typedef struct {
	u32 samples;
	f32 sampling_frequency;
	f32 transmit_frequency;
	f32 die_center_frequency;
	f32 low_pass_cutoff_frequency;
	i32 low_pass_filter_order;
	b32 analytic;
} fft_filter_context;

typedef struct {
	fftw_context fftw;
	u32 samples, channels, transmits;
	u32 channel_stride, channel_offset;
} work_thread_work;

function void
fftw_context_release(fftw_context *fftw)
{
	fftwf_destroy_plan(fftw->fplan);
	fftwf_destroy_plan(fftw->bplan);
	fftwf_free(fftw->input);
	fftwf_free(fftw->fft);
	fftwf_free(fftw->ifft);
	fftwf_free(fftw->filter);
}

function void
hadamard_product_1d(fftwf_complex *restrict a, fftwf_complex *restrict b, u32 n)
{
	for (u32 i = 0; i < n; i++) {
		f32 x = (a[i][0] * b[i][0] - a[i][1] * b[i][1]);
		f32 y = (a[i][0] * b[i][1] + a[i][1] * b[i][0]);
		a[i][0] = x;
		a[i][1] = y;
	}
}

function f32
fftw_create_filter(fftw_context *ctx, fft_filter_context filter_ctx)
{
	f32 result = 0;
	ctx->filter        = fftwf_alloc_complex(filter_ctx.samples);
	ctx->filter_length = filter_ctx.samples / 2;

	if (!ctx->filter)
		die("failed to allocate space for signal filter\n");

	ctx->filter[0][0] = 1;
	for (u32 i = 1; i < ctx->filter_length - 1; i++)
		ctx->filter[i][0] = 1 + filter_ctx.analytic;
	ctx->filter[ctx->filter_length - 1][0] = 1;

	f32           *f_in  = 0;
	fftwf_complex *f_out = 0;
	fftwf_plan     plan  = 0;

	f32 fs = filter_ctx.sampling_frequency;
	if (filter_ctx.transmit_frequency || filter_ctx.low_pass_cutoff_frequency) {
		f_in  = fftwf_alloc_real(filter_ctx.samples);
		f_out = fftwf_alloc_complex(filter_ctx.samples);
		plan  = fftwf_plan_dft_r2c_1d(filter_ctx.samples, f_in, f_out, FFTW_MEASURE);
		if (!f_in || !f_out)
			die("failed to allocate temporary space for filter\n");
	}

	if (filter_ctx.low_pass_cutoff_frequency) {
		i32 order    = filter_ctx.low_pass_filter_order;
		i32 midpoint = (order - 1) / 2;
		f32 wc = 2 * PI * filter_ctx.low_pass_cutoff_frequency / fs;
		for (i32 i = 0; i < order; i++) {
			f32 window = 0.5 - 0.5 * cosf(2 * PI * i / (order - 1));
			f32 value;
			if (i == midpoint) value = wc / PI;
			else               value = sinf(wc * (i - midpoint)) / (PI * (i - midpoint));
			f_in[i]  = value * window;
		}

		fftwf_execute_dft_r2c(plan, f_in, f_out);
		hadamard_product_1d(ctx->filter, f_out, ctx->filter_length);

		/* TODO(rnp): this is inaccurate for larger orders */
		result += (order - 1) / (4 * PI * filter_ctx.low_pass_cutoff_frequency);
	}

	if (filter_ctx.transmit_frequency) {
		mem_clear(f_in, 0, filter_ctx.samples);
		/* NOTE: impulse response - 1 cycle sine with hamming applied */
		i32 length = (fs / filter_ctx.die_center_frequency) + 1;
		f32 wc     = 2 * PI * filter_ctx.die_center_frequency / fs;
		for (i32 i = 0; i < length; i++) {
			f32 window = 0.5 - 0.5 * cosf(2 * PI * i / (length - 1));
			f32 value  = sinf(wc * i);
			f_in[i]    = value * window;
		}

		fftwf_execute_dft_r2c(plan, f_in, f_out);
		hadamard_product_1d(ctx->filter, f_out, ctx->filter_length);

		mem_clear(f_in, 0, length);

		/* TODO(rnp): refactor to include filtering for chirps */
		/* NOTE: transmit waveform - (2 cycle sine at transmit center frequency) */
		length = (i32)(2 * (fs / filter_ctx.transmit_frequency)) + 1;
		wc     = 2 * PI * filter_ctx.transmit_frequency / fs;
		for (i32 i = 0; i < length; i++)
			f_in[i] = sinf(wc * i);

		fftwf_execute_dft_r2c(plan, f_in, f_out);
		hadamard_product_1d(ctx->filter, f_out, ctx->filter_length);

		result += 1 / filter_ctx.transmit_frequency;
		//result += ((f32)length - 1) / (2 * fs);
	}

	fftwf_free(f_in);
	fftwf_free(f_out);
	fftwf_destroy_plan(plan);
	return result;
}

function void *
fft_thread(void *ctx_)
{
	work_thread_work ctx  = *(work_thread_work *)ctx_;
	fftw_context     fftw = ctx.fftw;
	for (;;) {
		#if THREAD_COUNT
		u32 channel = atomic_inc(&channel_index, 1);
		#else
		u32 channel = channel_index++;
		#endif
		if (channel >= ctx.channels)
			break;
		channel += ctx.channel_offset;

		fftwf_execute_dft_r2c(fftw.fplan, fftw.input + (channel * ctx.channel_stride),
		                      fftw.fft + (channel * ctx.channel_stride));

		if (fftw.filter_length) {
			ASSERT(fftw.filter_length == ctx.samples / 2);
			/* NOTE(rnp): dft_r2c won't actually compute anything for the second half
			 * of the array since the input was real (Hermetian Symmetry) - we don't
			 * need to zero it ourselves */
			u32 base_off = channel * ctx.channel_stride;
			for (u32 transmit = 0; transmit < ctx.transmits; transmit++) {
				u32 sample_off = transmit * ctx.samples;
				hadamard_product_1d(fftw.fft + base_off + sample_off,
				                    fftw.filter, fftw.filter_length);
			}
		}

		fftwf_execute_dft(fftw.bplan, fftw.fft + (channel * ctx.channel_stride),
		                  fftw.ifft + (channel * ctx.channel_stride));

		f32 scale = 1.0f / (f32)ctx.samples;
		for (u32 i = 0; i < ctx.channel_stride; i++) {
			fftw.ifft[channel * ctx.channel_stride + i][0] *= scale;
			fftw.ifft[channel * ctx.channel_stride + i][1] *= scale;
		}
	}
	return 0;
}

function void
run_fft_work(fftw_context *fftw, uv3 dec_data_dim, u32 channel_stride, u32 channel_offset)
{
	work_thread_work thread_ctx = {
		.fftw           = *fftw,
		.samples        = dec_data_dim.x,
		.channels       = dec_data_dim.y,
		.transmits      = dec_data_dim.z,
		.channel_stride = channel_stride,
		.channel_offset = channel_offset,
	};

	channel_index = 0;

	#ifdef TIME_FFT
	clock_t start = clock();
	#endif
	#if THREAD_COUNT
	pthread_t threads[THREAD_COUNT];
	for (u32 i = 0; i < THREAD_COUNT; i++)
		pthread_create(threads + i, 0, fft_thread, &thread_ctx);
	#endif

	fft_thread(&thread_ctx);

	#if THREAD_COUNT
	void *out;
	for (u32 i = 0; i < THREAD_COUNT; i++)
		pthread_join(threads[i], &out);
	#endif
	#ifdef TIME_FFT
	clock_t end = clock();
	printf("run_fft_work: took: %0.02f [ms]\n", 1000.0f * (f32)(end - start) / (f32)(CLOCKS_PER_SEC));
	#endif
}

function b32
send_frame(i16 *restrict i16_data, fftw_context *restrict fftw, f32 *restrict out_buf,
           BeamformerParametersV0 *restrict bp, b32 export)
{
	b32 result = 0;
	void *data = i16_data;
	u32   data_size = bp->rf_raw_dim.x * bp->rf_raw_dim.y * sizeof(i16);
	if (fftw) {
		u32 elements = bp->rf_raw_dim.x * bp->rf_raw_dim.y;
		convert_i16_to_f32_fast(i16_data, fftw->input, elements);
		run_fft_work(fftw, bp->dec_data_dim.xyz, bp->rf_raw_dim.x,
		             bp->dec_data_dim.y * ((bp->transmit_mode & 1u) == 0));
		data      = (f32 *)fftw->ifft;
		data_size = elements * 2 * sizeof(f32);
	}
	if (export) {
		result = beamform_data_synchronized(0, OS_SMEM_NAME, data, data_size,
		                                    bp->output_points, out_buf, 100000);
	} else {
		result = send_data(0, OS_SMEM_NAME, data, data_size);
	}
	return result;
}

function b32
execute_work_item(WorkFrame *w, Arena arena, Stream path, Options *options, f32 *out_data, b32 export)
{
	stream_append_s8(&path, w->study);
	stream_ensure_termination(&path, OS_PATH_SEPERATOR);
	stream_append_s8(&path, w->study);
	iz path_work_index = path.widx;

	stream_append_s8(&path, s8(".bp"));
	stream_ensure_termination(&path, 0);

	zemp_bp_v1 *zbp = read_zemp_bp_v1(path.data);
	if (!zbp) die("failed to unpack parameters file\n");

	BeamformerParametersV0 bp = {0};
	fill_beamformer_parameters_from_zemp_bp_v1(zbp, &bp);
	free(zbp);

	if (!uv3_equal(w->settings.output_points.xyz, (uv3){0}))
		bp.output_points = w->settings.output_points;
	else
		bp.output_points = g_output_points;
	bp.output_min_coordinate = (v4){.x = w->settings.lateral_extent.x, .z = w->settings.axial_extent.x};
	bp.output_max_coordinate = (v4){.x = w->settings.lateral_extent.y, .z = w->settings.axial_extent.y};
	bp.f_number              = w->settings.f_number;
	bp.beamform_plane        = options->beamform_plane;
	bp.interpolate           = 1;

	b32 do_fft = options->analytic || options->low_pass || options->matched_filter;
	fftw_context *fftw = 0;
	if (do_fft) {
		fftw = push_struct(&arena, fftw_context);

		iz total_samples = bp.rf_raw_dim.x * bp.rf_raw_dim.y;
		fftw->input  = fftwf_alloc_real(total_samples);
		fftw->fft    = fftwf_alloc_complex(total_samples);
		fftw->ifft   = fftwf_alloc_complex(total_samples);

		if (!fftw->input || !fftw->fft || !fftw->ifft)
			die("failed to alloc space for converted f32 input data\n");

		fft_filter_context filter = {
			.samples            = bp.dec_data_dim.x,
			.sampling_frequency = bp.sampling_frequency,
			.analytic           = options->analytic,
		};

		if (options->low_pass) {
			filter.low_pass_cutoff_frequency = w->settings.low_pass_cutoff_frequency;
			filter.low_pass_filter_order     = w->settings.low_pass_filter_order;
		}

		if (options->matched_filter) {
			filter.transmit_frequency   = bp.center_frequency;
			filter.die_center_frequency = bp.center_frequency;
		}

		bp.time_offset += fftw_create_filter(fftw, filter);
		fftw->fplan = fftwf_plan_many_dft_r2c(1, (i32 []){bp.dec_data_dim.x},
		                                      bp.dec_data_dim.z,
		                                      fftw->input, 0, 1, bp.dec_data_dim.x,
		                                      fftw->fft,   0, 1, bp.dec_data_dim.x,
		                                      FFTW_MEASURE);
		fftw->bplan = fftwf_plan_many_dft(1, (i32 []){bp.dec_data_dim.x},
		                                  bp.dec_data_dim.z,
		                                  fftw->fft,  0, 1, bp.dec_data_dim.x,
		                                  fftw->ifft, 0, 1, bp.dec_data_dim.x,
		                                  FFTW_BACKWARD, FFTW_MEASURE);
	}

	i32 valid_frame_indices[LAST_SAVED_FRAME_NUMBER + 1];
	i32 valid_frames = 0;
	for (i32 frame = 0; frame <= LAST_SAVED_FRAME_NUMBER; frame++) {
		stream_reset(&path, path_work_index);
		stream_push_file_end_at_index(&path, frame);
		if (os_file_exists((c8 *)path.data))
			valid_frame_indices[valid_frames++] = frame;
	}

	bp.output_points.w = MIN(valid_frames, w->settings.frame_count);

	set_beamformer_parameters(OS_SMEM_NAME, &bp);

	i32 shader_stages[16];
	i32 shader_stage_count = 0;
	if (do_fft) shader_stages[shader_stage_count++] = CS_DECODE_FLOAT_COMPLEX;
	else        shader_stages[shader_stage_count++] = CS_DECODE;
	shader_stages[shader_stage_count++] = CS_DAS;
	if (bp.output_points.w > 1) shader_stages[shader_stage_count++] = CS_SUM;

	set_beamformer_pipeline(OS_SMEM_NAME, shader_stages, shader_stage_count);

	stream_reset(&path, path_work_index);
	for (i32 frame = 0; frame < (i32)bp.output_points.w - 1; frame++) {
		i16 *data = decompress_data_at_work_index(path, valid_frame_indices[frame]);
		send_frame(data, fftw, out_data, &bp, 0);
		free(data);
	}

	i16 *data = decompress_data_at_work_index(path, bp.output_points.w - 1);
	b32 result = send_frame(data, fftw, out_data, &bp, export);
	if (export && result)
		write_output_data(options, w->settings.output_path, w->study, out_data,
		                  bp.output_points, bp.output_min_coordinate, bp.output_max_coordinate);
	if (fftw) fftw_context_release(fftw);
	free(data);

	return result;
}

function void
work_loop(Options *options, s8 path_base)
{
	Arena arena = os_alloc_arena((Arena){0}, KB(8));
	Stream path = stream_alloc(&arena, KB(4));
	stream_append_s8(&path, path_base);
	stream_ensure_termination(&path, OS_PATH_SEPERATOR);

	f32 *out_data;
	if (options->export) {
		iz out_size = g_output_points.x * g_output_points.y * g_output_points.z * 2 * sizeof(f32);
		out_data    = malloc(out_size);
		if (!out_data)
			die("failed to allocate space for exported frame\n");
	}

	b32 exit = 1;
	do {
		for (i32 i = 0; i < ARRAY_COUNT(work); i++) {
			WorkFrame *w = work + i;
			fprintf(stderr, "showing: %.*s\n", (i32)w->study.len, w->study.data);
			b32 result = execute_work_item(w, arena, path, options, out_data, options->export);

			if (options->export && !result)
				fprintf(stderr, "failed to export. will retry...\n");
			if (!options->export) {
				fprintf(stderr, "press enter to continue...");
				exit = fgetc(stdin) == EOF;
			}
		}
	} while (!exit);
}

int
main(i32 argc, char *argv[])
{
	Options options = parse_argv(argc, argv);

	if (options.remaining_count != 1)
		usage(argv[0]);

	work_loop(&options, c_str_to_s8(options.remaining[0]));

	return 0;
}
