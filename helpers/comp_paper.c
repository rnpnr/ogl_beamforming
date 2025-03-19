#include "../util.h"
#include "../beamformer_parameters.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <fftw3.h>
#include <zstd.h>

//#define TIME_HILBERT
/* NOTE: main thread will also do work */
#define THREAD_COUNT (1 - 1)
#if THREAD_COUNT
#include <pthread.h>
#endif

static uv4 g_output_points = {.x = 512, .y = 1, .z = 1024, .w = 1};

typedef struct {
	s8  output_path;
	uv4 output_points;
	v2  axial_extent;
	v2  lateral_extent;
	i32 frame_count;
	f32 f_number;
} WorkGroupSettings;

typedef struct {
	s8 study;
	WorkGroupSettings settings;
} WorkFrame;

#define low_freq_res_settings (WorkGroupSettings){ \
	.output_path    = s8("low_freq_res"),         \
	.axial_extent   = {.x =  5e-3,  .y = 170e-3}, \
	.lateral_extent = {.x = -50e-3, .y = 50e-3},  \
	.frame_count    = 8,                          \
	.f_number       = 0.5,                        \
}

#define low_freq_cyst_settings (WorkGroupSettings){ \
	.output_path    = s8("low_freq_cysts"),       \
	.axial_extent   = {.x =  10e-3, .y = 165e-3}, \
	.lateral_extent = {.x = -60e-3, .y = 60e-3},  \
	.frame_count    = 8,                          \
	.f_number       = 0.5,                        \
}

#define low_freq_contrast_settings (WorkGroupSettings){ \
	.output_points  = {{256, 1, 128, 1}},          \
	.output_path    = s8("low_freq_contrast"),     \
	.axial_extent   = {.x =  91e-3, .y = 108e-3},  \
	.lateral_extent = {.x = 2.7e-3, .y = 20.7e-3}, \
	.frame_count    = 8,                           \
	.f_number       = 1,                           \
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
	.frame_count    = 1,                             \
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

static WorkFrame work[] = {
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
	//{s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxRow"),        high_freq_cyst_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxColumn"),     high_freq_cyst_settings},
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

	{s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxRow"),        high_freq_contrast_settings},
	{s8("250319_A06_7.80MHz_ATS539_Cyst_TPW-128-TxColumn"),    high_freq_contrast_settings},
	{s8("250319_A06_7.80MHz_ATS539_Cyst_VLS-128-TxColumn"),    high_freq_contrast_settings},
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
stream_malloc(iz capacity)
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
	iz sidx = path.widx;

	stream_append_s8(&path, s8("_beamformed.bin"));
	stream_append_byte(&path, 0);

	iz out_size = points.x * points.y * points.z * 2 * sizeof(f32);
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

/* NOTE(rnp): I was going to write something fancy here but the function is so simple
 * that the compiler will generate the fast version with march=native and O >= 2 */
static void
convert_i16_to_f32_fast(i16 *restrict in, f32 *restrict out, uz elements)
{
	for (uz i = 0; i < elements; i++)
		out[i] = in[i];
}

static u32 channel_index;
struct work_thread_work {
	f32 *in;
	fftwf_complex *fft, *ifft;
	fftwf_plan fplan, bplan;
	u32 samples, channels, transmits;
	u32 channel_stride, channel_offset;
};

static void *
fftw_hilbert_thread(void *ctx_)
{
	struct work_thread_work ctx = *(struct work_thread_work *)ctx_;
	for (;;) {
		#if THEAD_COUNT
		u32 channel = atomic_inc(&channel_index, 1);
		#else
		u32 channel = channel_index++;
		#endif
		if (channel >= ctx.channels)
			break;
		channel += ctx.channel_offset;

		fftwf_execute_dft_r2c(ctx.fplan, ctx.in + (channel * ctx.channel_stride),
		                      ctx.fft + (channel * ctx.channel_stride));

		/* NOTE(rnp): dft_r2c won't actually compute anything for the second half
		 * of the array since the input was real (Hermetian Symmetry) - we don't
		 * need to zero it ourselves */
		for (u32 transmit = 0; transmit < ctx.transmits; transmit++) {
			for (u32 sample = 1; sample < ctx.samples / 2; sample++) {
				u32 index = (transmit * ctx.channels + channel) * ctx.samples + sample;
				ctx.fft[index][0] *= 2;
				ctx.fft[index][1] *= 2;
			}
		}

		fftwf_execute_dft(ctx.bplan,ctx.fft + (channel * ctx.channel_stride),
		                  ctx.ifft + (channel * ctx.channel_stride));

		f32 scale = 1 / (f32)ctx.samples;
		for (u32 i = 0; i < ctx.channel_stride; i++) {
			ctx.ifft[channel * ctx.channel_stride + i][0] *= scale;
			ctx.ifft[channel * ctx.channel_stride + i][1] *= scale;
		}
	}
	return 0;
}

static void
fftw_hilbert(fftwf_plan fplan, fftwf_plan bplan, f32 *restrict in, fftwf_complex *restrict fft,
             fftwf_complex *restrict ifft, uv3 dec_data_dim, u32 channel_stride, u32 channel_offset)
{
	struct work_thread_work thread_ctx = {
		.in = in, .fft = fft, .ifft = ifft,
		.fplan = fplan, .bplan = bplan,
		.samples        = dec_data_dim.x,
		.channels       = dec_data_dim.y,
		.transmits      = dec_data_dim.z,
		.channel_stride = channel_stride,
		.channel_offset = channel_offset,
	};

	channel_index = 0;

	#ifdef TIME_HILBERT
	clock_t start = clock();
	#endif
	#if THREAD_COUNT
	pthread_t threads[THREAD_COUNT];
	for (u32 i = 0; i < THREAD_COUNT; i++)
		pthread_create(threads + i, 0, fftw_hilbert_thread, &thread_ctx);
	#endif

	fftw_hilbert_thread(&thread_ctx);

	#if THREAD_COUNT
	void *out;
	for (u32 i = 0; i < THREAD_COUNT; i++)
		pthread_join(threads[i], &out);
	#endif
	#ifdef TIME_HILBERT
	clock_t end = clock();
	printf("fftw_hilbert: took: %0.02f [ms]\n", 1000.0f * (f32)(end - start) / (f32)(CLOCKS_PER_SEC));
	#endif
}

static b32
send_frame(i16 *restrict i16_data, f32 *restrict f32_buf, f32 *restrict fft_buf,
           f32 *restrict ifft_buf, f32 *restrict out_buf, BeamformerParameters *restrict bp,
           Options *restrict options, fftwf_plan fft_plan, fftwf_plan ifft_plan, b32 export)
{
	b32 result = 0;
	void *data = i16_data;
	u32   data_size = bp->rf_raw_dim.x * bp->rf_raw_dim.y * sizeof(i16);
	if (options->hilbert) {
		u32 elements = bp->rf_raw_dim.x * bp->rf_raw_dim.y;
		convert_i16_to_f32_fast(i16_data, f32_buf, elements);
		fftw_hilbert(fft_plan, ifft_plan, f32_buf, (fftwf_complex *)fft_buf,
		             (fftwf_complex *)ifft_buf, bp->dec_data_dim.xyz, bp->rf_raw_dim.x,
		             bp->dec_data_dim.y * ((bp->transmit_mode & 1u) == 0));
		data      = ifft_buf;
		data_size = elements * 2 * sizeof(f32);
	}
	if (export) {
		result = beamform_data_synchronized(OS_PIPE_NAME, OS_SMEM_NAME, data, data_size,
		                                    bp->output_points, out_buf, 100000);
	} else {
		result = send_data(OS_PIPE_NAME, OS_SMEM_NAME, data, data_size);
	}
	return result;
}

static b32
execute_work_item(WorkFrame *w, Stream path, Options *options, f32 *out_data, u32 average_frames,
                  u32 frame_off, b32 export)
{
	stream_append_s8(&path, w->study);
	stream_ensure_termination(&path, OS_PATH_SEPERATOR);
	stream_append_s8(&path, w->study);
	iz path_work_index = path.widx;

	stream_append_s8(&path, s8(".bp"));
	stream_ensure_termination(&path, 0);

	zemp_bp_v1 *zbp = read_zemp_bp_v1(path.data);
	if (!zbp) die("failed to unpack parameters file\n");

	BeamformerParameters bp = {0};
	fill_beamformer_parameters_from_zemp_bp_v1(zbp, &bp);
	free(zbp);

	if (!uv3_equal(w->settings.output_points.xyz, (uv3){0}))
		bp.output_points = w->settings.output_points;
	else
		bp.output_points = g_output_points;
	bp.output_points.w       = average_frames;
	bp.output_min_coordinate = (v4){.x = w->settings.lateral_extent.x, .z = w->settings.axial_extent.x};
	bp.output_max_coordinate = (v4){.x = w->settings.lateral_extent.y, .z = w->settings.axial_extent.y};
	bp.f_number              = w->settings.f_number;
	bp.beamform_plane        = options->beamform_plane;
	bp.interpolate           = 1;

	f32 *f32_input       = 0;
	f32 *f32_transformed = 0;
	f32 *f32_complex     = 0;
	fftwf_plan fft_plan = 0, ifft_plan = 0;
	if (options->hilbert) {
		iz size = bp.rf_raw_dim.x * bp.rf_raw_dim.y * sizeof(f32);
		f32_input       = fftwf_malloc(size);
		f32_transformed = fftwf_malloc(2 * size);
		f32_complex     = fftwf_malloc(2 * size);
		if (!f32_input || !f32_complex)
			die("failed to alloc space for converted f32 input data\n");

		fftwf_complex *f32_ifft = (fftwf_complex *)f32_complex;
		fftwf_complex *f32_fft  = (fftwf_complex *)f32_transformed;
		fft_plan = fftwf_plan_many_dft_r2c(1, (i32 []){bp.dec_data_dim.x},
		                                   bp.dec_data_dim.z,
		                                   f32_input, 0, 1, bp.dec_data_dim.x,
		                                   f32_fft,   0, 1, bp.dec_data_dim.x,
		                                   FFTW_MEASURE);
		ifft_plan = fftwf_plan_many_dft(1, (i32 []){bp.dec_data_dim.x},
		                                bp.dec_data_dim.z,
		                                f32_fft,  0, 1, bp.dec_data_dim.x,
		                                f32_ifft, 0, 1, bp.dec_data_dim.x,
		                                FFTW_BACKWARD, FFTW_MEASURE);
	}

	set_beamformer_parameters(OS_SMEM_NAME, &bp);

	i32 shader_stages[16];
	i32 shader_stage_count = 0;
	if (options->hilbert) shader_stages[shader_stage_count++] = CS_DECODE_FLOAT_COMPLEX;
	else                  shader_stages[shader_stage_count++] = CS_DECODE;
	shader_stages[shader_stage_count++] = CS_DAS;
	if (average_frames > 1) shader_stages[shader_stage_count++] = CS_SUM;

	set_beamformer_pipeline(OS_SMEM_NAME, shader_stages, shader_stage_count);

	for (i32 frame = 0; frame < w->settings.frame_count - 1; frame++) {
		stream_reset(&path, path_work_index);
		i16 *data = decompress_data_at_work_index(&path, (u32)frame + frame_off);
		send_frame(data, f32_input, f32_transformed, f32_complex, out_data, &bp,
		           options, fft_plan, ifft_plan, 0);
		free(data);
	}

	stream_reset(&path, path_work_index);
	i16 *data = decompress_data_at_work_index(&path, w->settings.frame_count - 1 + frame_off);
	b32 result = send_frame(data, f32_input, f32_transformed, f32_complex, out_data,
	                        &bp, options, fft_plan, ifft_plan, export);
	if (export && result)
		write_output_data(options, w->settings.output_path, w->study, out_data,
		                  bp.output_points, bp.output_min_coordinate, bp.output_max_coordinate);
	if (fft_plan)  fftwf_destroy_plan(fft_plan);
	if (ifft_plan) fftwf_destroy_plan(ifft_plan);
	free(f32_input);
	free(f32_transformed);
	free(f32_complex);
	free(data);

	return result;
}

static void
work_loop(Options *options, s8 path_base)
{
	Stream path = stream_malloc(KB(4));
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
			b32 result = execute_work_item(w, path, options, out_data,
			                               w->settings.frame_count, 0, options->export);

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

	if (options.remaining_count > 1)
		usage(argv[0]);

	work_loop(&options, c_str_to_s8(options.remaining[0]));

	return 0;
}
