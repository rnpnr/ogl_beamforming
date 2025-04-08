/* TODO(rnp):
 * [ ]: for finer grained evaluation of throughput latency just queue a data upload
 *      without replacing the data.
 */

#include "ogl_beamformer_lib.c"

#define NEW_PUSH_API 1
#define PARAMS_TYPE  BeamformerParametersV0

#include <stdarg.h>
#include <stdio.h>
#include <zstd.h>

global u32 g_output_points[4] = {512, 1, 1024, 1};

typedef struct {
	s8  output_path;
	uv4 output_points;
	v2  axial_extent;
	v2  lateral_extent;
	f32 f_number;
} WorkGroupSettings;

typedef struct {
	s8 study;
	WorkGroupSettings settings;
} WorkFrame;

global WorkFrame work = {
	.settings = {
		.axial_extent   = {.x =  10e-3, .y = 165e-3},
		.lateral_extent = {.x = -60e-3, .y = 60e-3},
		.f_number       = 0.5,
	}
};

typedef struct {
	b32 loop;
	b32 cuda;
	u32 frame_number;

	char **remaining;
	i32    remaining_count;
} Options;

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

//#include "../os_unix.c"

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#define OS_PIPE_NAME "/tmp/beamformer_data_fifo"
#define OS_SMEM_NAME "/ogl_beamformer_shared_memory"

#define OS_PATH_SEPERATOR '/'

function void os_init_timer(void) { }

function f64
os_get_time(void)
{
	struct timespec time = {0};
	clock_gettime(CLOCK_MONOTONIC, &time);
	f64 result = time.tv_sec + ((f64)(time.tv_nsec)) * 1e-9;
	return result;
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

global w32_context os_context;

#define OS_PIPE_NAME "\\\\.\\pipe\\beamformer_data_fifo"
#define OS_SMEM_NAME "Local\\ogl_beamformer_parameters"

#define OS_PATH_SEPERATOR '\\'

W32(b32) QueryPerformanceFrequency(u64 *);
W32(b32) QueryPerformanceCounter(u64 *);

function void
os_init_timer(void)
{
	QueryPerformanceFrequency(&os_context.timer_frequency);
	QueryPerformanceCounter(&os_context.timer_start_time);
}

function f64
os_get_time(void)
{
	u64 current;
	QueryPerformanceCounter(&current);
	f64 result = (f64)(current - os_context.timer_start_time) / os_context.timer_frequency;
	return result;
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
fill_beamformer_parameters_from_zemp_bp_v1(zemp_bp_v1 *zbp, PARAMS_TYPE *out)
{
	mem_copy(out->channel_mapping,   zbp->channel_mapping,   sizeof(out->channel_mapping));
	mem_copy(out->focal_depths,      zbp->focal_depths,      sizeof(out->focal_depths));
	mem_copy(out->transmit_angles,   zbp->transmit_angles,   sizeof(out->transmit_angles));
	mem_copy(out->xdc_transform,     zbp->xdc_transform,     sizeof(out->xdc_transform));
	mem_copy(out->dec_data_dim,      zbp->decoded_data_dim,  sizeof(out->dec_data_dim));
	mem_copy(out->xdc_element_pitch, zbp->xdc_element_pitch, sizeof(out->xdc_element_pitch));
	mem_copy(out->rf_raw_dim,        zbp->raw_data_dim,      sizeof(out->rf_raw_dim));

	if (zbp->sparse_elements[0] == -1) {
		for (u32 i = 0; i < zbp->decoded_data_dim[2]; i++)
			out->uforces_channels[i] = i;
	} else {
		mem_copy(out->uforces_channels, zbp->sparse_elements, sizeof(out->uforces_channels));
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
	die("%s [--loop] [--cuda] [--frame n] base_path study\n"
	    "    --loop:    reupload data forever\n"
	    "    --cuda:    use cuda for decoding\n"
	    "    --frame n: use frame n of the data for display\n",
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

		if (s8_equal(arg, s8("--loop"))) {
			shift(argv, argc);
			result.loop = 1;
		} else if (s8_equal(arg, s8("--cuda"))) {
			shift(argv, argc);
			result.cuda = 1;
		} else if (s8_equal(arg, s8("--frame"))) {
			shift(argv, argc);
			if (argc) {
				result.frame_number = atoi(*argv);
				shift(argv, argc);
			}
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
decompress_data_at_work_index(Stream *path_base, u32 index)
{
	stream_append_byte(path_base, '_');
	stream_append_u64_width(path_base, index, 2);
	stream_append_s8(path_base, s8(".zst"));
	stream_ensure_termination(path_base, 0);

	s8 compressed_data = os_read_file_simp((char *)path_base->data);
	i16 *result = decompress_zstd_data(compressed_data);
	if (!result)
		die("failed to decompress data: %s\n", path_base->data);
	free(compressed_data.data);

	return result;
}

function b32
send_frame(i16 *restrict i16_data, PARAMS_TYPE *restrict bp)
{
	b32 result    = 0;
	u32 data_size = bp->rf_raw_dim[0] * bp->rf_raw_dim[1] * sizeof(i16);

	#if NEW_PUSH_API
	if (beamformer_push_data(i16_data, data_size, 0))
		result = beamformer_start_compute(IPT_XZ);
	#else
	result = send_data(i16_data, data_size);
	#endif

	return result;
}

function void
execute_work_item(WorkFrame *w, Arena arena, Stream path, Options *options)
{
	stream_append_s8(&path, w->study);
	stream_ensure_termination(&path, OS_PATH_SEPERATOR);
	stream_append_s8(&path, w->study);
	iz path_work_index = path.widx;

	stream_append_s8(&path, s8(".bp"));
	stream_ensure_termination(&path, 0);

	zemp_bp_v1 *zbp = read_zemp_bp_v1(path.data);
	if (!zbp) die("failed to unpack parameters file\n");

	PARAMS_TYPE bp = {0};
	fill_beamformer_parameters_from_zemp_bp_v1(zbp, &bp);
	free(zbp);

	if (!uv3_equal(w->settings.output_points.xyz, (uv3){0}))
		mem_copy(bp.output_points, w->settings.output_points.E, sizeof(bp.output_points));
	else
		mem_copy(bp.output_points, g_output_points, sizeof(bp.output_points));
	bp.output_points[3] = 1;

	bp.output_min_coordinate[0] = w->settings.lateral_extent.x;
	bp.output_min_coordinate[1] = 0;
	bp.output_min_coordinate[2] = w->settings.axial_extent.x;
	bp.output_min_coordinate[3] = 0;

	bp.output_max_coordinate[0] = w->settings.lateral_extent.y;
	bp.output_max_coordinate[1] = 0;
	bp.output_max_coordinate[2] = w->settings.axial_extent.y;
	bp.output_max_coordinate[3] = 0;

	bp.f_number       = w->settings.f_number;
	bp.beamform_plane = 0;
	bp.interpolate    = 0;

	set_beamformer_parameters(&bp);

	i32 shader_stages[16];
	i32 shader_stage_count = 0;
	if (options->cuda) shader_stages[shader_stage_count++] = CS_CUDA_DECODE;
	else               shader_stages[shader_stage_count++] = CS_DECODE;
	shader_stages[shader_stage_count++] = CS_DAS;

	set_beamformer_pipeline(shader_stages, shader_stage_count);

	stream_reset(&path, path_work_index);

	i16 *data = decompress_data_at_work_index(&path, options->frame_number);

	if (options->loop) {
		u32 frame = 0;
		f32 times[32] = {0};
		f32 data_size = bp.rf_raw_dim[0] * bp.rf_raw_dim[1] * sizeof(*data);
		f64 start  = os_get_time();
		for (;;) {
			if (send_frame(data, &bp)) {
				f64 now   = os_get_time();
				f32 delta = now - start;
				start = now;

				if ((frame % 16) == 0) {
					f32 sum = 0;
					for (u32 i = 0; i < ARRAY_COUNT(times); i++)
						sum += times[i] / ARRAY_COUNT(times);
					printf("Frame Time: %8.3f [ms] | 32-Frame Average: %8.3f [ms] | %8.3f GB/s\n",
					       delta * 1e3, sum * 1e3, data_size / (sum * (GB(1))));
				}

				times[frame & 31] = delta;
				frame++;
			}
		}
	} else {
		send_frame(data, &bp);
	}

	free(data);
}

function void
work_loop(Options *options, s8 path_base, s8 study)
{
	Arena arena = os_alloc_arena((Arena){0}, KB(8));
	Stream path = stream_alloc(&arena, KB(4));
	stream_append_s8(&path, path_base);
	stream_ensure_termination(&path, OS_PATH_SEPERATOR);

	work.study = study;
	fprintf(stderr, "showing: %.*s\n", (i32)study.len, study.data);
	execute_work_item(&work, arena, path, options);
}

int
main(i32 argc, char *argv[])
{
	Options options = parse_argv(argc, argv);

	if (!BETWEEN(options.remaining_count, 1, 2))
		usage(argv[0]);

	os_init_timer();

	work_loop(&options, c_str_to_s8(options.remaining[0]), c_str_to_s8(options.remaining[1]));

	return 0;
}
