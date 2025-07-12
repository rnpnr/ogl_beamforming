/* See LICENSE for license details. */
/* TODO(rnp):
 * [ ]: for finer grained evaluation of throughput latency just queue a data upload
 *      without replacing the data.
 * [ ]: bug: we aren't inserting rf data between each frame
 */

#define LIB_FN function
#include "ogl_beamformer_lib.c"

#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <zstd.h>

global i32 g_output_points[4] = {512, 1, 1024, 1};
global v2  g_axial_extent     = {{ 10e-3f, 165e-3f}};
global v2  g_lateral_extent   = {{-60e-3f,  60e-3f}};
global f32 g_f_number         = 0.5f;

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

global b32 g_should_exit;

#define die(...) die_((char *)__func__, __VA_ARGS__)
function no_return void
die_(char *function_name, char *format, ...)
{
	if (function_name)
		fprintf(stderr, "%s: ", function_name);

	va_list ap;

	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);

	os_exit(1);
}

#if OS_LINUX

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

function void os_init_timer(void) { }

function f64
os_get_time(void)
{
	f64 result = (f64)os_get_timer_counter() / (f64)os_get_timer_frequency();
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
	result.data = malloc((uz)st.st_size);
	if (!result.data)
		die("couldn't alloc space for reading\n");

	iz rlen = read(fd, result.data, (u32)st.st_size);
	close(fd);

	if (rlen != st.st_size)
		die("couldn't read file: %s\n", fname);

	return result;
}

#elif OS_WINDOWS

global os_w32_context os_context;

function void
os_init_timer(void)
{
	os_context.timer_frequency = os_get_timer_frequency();
}

function f64
os_get_time(void)
{
	f64 result = (f64)os_get_timer_counter() / (f64)os_context.timer_frequency;
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
	if (!ReadFile(h, result.data, (i32)fileinfo.nFileSizeLow, &rlen, 0) && rlen != (i32)fileinfo.nFileSizeLow)
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

function void *
decompress_zstd_data(s8 raw)
{
	uz requested_size = ZSTD_getFrameContentSize(raw.data, (uz)raw.len);
	void *out         = malloc(requested_size);
	if (out) {
		uz decompressed = ZSTD_decompress(out, requested_size, raw.data, (uz)raw.len);
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
fill_beamformer_parameters_from_zemp_bp_v1(zemp_bp_v1 *zbp, BeamformerParameters *out)
{
	mem_copy(out->xdc_transform,     zbp->xdc_transform,     sizeof(out->xdc_transform));
	mem_copy(out->dec_data_dim,      zbp->decoded_data_dim,  sizeof(out->dec_data_dim));
	mem_copy(out->xdc_element_pitch, zbp->xdc_element_pitch, sizeof(out->xdc_element_pitch));
	mem_copy(out->rf_raw_dim,        zbp->raw_data_dim,      sizeof(out->rf_raw_dim));

	out->transmit_mode      = (i32)zbp->transmit_mode;
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
	for (iz i = 0; result && i < a.len; i++)
		result &= a.data[i] == b.data[i];
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
				result.frame_number = (u32)atoi(*argv);
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
send_frame(i16 *restrict i16_data, BeamformerParameters *restrict bp)
{
	b32 result    = 0;
	u32 data_size = bp->rf_raw_dim[0] * bp->rf_raw_dim[1] * sizeof(i16);
	if (beamformer_wait_for_compute_dispatch(10000))
		result = beamformer_push_data_with_compute(i16_data, data_size, BeamformerViewPlaneTag_XZ, 100);
	if (!result && !g_should_exit) printf("lib error: %s\n", beamformer_get_last_error_string());

	return result;
}

function void
execute_study(s8 study, Arena arena, Stream path, Options *options)
{
	fprintf(stderr, "showing: %.*s\n", (i32)study.len, study.data);

	stream_append_s8(&path, study);
	stream_ensure_termination(&path, OS_PATH_SEPARATOR_CHAR);
	stream_append_s8(&path, study);
	i32 path_work_index = path.widx;

	stream_append_s8(&path, s8(".bp"));
	stream_ensure_termination(&path, 0);

	zemp_bp_v1 *zbp = read_zemp_bp_v1(path.data);
	if (!zbp) die("failed to unpack parameters file\n");

	BeamformerParameters bp = {0};
	fill_beamformer_parameters_from_zemp_bp_v1(zbp, &bp);

	mem_copy(bp.output_points, g_output_points, sizeof(bp.output_points));
	bp.output_points[3] = 1;

	bp.output_min_coordinate[0] = g_lateral_extent.x;
	bp.output_min_coordinate[1] = 0;
	bp.output_min_coordinate[2] = g_axial_extent.x;
	bp.output_min_coordinate[3] = 0;

	bp.output_max_coordinate[0] = g_lateral_extent.y;
	bp.output_max_coordinate[1] = 0;
	bp.output_max_coordinate[2] = g_axial_extent.y;
	bp.output_max_coordinate[3] = 0;

	bp.f_number       = g_f_number;
	bp.beamform_plane = 0;
	bp.interpolate    = 0;

	if (zbp->sparse_elements[0] == -1) {
		for (i16 i = 0; i < countof(zbp->sparse_elements); i++)
			zbp->sparse_elements[i] = i;
	}

	{
		align_as(64) v2 focal_vectors[countof(zbp->focal_depths)];
		for (u32 i = 0; i < countof(zbp->focal_depths); i++)
			focal_vectors[i] = (v2){{zbp->transmit_angles[i], zbp->focal_depths[i]}};
		beamformer_push_focal_vectors((f32 *)focal_vectors, countof(focal_vectors), 0);
	}

	beamformer_push_channel_mapping(zbp->channel_mapping, countof(zbp->channel_mapping), 0);
	beamformer_push_sparse_elements(zbp->sparse_elements, countof(zbp->sparse_elements), 0);
	beamformer_push_parameters(&bp, 0);

	free(zbp);

	i32 shader_stages[16];
	u32 shader_stage_count = 0;
	if (options->cuda) shader_stages[shader_stage_count++] = BeamformerShaderKind_CudaDecode;
	else               shader_stages[shader_stage_count++] = BeamformerShaderKind_Decode;
	shader_stages[shader_stage_count++] = BeamformerShaderKind_DASCompute;

	set_beamformer_pipeline(shader_stages, shader_stage_count);

	stream_reset(&path, path_work_index);
	i16 *data = decompress_data_at_work_index(&path, options->frame_number);

	if (options->loop) {
		u32 frame = 0;
		f32 times[32] = {0};
		f32 data_size = (f32)(bp.rf_raw_dim[0] * bp.rf_raw_dim[1] * sizeof(*data));
		f64 start = os_get_time();
		for (;!g_should_exit;) {
			if (send_frame(data, &bp)) {
				f64 now   = os_get_time();
				f32 delta = (f32)(now - start);
				start = now;

				if ((frame % 16) == 0) {
					f32 sum = 0;
					for (u32 i = 0; i < countof(times); i++)
						sum += times[i] / countof(times);
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
sigint(i32 _signo)
{
	g_should_exit = 1;
}

extern i32
main(i32 argc, char *argv[])
{
	Options options = parse_argv(argc, argv);

	if (!BETWEEN(options.remaining_count, 1, 2))
		usage(argv[0]);

	os_init_timer();

	signal(SIGINT, sigint);

	Arena arena = os_alloc_arena(KB(8));
	Stream path = stream_alloc(&arena, KB(4));
	stream_append_s8(&path, c_str_to_s8(options.remaining[0]));
	stream_ensure_termination(&path, OS_PATH_SEPARATOR_CHAR);

	execute_study(c_str_to_s8(options.remaining[1]), arena, path, &options);

	return 0;
}
