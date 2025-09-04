/* See LICENSE for license details. */

#define LIB_FN function
#include "ogl_beamformer_lib.c"

typedef enum {
	#define X(type, id, ...) DASShaderKind_##type = id,
	DAS_SHADER_KIND_LIST
	#undef X
	DASShaderKind_Count
} DASShaderKind;

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <zstd.h>

global i32 g_output_points[4] = {512, 1, 1024, 1};
global v2  g_axial_extent     = {{  5e-3f, 55e-3f}};
global v2  g_lateral_extent   = {{-27e-3f, 27e-3f}};
global f32 g_f_number         = 0.5f;

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
	i32 transmit_mode;
} zemp_bp_v1;

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

global w32_context os_context;

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
beamformer_parameters_from_zemp_bp_v1(BeamformerParameters *out, zemp_bp_v1 *zbp)
{
	mem_copy(out->xdc_transform,       zbp->xdc_transform,     sizeof(out->xdc_transform));
	mem_copy(out->xdc_element_pitch,   zbp->xdc_element_pitch, sizeof(out->xdc_element_pitch));
	mem_copy(out->raw_data_dimensions, zbp->raw_data_dim,      sizeof(out->raw_data_dimensions));

	out->sample_count           = zbp->decoded_data_dim[0];
	out->channel_count          = zbp->decoded_data_dim[1];
	out->acquisition_count      = zbp->decoded_data_dim[2];
	out->transmit_mode          = (u8)((zbp->transmit_mode & 2) >> 1);
	out->receive_mode           = (u8)((zbp->transmit_mode & 1) >> 0);
	out->decode                 = (u8)zbp->decode_mode;
	out->das_shader_id          = zbp->beamform_mode;
	out->time_offset            = zbp->time_offset;
	out->sampling_frequency     = zbp->sampling_frequency;
	out->demodulation_frequency = zbp->center_frequency;
	out->speed_of_sound         = zbp->speed_of_sound;
}

#define shift_n(v, c, n) v += n, c -= n
#define shift(v, c) shift_n(v, c, 1)

function void
usage(char *argv0)
{
	die("%s base_path\n"
	    , argv0);
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
send_frame_at(i16 *restrict i16_data, BeamformerParameters *restrict bp, BeamformerViewPlaneTag tag, u8 block)
{
	u32 data_size = bp->raw_data_dimensions[0] * bp->raw_data_dimensions[1] * sizeof(i16);
	b32 result    = beamformer_push_data_with_compute(i16_data, data_size, tag, block);
	if (!result) printf("lib error: %s\n", beamformer_get_last_error_string());

	return result;
}

function void
execute_study_with_block(s8 study, Stream path, f32 chirp_length, BeamformerViewPlaneTag tag, u8 block)
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
	beamformer_parameters_from_zemp_bp_v1(&bp, zbp);

	mem_copy(bp.output_points, g_output_points, sizeof(bp.output_points));
	bp.output_points[3] = 1;

	bp.output_min_coordinate[0] = g_lateral_extent.x;
	bp.output_min_coordinate[1] = 0;
	bp.output_min_coordinate[2] = g_axial_extent.x;

	bp.output_max_coordinate[0] = g_lateral_extent.y;
	bp.output_max_coordinate[1] = 0;
	bp.output_max_coordinate[2] = g_axial_extent.y;

	bp.f_number       = g_f_number;
	bp.beamform_plane = tag;
	bp.interpolate    = 1;

	bp.decimation_rate = 1;

	bp.demodulation_frequency = bp.sampling_frequency / 4;
	bp.sampling_mode          = BeamformerSamplingMode_4X;

	BeamformerFilterParameters kaiser = {0};
	kaiser.Kaiser.beta             = 5.65f;
	kaiser.Kaiser.cutoff_frequency = 2.0e6f;
	kaiser.Kaiser.length           = 36;

	beamformer_create_filter(BeamformerFilterKind_Kaiser, (f32 *)&kaiser, sizeof(kaiser.Kaiser) / sizeof(f32),
	                         bp.sampling_frequency / 2, 0, 0, block);
	beamformer_set_pipeline_stage_parameters_at(0, 0, block);

	#if 0
	BeamformerFilterParameters matched = {0};
	typeof(matched.MatchedChirp) *mp = &matched.MatchedChirp;
	mp->duration      = chirp_length;
	mp->min_frequency = 2.9e6 - zbp->demodulation_frequency;
	mp->max_frequency = 6.0e6 - zbp->demodulation_frequency;

	beamformer_create_filter(BeamformerFilterKind_MatchedChirp, (f32 *)mp,
	                         sizeof(matched.MatchedChirp) / sizeof(f32),
	                         bp.sampling_frequency / 2, 1, 0, block);
	beamformer_set_pipeline_stage_parameters_at(0, 0, block);
	#endif

	if (zbp->beamform_mode == DASShaderKind_UHERCULES || zbp->beamform_mode == DASShaderKind_UFORCES)
		beamformer_push_sparse_elements_at(zbp->sparse_elements, bp.acquisition_count, block);

	{
		alignas(64) v2 focal_vectors[countof(zbp->focal_depths)];
		for (u32 i = 0; i < countof(zbp->focal_depths); i++)
			focal_vectors[i] = (v2){{zbp->transmit_angles[i], zbp->focal_depths[i]}};
		beamformer_push_focal_vectors_at((f32 *)focal_vectors, countof(focal_vectors), block);
	}

	beamformer_push_channel_mapping_at(zbp->channel_mapping, countof(zbp->channel_mapping), block);
	beamformer_push_parameters_at(&bp, block);

	i32 shader_stages[16];
	u32 shader_stage_count = 0;
	shader_stages[shader_stage_count++] = BeamformerShaderKind_Demodulate;
	shader_stages[shader_stage_count++] = BeamformerShaderKind_Decode;
	shader_stages[shader_stage_count++] = BeamformerShaderKind_DAS;

	beamformer_push_pipeline_at(shader_stages, shader_stage_count, BeamformerDataKind_Int16, block);

	stream_reset(&path, path_work_index);
	i16 *data = decompress_data_at_work_index(&path, 0);

	send_frame_at(data, &bp, tag, block);

	free(zbp);
	free(data);
}

extern i32
main(i32 argc, char *argv[])
{
	if (argc != 2) usage(argv[0]);

	os_init_timer();

	if (!beamformer_set_global_timeout(1000))    die((char *)beamformer_get_last_error_string());
	if (!beamformer_reserve_parameter_blocks(2)) die((char *)beamformer_get_last_error_string());

	Arena arena = os_alloc_arena(KB(8));
	Stream path = arena_stream(arena);
	stream_append_s8(&path, c_str_to_s8(argv[1]));
	stream_ensure_termination(&path, OS_PATH_SEPARATOR_CHAR);

	execute_study_with_block(s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxRow"),    path, 0, BeamformerViewPlaneTag_YZ, 0);
	execute_study_with_block(s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxColumn"), path, 0, BeamformerViewPlaneTag_XZ, 1);
	fprintf(stderr, "press enter to continue...");
	if (fgetc(stdin) == EOF) return 0;

	execute_study_with_block(s8("250319_A06_7.80MHz_ATS539_Cyst_TPW-128-TxColumn"), path, 0, BeamformerViewPlaneTag_YZ, 0);
	execute_study_with_block(s8("250319_A06_7.80MHz_ATS539_Cyst_TPW-128-TxColumn"), path, 0, BeamformerViewPlaneTag_XZ, 1);
	fprintf(stderr, "press enter to continue...");
	if (fgetc(stdin) == EOF) return 0;

	execute_study_with_block(s8("250319_A06_7.80MHz_ATS539_Cyst_VLS-128-TxColumn"), path, 0, BeamformerViewPlaneTag_YZ, 0);
	execute_study_with_block(s8("250319_A06_7.80MHz_ATS539_Cyst_VLS-128-TxColumn"), path, 0, BeamformerViewPlaneTag_XZ, 1);

	return 0;
}
