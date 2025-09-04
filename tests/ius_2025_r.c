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
global v2  g_axial_extent     = {{ 10e-3f, 105e-3f}};
global v2  g_lateral_extent   = {{-60e-3f,  60e-3f}};
global f32 g_f_number         = 0.5f;

#define ZEMP_BP_MAGIC (uint64_t)0x5042504D455AFECAull

#pragma pack(push, 1)
typedef struct {
	u64 magic;
	u32 version;
	u16 beamform_mode;
	u16 decode_mode;
	u16 construction_mode;
	u16 sampling_mode;
	u32 raw_data_dimension[4];
	u32 decoded_data_dimension[4];
	f32 transducer_element_pitch[2];
	f32 transducer_bandwidth[2];
	f32 transducer_transform_matrix[16];
	i16 channel_mapping[256];
	f32 speed_of_sound;
	f32 sampling_frequency;
	f32 demodulation_frequency;
	f32 time_offset;
	f32 ensemble_repitition_interval;
	u16 data_kind;
} zemp_bp_v2;

typedef struct {
	u16 transmit_mode;
	u16 receive_mode;
	f32 focal_depth;
	f32 steering_angle;
} zemp_bp_standard_mode_data;

typedef struct {
	u16  transmit_mode;
	u16  receive_mode;
	f32  focal_depth;
	f32  steering_angle;
	i16 *sparse_elements;
} zemp_bp_sparse_mode_data;

#pragma pack(pop)

typedef union {
	zemp_bp_standard_mode_data *standard;
	zemp_bp_sparse_mode_data   *sparse;
	void                       *generic;
} zemp_bp_mode_data;

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

function b32
read_zemp_bp_v2(zemp_bp_v2 *zbp, zemp_bp_mode_data *zmd, u8 *path)
{
	b32 result = 0;
	s8 raw = os_read_file_simp((char *)path);
	if (raw.len >= (iz)sizeof(*zbp)) {
		mem_copy(zbp, raw.data, sizeof(*zbp));
		if (zbp->magic == ZEMP_BP_MAGIC && zbp->version == 2) {
			switch (zbp->beamform_mode) {
			case DASShaderKind_RCA_TPW:
			case DASShaderKind_RCA_VLS:
			{
				zmd->standard = malloc(sizeof(*(zmd->standard)) * zbp->decoded_data_dimension[2]);
				mem_copy(zmd->standard, raw.data + sizeof(*zbp), sizeof(*(zmd->standard)) * zbp->decoded_data_dimension[2]);
				result = 1;
			}break;
			case DASShaderKind_FORCES:
			case DASShaderKind_HERCULES:
			{
				zmd->standard = malloc(sizeof(*(zmd->standard)));
				mem_copy(zmd->standard, raw.data + sizeof(*zbp), sizeof(*(zmd->standard)));
				result = 1;
			}break;
			case DASShaderKind_UFORCES:
			case DASShaderKind_UHERCULES:
			{
				uz sparse_size = sizeof(*zmd->sparse->sparse_elements) * zbp->decoded_data_dimension[2];
				zmd->sparse = malloc(sizeof(zmd->sparse) + sparse_size);
				zmd->sparse->sparse_elements = (i16 *)(zmd->sparse + 1);

				mem_copy(zmd->sparse, raw.data + sizeof(*zbp), sizeof(*(zmd->standard)));
				mem_copy(zmd->sparse->sparse_elements, raw.data + sizeof(*zbp) + sizeof(*(zmd->standard)), sparse_size);
				result = 1;
			}break;
			InvalidDefaultCase;
			}
		}
	}
	free(raw.data);
	return result;
}

function void
beamformer_parameters_from_zemp_bp_v2r(BeamformerParameters *out, zemp_bp_v2 *zbp, zemp_bp_mode_data *zmd)
{
	mem_copy(out->xdc_transform,       zbp->transducer_transform_matrix, sizeof(out->xdc_transform));
	mem_copy(out->xdc_element_pitch,   zbp->transducer_element_pitch,    sizeof(out->xdc_element_pitch));
	mem_copy(out->raw_data_dimensions, zbp->raw_data_dimension,          sizeof(out->raw_data_dimensions));

	out->sample_count           = zbp->decoded_data_dimension[0];
	out->channel_count          = zbp->decoded_data_dimension[1];
	out->acquisition_count      = zbp->decoded_data_dimension[2];

	// TODO(rnp): this was probably a bug in the draft
	out->transmit_mode          = (u8)zmd->standard->transmit_mode;
	out->receive_mode           = (u8)zmd->standard->receive_mode;
	out->sampling_mode          = (u8)zbp->sampling_mode;
	out->decode                 = (u8)zbp->decode_mode;
	out->das_shader_id          = zbp->beamform_mode;
	out->time_offset            = zbp->time_offset;
	out->sampling_frequency     = zbp->sampling_frequency;
	out->demodulation_frequency = zbp->demodulation_frequency;
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
decompress_data_at_work_index(Stream *path_base)
{
	stream_append_byte(path_base, '_');
	stream_append_u64_width(path_base, 0, 2);
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
	i32 path_work_index = path.widx;

	stream_append_s8(&path, s8(".bpr"));
	stream_ensure_termination(&path, 0);

	zemp_bp_v2 zbp        = {0};
	zemp_bp_mode_data zmd = {0};
	if (!read_zemp_bp_v2(&zbp, &zmd, path.data))
		die("failed to unpack parameters file\n");

	zbp.raw_data_dimension[0] = zbp.decoded_data_dimension[0] * zbp.decoded_data_dimension[1];
	zbp.sampling_mode = BeamformerSamplingMode_4X;

	BeamformerParameters bp = {0};
	beamformer_parameters_from_zemp_bp_v2r(&bp, &zbp, &zmd);

	mem_copy(bp.output_points, g_output_points, sizeof(bp.output_points));
	bp.output_points[3] = 1;

	bp.output_min_coordinate[0] = g_lateral_extent.x;
	bp.output_min_coordinate[1] = 0;
	bp.output_min_coordinate[2] = g_axial_extent.x;

	bp.output_max_coordinate[0] = g_lateral_extent.y;
	bp.output_max_coordinate[1] = 0;
	bp.output_max_coordinate[2] = g_axial_extent.y;

	bp.f_number       = g_f_number;
	bp.beamform_plane = 0;
	bp.interpolate    = 1;

	bp.decimation_rate = 1;

	#if 0
	BeamformerFilterParameters kaiser = {0};
	kaiser.Kaiser.beta             = 5.65f;
	kaiser.Kaiser.cutoff_frequency = 2.0e6f;
	kaiser.Kaiser.length           = 36;

	f32 kaiser_parameters[sizeof(kaiser.Kaiser) / sizeof(f32)];
	mem_copy(kaiser_parameters, &kaiser.Kaiser, sizeof(kaiser.Kaiser));
	beamformer_create_filter(BeamformerFilterKind_Kaiser, kaiser_parameters,
	                         countof(kaiser_parameters), bp.sampling_frequency / 2, 0, 0, 0);
	beamformer_set_pipeline_stage_parameters(0, 0);
	#endif

	BeamformerFilterParameters matched = {0};
	typeof(matched.MatchedChirp) *mp = &matched.MatchedChirp;
	mp->duration      = chirp_length;
	mp->min_frequency = zbp.transducer_bandwidth[0] - zbp.demodulation_frequency;
	mp->max_frequency = zbp.transducer_bandwidth[1] - zbp.demodulation_frequency;

	beamformer_create_filter(BeamformerFilterKind_MatchedChirp, (f32 *)mp,
	                         sizeof(matched.MatchedChirp) / sizeof(f32),
	                         bp.sampling_frequency / 2, 1, 0, block);
	beamformer_set_pipeline_stage_parameters_at(0, 0, block);

	if (zbp.beamform_mode == DASShaderKind_UHERCULES || zbp.beamform_mode == DASShaderKind_UFORCES)
		beamformer_push_sparse_elements_at(zmd.sparse->sparse_elements, bp.acquisition_count, block);

	{
		alignas(64) v2 focal_vectors[countof(zbp.channel_mapping)];
		u32 vector_count = 1;
		if (bp.das_shader_id == DASShaderKind_RCA_TPW || bp.das_shader_id == DASShaderKind_RCA_VLS) {
			vector_count = bp.acquisition_count;
		}
		for (u32 i = 0; i < vector_count; i++)
			focal_vectors[i] = (v2){{zmd.standard[i].steering_angle, zmd.standard[i].focal_depth}};
		beamformer_push_focal_vectors_at((f32 *)focal_vectors, vector_count, block);
	}

	beamformer_push_channel_mapping_at(zbp.channel_mapping, countof(zbp.channel_mapping), block);
	beamformer_push_parameters_at(&bp, block);

	i32 shader_stages[16];
	u32 shader_stage_count = 0;
	shader_stages[shader_stage_count++] = BeamformerShaderKind_Demodulate;
	shader_stages[shader_stage_count++] = BeamformerShaderKind_Decode;
	shader_stages[shader_stage_count++] = BeamformerShaderKind_DAS;

	beamformer_push_pipeline_at(shader_stages, shader_stage_count, zbp.data_kind, block);

	stream_reset(&path, path_work_index);
	i16 *data = decompress_data_at_work_index(&path);

	send_frame_at(data, &bp, tag, block);

	free(zmd.generic);
	free(data);
}

extern i32
main(i32 argc, char *argv[])
{
	if (argc != 2) usage(argv[0]);

	os_init_timer();

	if (!beamformer_set_global_timeout(1000))    return 1;
	if (!beamformer_reserve_parameter_blocks(2)) return 1;

	Arena arena = os_alloc_arena(KB(8));
	Stream path = arena_stream(arena);
	stream_append_s8(&path, c_str_to_s8(argv[1]));
	stream_ensure_termination(&path, OS_PATH_SEPARATOR_CHAR);

	execute_study_with_block(s8("250902_MN32-4_ATS539_Cysts_FORCES-Tx-Row-Chirp-2e-05"),    path, 20e-6f, BeamformerViewPlaneTag_YZ, 0);
	execute_study_with_block(s8("250902_MN32-4_ATS539_Cysts_FORCES-Tx-Column-Chirp-2e-05"), path, 20e-6f, BeamformerViewPlaneTag_XZ, 1);
	fprintf(stderr, "press enter to continue...");
	if (fgetc(stdin) == EOF) return 0;

	#if 0
	execute_study_with_block(s8("250319_A06_7.80MHz_ATS539_Cyst_TPW-128-TxColumn"), arena, path, 0, BeamformerViewPlaneTag_YZ, 0);
	execute_study_with_block(s8("250319_A06_7.80MHz_ATS539_Cyst_TPW-128-TxColumn"), arena, path, 0, BeamformerViewPlaneTag_XZ, 1);
	fprintf(stderr, "press enter to continue...");
	if (fgetc(stdin) == EOF) return 0;

	execute_study_with_block(s8("250319_A06_7.80MHz_ATS539_Cyst_VLS-128-TxColumn"), arena, path, 0, BeamformerViewPlaneTag_YZ, 0);
	execute_study_with_block(s8("250319_A06_7.80MHz_ATS539_Cyst_VLS-128-TxColumn"), arena, path, 0, BeamformerViewPlaneTag_XZ, 1);
	#endif

	return 0;
}
