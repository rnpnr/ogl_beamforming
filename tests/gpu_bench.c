/* See LICENSE for license details. */

#define LIB_FN function
#include "ogl_beamformer_lib.c"

#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <time.h>
#include <zstd.h>

#define AVERAGE_SAMPLES countof(((BeamformerComputeStatsTable *)0)->times)
#define WARMUP_COUNT    AVERAGE_SAMPLES
//#define WARMUP_COUNT    4
//#define RF_TIME_SAMPLES 2432
#define RF_TIME_SAMPLES 4096

#define TIMEOUT_MS 100000

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

typedef enum {
	WorkKind_Decode,
	WorkKind_FullCompute,
} WorkKind;

typedef struct {
	b32 cuda;
	b32 full_aperture;
} DecodeWork;

typedef struct {
	iv3 output_points;
	v3  min_coordinate;
	v3  max_coordinate;
	f32 chirp_length;
	f32 bandwidth_min;
	f32 bandwidth_max;
	f32 speed_of_sound;

	u32 decimation_rate;
	BeamformerInterpolationMode interpolation;
} ComputeWorkSettings;

typedef enum {
	ComputeWorkKind_BScan,
	ComputeWorkKind_DTUVolumeCubic,
	ComputeWorkKind_DTUVolumeLinear,
	ComputeWorkKind_Volume,
} ComputeWorkKind;

typedef struct {
	s8 study;
	ComputeWorkKind settings_kind;
} ComputeWork;

/* TODO(rnp): test NPOT in z on NVIDIA */
//#define DTU_VOLUME_POINTS (iv3){.x = 133, .y = 115, .z = 250}
#define DTU_VOLUME_POINTS (iv3){.x = 122, .y = 123, .z = 256}
#define DTU_VOLUME_MIN (v3){{-30e-3f, -30e-3f,  35e-3f}}
#define DTU_VOLUME_MAX (v3){{ 30e-3f,  30e-3f,  55e-3f}}

ComputeWorkSettings compute_work_settings[] = {
	[ComputeWorkKind_BScan] = {
		.output_points  = (iv3){{1024, 1, 1024}},
		.min_coordinate = (v3){{-60e-3f, 0,  25e-3f}},
		.max_coordinate = (v3){{ 60e-3f, 0, 130e-3f}},
		.chirp_length   = 2e-5f,
		.bandwidth_min  = 2.9e6f,
		.bandwidth_max  = 6.0e6f,
		.speed_of_sound = 1480.0f,
	},
	[ComputeWorkKind_DTUVolumeCubic] = {
		.output_points   = DTU_VOLUME_POINTS,
		.min_coordinate  = DTU_VOLUME_MIN,
		.max_coordinate  = DTU_VOLUME_MAX,
		.interpolation   = BeamformerInterpolationMode_Cubic,
		.decimation_rate = 4,
	},
	[ComputeWorkKind_DTUVolumeLinear] = {
		.output_points   = DTU_VOLUME_POINTS,
		.min_coordinate  = DTU_VOLUME_MIN,
		.max_coordinate  = DTU_VOLUME_MAX,
		.interpolation   = BeamformerInterpolationMode_Linear,
	},
	[ComputeWorkKind_Volume] = {
		.output_points   = (iv3){{256, 256, 256}},
		.min_coordinate  = (v3){{-30e-3f, -30e-3f,  30e-3f}},
		.max_coordinate  = (v3){{ 30e-3f,  30e-3f,  90e-3f}},
		.interpolation   = BeamformerInterpolationMode_Cubic,
		.decimation_rate = 4,
	},
};

typedef struct {
	WorkKind kind;
	union {
		DecodeWork  decode;
		ComputeWork compute;
	};
	s8 subdir;
} Work;

read_only global Work works[] = {
	//{.kind = WorkKind_Decode, .subdir = s8_comp("decode_cuda_256rx"), .decode = {.full_aperture = 1, .cuda = 1}},
	//{.kind = WorkKind_Decode, .subdir = s8_comp("decode_cuda_txrx"),  .decode = {.cuda = 1}                    },
	{.kind = WorkKind_Decode, .subdir = s8_comp("decode_256rx"),      .decode = {.full_aperture = 1}           },
	{.kind = WorkKind_Decode, .subdir = s8_comp("decode_txrx")                                                 },

	{
		.kind    = WorkKind_FullCompute,
		.subdir  = s8_comp("bscan"),
		.compute = {
			.study = s8_comp("251014_MN32-4_Beating_Heart_FORCES-Tx-Row-Chirp-2e-05"),
			.settings_kind = ComputeWorkKind_BScan,
		},
	},
	{
		.kind    = WorkKind_FullCompute,
		.subdir  = s8_comp("bscan"),
		.compute = {
			.study = s8_comp("251014_MN32-4_Beating_Heart_HERCULES-Diverging-Tx-Column-Chirp-2e-05"),
			.settings_kind = ComputeWorkKind_BScan,
		},
	},
	{
		.kind    = WorkKind_FullCompute,
		.subdir  = s8_comp("bscan"),
		.compute = {
			.study = s8_comp("251014_MN32-4_Beating_Heart_TPW-128-Tx-Column-Chirp-2e-05"),
			.settings_kind = ComputeWorkKind_BScan,
		},
	},

	{
		.kind    = WorkKind_FullCompute,
		.subdir  = s8_comp("dtu_volume_linear"),
		.compute = {
			.study = s8_comp("251014_MN32-4_ATS_Cysts_VLS-96-Tx-Column-Chirp-2e-05"),
			.settings_kind = ComputeWorkKind_DTUVolumeLinear,
		},
	},
	{
		.kind    = WorkKind_FullCompute,
		.subdir  = s8_comp("dtu_volume_linear"),
		.compute = {
			.study = s8_comp("251014_MN32-4_ATS539_Cyst_2_VLS-128-Tx-Column-Chirp-2e-05"),
			.settings_kind = ComputeWorkKind_DTUVolumeLinear,
		},
	},
	{
		.kind    = WorkKind_FullCompute,
		.subdir  = s8_comp("dtu_volume_linear"),
		.compute = {
			.study = s8_comp("251014_MN32-4_ATS539_Cyst_2_HERCULES-Diverging-Tx-Column-Chirp-2e-05"),
			.settings_kind = ComputeWorkKind_DTUVolumeLinear,
		},
	},

	{
		.kind    = WorkKind_FullCompute,
		.subdir  = s8_comp("dtu_volume_cubic_4x"),
		.compute = {
			.study = s8_comp("251014_MN32-4_ATS_Cysts_VLS-96-Tx-Column-Chirp-2e-05"),
			.settings_kind = ComputeWorkKind_DTUVolumeCubic,
			//.chirp_length   = 2e-5f,
			//.bandwidth_min  = 2.9e6f,
			//.bandwidth_max  = 6.0e6f,
		},
	},
	{
		.kind    = WorkKind_FullCompute,
		.subdir  = s8_comp("dtu_volume_cubic_4x"),
		.compute = {
			.study = s8_comp("251014_MN32-4_ATS539_Cyst_2_VLS-128-Tx-Column-Chirp-2e-05"),
			.settings_kind = ComputeWorkKind_DTUVolumeCubic,
		},
	},
	{
		.kind    = WorkKind_FullCompute,
		.subdir  = s8_comp("dtu_volume_cubic_4x"),
		.compute = {
			.study = s8_comp("251014_MN32-4_ATS539_Cyst_2_HERCULES-Diverging-Tx-Column-Chirp-2e-05"),
			.settings_kind = ComputeWorkKind_DTUVolumeCubic,
		},
	},

	{
		.kind    = WorkKind_FullCompute,
		.subdir  = s8_comp("volume"),
		.compute = {
			.study = s8_comp("251014_MN32-4_ATS_Wires_Full_HERCULES-Diverging-Tx-Column"),
			.settings_kind = ComputeWorkKind_Volume,
		},
	},
	{
		.kind    = WorkKind_FullCompute,
		.subdir  = s8_comp("volume"),
		.compute = {
			.study = s8_comp("251014_MN32-4_ATS_Wires_Full_VLS-128-Tx-Column"),
			.settings_kind = ComputeWorkKind_Volume,
		},
	},
};

read_only global u32 decode_transmit_counts[] = {
	2, 4, 8, 12, 16, 20, 24, 32, 40, 48, 64, 80, 96, 128, 160, 192, 256
};
global f32 g_f_number = 0.5f;

#define LINE_BREAK "-----------------------------------------\n"

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

function f64
os_get_time(void)
{
	f64 result = (f64)os_get_timer_counter() / (f64)os_get_timer_frequency();
	return result;
}

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

W32(b32) CreateDirectoryA(c8 *, void *);

function f64
os_get_time(void)
{
	f64 result = (f64)os_get_timer_counter() / (f64)os_w32_context.timer_frequency;
	return result;
}

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

function void
stream_append_acquisition_kind(Stream *s, BeamformerAcquisitionKind kind, u32 transmit_count)
{
	s8 name = beamformer_acquisition_kind_strings[kind];
	stream_append_s8(s, name);
	stream_append_byte(s, '-');
	stream_append_u64(s, transmit_count);
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
beamformer_simple_parameters_from_file(u8 *file, BeamformerSimpleParameters *out)
{
	zemp_bp_v1 *zbp = read_zemp_bp_v1(file);
	if (!zbp) die("failed to unpack: %s\n", file);

	mem_copy(out->xdc_transform.E,       zbp->xdc_transform,     sizeof(out->xdc_transform));
	mem_copy(out->xdc_element_pitch.E,   zbp->xdc_element_pitch, sizeof(out->xdc_element_pitch));
	mem_copy(out->raw_data_dimensions.E, zbp->raw_data_dim,      sizeof(out->raw_data_dimensions));
	mem_copy(out->channel_mapping,       zbp->channel_mapping,   sizeof(out->channel_mapping));

	out->sample_count           = zbp->decoded_data_dim[0];
	out->channel_count          = zbp->decoded_data_dim[1];
	out->acquisition_count      = zbp->decoded_data_dim[2];
	out->decode_mode            = (u8)zbp->decode_mode;
	out->das_shader_id          = zbp->beamform_mode;
	out->time_offset            = zbp->time_offset;
	out->sampling_frequency     = zbp->sampling_frequency;
	out->speed_of_sound         = zbp->speed_of_sound;

	/* NOTE(rnp): v1 files didn't specify sampling mode. it was almost always 4X */
	out->sampling_mode          = BeamformerSamplingMode_4X;
	out->demodulation_frequency = out->sampling_frequency / 4;

	if (out->das_shader_id == BeamformerAcquisitionKind_UFORCES ||
	    out->das_shader_id == BeamformerAcquisitionKind_UHERCULES)
	{
		mem_copy(out->sparse_elements, zbp->sparse_elements,
		         sizeof(*out->sparse_elements) * out->acquisition_count);
	}

	b32 tx_rows = (zbp->transmit_mode & (1 << 1)) == 0;
	b32 rx_rows = (zbp->transmit_mode & (1 << 0)) == 0;
	u8 packed_tx_rx = 0;
	if (tx_rows) packed_tx_rx |= BeamformerRCAOrientation_Rows    << 4;
	else         packed_tx_rx |= BeamformerRCAOrientation_Columns << 4;
	if (rx_rows) packed_tx_rx |= BeamformerRCAOrientation_Rows    << 0;
	else         packed_tx_rx |= BeamformerRCAOrientation_Columns << 0;

	if (out->das_shader_id == BeamformerAcquisitionKind_HERCULES ||
	    out->das_shader_id == BeamformerAcquisitionKind_UHERCULES)
	{
		out->single_focus       = 1;
		out->single_orientation = 1;

		out->transmit_receive_orientation = packed_tx_rx;
		out->focal_vector.E[0] = zbp->transmit_angles[0];
		out->focal_vector.E[1] = zbp->focal_depths[0];
	} else {
		mem_copy(out->focal_depths, zbp->focal_depths,
		         sizeof(*out->focal_depths) * out->acquisition_count);
		mem_copy(out->steering_angles, zbp->transmit_angles,
		         sizeof(*out->steering_angles) * out->acquisition_count);
		for (u32 i = 0; i < countof(out->transmit_receive_orientations); i++)
			out->transmit_receive_orientations[i] = packed_tx_rx;
	}

	free(zbp);
}

#define shift_n(v, c, n) v += n, c -= n
#define shift(v, c) shift_n(v, c, 1)

function void
usage(char *argv0)
{
	die("%s base_data_path\n", argv0);
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

function void
compute_work_parameters(BeamformerSimpleParameters *bsp, u8 *bp_filepath, ComputeWorkSettings *cw)
{
	beamformer_simple_parameters_from_file(bp_filepath, bsp);

	bsp->output_points.xyz  = cw->output_points;
	bsp->output_points.E[3] = 1;

	bsp->output_min_coordinate = cw->min_coordinate;
	bsp->output_max_coordinate = cw->max_coordinate;

	if (cw->speed_of_sound != 0) bsp->speed_of_sound = cw->speed_of_sound;

	bsp->f_number           = g_f_number;
	bsp->beamform_plane     = 0;
	bsp->interpolation_mode = cw->interpolation;
	bsp->decimation_rate    = cw->decimation_rate;

	BeamformerFilterParameters filter = {0};
	BeamformerFilterKind filter_kind;
	b32   filter_is_complex = 0;
	u32   filter_parameters_size;
	void *filter_parameters;
	if (cw->chirp_length == 0) {
		filter.kaiser.beta             = 5.65f;
		filter.kaiser.cutoff_frequency = 2.0e6f;
		filter.kaiser.length           = 36;

		filter_kind            = BeamformerFilterKind_Kaiser;
		filter_parameters      = &filter.kaiser;
		filter_parameters_size = sizeof(filter.kaiser);
	} else {
		filter.matched_chirp.duration      = cw->chirp_length;
		filter.matched_chirp.min_frequency = cw->bandwidth_min - bsp->demodulation_frequency;
		filter.matched_chirp.max_frequency = cw->bandwidth_max - bsp->demodulation_frequency;

		filter_kind            = BeamformerFilterKind_MatchedChirp;
		filter_parameters      = &filter.matched_chirp;
		filter_parameters_size = sizeof(filter.matched_chirp);
		filter_is_complex      = 1;

		bsp->time_offset += cw->chirp_length / 2;
	}

	beamformer_create_filter(filter_kind, filter_parameters, filter_parameters_size,
	                         bsp->sampling_frequency / 2, filter_is_complex, 0, 0);
	bsp->compute_stage_parameters[0] = 0;

	u32 shader_stage_count = 0;
	bsp->compute_stages[shader_stage_count++] = BeamformerShaderKind_Demodulate;
	bsp->compute_stages[shader_stage_count++] = BeamformerShaderKind_Decode;
	bsp->compute_stages[shader_stage_count++] = BeamformerShaderKind_DAS;
	bsp->compute_stages_count = shader_stage_count;
	bsp->data_kind = BeamformerDataKind_Int16;
}

function uv4
decoded_data_dim(u32 transmit_count, b32 full_aperture)
{
	u32 max_transmits = decode_transmit_counts[countof(decode_transmit_counts) - 1];
	uv4 result = {{RF_TIME_SAMPLES, full_aperture? max_transmits: transmit_count, transmit_count, 1}};
	return result;
}

function uv2
raw_data_dim(u32 transmit_count)
{
	uv4 dec = decoded_data_dim(transmit_count, 0);
	uv2 result = {{dec.x * transmit_count, 256}};
	return result;
}

function u32
data_size_for_transmit_count(u32 transmit_count)
{
	uv2 rf_dim = raw_data_dim(transmit_count);
	u32 result = rf_dim.x * rf_dim.y * sizeof(i16);
	return result;
}

function void
decode_work_parameters(BeamformerSimpleParameters *bsp, u32 transmit_count, b32 full_aperture, b32 cuda)
{
	/* NOTE(rnp): use real channel mapping so that we still get ~random~ access pattern */
	read_only local_persist i16 channel_mapping[] = {
		217, 129, 212, 188, 255, 131, 237, 190, 241, 130, 248, 187, 219, 128, 218, 181,
		216, 134, 247, 180, 220, 132, 238, 178, 246, 133, 240, 179, 221, 135, 239, 173,
		231, 137, 211, 172, 222, 139, 213, 170, 249, 138, 210, 171, 223, 136, 232, 189,
		233, 142, 209, 164, 224, 140, 214, 186, 254, 141, 208, 163, 225, 143, 215, 185,
		230, 145, 204, 162, 226, 147, 206, 165, 229, 146, 207, 161, 227, 144, 205, 182,
		234, 150, 203, 160, 228, 148, 201, 166, 236, 149, 200, 159, 235, 175, 202, 177,
		242, 151, 196, 191, 243, 155, 198, 167, 245, 154, 199, 158, 244, 176, 197, 174,
		250, 168, 195, 184, 251, 156, 193, 152, 253, 153, 192, 157, 252, 183, 194, 169,
		102,  62,  71,   3, 100,  60,  82,   1,  78,  61,  72,   4,  64,  63, 101,  10,
		103,  57, 107,  11,  99,  59,  81,  13,  73,  58,  79,  12,  98,  56,  80,  18,
		 88,  54, 108,  19,  97,  52, 106,  21,  70,  53, 109,  20,  96,  55,  87,   2,
		 86,  49, 110,  27,  95,  51, 105,   5,  65,  50, 111,  28,  94,  48, 104,   6,
		 89,  46, 115,  29,  93,  44, 113,  26,  90,  45, 112,  30,  92,  47, 114,   9,
		 85,  41, 116,  31,  91,  43, 118,  25,  83,  42, 119,  32,  84,  16, 117,  14,
		 77,  40, 123,   0,  76,  36, 121,  24,  74,  37, 120,  33,  75,  15, 122,  17,
		 69,  23, 124,   7,  68,  35, 126,  39,  66,  38, 127,  34,  67,   8, 125,  22,
	};

	uv3 dec_data_dim = decoded_data_dim(transmit_count, full_aperture).xyz;
	bsp->decode_mode       = BeamformerDecodeMode_Hadamard;
	bsp->sample_count      = dec_data_dim.x;
	bsp->channel_count     = dec_data_dim.y;
	bsp->acquisition_count = dec_data_dim.z;

	bsp->raw_data_dimensions = raw_data_dim(transmit_count);
	mem_copy(bsp->channel_mapping, channel_mapping, sizeof(bsp->channel_mapping));

	bsp->compute_stages[0]    = cuda? BeamformerShaderKind_CudaDecode : BeamformerShaderKind_Decode;
	bsp->compute_stages_count = 1;
	bsp->data_kind            = BeamformerDataKind_Int16;
}

function i16 *
generate_test_data_for_transmit_count(u32 transmit_count, b32 full_aperture)
{
	u32  rf_size = data_size_for_transmit_count(transmit_count);
	i16 *result  = malloc(rf_size);
	if (!result) die("malloc\n");
	return result;
}

function void
lib_error(void)
{
	printf("lib error: %s\n", beamformer_get_last_error_string());
}

function void
execute_and_write(BeamformerSimpleParameters *bsp, u8 *path, i16 *data)
{
	b32 result = beamformer_push_simple_parameters(bsp);
	if (!result) lib_error();

	if (result) {
		BeamformerComputeStatsTable stats = {0};
		u32 data_size = bsp->raw_data_dimensions.E[0] * bsp->raw_data_dimensions.E[1] * sizeof(*data);
		for (u32 i = 0; i < WARMUP_COUNT + AVERAGE_SAMPLES; i++) {
			if (!beamformer_push_data_with_compute(data, data_size, BeamformerViewPlaneTag_XZ, 0))
				lib_error();
			fprintf(stderr, "\r\x1B[2K" "step [%u/%u]", i + 1, (u32)(WARMUP_COUNT + AVERAGE_SAMPLES));
		}
		fprintf(stderr, "\r\x1B[2K");
		if (beamformer_compute_timings(&stats, TIMEOUT_MS)) {
			os_write_new_file((c8 *)path, (s8){.len = sizeof(stats), .data = (u8 *)&stats});
		} else {
			lib_error();
		}
	}
}

function void
sigint(i32 _signo)
{
	BeamformerLiveImagingParameters lip = {0};
	beamformer_set_live_parameters(&lip);
	os_exit(0);
}

extern i32
main(i32 argc, char *argv[])
{
	if (argc != 2) usage(argv[0]);

	os_common_init();

	signal(SIGINT, sigint);

	if (!beamformer_set_global_timeout(TIMEOUT_MS)) {
		lib_error();
		die("");
	}

	Stream data_path   = arena_stream(os_alloc_arena(KB(8)));
	Stream output_path = arena_stream(os_alloc_arena(KB(8)));

	time_t t = time(0);
	output_path.widx += (i32)strftime((c8 *)output_path.data + output_path.widx,
	                                  (uz)(output_path.cap - output_path.widx),
	                                  "%y%m%d%H%M_gpubench", localtime(&t));
	os_make_directory((c8 *)output_path.data);
	stream_ensure_termination(&output_path, OS_PATH_SEPARATOR_CHAR);

	stream_append_s8(&data_path, c_str_to_s8(argv[1]));
	stream_ensure_termination(&data_path, OS_PATH_SEPARATOR_CHAR);

	BeamformerLiveImagingParameters lip = {.active = 1, .save_enabled = 1};
	s8 short_name = s8("GPU Bench");
	mem_copy(lip.save_name_tag, short_name.data, (uz)short_name.len);
	lip.save_name_tag_length = (i32)short_name.len;
	beamformer_set_live_parameters(&lip);

	f64 runtime = 0;
	i32 output_path_start_index = output_path.widx;
	for EachElement(works, it) {
		Work *work = works + it;

		stream_reset(&output_path, output_path_start_index);
		stream_append_s8(&output_path, work->subdir);
		stream_append_byte(&output_path, 0);
		os_make_directory((c8 *)output_path.data);
		stream_reset(&output_path, output_path.widx - 1);
		stream_append_byte(&output_path, OS_PATH_SEPARATOR_CHAR);

		BeamformerSimpleParameters bsp[1];
		zero_struct(bsp);

		f64 start = os_get_time();
		switch (work->kind) {
		case WorkKind_Decode:{
			printf(LINE_BREAK "[%td/%td]: decode %s\n", it + 1, countof(works),
			       work->decode.full_aperture? "(full aperture)" : "");

			u32 max_transmit_count = decode_transmit_counts[countof(decode_transmit_counts) - 1];
			i16 *data = generate_test_data_for_transmit_count(max_transmit_count, work->decode.full_aperture);

			for EachElement(decode_transmit_counts, transmits) {
				u64 index = countof(decode_transmit_counts) - 1 - transmits;
				u32 transmit_count = decode_transmit_counts[index];
				printf("count: %u\n", transmit_count);

				Stream output = output_path;
				stream_append_s8(&output, s8("decode_"));
				stream_append_u64(&output, transmit_count);
				stream_append_s8(&output, s8(".bin"));
				stream_append_byte(&output, 0);
				decode_work_parameters(bsp, transmit_count, work->decode.full_aperture, work->decode.cuda);
				execute_and_write(bsp, output.data, data);
			}

			free(data);
		}break;
		case WorkKind_FullCompute:{
			printf(LINE_BREAK "[%td/%td]: compute: %.*s\n", it + 1, countof(works),
			       (i32)work->compute.study.len, work->compute.study.data);

			Stream input = data_path;
			stream_append_s8(&input, work->compute.study);
			i32 input_widx = input.widx;
			stream_append_s8(&input, s8(".bp"));
			stream_append_byte(&input, 0);
			compute_work_parameters(bsp, input.data, compute_work_settings + work->compute.settings_kind);

			stream_reset(&input, input_widx);
			i16 *data = decompress_data_at_work_index(&input, 0);

			Stream output = output_path;
			stream_append_acquisition_kind(&output, bsp->das_shader_id, bsp->acquisition_count);
			stream_append_s8(&output, s8(".bin"));
			stream_append_byte(&output, 0);
			execute_and_write(bsp, output.data, data);

			free(data);
		}break;
		InvalidDefaultCase;
		}

		f64 delta = os_get_time() - start;
		printf("took: %4.3f [ms]\n", delta * 1e3);
		runtime += delta;
	}

	lip.active = 0;
	beamformer_set_live_parameters(&lip);

	printf(LINE_BREAK "runtime: %4.3f [s]\n", runtime);

	return 0;
}
