/* See LICENSE for license details. */

// GENERATED CODE

typedef enum {
	BeamformerDecodeMode_None     = 0,
	BeamformerDecodeMode_Hadamard = 1,
	BeamformerDecodeMode_Count,
} BeamformerDecodeMode;

typedef enum {
	BeamformerRCAOrientation_None    = 0,
	BeamformerRCAOrientation_Rows    = 1,
	BeamformerRCAOrientation_Columns = 2,
	BeamformerRCAOrientation_Count,
} BeamformerRCAOrientation;

typedef enum {
	BeamformerSamplingMode_2X = 0,
	BeamformerSamplingMode_4X = 1,
	BeamformerSamplingMode_Count,
} BeamformerSamplingMode;

typedef enum {
	BeamformerEmissionKind_Sine   = 0,
	BeamformerEmissionKind_SineAM = 1,
	BeamformerEmissionKind_Chirp  = 2,
	BeamformerEmissionKind_Count,
} BeamformerEmissionKind;

typedef enum {
	BeamformerAcquisitionKind_FORCES         = 0,
	BeamformerAcquisitionKind_UFORCES        = 1,
	BeamformerAcquisitionKind_HERCULES       = 2,
	BeamformerAcquisitionKind_RCA_VLS        = 3,
	BeamformerAcquisitionKind_RCA_TPW        = 4,
	BeamformerAcquisitionKind_UHERCULES      = 5,
	BeamformerAcquisitionKind_RACES          = 6,
	BeamformerAcquisitionKind_EPIC_FORCES    = 7,
	BeamformerAcquisitionKind_EPIC_UFORCES   = 8,
	BeamformerAcquisitionKind_EPIC_UHERCULES = 9,
	BeamformerAcquisitionKind_Flash          = 10,
	BeamformerAcquisitionKind_HERO_PA        = 11,
	BeamformerAcquisitionKind_Count,
} BeamformerAcquisitionKind;

typedef enum {
	BeamformerDataKind_Int16          = 0,
	BeamformerDataKind_Int16Complex   = 1,
	BeamformerDataKind_Float32        = 2,
	BeamformerDataKind_Float32Complex = 3,
	BeamformerDataKind_Count,
} BeamformerDataKind;

typedef enum {
	BeamformerFilterKind_Kaiser       = 0,
	BeamformerFilterKind_MatchedChirp = 1,
	BeamformerFilterKind_Count,
} BeamformerFilterKind;

typedef enum {
	BeamformerInterpolationMode_Nearest = 0,
	BeamformerInterpolationMode_Linear  = 1,
	BeamformerInterpolationMode_Cubic   = 2,
	BeamformerInterpolationMode_Count,
} BeamformerInterpolationMode;

typedef enum {
	BeamformerShaderDecodeFlags_DilateOutput    = (1 << 0),
	BeamformerShaderDecodeFlags_UseSharedMemory = (1 << 1),
} BeamformerShaderDecodeFlags;

typedef enum {
	BeamformerShaderFilterFlags_ComplexFilter = (1 << 0),
	BeamformerShaderFilterFlags_OutputFloats  = (1 << 1),
	BeamformerShaderFilterFlags_Demodulate    = (1 << 2),
} BeamformerShaderFilterFlags;

typedef enum {
	BeamformerShaderDASFlags_Fast               = (1 << 0),
	BeamformerShaderDASFlags_Sparse             = (1 << 1),
	BeamformerShaderDASFlags_CoherencyWeighting = (1 << 2),
	BeamformerShaderDASFlags_SingleFocus        = (1 << 3),
	BeamformerShaderDASFlags_SingleOrientation  = (1 << 4),
} BeamformerShaderDASFlags;

typedef enum {
	BeamformerShaderKind_CudaDecode  = 0,
	BeamformerShaderKind_CudaHilbert = 1,
	BeamformerShaderKind_Decode      = 2,
	BeamformerShaderKind_Filter      = 3,
	BeamformerShaderKind_Demodulate  = 4,
	BeamformerShaderKind_DAS         = 5,
	BeamformerShaderKind_MinMax      = 6,
	BeamformerShaderKind_Sum         = 7,
	BeamformerShaderKind_Render3D    = 8,
	BeamformerShaderKind_Count,

	BeamformerShaderKind_ComputeFirst = BeamformerShaderKind_CudaDecode,
	BeamformerShaderKind_ComputeLast  = BeamformerShaderKind_Sum,
	BeamformerShaderKind_ComputeCount = 8,
	BeamformerShaderKind_RenderFirst  = BeamformerShaderKind_Render3D,
	BeamformerShaderKind_RenderLast   = BeamformerShaderKind_Render3D,
	BeamformerShaderKind_RenderCount  = 1,
} BeamformerShaderKind;

typedef struct {
	u32 decode_mode;
	u32 input_channel_stride;
	u32 input_sample_stride;
	u32 input_transmit_stride;
	u32 output_channel_stride;
	u32 output_sample_stride;
	u32 output_transmit_stride;
	u32 to_process;
	u32 transmit_count;
} BeamformerShaderDecodeBakeParameters;

typedef struct {
	u32 decimation_rate;
	u32 filter_length;
	u32 input_channel_stride;
	u32 input_sample_stride;
	u32 input_transmit_stride;
	u32 output_channel_stride;
	u32 output_sample_stride;
	u32 output_transmit_stride;
	u32 sample_count;
	f32 demodulation_frequency;
	f32 sampling_frequency;
} BeamformerShaderFilterBakeParameters;

typedef struct {
	u32 acquisition_count;
	u32 acquisition_kind;
	u32 channel_count;
	u32 interpolation_mode;
	u32 sample_count;
	u32 transmit_receive_orientation;
	u32 out_components;
	u32 out_image_x;
	u32 out_image_y;
	u32 out_image_z;
	f32 demodulation_frequency;
	f32 f_number;
	f32 focus_depth;
	f32 sampling_frequency;
	f32 speed_of_sound;
	f32 time_offset;
	f32 transmit_angle;
} BeamformerShaderDASBakeParameters;

typedef struct {
	u32 components;
	u32 elements;
	f32 scale;
} BeamformerShaderSumBakeParameters;

typedef struct {
	union {
		BeamformerShaderDecodeBakeParameters Decode;
		BeamformerShaderFilterBakeParameters Filter;
		BeamformerShaderDASBakeParameters    DAS;
		BeamformerShaderSumBakeParameters    Sum;
	};
	u32 data_kind;
	u32 flags;
} BeamformerShaderBakeParameters;

typedef union {
	struct {
		f32 cycles;
		f32 frequency;
	} sine;
	struct {
		f32 cycles;
		f32 frequency;
		u32 emissions;
	} sine_am;
	struct {
		f32 duration;
		f32 min_frequency;
		f32 max_frequency;
	} chirp;
} BeamformerEmissionParameters;

typedef struct {
	m4  xdc_transform;
	v2  xdc_element_pitch;
	uv2 raw_data_dimensions;
	v2  focal_vector;
	u32 transmit_receive_orientation;
	u32 sample_count;
	u32 channel_count;
	u32 acquisition_count;
	u32 das_shader_id;
	f32 time_offset;
	u8  single_focus;
	u8  single_orientation;
	u8  decode_mode;
	u8  sampling_mode;
	v3  output_min_coordinate;
	v3  output_max_coordinate;
	iv4 output_points;
	f32 sampling_frequency;
	f32 demodulation_frequency;
	f32 speed_of_sound;
	f32 f_number;
	f32 off_axis_pos;
	u32 interpolation_mode;
	u32 coherency_weighting;
	u32 beamform_plane;
	u32 decimation_rate;
	BeamformerEmissionKind       emission_kind;
	BeamformerEmissionParameters emission_parameters;
} BeamformerParameters;

typedef struct {
	m4  xdc_transform;
	v2  xdc_element_pitch;
	uv2 raw_data_dimensions;
	v2  focal_vector;
	u32 transmit_receive_orientation;
	u32 sample_count;
	u32 channel_count;
	u32 acquisition_count;
	u32 das_shader_id;
	f32 time_offset;
	u8  single_focus;
	u8  single_orientation;
	u8  decode_mode;
	u8  sampling_mode;
} BeamformerParametersHead;

typedef struct {
	v3  output_min_coordinate;
	v3  output_max_coordinate;
	iv4 output_points;
	f32 sampling_frequency;
	f32 demodulation_frequency;
	f32 speed_of_sound;
	f32 f_number;
	f32 off_axis_pos;
	u32 interpolation_mode;
	u32 coherency_weighting;
	u32 beamform_plane;
	u32 decimation_rate;
} BeamformerUIParameters;

typedef struct {
	m4  xdc_transform;
	v2  xdc_element_pitch;
	uv2 raw_data_dimensions;
	v2  focal_vector;
	u32 transmit_receive_orientation;
	u32 sample_count;
	u32 channel_count;
	u32 acquisition_count;
	u32 das_shader_id;
	f32 time_offset;
	u8  single_focus;
	u8  single_orientation;
	u8  decode_mode;
	u8  sampling_mode;
	v3  output_min_coordinate;
	v3  output_max_coordinate;
	iv4 output_points;
	f32 sampling_frequency;
	f32 demodulation_frequency;
	f32 speed_of_sound;
	f32 f_number;
	f32 off_axis_pos;
	u32 interpolation_mode;
	u32 coherency_weighting;
	u32 beamform_plane;
	u32 decimation_rate;
	BeamformerEmissionKind       emission_kind;
	BeamformerEmissionParameters emission_parameters;
	i16 channel_mapping[256];
	i16 sparse_elements[256];
	u8  transmit_receive_orientations[256];
	f32 steering_angles[256];
	f32 focal_depths[256];
	i32 compute_stages[16];
	i32 compute_stage_parameters[16];
	u32 compute_stages_count;
	i32 data_kind;
} BeamformerSimpleParameters;

typedef struct {
	BeamformerFilterKind kind;
	union {
		struct {
			f32 cutoff_frequency;
			f32 beta;
			u32 length;
		} kaiser;
		struct {
			f32 duration;
			f32 min_frequency;
			f32 max_frequency;
		} matched_chirp;
	};
	f32 sampling_frequency;
	b16 complex;
} BeamformerFilterParameters;

read_only global u8 beamformer_data_kind_element_size[] = {
	2,
	2,
	4,
	4,
};

read_only global u8 beamformer_data_kind_element_count[] = {
	1,
	2,
	1,
	2,
};

read_only global u8 beamformer_data_kind_byte_size[] = {
	2 * 1,
	2 * 2,
	4 * 1,
	4 * 2,
};

read_only global u8 beamformer_acquisition_kind_has_fixed_transmits[] = {
	1,
	0,
	1,
	0,
	0,
	0,
	1,
	1,
	0,
	0,
	0,
	0,
};

read_only global s8 beamformer_acquisition_kind_strings[] = {
	s8_comp("FORCES"),
	s8_comp("UFORCES"),
	s8_comp("HERCULES"),
	s8_comp("VLS"),
	s8_comp("TPW"),
	s8_comp("UHERCULES"),
	s8_comp("RACES"),
	s8_comp("EPIC-FORCES"),
	s8_comp("EPIC-UFORCES"),
	s8_comp("EPIC-UHERCULES"),
	s8_comp("Flash"),
	s8_comp("HERO-PA"),
};

read_only global s8 beamformer_filter_kind_strings[] = {
	s8_comp("Kaiser"),
	s8_comp("MatchedChirp"),
};

read_only global s8 beamformer_interpolation_mode_strings[] = {
	s8_comp("Nearest"),
	s8_comp("Linear"),
	s8_comp("Cubic"),
};

read_only global s8 beamformer_shader_names[] = {
	s8_comp("CudaDecode"),
	s8_comp("CudaHilbert"),
	s8_comp("Decode"),
	s8_comp("Filter"),
	s8_comp("Demodulate"),
	s8_comp("DAS"),
	s8_comp("MinMax"),
	s8_comp("Sum"),
	s8_comp("Render3D"),
};

read_only global BeamformerShaderKind beamformer_reloadable_shader_kinds[] = {
	BeamformerShaderKind_Decode,
	BeamformerShaderKind_Filter,
	BeamformerShaderKind_DAS,
	BeamformerShaderKind_MinMax,
	BeamformerShaderKind_Sum,
	BeamformerShaderKind_Render3D,
};

read_only global s8 beamformer_reloadable_shader_files[] = {
	s8_comp("decode.glsl"),
	s8_comp("filter.glsl"),
	s8_comp("das.glsl"),
	s8_comp("min_max.glsl"),
	s8_comp("sum.glsl"),
	s8_comp("render_3d.frag.glsl"),
};

read_only global i32 beamformer_shader_reloadable_index_by_shader[] = {
	-1,
	-1,
	0,
	1,
	1,
	2,
	3,
	4,
	5,
};

read_only global i32 beamformer_reloadable_compute_shader_info_indices[] = {
	0,
	1,
	2,
	3,
	4,
};

read_only global i32 beamformer_reloadable_render_shader_info_indices[] = {
	5,
};

read_only global s8 beamformer_shader_global_header_strings[] = {
	s8_comp(""
	"#define DataKind_Int16          0\n"
	"#define DataKind_Int16Complex   1\n"
	"#define DataKind_Float32        2\n"
	"#define DataKind_Float32Complex 3\n"
	"\n"),
	s8_comp(""
	"#define DecodeMode_None     0\n"
	"#define DecodeMode_Hadamard 1\n"
	"\n"),
	s8_comp(""
	"#define AcquisitionKind_FORCES         0\n"
	"#define AcquisitionKind_UFORCES        1\n"
	"#define AcquisitionKind_HERCULES       2\n"
	"#define AcquisitionKind_RCA_VLS        3\n"
	"#define AcquisitionKind_RCA_TPW        4\n"
	"#define AcquisitionKind_UHERCULES      5\n"
	"#define AcquisitionKind_RACES          6\n"
	"#define AcquisitionKind_EPIC_FORCES    7\n"
	"#define AcquisitionKind_EPIC_UFORCES   8\n"
	"#define AcquisitionKind_EPIC_UHERCULES 9\n"
	"#define AcquisitionKind_Flash          10\n"
	"#define AcquisitionKind_HERO_PA        11\n"
	"\n"),
	s8_comp(""
	"#define InterpolationMode_Nearest 0\n"
	"#define InterpolationMode_Linear  1\n"
	"#define InterpolationMode_Cubic   2\n"
	"\n"),
	s8_comp(""
	"#define RCAOrientation_None    0\n"
	"#define RCAOrientation_Rows    1\n"
	"#define RCAOrientation_Columns 2\n"
	"\n"),
};

read_only global s8 *beamformer_shader_flag_strings[] = {
	(s8 []){
		s8_comp("DilateOutput"),
		s8_comp("UseSharedMemory"),
	},
	(s8 []){
		s8_comp("ComplexFilter"),
		s8_comp("OutputFloats"),
		s8_comp("Demodulate"),
	},
	(s8 []){
		s8_comp("Fast"),
		s8_comp("Sparse"),
		s8_comp("CoherencyWeighting"),
		s8_comp("SingleFocus"),
		s8_comp("SingleOrientation"),
	},
	0,
	0,
	0,
};

read_only global u8 beamformer_shader_flag_strings_count[] = {
	2,
	3,
	5,
	0,
	0,
	0,
};

read_only global i32 *beamformer_shader_header_vectors[] = {
	(i32 []){0, 1},
	(i32 []){0},
	(i32 []){2, 0, 3, 4},
	0,
	0,
	0,
};

read_only global i32 beamformer_shader_header_vector_lengths[] = {
	2,
	1,
	4,
	0,
	0,
	0,
};

read_only global s8 *beamformer_shader_bake_parameter_names[] = {
	(s8 []){
		s8_comp("DecodeMode"),
		s8_comp("InputChannelStride"),
		s8_comp("InputSampleStride"),
		s8_comp("InputTransmitStride"),
		s8_comp("OutputChannelStride"),
		s8_comp("OutputSampleStride"),
		s8_comp("OutputTransmitStride"),
		s8_comp("ToProcess"),
		s8_comp("TransmitCount"),
	},
	(s8 []){
		s8_comp("DecimationRate"),
		s8_comp("FilterLength"),
		s8_comp("InputChannelStride"),
		s8_comp("InputSampleStride"),
		s8_comp("InputTransmitStride"),
		s8_comp("OutputChannelStride"),
		s8_comp("OutputSampleStride"),
		s8_comp("OutputTransmitStride"),
		s8_comp("SampleCount"),
		s8_comp("DemodulationFrequency"),
		s8_comp("SamplingFrequency"),
	},
	(s8 []){
		s8_comp("AcquisitionCount"),
		s8_comp("AcquisitionKind"),
		s8_comp("ChannelCount"),
		s8_comp("InterpolationMode"),
		s8_comp("SampleCount"),
		s8_comp("TransmitReceiveOrientation"),
		s8_comp("OutComponents"),
		s8_comp("OutImageX"),
		s8_comp("OutImageY"),
		s8_comp("OutImageZ"),
		s8_comp("DemodulationFrequency"),
		s8_comp("FNumber"),
		s8_comp("FocusDepth"),
		s8_comp("SamplingFrequency"),
		s8_comp("SpeedOfSound"),
		s8_comp("TimeOffset"),
		s8_comp("TransmitAngle"),
	},
	0,
	(s8 []){
		s8_comp("Components"),
		s8_comp("Elements"),
		s8_comp("Scale"),
	},
	0,
};

read_only global u32 beamformer_shader_bake_parameter_float_bits[] = {
	0x00000000UL,
	0x00000600UL,
	0x0001fc00UL,
	0x00000000UL,
	0x00000004UL,
	0x00000000UL,
};

read_only global i32 beamformer_shader_bake_parameter_counts[] = {
	9,
	11,
	17,
	0,
	3,
	0,
};

