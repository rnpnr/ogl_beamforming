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
	BeamformerShaderDASFlags_Sparse             = (1 << 0),
	BeamformerShaderDASFlags_CoherencyWeighting = (1 << 1),
	BeamformerShaderDASFlags_SingleFocus        = (1 << 2),
	BeamformerShaderDASFlags_SingleOrientation  = (1 << 3),
} BeamformerShaderDASFlags;

typedef enum {
	BeamformerShaderKind_CudaDecode         = 0,
	BeamformerShaderKind_CudaHilbert        = 1,
	BeamformerShaderKind_Decode             = 2,
	BeamformerShaderKind_Filter             = 3,
	BeamformerShaderKind_Demodulate         = 4,
	BeamformerShaderKind_DAS                = 5,
	BeamformerShaderKind_Sum                = 6,
	BeamformerShaderKind_MinMax             = 7,
	BeamformerShaderKind_CoherencyWeighting = 8,
	BeamformerShaderKind_BufferClear        = 9,
	BeamformerShaderKind_RenderBeamformed   = 10,
	BeamformerShaderKind_Count,

	BeamformerShaderKind_ComputeFirst         = BeamformerShaderKind_CudaDecode,
	BeamformerShaderKind_ComputeLast          = BeamformerShaderKind_MinMax,
	BeamformerShaderKind_ComputeCount         = 8,
	BeamformerShaderKind_ComputeHelpersFirst  = BeamformerShaderKind_CoherencyWeighting,
	BeamformerShaderKind_ComputeHelpersLast   = BeamformerShaderKind_CoherencyWeighting,
	BeamformerShaderKind_ComputeHelpersCount  = 1,
	BeamformerShaderKind_ComputeInternalFirst = BeamformerShaderKind_BufferClear,
	BeamformerShaderKind_ComputeInternalLast  = BeamformerShaderKind_BufferClear,
	BeamformerShaderKind_ComputeInternalCount = 1,
	BeamformerShaderKind_RenderFirst          = BeamformerShaderKind_RenderBeamformed,
	BeamformerShaderKind_RenderLast           = BeamformerShaderKind_RenderBeamformed,
	BeamformerShaderKind_RenderCount          = 1,
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
	u32 channel_chunk_count;
	u32 interpolation_mode;
	u32 sample_count;
	u32 transmit_receive_orientation;
	f32 demodulation_frequency;
	f32 f_number;
	f32 focus_depth;
	f32 sampling_frequency;
	f32 speed_of_sound;
	f32 time_offset;
	f32 transmit_angle;
} BeamformerShaderDASBakeParameters;

typedef struct {
	u64 hadamard_buffer;
	u64 rf_buffer;
	u64 output_buffer;
	u64 output_rf_buffer;
	b32 first_pass;
} BeamformerShaderDecodePushConstants;

typedef struct {
	u64 input_data;
	u64 output_data;
	u64 filter_coefficients;
} BeamformerShaderFilterPushConstants;

typedef struct {
	m4  voxel_transform;
	m4  xdc_transform;
	v2  xdc_element_pitch;
	u64 rf_data;
	u64 output_data;
	u64 incoherent_output;
	u64 focal_vectors;
	u64 sparse_elements;
	u64 transmit_receive_orientations;
	u32 output_size_x;
	u32 output_size_y;
	u32 output_size_z;
	u32 cycle_t;
	i32 channel_offset;
} BeamformerShaderDASPushConstants;

typedef struct {
	u64 output_data;
	u64 input_data;
	u32 image_elements;
	f32 scale;
} BeamformerShaderSumPushConstants;

typedef struct {
	u64 left_side_buffer;
	u64 right_side_buffer;
	u32 elements;
	f32 scale;
	u32 output_size_x;
	u32 output_size_y;
	u32 output_size_z;
} BeamformerShaderCoherencyWeightingPushConstants;

typedef struct {
	u64 data;
	u32 clear_word;
	u32 words;
} BeamformerShaderBufferClearPushConstants;

typedef struct {
	m4  mvp_matrix;
	u64 positions;
	u64 normals;
	v4  bounding_box_colour;
	f32 bounding_box_fraction;
	f32 db_cutoff;
	f32 threshold;
	f32 gamma;
	u64 input_data;
	u32 input_size_x;
	u32 input_size_y;
	u32 input_size_z;
	u32 data_kind;
} BeamformerShaderRenderBeamformedPushConstants;

typedef struct {
	union {
		BeamformerShaderDecodeBakeParameters Decode;
		BeamformerShaderFilterBakeParameters Filter;
		BeamformerShaderDASBakeParameters    DAS;
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
	u32 acquisition_kind;
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
	u32 acquisition_kind;
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
	u32 acquisition_kind;
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
	s8_comp("Sum"),
	s8_comp("MinMax"),
	s8_comp("CoherencyWeighting"),
	s8_comp("BufferClear"),
	s8_comp("RenderBeamformed"),
};

read_only global BeamformerShaderKind beamformer_reloadable_shader_kinds[] = {
	BeamformerShaderKind_Decode,
	BeamformerShaderKind_Filter,
	BeamformerShaderKind_DAS,
	BeamformerShaderKind_Sum,
	BeamformerShaderKind_MinMax,
	BeamformerShaderKind_CoherencyWeighting,
	BeamformerShaderKind_BufferClear,
	BeamformerShaderKind_RenderBeamformed,
};

read_only global s8 *beamformer_reloadable_shader_files[] = {
	(s8 []){s8_comp("decode.glsl")},
	(s8 []){s8_comp("filter.glsl")},
	(s8 []){s8_comp("das.glsl")},
	(s8 []){s8_comp("sum.glsl")},
	(s8 []){s8_comp("min_max.glsl")},
	(s8 []){s8_comp("coherency_weighting.glsl")},
	(s8 []){s8_comp("buffer_clear.glsl")},
	(s8 []){s8_comp("render_3d.vert.glsl"), s8_comp("render_3d.frag.glsl")},
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
	6,
	7,
};

read_only global i32 beamformer_reloadable_compute_shader_info_indices[] = {
	0,
	1,
	2,
	3,
	4,
};

read_only global i32 beamformer_reloadable_compute_helpers_shader_info_indices[] = {
	5,
};

read_only global i32 beamformer_reloadable_compute_internal_shader_info_indices[] = {
	6,
};

read_only global i32 beamformer_reloadable_render_shader_info_indices[] = {
	7,
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
		s8_comp("Sparse"),
		s8_comp("CoherencyWeighting"),
		s8_comp("SingleFocus"),
		s8_comp("SingleOrientation"),
	},
	0,
	0,
	0,
	0,
	0,
};

read_only global u8 beamformer_shader_flag_strings_count[] = {
	2,
	3,
	4,
	0,
	0,
	0,
	0,
	0,
};

read_only global b8 beamformer_shader_has_primitive[] = {
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	1,
};

read_only global b8 beamformer_shader_primitive_is_vertex[] = {
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	1,
};

read_only global i32 *beamformer_shader_header_vectors[] = {
	(i32 []){0, 1},
	(i32 []){0},
	(i32 []){2, 0, 3, 4},
	(i32 []){0},
	0,
	(i32 []){0},
	0,
	(i32 []){0},
};

read_only global i32 beamformer_shader_header_vector_lengths[] = {
	2,
	1,
	4,
	1,
	0,
	1,
	0,
	1,
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
		s8_comp("ChannelChunkCount"),
		s8_comp("InterpolationMode"),
		s8_comp("SampleCount"),
		s8_comp("TransmitReceiveOrientation"),
		s8_comp("DemodulationFrequency"),
		s8_comp("FNumber"),
		s8_comp("FocusDepth"),
		s8_comp("SamplingFrequency"),
		s8_comp("SpeedOfSound"),
		s8_comp("TimeOffset"),
		s8_comp("TransmitAngle"),
	},
	0,
	0,
	0,
	0,
	0,
};

read_only global u32 beamformer_shader_bake_parameter_float_bits[] = {
	0x00000000UL,
	0x00000600UL,
	0x00003f80UL,
	0x00000000UL,
	0x00000000UL,
	0x00000000UL,
	0x00000000UL,
	0x00000000UL,
};

read_only global i32 beamformer_shader_bake_parameter_counts[] = {
	9,
	11,
	14,
	0,
	0,
	0,
	0,
	0,
};

read_only global s8 *beamformer_shader_push_constant_vk_types[] = {
	(s8 []){
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
		s8_comp("bool"),
	},
	(s8 []){
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
	},
	(s8 []){
		s8_comp("mat4"),
		s8_comp("mat4"),
		s8_comp("vec2"),
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
		s8_comp("uint32_t"),
		s8_comp("uint32_t"),
		s8_comp("uint32_t"),
		s8_comp("uint32_t"),
		s8_comp("int32_t"),
	},
	(s8 []){
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
		s8_comp("uint32_t"),
		s8_comp("float"),
	},
	0,
	(s8 []){
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
		s8_comp("uint32_t"),
		s8_comp("float"),
		s8_comp("uint32_t"),
		s8_comp("uint32_t"),
		s8_comp("uint32_t"),
	},
	(s8 []){
		s8_comp("uint64_t"),
		s8_comp("uint32_t"),
		s8_comp("uint32_t"),
	},
	(s8 []){
		s8_comp("mat4"),
		s8_comp("uint64_t"),
		s8_comp("uint64_t"),
		s8_comp("vec4"),
		s8_comp("float"),
		s8_comp("float"),
		s8_comp("float"),
		s8_comp("float"),
		s8_comp("uint64_t"),
		s8_comp("uint32_t"),
		s8_comp("uint32_t"),
		s8_comp("uint32_t"),
		s8_comp("uint32_t"),
	},
};

read_only global s8 *beamformer_shader_push_constant_names[] = {
	(s8 []){
		s8_comp("hadamard_buffer"),
		s8_comp("rf_buffer"),
		s8_comp("output_buffer"),
		s8_comp("output_rf_buffer"),
		s8_comp("first_pass"),
	},
	(s8 []){
		s8_comp("input_data"),
		s8_comp("output_data"),
		s8_comp("filter_coefficients"),
	},
	(s8 []){
		s8_comp("voxel_transform"),
		s8_comp("xdc_transform"),
		s8_comp("xdc_element_pitch"),
		s8_comp("rf_data"),
		s8_comp("output_data"),
		s8_comp("incoherent_output"),
		s8_comp("focal_vectors"),
		s8_comp("sparse_elements"),
		s8_comp("transmit_receive_orientations"),
		s8_comp("output_size_x"),
		s8_comp("output_size_y"),
		s8_comp("output_size_z"),
		s8_comp("cycle_t"),
		s8_comp("channel_offset"),
	},
	(s8 []){
		s8_comp("output_data"),
		s8_comp("input_data"),
		s8_comp("image_elements"),
		s8_comp("scale"),
	},
	0,
	(s8 []){
		s8_comp("left_side_buffer"),
		s8_comp("right_side_buffer"),
		s8_comp("elements"),
		s8_comp("scale"),
		s8_comp("output_size_x"),
		s8_comp("output_size_y"),
		s8_comp("output_size_z"),
	},
	(s8 []){
		s8_comp("data"),
		s8_comp("clear_word"),
		s8_comp("words"),
	},
	(s8 []){
		s8_comp("mvp_matrix"),
		s8_comp("positions"),
		s8_comp("normals"),
		s8_comp("bounding_box_colour"),
		s8_comp("bounding_box_fraction"),
		s8_comp("db_cutoff"),
		s8_comp("threshold"),
		s8_comp("gamma"),
		s8_comp("input_data"),
		s8_comp("input_size_x"),
		s8_comp("input_size_y"),
		s8_comp("input_size_z"),
		s8_comp("data_kind"),
	},
};

read_only global u8 beamformer_shader_push_constant_counts[] = {
	5,
	3,
	14,
	4,
	0,
	7,
	3,
	13,
};

read_only global u8 beamformer_shader_push_constant_sizes[] = {
	sizeof(BeamformerShaderDecodePushConstants),
	sizeof(BeamformerShaderFilterPushConstants),
	sizeof(BeamformerShaderDASPushConstants),
	sizeof(BeamformerShaderSumPushConstants),
	0,
	sizeof(BeamformerShaderCoherencyWeightingPushConstants),
	sizeof(BeamformerShaderBufferClearPushConstants),
	sizeof(BeamformerShaderRenderBeamformedPushConstants),
};

