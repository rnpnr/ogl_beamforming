/* See LICENSE for license details. */

// GENERATED CODE

typedef enum {
	BeamformerDataKind_Int16          = 0,
	BeamformerDataKind_Int16Complex   = 1,
	BeamformerDataKind_Float32        = 2,
	BeamformerDataKind_Float32Complex = 3,
	BeamformerDataKind_Count,
} BeamformerDataKind;

typedef enum {
	BeamformerDecodeMode_None     = 0,
	BeamformerDecodeMode_Hadamard = 1,
	BeamformerDecodeMode_Count,
} BeamformerDecodeMode;

typedef enum {
	BeamformerRCAOrientation_Rows    = 0,
	BeamformerRCAOrientation_Columns = 1,
	BeamformerRCAOrientation_Count,
} BeamformerRCAOrientation;

typedef enum {
	BeamformerSamplingMode_2X = 0,
	BeamformerSamplingMode_4X = 1,
	BeamformerSamplingMode_Count,
} BeamformerSamplingMode;

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
	BeamformerShaderDecodeFlags_DilateOutput = (1 << 0),
} BeamformerShaderDecodeFlags;

typedef enum {
	BeamformerShaderFilterFlags_ComplexFilter = (1 << 0),
	BeamformerShaderFilterFlags_MapChannels   = (1 << 1),
	BeamformerShaderFilterFlags_Demodulate    = (1 << 2),
} BeamformerShaderFilterFlags;

typedef enum {
	BeamformerShaderDASFlags_Fast               = (1 << 0),
	BeamformerShaderDASFlags_Sparse             = (1 << 1),
	BeamformerShaderDASFlags_Interpolate        = (1 << 2),
	BeamformerShaderDASFlags_CoherencyWeighting = (1 << 3),
	BeamformerShaderDASFlags_ReceiveOnly        = (1 << 4),
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

typedef union {
	struct {
		u32 data_kind;
		u32 decode_mode;
		u32 input_channel_stride;
		u32 input_sample_stride;
		u32 input_transmit_stride;
		u32 output_channel_stride;
		u32 output_sample_stride;
		u32 output_transmit_stride;
		u32 transmit_count;
	};
	u32 E[9];
} BeamformerShaderDecodeBakeParameters;

typedef union {
	struct {
		u32 data_kind;
		u32 decimation_rate;
		u32 filter_length;
		u32 input_channel_stride;
		u32 input_sample_stride;
		u32 input_transmit_stride;
		u32 output_channel_stride;
		u32 output_sample_stride;
		u32 output_transmit_stride;
		u32 sampling_mode;
		f32 demodulation_frequency;
		f32 sampling_frequency;
	};
	u32 E[12];
} BeamformerShaderFilterBakeParameters;

typedef union {
	struct {
		u32 acquisition_count;
		u32 channel_count;
		u32 data_kind;
		u32 sample_count;
		u32 acquisition_kind;
		f32 demodulation_frequency;
		f32 f_number;
		f32 sampling_frequency;
		f32 speed_of_sound;
		f32 time_offset;
	};
	u32 E[10];
} BeamformerShaderDASBakeParameters;

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
	"#define RCAOrientation_Rows    0\n"
	"#define RCAOrientation_Columns 1\n"
	"\n"),
	s8_comp(""
	"#define SamplingMode_2X 0\n"
	"#define SamplingMode_4X 1\n"
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
};

read_only global s8 *beamformer_shader_flag_strings[] = {
	(s8 []){
		s8_comp("DilateOutput"),
	},
	(s8 []){
		s8_comp("ComplexFilter"),
		s8_comp("MapChannels"),
		s8_comp("Demodulate"),
	},
	(s8 []){
		s8_comp("Fast"),
		s8_comp("Sparse"),
		s8_comp("Interpolate"),
		s8_comp("CoherencyWeighting"),
		s8_comp("ReceiveOnly"),
	},
	0,
	0,
	0,
};

read_only global u8 beamformer_shader_flag_strings_count[] = {
	1,
	3,
	5,
	0,
	0,
	0,
};

read_only global i32 *beamformer_shader_header_vectors[] = {
	(i32 []){0, 1},
	(i32 []){0, 3},
	(i32 []){4, 0, 2},
	0,
	0,
	0,
};

read_only global i32 beamformer_shader_header_vector_lengths[] = {
	2,
	2,
	3,
	0,
	0,
	0,
};

read_only global s8 *beamformer_shader_bake_parameter_names[] = {
	(s8 []){
		s8_comp("DataKind"),
		s8_comp("DecodeMode"),
		s8_comp("InputChannelStride"),
		s8_comp("InputSampleStride"),
		s8_comp("InputTransmitStride"),
		s8_comp("OutputChannelStride"),
		s8_comp("OutputSampleStride"),
		s8_comp("OutputTransmitStride"),
		s8_comp("TransmitCount"),
	},
	(s8 []){
		s8_comp("DataKind"),
		s8_comp("DecimationRate"),
		s8_comp("FilterLength"),
		s8_comp("InputChannelStride"),
		s8_comp("InputSampleStride"),
		s8_comp("InputTransmitStride"),
		s8_comp("OutputChannelStride"),
		s8_comp("OutputSampleStride"),
		s8_comp("OutputTransmitStride"),
		s8_comp("SamplingMode"),
		s8_comp("DemodulationFrequency"),
		s8_comp("SamplingFrequency"),
	},
	(s8 []){
		s8_comp("AcquisitionCount"),
		s8_comp("ChannelCount"),
		s8_comp("DataKind"),
		s8_comp("SampleCount"),
		s8_comp("AcquisitionKind"),
		s8_comp("DemodulationFrequency"),
		s8_comp("FNumber"),
		s8_comp("SamplingFrequency"),
		s8_comp("SpeedOfSound"),
		s8_comp("TimeOffset"),
	},
	0,
	0,
	0,
};

read_only global u8 *beamformer_shader_bake_parameter_is_float[] = {
	(u8 []){0, 0, 0, 0, 0, 0, 0, 0, 0},
	(u8 []){0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1},
	(u8 []){0, 0, 0, 0, 0, 1, 1, 1, 1, 1},
	0,
	0,
	0,
};

read_only global i32 beamformer_shader_bake_parameter_counts[] = {
	9,
	12,
	10,
	0,
	0,
	0,
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

