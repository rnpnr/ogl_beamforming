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
		u32 shader_flags;
		u32 transmit_count;
	};
	u32 E[10];
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
		u32 shader_flags;
		u32 sampling_mode;
	};
	u32 E[11];
} BeamformerShaderFilterBakeParameters;

typedef union {
	struct {
		u32 acquisition_count;
		u32 channel_count;
		u32 data_kind;
		u32 sample_count;
		u32 shader_flags;
		u32 shader_kind;
	};
	u32 E[6];
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
};

read_only global s8 beamformer_shader_local_header_strings[] = {
	s8_comp(""
	"#define ShaderFlags_DilateOutput (1 << 0)\n"
	"\n"),
	s8_comp(""
	"#define ShaderFlags_ComplexFilter (1 << 0)\n"
	"#define ShaderFlags_MapChannels   (1 << 1)\n"
	"#define ShaderFlags_Demodulate    (1 << 2)\n"
	"\n"),
	s8_comp(""
	"#define ShaderFlags_Fast               (1 << 0)\n"
	"#define ShaderFlags_Sparse             (1 << 1)\n"
	"#define ShaderFlags_Interpolate        (1 << 2)\n"
	"#define ShaderFlags_CoherencyWeighting (1 << 3)\n"
	"\n"),
	{0},
	{0},
	{0},
};

read_only global i32 *beamformer_shader_header_vectors[] = {
	(i32 []){0, 1},
	(i32 []){0, 3},
	(i32 []){0, 2},
	0,
	0,
	0,
};

read_only global i32 beamformer_shader_header_vector_lengths[] = {
	2,
	2,
	2,
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
		s8_comp("ShaderFlags"),
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
		s8_comp("ShaderFlags"),
		s8_comp("SamplingMode"),
	},
	(s8 []){
		s8_comp("AcquisitionCount"),
		s8_comp("ChannelCount"),
		s8_comp("DataKind"),
		s8_comp("SampleCount"),
		s8_comp("ShaderFlags"),
		s8_comp("ShaderKind"),
	},
	0,
	0,
	0,
};

read_only global i32 beamformer_shader_bake_parameter_name_counts[] = {
	10,
	11,
	6,
	0,
	0,
	0,
};

