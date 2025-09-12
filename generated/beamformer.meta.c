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
	BeamformerShaderFilterFlags_MapChannels   = (1 << 0),
	BeamformerShaderFilterFlags_ComplexFilter = (1 << 1),
	BeamformerShaderFilterFlags_Demodulate    = (1 << 2),
} BeamformerShaderFilterFlags;

typedef enum {
	BeamformerShaderDASFlags_Fast               = (1 << 0),
	BeamformerShaderDASFlags_Sparse             = (1 << 1),
	BeamformerShaderDASFlags_Interpolate        = (1 << 2),
	BeamformerShaderDASFlags_CoherencyWeighting = (1 << 3),
	BeamformerShaderDASFlags_RxColumns          = (1 << 4),
	BeamformerShaderDASFlags_TxColumns          = (1 << 5),
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
	i32 first_match_vector_index;
	i32 one_past_last_match_vector_index;
	i16 match_vector_length;
	i16 header_vector_length;
	b32 has_local_flags;
} BeamformerShaderDescriptor;

typedef struct {
	BeamformerShaderKind kind;
	i32                  sub_shader_descriptor_index_count;
	i32 *                sub_shader_descriptor_indices;
} BeamformerReloadableShaderInfo;

read_only global i32 *beamformer_shader_match_vectors[] = {
	// CudaDecode
	0,
	// CudaHilbert
	0,
	// Decode
	(i32 []){BeamformerDataKind_Int16, 0x00},
	(i32 []){BeamformerDataKind_Int16, 0x01},
	(i32 []){BeamformerDataKind_Int16Complex, 0x00},
	(i32 []){BeamformerDataKind_Float32, 0x00},
	(i32 []){BeamformerDataKind_Float32Complex, 0x00},
	// Filter
	(i32 []){BeamformerDataKind_Int16Complex, 0x00},
	(i32 []){BeamformerDataKind_Int16Complex, 0x01},
	(i32 []){BeamformerDataKind_Int16Complex, 0x02},
	(i32 []){BeamformerDataKind_Int16Complex, 0x03},
	(i32 []){BeamformerDataKind_Float32, 0x00},
	(i32 []){BeamformerDataKind_Float32, 0x01},
	(i32 []){BeamformerDataKind_Float32, 0x02},
	(i32 []){BeamformerDataKind_Float32, 0x03},
	(i32 []){BeamformerDataKind_Float32Complex, 0x00},
	(i32 []){BeamformerDataKind_Float32Complex, 0x01},
	(i32 []){BeamformerDataKind_Float32Complex, 0x02},
	(i32 []){BeamformerDataKind_Float32Complex, 0x03},
	// Demodulate
	(i32 []){BeamformerDataKind_Int16, BeamformerSamplingMode_2X, 0x04},
	(i32 []){BeamformerDataKind_Int16, BeamformerSamplingMode_2X, 0x05},
	(i32 []){BeamformerDataKind_Int16, BeamformerSamplingMode_2X, 0x06},
	(i32 []){BeamformerDataKind_Int16, BeamformerSamplingMode_2X, 0x07},
	(i32 []){BeamformerDataKind_Int16, BeamformerSamplingMode_4X, 0x04},
	(i32 []){BeamformerDataKind_Int16, BeamformerSamplingMode_4X, 0x05},
	(i32 []){BeamformerDataKind_Int16, BeamformerSamplingMode_4X, 0x06},
	(i32 []){BeamformerDataKind_Int16, BeamformerSamplingMode_4X, 0x07},
	(i32 []){BeamformerDataKind_Int16, -1, 0x04},
	(i32 []){BeamformerDataKind_Int16, -1, 0x05},
	(i32 []){BeamformerDataKind_Int16, -1, 0x06},
	(i32 []){BeamformerDataKind_Int16, -1, 0x07},
	(i32 []){BeamformerDataKind_Float32, BeamformerSamplingMode_2X, 0x04},
	(i32 []){BeamformerDataKind_Float32, BeamformerSamplingMode_2X, 0x05},
	(i32 []){BeamformerDataKind_Float32, BeamformerSamplingMode_2X, 0x06},
	(i32 []){BeamformerDataKind_Float32, BeamformerSamplingMode_2X, 0x07},
	(i32 []){BeamformerDataKind_Float32, BeamformerSamplingMode_4X, 0x04},
	(i32 []){BeamformerDataKind_Float32, BeamformerSamplingMode_4X, 0x05},
	(i32 []){BeamformerDataKind_Float32, BeamformerSamplingMode_4X, 0x06},
	(i32 []){BeamformerDataKind_Float32, BeamformerSamplingMode_4X, 0x07},
	(i32 []){BeamformerDataKind_Float32, -1, 0x04},
	(i32 []){BeamformerDataKind_Float32, -1, 0x05},
	(i32 []){BeamformerDataKind_Float32, -1, 0x06},
	(i32 []){BeamformerDataKind_Float32, -1, 0x07},
	// DAS
	(i32 []){BeamformerDataKind_Float32, 0x00},
	(i32 []){BeamformerDataKind_Float32, 0x01},
	(i32 []){BeamformerDataKind_Float32, 0x02},
	(i32 []){BeamformerDataKind_Float32, 0x03},
	(i32 []){BeamformerDataKind_Float32, 0x04},
	(i32 []){BeamformerDataKind_Float32, 0x05},
	(i32 []){BeamformerDataKind_Float32, 0x06},
	(i32 []){BeamformerDataKind_Float32, 0x07},
	(i32 []){BeamformerDataKind_Float32Complex, 0x00},
	(i32 []){BeamformerDataKind_Float32Complex, 0x01},
	(i32 []){BeamformerDataKind_Float32Complex, 0x02},
	(i32 []){BeamformerDataKind_Float32Complex, 0x03},
	(i32 []){BeamformerDataKind_Float32Complex, 0x04},
	(i32 []){BeamformerDataKind_Float32Complex, 0x05},
	(i32 []){BeamformerDataKind_Float32Complex, 0x06},
	(i32 []){BeamformerDataKind_Float32Complex, 0x07},
	// MinMax
	0,
	// Sum
	0,
	// Render3D
	0,
};
#define beamformer_match_vectors_count (62)

read_only global BeamformerShaderDescriptor beamformer_shader_descriptors[] = {
	{0,  1,  0, 0, 0},
	{1,  2,  0, 0, 0},
	{2,  7,  1, 2, 1},
	{7,  19, 1, 1, 1},
	{19, 43, 2, 2, 1},
	{43, 59, 1, 2, 1},
	{59, 60, 0, 0, 0},
	{60, 61, 0, 0, 0},
	{61, 62, 0, 0, 0},
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

read_only global BeamformerReloadableShaderInfo beamformer_reloadable_shader_infos[] = {
	{BeamformerShaderKind_Decode,   0, 0},
	{BeamformerShaderKind_Filter,   1, (i32 []){4}},
	{BeamformerShaderKind_DAS,      0, 0},
	{BeamformerShaderKind_MinMax,   0, 0},
	{BeamformerShaderKind_Sum,      0, 0},
	{BeamformerShaderKind_Render3D, 0, 0},
};

read_only global s8 beamformer_reloadable_shader_files[] = {
	s8_comp("decode.glsl"),
	s8_comp("filter.glsl"),
	s8_comp("das.glsl"),
	s8_comp("min_max.glsl"),
	s8_comp("sum.glsl"),
	s8_comp("render_3d.frag.glsl"),
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
	"#define ShaderFlags_MapChannels   (1 << 0)\n"
	"#define ShaderFlags_ComplexFilter (1 << 1)\n"
	"#define ShaderFlags_Demodulate    (1 << 2)\n"
	"\n"),
	s8_comp(""
	"#define ShaderFlags_Fast               (1 << 0)\n"
	"#define ShaderFlags_Sparse             (1 << 1)\n"
	"#define ShaderFlags_Interpolate        (1 << 2)\n"
	"#define ShaderFlags_CoherencyWeighting (1 << 3)\n"
	"#define ShaderFlags_RxColumns          (1 << 4)\n"
	"#define ShaderFlags_TxColumns          (1 << 5)\n"
	"\n"),
	{0},
	{0},
	{0},
};

read_only global s8 beamformer_shader_descriptor_header_strings[] = {
	s8_comp("DataKind"),
	s8_comp("DecodeMode"),
	s8_comp("RCAOrientation"),
	s8_comp("SamplingMode"),
};

read_only global i32 *beamformer_shader_header_vectors[] = {
	0,
	0,
	(i32 []){0, 1},
	(i32 []){0},
	(i32 []){0, 3},
	(i32 []){0, 2},
	0,
	0,
	0,
};

function iz
beamformer_shader_match(i32 *match_vector, i32 first_index, i32 one_past_last_index, i32 vector_length)
{
	iz result = first_index;
	i32 best_score = 0;
	for (i32 index = first_index; index < one_past_last_index; index++)
	{
		i32 score = 0;
		i32 *v = beamformer_shader_match_vectors[index];
		for (i32 i = 0; i < vector_length; i++) {
			if (match_vector[i] == v[i]) {
				score++;
			}
		}
		if (best_score < score) {
			result     = index;
			best_score = score;
		}
	}
	return result;
}

function iz
beamformer_shader_decode_match(BeamformerDataKind a, i32 flags)
{
	iz result = beamformer_shader_match((i32 []){(i32)a, flags}, 2, 7, 2);
	return result;
}

function iz
beamformer_shader_filter_match(BeamformerDataKind a, i32 flags)
{
	iz result = beamformer_shader_match((i32 []){(i32)a, flags}, 7, 19, 2);
	return result;
}

function iz
beamformer_shader_demodulate_match(BeamformerDataKind a, BeamformerSamplingMode b, i32 flags)
{
	iz result = beamformer_shader_match((i32 []){(i32)a, (i32)b, flags}, 19, 43, 3);
	return result;
}

function iz
beamformer_shader_das_match(BeamformerDataKind a, i32 flags)
{
	iz result = beamformer_shader_match((i32 []){(i32)a, flags}, 43, 59, 2);
	return result;
}

