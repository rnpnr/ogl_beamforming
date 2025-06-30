/* See LICENSE for license details. */
#include <stdint.h>

/* TODO(rnp):
 * [ ]: Have a method for the library caller to take ownership of a "compute context"
 * [ ]: Upload previously exported data for display. maybe this is a UI thing but doing it
 *      programatically would be nice.
 */

/* X(enumarant, number, shader file name, needs header, pretty name) */
#define COMPUTE_SHADERS \
	X(CudaDecode,         0, "",         0, "CUDA Decode")   \
	X(CudaHilbert,        1, "",         0, "CUDA Hilbert")  \
	X(DASCompute,         2, "das",      1, "DAS")           \
	X(Decode,             3, "decode",   1, "Decode")        \
	X(DecodeFloat,        4, "",         1, "Decode (F32)")  \
	X(DecodeFloatComplex, 5, "",         1, "Decode (F32C)") \
	X(Demodulate,         6, "demod",    1, "Demodulate")    \
	X(MinMax,             7, "min_max",  0, "Min/Max")       \
	X(Sum,                8, "sum",      0, "Sum")

typedef enum {
	#define X(e, n, s, h, pn) BeamformerShaderKind_##e = n,
	COMPUTE_SHADERS
	#undef X
	BeamformerShaderKind_Render2D,
	BeamformerShaderKind_Render3D,
	BeamformerShaderKind_Count,

	BeamformerShaderKind_ComputeCount = BeamformerShaderKind_Render2D,
} BeamformerShaderKind;

typedef struct {
	/* NOTE(rnp): this wants to be iterated on both dimensions. it depends entirely on which
	 * visualization method you want to use. the coalescing function wants both directions */
	float times[32][BeamformerShaderKind_Count];
	float rf_time_deltas[32];
} BeamformerComputeStatsTable;

/* X(type, id, pretty name) */
#define DECODE_TYPES \
	X(NONE,     0, "None")     \
	X(HADAMARD, 1, "Hadamard")

/* X(type, id, pretty name) */
#define BEAMFORMER_VIEW_PLANE_TAG_LIST \
	X(XZ,        0, "XZ")        \
	X(YZ,        1, "YZ")        \
	X(XY,        2, "XY")        \
	X(Arbitrary, 3, "Arbitrary")

typedef enum {
	#define X(type, id, pretty) BeamformerViewPlaneTag_##type = id,
	BEAMFORMER_VIEW_PLANE_TAG_LIST
	#undef X
	BeamformerViewPlaneTag_Count,
} BeamformerViewPlaneTag;

/* X(type, id, pretty name, fixed transmits) */
#define DAS_TYPES \
	X(FORCES,          0, "FORCES",         1) \
	X(UFORCES,         1, "UFORCES",        0) \
	X(HERCULES,        2, "HERCULES",       1) \
	X(RCA_VLS,         3, "VLS",            0) \
	X(RCA_TPW,         4, "TPW",            0) \
	X(UHERCULES,       5, "UHERCULES",      0) \
	X(RACES,           6, "RACES",          1) \
	X(EPIC_FORCES,     7, "EPIC-FORCES",    1) \
	X(EPIC_UFORCES,    8, "EPIC-UFORCES",   0) \
	X(EPIC_UHERCULES,  9, "EPIC-UHERCULES", 0) \
	X(FLASH,          10, "Flash",          0)

#define DECODE_LOCAL_SIZE_X  4
#define DECODE_LOCAL_SIZE_Y  1
#define DECODE_LOCAL_SIZE_Z 16

#define DECODE_FIRST_PASS_UNIFORM_LOC 1

#define DAS_LOCAL_SIZE_X 32
#define DAS_LOCAL_SIZE_Y  1
#define DAS_LOCAL_SIZE_Z 32

#define DAS_VOXEL_OFFSET_UNIFORM_LOC 2
#define DAS_CYCLE_T_UNIFORM_LOC      3

#define MIN_MAX_MIPS_LEVEL_UNIFORM_LOC 1
#define SUM_PRESCALE_UNIFORM_LOC       1

#define MAX_BEAMFORMED_SAVED_FRAMES 16
#define MAX_COMPUTE_SHADER_STAGES   16

/* TODO(rnp): actually use a substruct but generate a header compatible with MATLAB */
/* X(name, type, size, gltype, glsize, comment) */
#define BEAMFORMER_UI_PARAMS \
	X(output_min_coordinate, float,    [4], vec4,    , "/* [m] Back-Top-Left corner of output region */")                    \
	X(output_max_coordinate, float,    [4], vec4,    , "/* [m] Front-Bottom-Right corner of output region */")               \
	X(output_points,         uint32_t, [4], uvec4,   , "/* Width * Height * Depth * (Frame Average Count) */")               \
	X(sampling_frequency,    float,       , float,   , "/* [Hz]  */")                                                        \
	X(center_frequency,      float,       , float,   , "/* [Hz]  */")                                                        \
	X(speed_of_sound,        float,       , float,   , "/* [m/s] */")                                                        \
	X(off_axis_pos,          float,       , float,   , "/* [m] Position on screen normal to beamform in TPW/VLSHERCULES */") \
	X(beamform_plane,        int32_t,     , int,     , "/* Plane to Beamform in TPW/VLS/HERCULES */")                        \
	X(f_number,              float,       , float,   , "/* F# (set to 0 to disable) */")                                     \
	X(interpolate,           uint32_t,    , bool,    , "/* Perform Cubic Interpolation of RF Samples */")                    \
	X(coherency_weighting,   uint32_t,    , bool,    , "/* Apply coherency weighting to output data */")

#define BEAMFORMER_PARAMS_HEAD_V0 \
	X(channel_mapping,   uint16_t, [256], uvec4, [32], "/* Transducer Channel to Verasonics Channel */")                           \
	X(uforces_channels,  uint16_t, [256], uvec4, [32], "/* Channels used for virtual UFORCES elements */")                         \
	X(focal_depths,      float,    [256], vec4,  [64], "/* [m] Focal Depths for each transmit of a RCA imaging scheme*/")          \
	X(transmit_angles,   float,    [256], vec4,  [64], "/* [degrees] Transmit Angles for each transmit of a RCA imaging scheme*/") \
	X(xdc_transform,     float,    [16] , mat4,      , "/* IMPORTANT: column major order */")                                      \
	X(dec_data_dim,      uint32_t, [4]  , uvec4,     , "/* Samples * Channels * Acquisitions; last element ignored */")            \
	X(xdc_element_pitch, float,    [2]  , vec2,      , "/* [m] Transducer Element Pitch {row, col} */")                            \
	X(rf_raw_dim,        uint32_t, [2]  , uvec2,     , "/* Raw Data Dimensions */")                                                \
	X(transmit_mode,     int32_t,       , int,       , "/* Method/Orientation of Transmit */")                                     \
	X(decode,            uint32_t,      , uint,      , "/* Decode or just reshape data */")                                        \
	X(das_shader_id,     uint32_t,      , uint,      , "")                                                                         \
	X(time_offset,       float,         , float,     , "/* pulse length correction time [s] */")

#define BEAMFORMER_PARAMS_HEAD \
	X(xdc_transform,     float,    [16], mat4,       , "/* IMPORTANT: column major order */")                                      \
	X(dec_data_dim,      uint32_t, [4] , uvec4,      , "/* Samples * Channels * Acquisitions; last element ignored */")            \
	X(xdc_element_pitch, float,    [2] , vec2,       , "/* [m] Transducer Element Pitch {row, col} */")                            \
	X(rf_raw_dim,        uint32_t, [2] , uvec2,      , "/* Raw Data Dimensions */")                                                \
	X(transmit_mode,     int32_t,      , int,        , "/* Method/Orientation of Transmit */")                                     \
	X(decode,            uint32_t,     , uint,       , "/* Decode or just reshape data */")                                        \
	X(das_shader_id,     uint32_t,     , uint,       , "")                                                                         \
	X(time_offset,       float,        , float,      , "/* pulse length correction time [s] */")

#define BEAMFORMER_PARAMS_TAIL \
	X(readi_group_id,   uint32_t, , uint, , "/* Which readi group this data is from */") \
	X(readi_group_size, uint32_t, , uint, , "/* Size of readi transmit group */")

#define X(name, type, size, gltype, glsize, comment) type name size;
typedef struct { BEAMFORMER_UI_PARAMS }    BeamformerUIParameters;
typedef struct { BEAMFORMER_PARAMS_HEAD }  BeamformerParametersHead;
typedef struct { BEAMFORMER_PARAMS_TAIL }  BeamformerParametersTail;

typedef struct {
	BEAMFORMER_PARAMS_HEAD_V0
	BEAMFORMER_UI_PARAMS
	BEAMFORMER_PARAMS_TAIL
	float _pad[2];
} BeamformerParametersV0;

/* NOTE: This struct follows the OpenGL std140 layout. DO NOT modify unless you have
 * read and understood the rules, particulary with regards to _member alignment_ */
typedef struct {
	BEAMFORMER_PARAMS_HEAD
	BEAMFORMER_UI_PARAMS
	BEAMFORMER_PARAMS_TAIL
	float _pad[2];
} BeamformerParameters;
#undef X

/* NOTE(rnp): keep this header importable for old C versions */
#if __STDC_VERSION__ >= 201112L
_Static_assert((offsetof(BeamformerParameters, output_min_coordinate) & 15) == 0,
               "BeamformerParameters.output_min_coordinate must lie on a 16 byte boundary");
_Static_assert((sizeof(BeamformerParameters) & 15) == 0,
               "sizeof(BeamformerParameters) must be a multiple of 16");
#endif
