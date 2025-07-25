/* See LICENSE for license details. */
#include <stdint.h>

/* TODO(rnp):
 * [ ]: Have a method for the library caller to take ownership of a "compute context"
 * [ ]: Upload previously exported data for display. maybe this is a UI thing but doing it
 *      programatically would be nice.
 * [ ]: Add interface for multi frame upload. RF upload already uses an offset into SM so
 *      that part works fine. We just need a way of specify a multi frame upload. (Data must
 *      be organized for simple offset access per frame).
 */

/* X(enumarant, number, shader file name, pretty name) */
#define COMPUTE_SHADERS \
	X(CudaDecode,          0, "",        "CUDA Decode")      \
	X(CudaHilbert,         1, "",        "CUDA Hilbert")     \
	X(DAS,                 2, "das",     "DAS")              \
	X(Decode,              3, "decode",  "Decode (I16)")     \
	X(Demodulate,          4, "demod",   "Demodulate (I16)") \
	X(MinMax,              5, "min_max", "Min/Max")          \
	X(Sum,                 6, "sum",     "Sum")

#define COMPUTE_SHADERS_INTERNAL \
	COMPUTE_SHADERS \
	X(DecodeInt16Complex,  7, "",        "Decode (I16C)") \
	X(DecodeFloat,         8, "",        "Decode (F32)")  \
	X(DecodeFloatComplex,  9, "",        "Decode (F32C)") \
	X(DecodeInt16ToFloat, 10, "",        "Decode (I16-F32)") \
	X(DemodulateFloat,    11, "",        "Demodulate (F32)") \
	X(DASFast,            12, "",        "DAS (Fast)")

typedef enum {
	#define X(e, n, ...) BeamformerShaderKind_##e = n,
	COMPUTE_SHADERS_INTERNAL
	#undef X
	BeamformerShaderKind_Render3D,
	BeamformerShaderKind_Count,

	BeamformerShaderKind_ComputeCount = BeamformerShaderKind_Render3D,
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

#define BEAMFORMER_DATA_KIND_LIST \
	X(Int16,          0) \
	X(Int16Complex,   1) \
	X(Float32,        2) \
	X(Float32Complex, 3)

#define X(k, id) BeamformerDataKind_##k = id,
typedef enum {BEAMFORMER_DATA_KIND_LIST} BeamformerDataKind;
#undef X

#define BEAMFORMER_FILTER_KIND_LIST \
	X(Kaiser,       0) \
	X(MatchedSine,  1)

#define X(k, id) BeamformerFilterKind_##k = id,
typedef enum {BEAMFORMER_FILTER_KIND_LIST} BeamformerFilterKind;
#undef X

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

#define DEMOD_LOCAL_SIZE_X  64
#define DEMOD_LOCAL_SIZE_Y   1
#define DEMOD_LOCAL_SIZE_Z   1

#define DECODE_LOCAL_SIZE_X  4
#define DECODE_LOCAL_SIZE_Y  1
#define DECODE_LOCAL_SIZE_Z 16

#define DECODE_FIRST_PASS_UNIFORM_LOC 1

#define DAS_LOCAL_SIZE_X  16
#define DAS_LOCAL_SIZE_Y   1
#define DAS_LOCAL_SIZE_Z  16

#define DAS_FAST_LOCAL_SIZE_X 16
#define DAS_FAST_LOCAL_SIZE_Y  1
#define DAS_FAST_LOCAL_SIZE_Z 16

#define DAS_VOXEL_OFFSET_UNIFORM_LOC  2
#define DAS_CYCLE_T_UNIFORM_LOC       3
#define DAS_VOXEL_MATRIX_LOC          4
#define DAS_FAST_CHANNEL_UNIFORM_LOC  5

#define MIN_MAX_MIPS_LEVEL_UNIFORM_LOC 1
#define SUM_PRESCALE_UNIFORM_LOC       1

#define MAX_BEAMFORMED_SAVED_FRAMES 16
#define MAX_COMPUTE_SHADER_STAGES   16

#define BEAMFORMER_FILTER_SLOTS      4

/* TODO(rnp): actually use a substruct but generate a header compatible with MATLAB */
/* X(name, type, size, gltype, glsize, comment) */
#define BEAMFORMER_UI_PARAMS \
	X(output_min_coordinate, float,    [4], vec4,    , "/* [m] Back-Top-Left corner of output region */")                    \
	X(output_max_coordinate, float,    [4], vec4,    , "/* [m] Front-Bottom-Right corner of output region */")               \
	X(output_points,         int32_t,  [4], uvec4,   , "/* Width * Height * Depth * (Frame Average Count) */")               \
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
	X(decimation_rate,  uint32_t, , uint, , "/* Number of times to decimate */")         \
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
	float _pad[1];
} BeamformerParametersV0;

/* NOTE: This struct follows the OpenGL std140 layout. DO NOT modify unless you have
 * read and understood the rules, particulary with regards to _member alignment_ */
typedef struct {
	BEAMFORMER_PARAMS_HEAD
	BEAMFORMER_UI_PARAMS
	BEAMFORMER_PARAMS_TAIL
	float _pad[1];
} BeamformerParameters;
#undef X

/* NOTE(rnp): keep this header importable for old C versions */
#if __STDC_VERSION__ >= 201112L
_Static_assert((offsetof(BeamformerParameters, output_min_coordinate) & 15) == 0,
               "BeamformerParameters.output_min_coordinate must lie on a 16 byte boundary");
_Static_assert((sizeof(BeamformerParameters) & 15) == 0, "UBO size must be a multiple of 16");
#endif

#define BEAMFORMER_LIVE_IMAGING_DIRTY_FLAG_LIST \
	X(ImagePlaneOffsets, 0) \
	X(TransmitPower,     1) \
	X(TGCControlPoints,  2) \
	X(SaveData,          3) \
	X(StopImaging,       4)
/* NOTE(rnp): if this exceeds 32 you need to fix the flag handling code */

typedef struct {
	uint32_t active;
	uint32_t save_enabled;
	uint32_t save_active;
	float    transmit_power;
	float    image_plane_offsets[BeamformerViewPlaneTag_Count];
	float    tgc_control_points[8];
} BeamformerLiveImagingParameters;
