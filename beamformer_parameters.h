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

/* X(enumarant, shader file name, pretty name) */
#define COMPUTE_SHADERS \
	X(CudaDecode,  "",        "CUDA Decode")      \
	X(CudaHilbert, "",        "CUDA Hilbert")     \
	X(DAS,         "das",     "DAS")              \
	X(Decode,      "decode",  "Decode (I16)")     \
	X(Filter,      "filter",  "Filter (F32C)")    \
	X(Demodulate,  "",        "Demodulate (I16)") \
	X(MinMax,      "min_max", "Min/Max")          \
	X(Sum,         "sum",     "Sum")

#define DECODE_SHADER_VARIATIONS \
	X(DecodeInt16Complex, "", "Decode (I16C)",    " (I16)")     \
	X(DecodeFloat,        "", "Decode (F32)",     " (F32)")     \
	X(DecodeFloatComplex, "", "Decode (F32C)",    " (F32C)")    \
	X(DecodeInt16ToFloat, "", "Decode (I16-F32)", " (I16-F32)")

#define FILTER_SHADER_VARIATIONS \
	X(FilterCF,          "", "Filter (F32C-CF)",    " (F32C-CF)") \
	X(DemodulateCF,      "", "Demodulate (I16-CF)", " (I16-CF)")  \
	X(DemodulateFloat,   "", "Demodulate (F32)",    " (F32)")     \
	X(DemodulateFloatCF, "", "Demodulate (F32-CF)", " (F32-CF)")

#define COMPUTE_SHADERS_INTERNAL \
	COMPUTE_SHADERS              \
	DECODE_SHADER_VARIATIONS     \
	FILTER_SHADER_VARIATIONS     \
	X(DASFast, "", "DAS (Fast)")

typedef enum {
	#define X(e, ...) BeamformerShaderKind_##e,
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

/* TODO(rnp): this is an absolute abuse of the preprocessor, but now is
 * not a good time to write a full metaprogram */
#define BEAMFORMER_FILTER_KIND_LIST(type, _) \
	X(Kaiser,       type cutoff_frequency _ type beta          _ type length) \
	X(MatchedChirp, type duration         _ type min_frequency _ type max_frequency)

#define X(kind, ...) BeamformerFilterKind_##kind,
typedef enum {BEAMFORMER_FILTER_KIND_LIST(,) BeamformerFilterKind_Count} BeamformerFilterKind;
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

#define FILTER_LOCAL_SIZE_X 64
#define FILTER_LOCAL_SIZE_Y  1
#define FILTER_LOCAL_SIZE_Z  1

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

#define BEAMFORMER_CONSTANTS_LIST \
	X(FilterSlots,                4) \
	X(MaxChannelCount,          256) \
	X(MaxComputeShaderStages,    16) \
	X(MaxParameterBlockSlots,    16) \
	X(MaxRawDataFramesInFlight,   3) \
	X(MaxSavedFrames,            16)
#define X(k, v, ...) Beamformer##k = v,
enum {BEAMFORMER_CONSTANTS_LIST};
#undef X

/* TODO(rnp): actually use a substruct but generate a header compatible with MATLAB */
/* X(name, type, size, elements, gltype, glsize, comment) */
#define BEAMFORMER_UI_PARAMS \
	X(output_min_coordinate, float,    [4], 4, vec4,    , "/* [m] Back-Top-Left corner of output region */")                    \
	X(output_max_coordinate, float,    [4], 4, vec4,    , "/* [m] Front-Bottom-Right corner of output region */")               \
	X(output_points,         int32_t,  [4], 4, uvec4,   , "/* Width * Height * Depth * (Frame Average Count) */")               \
	X(sampling_frequency,    float,       , 1, float,   , "/* [Hz]  */")                                                        \
	X(center_frequency,      float,       , 1, float,   , "/* [Hz]  */")                                                        \
	X(speed_of_sound,        float,       , 1, float,   , "/* [m/s] */")                                                        \
	X(off_axis_pos,          float,       , 1, float,   , "/* [m] Position on screen normal to beamform in TPW/VLSHERCULES */") \
	X(beamform_plane,        int32_t,     , 1, int,     , "/* Plane to Beamform in TPW/VLS/HERCULES */")                        \
	X(f_number,              float,       , 1, float,   , "/* F# (set to 0 to disable) */")                                     \
	X(interpolate,           uint32_t,    , 1, bool,    , "/* Perform Cubic Interpolation of RF Samples */")                    \
	X(coherency_weighting,   uint32_t,    , 1, bool,    , "/* Apply coherency weighting to output data */")                     \
	X(decimation_rate,       uint32_t,    , 1, uint,    , "/* Number of times to decimate */")

#define BEAMFORMER_PARAMS_HEAD \
	X(xdc_transform,     float,    [16], 16, mat4,       , "/* IMPORTANT: column major order */")                                      \
	X(dec_data_dim,      uint32_t, [4] ,  4, ivec4,      , "/* Samples * Channels * Acquisitions; last element ignored */")            \
	X(xdc_element_pitch, float,    [2] ,  2, vec2,       , "/* [m] Transducer Element Pitch {row, col} */")                            \
	X(rf_raw_dim,        uint32_t, [2] ,  2, ivec2,      , "/* Raw Data Dimensions */")                                                \
	X(transmit_mode,     int32_t,      ,  1, int,        , "/* Method/Orientation of Transmit */")                                     \
	X(decode,            uint32_t,     ,  1, uint,       , "/* Decode or just reshape data */")                                        \
	X(das_shader_id,     uint32_t,     ,  1, uint,       , "")                                                                         \
	X(time_offset,       float,        ,  1, float,      , "/* pulse length correction time [s] */")

#define BEAMFORMER_PARAMS_TAIL \
	X(readi_group_id,   uint32_t, , 1, uint, , "/* Which readi group this data is from */") \
	X(readi_group_size, uint32_t, , 1, uint, , "/* Size of readi transmit group */")

#define X(name, type, size, __e, gltype, glsize, comment) type name size;
typedef struct { BEAMFORMER_UI_PARAMS }    BeamformerUIParameters;
typedef struct { BEAMFORMER_PARAMS_HEAD }  BeamformerParametersHead;
typedef struct { BEAMFORMER_PARAMS_TAIL }  BeamformerParametersTail;

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
	X(SaveNameTag,       4) \
	X(StopImaging,       5)
/* NOTE(rnp): if this exceeds 32 you need to fix the flag handling code */

#define BEAMFORMER_LIVE_IMAGING_PARAMETERS_LIST \
	X(active,               uint32_t, ,                               1)   \
	X(save_enabled,         uint32_t, ,                               1)   \
	X(save_active,          uint32_t, ,                               1)   \
	X(transmit_power,       float,    ,                               1)   \
	X(image_plane_offsets,  float,    [BeamformerViewPlaneTag_Count], BeamformerViewPlaneTag_Count) \
	X(tgc_control_points,   float,    [8],                            8)   \
	X(save_name_tag_length, int32_t,  ,                               1)   \
	X(save_name_tag,        char,     [128],                          128)

#define X(name, type, size, ...) type name size;
typedef struct {BEAMFORMER_LIVE_IMAGING_PARAMETERS_LIST} BeamformerLiveImagingParameters;
#undef X
