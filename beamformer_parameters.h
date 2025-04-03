/* See LICENSE for license details. */

/* X(enumarant, number, shader file name, needs header, pretty name) */
#define COMPUTE_SHADERS \
	X(CUDA_DECODE,           0, "",         0, "CUDA Decoding")   \
	X(CUDA_HILBERT,          1, "",         0, "CUDA Hilbert")    \
	X(DAS,                   2, "das",      1, "DAS")             \
	X(DECODE,                3, "decode",   1, "Decoding")        \
	X(DECODE_FLOAT,          4, "",         1, "Decoding (F32)")  \
	X(DECODE_FLOAT_COMPLEX,  5, "",         1, "Decoding (F32C)") \
	X(DEMOD,                 6, "demod",    1, "Demodulation")    \
	X(MIN_MAX,               7, "min_max",  0, "Min/Max")         \
	X(SUM,                   8, "sum",      0, "Sum")

typedef enum {
	#define X(e, n, s, h, pn) CS_ ##e = n,
	COMPUTE_SHADERS
	#undef X
	CS_LAST
} ComputeShaderID;

/* X(type, id, pretty name) */
#define DECODE_TYPES \
	X(NONE,     0, "None")     \
	X(HADAMARD, 1, "Hadamard")

/* X(type, id, pretty name, fixed transmits) */
#define DAS_TYPES \
	X(FORCES,       0, "FORCES",       1) \
	X(UFORCES,      1, "UFORCES",      0) \
	X(HERCULES,     2, "HERCULES",     1) \
	X(RCA_VLS,      3, "VLS",          0) \
	X(RCA_TPW,      4, "TPW",          0) \
	X(UHERCULES,    5, "UHERCULES",    0) \
	X(ACE_HERCULES, 6, "ACE-HERCULES", 1)

#define DAS_LOCAL_SIZE_X 32
#define DAS_LOCAL_SIZE_Y  1
#define DAS_LOCAL_SIZE_Z 32

#define MAX_BEAMFORMED_SAVED_FRAMES 16

/* TODO(rnp): actually use a substruct but generate a header compatible with MATLAB */
/* X(name, type, size, gltype, glsize, comment) */
#define BEAMFORMER_UI_PARAMS \
	X(output_min_coordinate, v4,  , vec4,  , "/* [m] Back-Top-Left corner of output region */")                    \
	X(output_max_coordinate, v4,  , vec4,  , "/* [m] Front-Bottom-Right corner of output region */")               \
	X(output_points,         uv4, , uvec4, , "/* Width * Height * Depth * (Frame Average Count) */")               \
	X(sampling_frequency,    f32, , float, , "/* [Hz]  */")                                                        \
	X(center_frequency,      f32, , float, , "/* [Hz]  */")                                                        \
	X(speed_of_sound,        f32, , float, , "/* [m/s] */")                                                        \
	X(off_axis_pos,          f32, , float, , "/* [m] Position on screen normal to beamform in TPW/VLSHERCULES */") \
	X(beamform_plane,        i32, , int,   , "/* Plane to Beamform in TPW/VLS/HERCULES */")                        \
	X(f_number,              f32, , float, , "/* F# (set to 0 to disable) */")                                     \
	X(interpolate,           b32, , bool,  , "/* Perform Cubic Interpolation of RF Samples */")

#define BEAMFORMER_PARAMS_HEAD \
	X(channel_mapping,   u16, [256], uvec4, [32], "/* Transducer Channel to Verasonics Channel */")                           \
	X(uforces_channels,  u16, [256], uvec4, [32], "/* Channels used for virtual UFORCES elements */")                         \
	X(focal_depths,      f32, [256], vec4,  [64], "/* [m] Focal Depths for each transmit of a RCA imaging scheme*/")          \
	X(transmit_angles,   f32, [256], vec4,  [64], "/* [radians] Transmit Angles for each transmit of a RCA imaging scheme*/") \
	X(xdc_transform,     f32, [16] , mat4,      , "/* IMPORTANT: column major order */")                                      \
	X(dec_data_dim,      uv4,      , uvec4,     , "/* Samples * Channels * Acquisitions; last element ignored */")            \
	X(xdc_element_pitch, f32, [2]  , vec2,      , "/* [m] Transducer Element Pitch {row, col} */")                            \
	X(rf_raw_dim,        uv2,      , uvec2,     , "/* Raw Data Dimensions */")                                                \
	X(transmit_mode,     i32,      , int,       , "/* Method/Orientation of Transmit */")                                     \
	X(decode,            u32,      , uint,      , "/* Decode or just reshape data */")                                        \
	X(das_shader_id,     u32,      , uint,      , "")                                                                         \
	X(time_offset,       f32,      , float,     , "/* pulse length correction time [s] */")

#define BEAMFORMER_PARAMS_TAIL \
	X(readi_group_id,   u32, , uint, , "/* Which readi group this data is from */") \
	X(readi_group_size, u32, , uint, , "/* Size of readi transmit group */")

#define X(name, type, size, gltype, glsize, comment) type name size;
typedef struct { BEAMFORMER_UI_PARAMS }   BeamformerUIParameters;
typedef struct { BEAMFORMER_PARAMS_HEAD } BeamformerFixedParameters;

/* NOTE: This struct follows the OpenGL std140 layout. DO NOT modify unless you have
 * read and understood the rules, particulary with regards to _member alignment_ */
typedef struct {
	BEAMFORMER_PARAMS_HEAD
	BEAMFORMER_UI_PARAMS
	BEAMFORMER_PARAMS_TAIL
	f32 _pad[3];
} BeamformerParameters;
#undef X

_Static_assert((offsetof(BeamformerParameters, output_min_coordinate) & 15) == 0,
               "BeamformerParameters.output_min_coordinate must lie on a 16 byte boundary");
_Static_assert((sizeof(BeamformerParameters) & 15) == 0,
               "sizeof(BeamformerParameters) must be a multiple of 16");
