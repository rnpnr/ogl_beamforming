/* See LICENSE for license details. */

/* X(enumarant, number, shader file name, needs header, pretty name) */
#define COMPUTE_SHADERS                                    \
	X(CUDA_DECODE,  0, "",         0, "CUDA Decoding") \
	X(CUDA_HILBERT, 1, "",         0, "CUDA Hilbert")  \
	X(DAS,          2, "das",      1, "DAS")           \
	X(DEMOD,        3, "demod",    1, "Demodulation")  \
	X(HADAMARD,     4, "hadamard", 1, "Decoding")      \
	X(MIN_MAX,      5, "min_max",  0, "Min/Max")       \
	X(SUM,          6, "sum",      0, "Sum")

enum compute_shaders {
	#define X(e, n, s, h, pn) CS_ ##e = n,
	COMPUTE_SHADERS
	#undef X
	CS_LAST
};

#define DAS_ID_UFORCES  0
#define DAS_ID_HERCULES 1
#define DAS_ID_RCA      2

#define MAX_BEAMFORMED_SAVED_FRAMES 16
#define MAX_MULTI_XDC_COUNT         4
/* NOTE: This struct follows the OpenGL std140 layout. DO NOT modify unless you have
 * read and understood the rules, particulary with regards to _member alignment_ */
typedef struct {
	u16 channel_mapping[512];   /* Transducer Channel to Verasonics Channel */
	u32 uforces_channels[128];  /* Channels used for virtual UFORCES elements */
	f32 focal_depths[128];      /* [m] Focal Depths for each transmit of a RCA imaging scheme*/
	f32 transmit_angles[128];   /* [radians] Transmit Angles for each transmit of a RCA imaging scheme*/
	f32 xdc_origin[4 * MAX_MULTI_XDC_COUNT];  /* [m] Corner of transducer being treated as origin */
	f32 xdc_corner1[4 * MAX_MULTI_XDC_COUNT]; /* [m] Corner of transducer along first axis */
	f32 xdc_corner2[4 * MAX_MULTI_XDC_COUNT]; /* [m] Corner of transducer along second axis */
	uv4 dec_data_dim;           /* Samples * Channels * Acquisitions; last element ignored */
	uv4 output_points;          /* Width * Height * Depth * (Frame Average Count) */
	v4  output_min_coordinate;  /* [m] Back-Top-Left corner of output region (w ignored) */
	v4  output_max_coordinate;  /* [m] Front-Bottom-Right corner of output region (w ignored)*/
	uv2 rf_raw_dim;             /* Raw Data Dimensions */
	u32 decode;                 /* Decode or just reshape data */
	u32 xdc_count;              /* Number of Transducer Arrays (4 max) */
	u32 channel_offset;         /* Offset into channel_mapping: 0 or 128 (rows or columns) */
	f32 speed_of_sound;         /* [m/s] */
	f32 sampling_frequency;     /* [Hz]  */
	f32 center_frequency;       /* [Hz]  */
	f32 time_offset;            /* pulse length correction time [s]   */
	f32 off_axis_pos;           /* [m] Position on screen normal to beamform in 2D HERCULES */
	i32 beamform_plane;         /* Plane to Beamform in 2D HERCULES */
	f32 f_number;               /* F# (set to 0 to disable) */
	u32 das_shader_id;
	u32 readi_group_id;			/* Which readi group this data is from*/
	u32 readi_group_size;		/* Size of readi transmit group */
	f32 _pad[3];
} BeamformerParameters;

/* NOTE: garbage to get the prepocessor to properly stringize the value of a macro */
#define str_(x) #x
#define str(x) str_(x)

#define COMPUTE_SHADER_HEADER "\
#version 460 core\n\
\n\
layout(std140, binding = 0) uniform parameters {\n\
	uvec4 channel_mapping[64];    /* Transducer Channel to Verasonics Channel */\n\
	uvec4 uforces_channels[32];   /* Channels used for virtual UFORCES elements */\n\
	vec4  focal_depths[32];       /* [m] Focal Depths for each transmit of a RCA imaging scheme*/\n\
	vec4  transmit_angles[32];    /* [radians] Transmit Angles for each transmit of a RCA imaging scheme*/\n\
	vec4  xdc_origin[" str(MAX_MULTI_XDC_COUNT) "];          /* [m] Corner of transducer being treated as origin */\n\
	vec4  xdc_corner1[" str(MAX_MULTI_XDC_COUNT) "];         /* [m] Corner of transducer along first axis (arbitrary) */\n\
	vec4  xdc_corner2[" str(MAX_MULTI_XDC_COUNT) "];         /* [m] Corner of transducer along second axis (arbitrary) */\n\
	uvec4 dec_data_dim;           /* Samples * Channels * Acquisitions; last element ignored */\n\
	uvec4 output_points;          /* Width * Height * Depth * (Frame Average Count) */\n\
	vec4  output_min_coord;       /* [m] Top left corner of output region */\n\
	vec4  output_max_coord;       /* [m] Bottom right corner of output region */\n\
	uvec2 rf_raw_dim;             /* Raw Data Dimensions */\n\
	uint  decode;                 /* Decode or just reshape data */\n\
	uint  xdc_count;              /* Number of Transducer Arrays (4 max) */\n\
	uint  channel_offset;         /* Offset into channel_mapping: 0 or 128 (rows or columns) */\n\
	float speed_of_sound;         /* [m/s] */\n\
	float sampling_frequency;     /* [Hz]  */\n\
	float center_frequency;       /* [Hz]  */\n\
	float time_offset;            /* pulse length correction time [s]   */\n\
	float off_axis_pos;           /* [m] Position on screen normal to beamform in 2D HERCULES */\n\
	int   beamform_plane;         /* Plane to Beamform in 2D HERCULES */\n\
	float f_number;               /* F# (set to 0 to disable) */\n\
	uint  das_shader_id;\n\
};\n\
\n\
#define DAS_ID_UFORCES  " str(DAS_ID_UFORCES) "\n\
#define DAS_ID_HERCULES " str(DAS_ID_HERCULES) "\n\
#define DAS_ID_RCA " str(DAS_ID_RCA) "\n\n\
#line 0\n"
