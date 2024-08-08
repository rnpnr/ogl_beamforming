/* See LICENSE for license details. */
#ifndef _BEAMFORMER_H_
#define _BEAMFORMER_H_

#include <immintrin.h>

#include <glad.h>

#define GRAPHICS_API_OPENGL_43
#include <raylib.h>
#include <rlgl.h>

#include "util.h"

#define BG_COLOUR              (v4){.r = 0.15, .g = 0.12, .b = 0.13, .a = 1.0}
#define FG_COLOUR              (v4){.r = 0.92, .g = 0.88, .b = 0.78, .a = 1.0}
#define FOCUSED_COLOUR         (v4){.r = 0.86, .g = 0.28, .b = 0.21, .a = 1.0}
#define HOVERED_COLOUR         (v4){.r = 0.11, .g = 0.50, .b = 0.59, .a = 1.0}

/* NOTE: extra space used for allowing mouse clicks after end of text */
#define TEXT_BOX_EXTRA_X       10.0f

#define TEXT_HOVER_SPEED       5.0f

typedef union {
	struct { f32 x, y; };
	struct { f32 w, h; };
	f32 E[2];
	Vector2 rl;
} v2;

typedef union {
	struct { f32 x, y, z, w; };
	struct { f32 r, g, b, a; };
	f32 E[4];
	Vector4 rl;
} v4;

typedef union {
	struct { v2 pos, size; };
	Rectangle rl;
} Rect;

enum compute_shaders {
//	CS_FORCES,
	CS_HADAMARD,
//	CS_HERCULES,
	CS_LPF,
	CS_MIN_MAX,
	CS_UFORCES,
	CS_LAST
};

enum program_flags {
	RELOAD_SHADERS = 1 << 0,
	ALLOC_SSBOS    = 1 << 1,
	ALLOC_OUT_TEX  = 1 << 2,
	UPLOAD_FILTER  = 1 << 3,
	DO_COMPUTE     = 1 << 30,
};

typedef struct {
	char buf[64];
	i32  buf_len;
	i32  idx;
	i32  cursor;
	f32  cursor_hover_p;
	f32  cursor_blink_t;
	f32  cursor_blink_target;
} InputState;

#include "beamformer_parameters.h"
typedef struct {
	BeamformerParameters raw;
	b32 upload;
} BeamformerParametersFull;

#if defined(__unix__)
	#include "os_unix.c"

	#define OS_PIPE_NAME "/tmp/beamformer_data_fifo"
	#define OS_SMEM_NAME "/ogl_beamformer_parameters"
#elif defined(_WIN32)
	#include "os_win32.c"

	#define OS_PIPE_NAME "\\\\.\\pipe\\beamformer_data_fifo"
	#define OS_SMEM_NAME "Local\\ogl_beamformer_parameters"
#else
	#error Unsupported Platform!
#endif

typedef struct {
	u32 programs[CS_LAST];

	u32    timer_ids[CS_LAST];
	i32    timer_idx;
	GLsync timer_fence;
	f32    last_frame_time[CS_LAST];

	/* NOTE: multiple raw data SSBOs for unsynchronized mapping.
	 * Decoded data is only relavent in the context of a single frame, two are
	 * used so that they can be swapped when chaining multiple compute stages */
	GLsync raw_data_fences[3];
	u32    raw_data_ssbos[3];
	u32    raw_data_index;

	u32 rf_data_ssbos[2];
	u32 last_output_ssbo_index;
	u32 hadamard_ssbo;
	uv2 hadamard_dim;

	u32 shared_ubo;

	uv4 dec_data_dim;
	uv2 rf_raw_dim;
	i32 out_data_tex_id;
	i32 mip_view_tex_id;
	i32 mips_level_id;

	//u32 lpf_ssbo;
	//u32 lpf_order;
	//i32 lpf_order_id;
} ComputeShaderCtx;

typedef struct {
	Shader          shader;
	RenderTexture2D output;
	i32             out_data_tex_id;
	i32             db_cutoff_id;
	f32             db;
} FragmentShaderCtx;

typedef struct {
	uv2 window_size;
	u32 flags;

	f32 dt;

	/* UI Theming */
	Font  font;
	u32   font_size;
	u32   font_spacing;

	InputState is;

	uv4 out_data_dim;
	u32 out_texture;
	u32 out_texture_unit;
	u32 out_texture_mips;

	ComputeShaderCtx  csctx;
	FragmentShaderCtx fsctx;

	os_pipe data_pipe;
	u32     partial_transfer_count;

	BeamformerParametersFull *params;
} BeamformerCtx;

#endif /*_BEAMFORMER_H_ */
