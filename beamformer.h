/* See LICENSE for license details. */
#ifndef _BEAMFORMER_H_
#define _BEAMFORMER_H_

#include <glad.h>

#define GRAPHICS_API_OPENGL_43
#include <raylib.h>
#include <rlgl.h>

#include "util.h"

#define BG_COLOUR              (v4){.r = 0.15, .g = 0.12, .b = 0.13, .a = 1.0}
#define FG_COLOUR              (v4){.r = 0.92, .g = 0.88, .b = 0.78, .a = 1.0}
#define FOCUSED_COLOUR         (v4){.r = 0.86, .g = 0.28, .b = 0.21, .a = 1.0}
#define HOVERED_COLOUR         (v4){.r = 0.11, .g = 0.50, .b = 0.59, .a = 1.0}
#define RULER_COLOUR           (v4){.r = 1.00, .g = 0.70, .b = 0.00, .a = 1.0}

#define INFO_COLUMN_WIDTH      560
/* NOTE: extra space used for allowing mouse clicks after end of text */
#define TEXT_BOX_EXTRA_X       10.0f

#define TEXT_HOVER_SPEED       5.0f

#define RULER_TEXT_PAD         10.0f
#define RULER_TICK_LENGTH      20.0f

#define RECT_BTN_COLOUR        (Color){.r = 0x43, .g = 0x36, .b = 0x3a, .a = 0xff}
#define RECT_BTN_BORDER_COLOUR (Color){.r = 0x00, .g = 0x00, .b = 0x00, .a = 0xCC}
#define RECT_BTN_ROUNDNESS     0.3f
#define RECT_BTN_BORDER_WIDTH  6.0f

/* TODO: multiple views */
#define MAX_DISPLAYS 1

enum program_flags {
	SHOULD_EXIT    = 1 << 0,
	START_COMPUTE  = 1 << 1,
};

enum gl_vendor_ids {
	GL_VENDOR_AMD,
	GL_VENDOR_ARM,
	GL_VENDOR_INTEL,
	GL_VENDOR_NVIDIA,
};

typedef struct {
	u8   buf[64];
	i32  buf_len;
	i32  cursor;
	f32  cursor_blink_t;
	f32  cursor_blink_scale;
} InputState;

enum variable_flags {
	V_CAUSES_COMPUTE = 1 << 29,
	V_GEN_MIPMAPS    = 1 << 30,
};

enum interaction_states {
	IS_NONE,
	IS_NOP,
	IS_SET,
	IS_DRAG,
	IS_SCROLL,
	IS_TEXT,

	IS_DISPLAY,
	IS_SCALE_BAR,
};

enum ruler_state {
	RS_NONE,
	RS_START,
	RS_HOLD,
};

enum scale_bar_directions {
	SB_LATERAL,
	SB_AXIAL,
};

typedef struct {
	Variable hot;
	Variable next_hot;
	Variable active;
	u32      hot_state;
	u32      state;
} InteractionState;

typedef struct v2_sll {
	struct v2_sll *next;
	v2             v;
} v2_sll;

typedef struct {
	f32    *min_value, *max_value;
	v2_sll *savepoint_stack;
	v2      zoom_starting_point;
	v2      screen_offset;
	v2      screen_space_to_value;
	f32     hover_t;
	b32     scroll_both;
} ScaleBar;

typedef struct {
	TempArena frame_temporary_arena;
	Arena     arena_for_frame;

	Font font;
	Font small_font;
	f32  font_height;
	f32  small_font_height;

	InteractionState interaction;
	InputState       text_input_state;

	ScaleBar scale_bars[MAX_DISPLAYS][2];
	v2_sll   *scale_bar_savepoint_freelist;

	v2  ruler_start_p;
	v2  ruler_stop_p;
	u32 ruler_state;
} BeamformerUI;

#define MAX_FRAMES_IN_FLIGHT 3

#define INIT_CUDA_CONFIGURATION_FN(name) void name(u32 *input_dims, u32 *decoded_dims, u16 *channel_mapping)
typedef INIT_CUDA_CONFIGURATION_FN(init_cuda_configuration_fn);
INIT_CUDA_CONFIGURATION_FN(init_cuda_configuration_stub) {}

#define REGISTER_CUDA_BUFFERS_FN(name) void name(u32 *rf_data_ssbos, u32 rf_buffer_count, u32 raw_data_ssbo)
typedef REGISTER_CUDA_BUFFERS_FN(register_cuda_buffers_fn);
REGISTER_CUDA_BUFFERS_FN(register_cuda_buffers_stub) {}

#define CUDA_DECODE_FN(name) void name(size_t input_offset, u32 output_buffer_idx, u32 rf_channel_offset)
typedef CUDA_DECODE_FN(cuda_decode_fn);
CUDA_DECODE_FN(cuda_decode_stub) {}

#define CUDA_HILBERT_FN(name) void name(u32 input_buffer_idx, u32 output_buffer_idx)
typedef CUDA_HILBERT_FN(cuda_hilbert_fn);
CUDA_HILBERT_FN(cuda_hilbert_stub) {}

#define CUDA_LIB_FNS               \
	X(cuda_decode)             \
	X(cuda_hilbert)            \
	X(init_cuda_configuration) \
	X(register_cuda_buffers)

typedef struct {
	void                       *lib;
	u64                         timestamp;
	#define X(name) name ## _fn *name;
	CUDA_LIB_FNS
	#undef X
} CudaLib;

#include "beamformer_parameters.h"
typedef struct {
	BeamformerParameters raw;
	enum compute_shaders compute_stages[16];
	u32                  compute_stages_count;
	b32                  upload;
	b32                  export_next_frame;
	c8                   export_pipe_name[1024];
} BeamformerParametersFull;

#define CS_UNIFORMS                             \
	X(CS_DAS,     volume_export_dim_offset) \
	X(CS_DAS,     volume_export_pass)       \
	X(CS_DAS,     xdc_index)                \
	X(CS_DAS,     xdc_transform)            \
	X(CS_DAS,     cycle_t)                  \
	X(CS_MIN_MAX, mips_level)               \
	X(CS_SUM,     sum_prescale)

typedef struct {
	u32 programs[CS_LAST];

	u32    timer_index;
	u32    timer_ids[MAX_FRAMES_IN_FLIGHT][CS_LAST];
	b32    timer_active[MAX_FRAMES_IN_FLIGHT][CS_LAST];
	GLsync timer_fences[MAX_FRAMES_IN_FLIGHT];
	f32    last_frame_time[CS_LAST];

	/* NOTE: the raw_data_ssbo is allocated at 3x the required size to allow for tiled
	 * transfers when the GPU is running behind the CPU. It is not mapped on NVIDIA because
	 * their drivers _will_ store the buffer in the system memory. This doesn't happen
	 * for Intel or AMD and mapping the buffer is preferred. In either case incoming data can
	 * be written to the arena at the appropriate offset for the current raw_data_index. An
	 * additional BufferSubData is needed on NVIDIA to upload the data. */
	GLsync raw_data_fences[MAX_FRAMES_IN_FLIGHT];
	Arena  raw_data_arena;
	u32    raw_data_ssbo;
	u32    raw_data_index;

	/* NOTE: Decoded data is only relevant in the context of a single frame. We use two
	 * buffers so that they can be swapped when chaining multiple compute stages */
	u32 rf_data_ssbos[2];
	u32 last_output_ssbo_index;
	u32 hadamard_ssbo;
	uv2 hadamard_dim;

	u32 shared_ubo;

	uv4 dec_data_dim;
	uv2 rf_raw_dim;

	#define X(idx, name) i32 name ## _id;
	CS_UNIFORMS
	#undef X
} ComputeShaderCtx;

typedef struct {
	Shader          shader;
	RenderTexture2D output;
	/* TODO: cleanup: X macro? */
	i32             db_cutoff_id;
	i32             threshold_id;
	f32             db;
	f32             threshold;
	b32             gen_mipmaps;
} FragmentShaderCtx;

typedef struct {
	/* NOTE: we always have one extra texture to sum into; thus the final output data
	 * is always found in textures[dim.w - 1] */
	u32 textures[MAX_MULTI_XDC_COUNT + 1];
	uv4 dim;

	/* NOTE: for use when displaying either prebeamformed frames or on the current frame
	 * when we intend to recompute on the next frame */
	v4  min_coordinate;
	v4  max_coordinate;

	u32 mips;
} BeamformFrame;

typedef struct {
	BeamformFrame frame;
	u32 timer_ids[2];
	f32 runtime;
	u32 rf_data_ssbo;
	u32 shader;
	u32 dispatch_index;
	b32 timer_active;
} PartialComputeCtx;

typedef struct {
	enum gl_vendor_ids vendor_id;
	i32  version_major;
	i32  version_minor;
	i32  max_2d_texture_dim;
	i32  max_3d_texture_dim;
	i32  max_ssbo_size;
	i32  max_ubo_size;
} GLParams;

enum beamform_work {
	BW_FULL_COMPUTE,
	BW_RECOMPUTE,
	BW_PARTIAL_COMPUTE,
	BW_SAVE_FRAME,
	BW_SEND_FRAME,
	BW_SSBO_COPY,
};

typedef struct {
	u32 source_ssbo;
	u32 dest_ssbo;
} BeamformSSBOCopy;

typedef struct {
	BeamformFrame *frame;
	iptr export_handle;
	u32  raw_data_ssbo_index;
	b32  first_pass;
} BeamformCompute;

typedef struct {
	BeamformFrame *frame;
	iptr output_handle;
} BeamformOutputFrame;

/* NOTE: discriminated union based on type */
typedef struct BeamformWork {
	struct BeamformWork *next;
	union {
		BeamformSSBOCopy    ssbo_copy_ctx;
		BeamformCompute     compute_ctx;
		BeamformOutputFrame output_frame_ctx;
	};
	u32 type;
} BeamformWork;

typedef struct {
	BeamformWork *first;
	BeamformWork *last;
	BeamformWork *next_free;
	i32 compute_in_flight;
	b32 did_compute_this_frame;
} BeamformWorkQueue;

typedef struct {
	BeamformFrame *frames;
	u32 capacity;
	u32 offset;
	u32 cursor;
	u32 needed_frames;
} BeamformFrameIterator;

typedef struct BeamformerCtx {
	GLParams gl;

	uv2 window_size;
	u32 flags;

	Arena ui_backing_store;
	BeamformerUI *ui;

	BeamformFrame beamform_frames[MAX_BEAMFORMED_SAVED_FRAMES];
	u32 displayed_frame_index;

	/* NOTE: this will only be used when we are averaging */
	BeamformFrame averaged_frame;
	ComputeShaderCtx  csctx;
	FragmentShaderCtx fsctx;
	PartialComputeCtx partial_compute_ctx;

	Arena export_buffer;

	CudaLib  cuda_lib;
	Platform platform;
	Stream   error_stream;

	BeamformWorkQueue beamform_work_queue;

	BeamformerParametersFull *params;
} BeamformerCtx;

#define LABEL_GL_OBJECT(type, id, s) {s8 _s = (s); glObjectLabel(type, id, _s.len, (c8 *)_s.data);}

#define BEAMFORMER_FRAME_STEP_FN(name) void name(BeamformerCtx *ctx, Arena *arena, \
                                                 BeamformerInput *input)
typedef BEAMFORMER_FRAME_STEP_FN(beamformer_frame_step_fn);

#endif /*_BEAMFORMER_H_ */
