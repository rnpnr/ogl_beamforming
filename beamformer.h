/* See LICENSE for license details. */
#ifndef _BEAMFORMER_H_
#define _BEAMFORMER_H_

#include <raylib_extended.h>
#include <rlgl.h>

#include "util.h"
#include "opengl.h"
#include "util_gl.c"

enum gl_vendor_ids {
	GL_VENDOR_AMD,
	GL_VENDOR_ARM,
	GL_VENDOR_INTEL,
	GL_VENDOR_NVIDIA,
};

typedef struct {
	v2   mouse;
	v2   last_mouse;
	b32  executable_reloaded;
	f32  dt;
} BeamformerInput;

#define CUDA_INIT_FN(name) void name(u32 *input_dims, u32 *decoded_dims)
typedef CUDA_INIT_FN(cuda_init_fn);
CUDA_INIT_FN(cuda_init_stub) {}

#define CUDA_REGISTER_BUFFERS_FN(name) void name(u32 *rf_data_ssbos, u32 rf_buffer_count, u32 raw_data_ssbo)
typedef CUDA_REGISTER_BUFFERS_FN(cuda_register_buffers_fn);
CUDA_REGISTER_BUFFERS_FN(cuda_register_buffers_stub) {}

#define CUDA_DECODE_FN(name) void name(size_t input_offset, u32 output_buffer_idx, u32 rf_channel_offset)
typedef CUDA_DECODE_FN(cuda_decode_fn);
CUDA_DECODE_FN(cuda_decode_stub) {}

#define CUDA_HILBERT_FN(name) void name(u32 input_buffer_idx, u32 output_buffer_idx)
typedef CUDA_HILBERT_FN(cuda_hilbert_fn);
CUDA_HILBERT_FN(cuda_hilbert_stub) {}

#define CUDA_SET_CHANNEL_MAPPING_FN(name) void name(i16 *channel_mapping)
typedef CUDA_SET_CHANNEL_MAPPING_FN(cuda_set_channel_mapping_fn);
CUDA_SET_CHANNEL_MAPPING_FN(cuda_set_channel_mapping_stub) {}

#define CUDA_LIB_FNS \
	X(decode,              "cuda_decode")              \
	X(hilbert,             "cuda_hilbert")             \
	X(init,                "init_cuda_configuration")  \
	X(register_buffers,    "register_cuda_buffers")    \
	X(set_channel_mapping, "cuda_set_channel_mapping")

typedef struct {
	void *lib;
	#define X(name, symname) cuda_ ## name ## _fn *name;
	CUDA_LIB_FNS
	#undef X
} CudaLib;

/* TODO(rnp): this should be a UBO */
#define FRAME_VIEW_MODEL_MATRIX_LOC   0
#define FRAME_VIEW_VIEW_MATRIX_LOC    1
#define FRAME_VIEW_PROJ_MATRIX_LOC    2
#define FRAME_VIEW_DYNAMIC_RANGE_LOC  3
#define FRAME_VIEW_THRESHOLD_LOC      4
#define FRAME_VIEW_GAMMA_LOC          5
#define FRAME_VIEW_LOG_SCALE_LOC      6
#define FRAME_VIEW_BB_COLOUR_LOC      7
#define FRAME_VIEW_BB_FRACTION_LOC    8
#define FRAME_VIEW_SOLID_BB_LOC      10

#define FRAME_VIEW_BB_COLOUR   0.92, 0.88, 0.78, 1.0
#define FRAME_VIEW_BB_FRACTION 0.007f

#define FRAME_VIEW_RENDER_TARGET_SIZE 1024, 1024

typedef struct {
	u32 shader;
	u32 framebuffers[2];  /* [0] -> multisample target, [1] -> normal target for resolving */
	u32 renderbuffers[2]; /* only used for 3D views, size is fixed */
	b32 updated;
} FrameViewRenderContext;

#include "beamformer_parameters.h"
#include "beamformer_work_queue.h"

typedef struct {
	iptr elements_offset;
	i32  elements;
	u32  buffer;
	u32  vao;
} BeamformerRenderModel;

typedef struct {
	u32 programs[BeamformerShaderKind_ComputeCount];

	/* NOTE: Decoded data is only relevant in the context of a single frame. We use two
	 * buffers so that they can be swapped when chaining multiple compute stages */
	u32 rf_data_ssbos[2];
	u32 last_output_ssbo_index;

	u32 raw_data_ssbo;
	u32 shared_ubo;

	u32 channel_mapping_texture;
	u32 sparse_elements_texture;
	u32 focal_vectors_texture;
	u32 hadamard_texture;

	f32 processing_progress;
	b32 processing_compute;

	u32 rf_data_timestamp_query;

	u32 shader_timer_ids[MAX_COMPUTE_SHADER_STAGES];

	uv4 dec_data_dim;
	u32 rf_raw_size;

	BeamformerRenderModel unit_cube_model;
} ComputeShaderCtx;

typedef enum {
	#define X(type, id, pretty, fixed_tx) DASShaderKind_##type = id,
	DAS_TYPES
	#undef X
	DASShaderKind_Count
} DASShaderKind;

typedef struct {
	BeamformerComputeStatsTable table;
	f32 average_times[BeamformerShaderKind_Count];

	u64 last_rf_timer_count;
	f32 rf_time_delta_average;

	u32 latest_frame_index;
	u32 latest_rf_index;
} ComputeShaderStats;

/* TODO(rnp): maybe this also gets used for CPU timing info as well */
typedef enum {
	ComputeTimingInfoKind_ComputeFrameBegin,
	ComputeTimingInfoKind_ComputeFrameEnd,
	ComputeTimingInfoKind_Shader,
	ComputeTimingInfoKind_RF_Data,
} ComputeTimingInfoKind;

typedef struct {
	u64 timer_count;
	ComputeTimingInfoKind kind;
	union {
		BeamformerShaderKind shader;
	};
} ComputeTimingInfo;

typedef struct {
	u32 write_index;
	u32 read_index;
	b32 compute_frame_active;
	ComputeTimingInfo buffer[4096];
} ComputeTimingTable;

typedef struct BeamformerFrame BeamformerFrame;
struct BeamformerFrame {
	iv3 dim;
	u32 texture;

	/* NOTE: for use when displaying either prebeamformed frames or on the current frame
	 * when we intend to recompute on the next frame */
	v4  min_coordinate;
	v4  max_coordinate;

	i32 mips;
	DASShaderKind das_shader_kind;
	u32 compound_count;
	u32 id;

	BeamformerFrame *next;
};

struct BeamformerComputeFrame {
	BeamformerFrame frame;
	b32             ready_to_present;
	BeamformerViewPlaneTag view_plane_tag;
};

#define GL_PARAMETERS \
	X(MAJOR_VERSION,                   version_major,                   "")      \
	X(MINOR_VERSION,                   version_minor,                   "")      \
	X(TEXTURE_BUFFER_OFFSET_ALIGNMENT, texture_buffer_offset_alignment, "")      \
	X(MAX_TEXTURE_BUFFER_SIZE,         max_texture_buffer_size,         "")      \
	X(MAX_TEXTURE_SIZE,                max_2d_texture_dim,              "")      \
	X(MAX_3D_TEXTURE_SIZE,             max_3d_texture_dim,              "")      \
	X(MAX_SHADER_STORAGE_BLOCK_SIZE,   max_ssbo_size,                   "")      \
	X(MAX_UNIFORM_BLOCK_SIZE,          max_ubo_size,                    "")      \
	X(MAX_SERVER_WAIT_TIMEOUT,         max_server_wait_time,            " [ns]")

typedef struct {
	enum gl_vendor_ids vendor_id;
	#define X(glname, name, suffix) i32 name;
	GL_PARAMETERS
	#undef X
} GLParams;

typedef struct {
	GLParams gl;

	iv2 window_size;
	b32 should_exit;

	Arena  ui_backing_store;
	void  *ui;
	/* TODO(rnp): this is nasty and should be removed */
	b32    ui_read_params;

	BeamformerComputeFrame beamform_frames[MAX_BEAMFORMED_SAVED_FRAMES];
	BeamformerComputeFrame *latest_frame;
	u32 next_render_frame_index;
	u32 display_frame_index;

	/* NOTE: this will only be used when we are averaging */
	u32                    averaged_frame_index;
	BeamformerComputeFrame averaged_frames[2];

	ComputeShaderCtx  csctx;

	/* TODO(rnp): ideally this would go in the UI but its hard to manage with the UI
	 * destroying itself on hot-reload */
	FrameViewRenderContext frame_view_render_context;

	CudaLib cuda_lib;
	OS      os;
	Stream  error_stream;

	BeamformWorkQueue *beamform_work_queue;

	ComputeShaderStats *compute_shader_stats;
	ComputeTimingTable *compute_timing_table;

	SharedMemoryRegion shared_memory;
} BeamformerCtx;

struct ShaderReloadContext {
	BeamformerCtx *beamformer_context;
	s8   path;
	s8   name;
	s8   header;
	u32 *shader;
	ShaderReloadContext *link;
	GLenum     gl_type;
	BeamformerShaderKind kind;
};

#define BEAMFORMER_FRAME_STEP_FN(name) void name(BeamformerCtx *ctx, BeamformerInput *input)
typedef BEAMFORMER_FRAME_STEP_FN(beamformer_frame_step_fn);

#define BEAMFORMER_COMPUTE_SETUP_FN(name) void name(iptr user_context)
typedef BEAMFORMER_COMPUTE_SETUP_FN(beamformer_compute_setup_fn);

#define BEAMFORMER_COMPLETE_COMPUTE_FN(name) void name(iptr user_context, Arena arena, iptr gl_context)
typedef BEAMFORMER_COMPLETE_COMPUTE_FN(beamformer_complete_compute_fn);

#define BEAMFORMER_RELOAD_SHADER_FN(name) b32 name(OS *os, BeamformerCtx *ctx, \
                                                   ShaderReloadContext *src, Arena arena, s8 shader_name)
typedef BEAMFORMER_RELOAD_SHADER_FN(beamformer_reload_shader_fn);

#endif /*_BEAMFORMER_H_ */
