/* See LICENSE for license details. */
#ifndef _BEAMFORMER_H_
#define _BEAMFORMER_H_

#include <glad.h>

#define GRAPHICS_API_OPENGL_43
#include <raylib_extended.h>
#include <rlgl.h>

#include "util.h"

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
} BeamformerInput;

#define INIT_CUDA_CONFIGURATION_FN(name) void name(u32 *input_dims, u32 *decoded_dims, i16 *channel_mapping)
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

typedef struct {
	Shader shader;
	b32    updated;
	i32    db_cutoff_id;
	i32    threshold_id;
	i32    gamma_id;
} FragmentShaderCtx;

#include "beamformer_parameters.h"
#include "beamformer_work_queue.h"

#define CS_MIN_MAX_MIPS_LEVEL_UNIFORM_LOC 1
#define CS_SUM_PRESCALE_UNIFORM_LOC       1

typedef struct {
	u32 programs[CS_LAST];

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

	uv4 dec_data_dim;
	u32 rf_raw_size;
} ComputeShaderCtx;

typedef enum {
#define X(type, id, pretty, fixed_tx) DAS_ ##type = id,
DAS_TYPES
#undef X
DAS_LAST
} DASShaderID;

typedef struct {
	/* TODO(rnp): there is assumption here that each shader will occur only once
	 * per compute. add an insertion index and change these to hold the max number
	 * of executed compute stages */
	u32 timer_ids[CS_LAST];
	f32 times[CS_LAST];
	b32 timer_active[CS_LAST];
} ComputeShaderStats;

typedef struct BeamformFrame {
	uv3 dim;
	u32 texture;

	/* NOTE: for use when displaying either prebeamformed frames or on the current frame
	 * when we intend to recompute on the next frame */
	v4  min_coordinate;
	v4  max_coordinate;

	u32 mips;
	DASShaderID das_shader_id;
	u32 compound_count;
	u32 id;

	struct BeamformFrame *next;
} BeamformFrame;

struct BeamformComputeFrame {
	BeamformFrame      frame;
	ComputeShaderStats stats;
	ImagePlaneTag image_plane_tag;
	b32 in_flight;
	b32 ready_to_present;
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

	uv2 window_size;
	b32 start_compute;
	b32 should_exit;

	Arena  ui_backing_store;
	void  *ui;
	/* TODO(rnp): this is nasty and should be removed */
	b32    ui_read_params;

	BeamformComputeFrame beamform_frames[MAX_BEAMFORMED_SAVED_FRAMES];
	u32 next_render_frame_index;
	u32 display_frame_index;

	/* NOTE: this will only be used when we are averaging */
	u32                  averaged_frame_index;
	BeamformComputeFrame averaged_frames[2];

	ComputeShaderCtx  csctx;
	FragmentShaderCtx fsctx;

	Arena export_buffer;

	CudaLib cuda_lib;
	OS      os;
	Stream  error_stream;

	BeamformWorkQueue *beamform_work_queue;

	BeamformerSharedMemory *shared_memory;
} BeamformerCtx;

struct ComputeShaderReloadContext {
	BeamformerCtx *beamformer_ctx;
	s8    label;
	s8    path;
	ComputeShaderID shader;
	b32   needs_header;
};

#define LABEL_GL_OBJECT(type, id, s) {s8 _s = (s); glObjectLabel(type, id, _s.len, (c8 *)_s.data);}

#define BEAMFORMER_FRAME_STEP_FN(name) void name(BeamformerCtx *ctx, Arena *arena, \
                                                 BeamformerInput *input)
typedef BEAMFORMER_FRAME_STEP_FN(beamformer_frame_step_fn);

#define BEAMFORMER_COMPUTE_SETUP_FN(name) void name(iptr user_context, Arena arena, iptr gl_context)
typedef BEAMFORMER_COMPUTE_SETUP_FN(beamformer_compute_setup_fn);

#define BEAMFORMER_COMPLETE_COMPUTE_FN(name) void name(iptr user_context, Arena arena, iptr gl_context)
typedef BEAMFORMER_COMPLETE_COMPUTE_FN(beamformer_complete_compute_fn);

#endif /*_BEAMFORMER_H_ */
