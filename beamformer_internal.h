/* See LICENSE for license details. */
#ifndef BEAMFORMER_INTERNAL_H
#define BEAMFORMER_INTERNAL_H

#include "util.h"

#include "beamformer.h"
#include "opengl.h"

#include "generated/beamformer.meta.c"
#include "generated/beamformer_shaders.c"

#include <raylib_extended.h>
#include <rlgl.h>

#include "threads.c"
#include "util_gl.c"

///////////////////////////////
// NOTE: CUDA Library Bindings

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

#define CUDALibraryProcedureList \
	X(decode,              "cuda_decode")              \
	X(hilbert,             "cuda_hilbert")             \
	X(init,                "init_cuda_configuration")  \
	X(register_buffers,    "register_cuda_buffers")    \
	X(set_channel_mapping, "cuda_set_channel_mapping")

#define X(name, ...) DEBUG_IMPORT cuda_## name ##_fn *cuda_## name;
CUDALibraryProcedureList
#undef X

/////////////////////////////////////
// NOTE: Core Beamformer Definitions

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
#include "beamformer_shared_memory.c"

typedef struct {
	iptr elements_offset;
	i32  elements;
	u32  buffer;
	u32  vao;
} BeamformerRenderModel;

typedef struct {
	BeamformerFilterParameters parameters;
	f32 time_delay;
	i32 length;
	u32 ssbo;
} BeamformerFilter;

/* X(name, type, gltype) */
#define BEAMFORMER_DAS_UBO_PARAM_LIST \
	X(voxel_transform,        m4,  mat4) \
	X(xdc_transform,          m4,  mat4) \
	X(xdc_element_pitch,      v2,  vec2)

typedef alignas(16) struct {
	#define X(name, type, ...) type name;
	BEAMFORMER_DAS_UBO_PARAM_LIST
	#undef X
	float _pad[2];
} BeamformerDASUBO;
static_assert((sizeof(BeamformerDASUBO) & 15) == 0, "UBO size must be a multiple of 16");

/* TODO(rnp): need 1 UBO per filter slot */
#define BEAMFORMER_COMPUTE_UBO_LIST \
	X(DAS,        BeamformerDASUBO,    das)

#define X(k, ...) BeamformerComputeUBOKind_##k,
typedef enum {BEAMFORMER_COMPUTE_UBO_LIST BeamformerComputeUBOKind_Count} BeamformerComputeUBOKind;
#undef X

// X(kind, gl_kind, texture_format, pixel_type)
#define BEAMFORMER_COMPUTE_TEXTURE_LIST \
	X(FocalVectors,                GL_RG32F, GL_RG,          GL_FLOAT) \
	X(SparseElements,              GL_R16I,  GL_RED_INTEGER, GL_SHORT) \
	X(TransmitReceiveOrientations, GL_R8I,   GL_RED_INTEGER, GL_BYTE)

#define BEAMFORMER_COMPUTE_TEXTURE_LIST_FULL \
	BEAMFORMER_COMPUTE_TEXTURE_LIST \
	X(Hadamard,       GL_R32F)

typedef enum {
	#define X(k, ...) BeamformerComputeTextureKind_##k,
	BEAMFORMER_COMPUTE_TEXTURE_LIST_FULL
	#undef X
	BeamformerComputeTextureKind_Count
} BeamformerComputeTextureKind;
static_assert((BeamformerComputeTextureKind_Count - 1) == BeamformerComputeTextureKind_Hadamard,
              "BeamformerComputeTextureKind_Hadamard must be end of TextureKinds");

typedef struct {
	uv3 layout;
	uv3 dispatch;
	BeamformerShaderBakeParameters bake;
} BeamformerShaderDescriptor;

typedef struct BeamformerComputePlan BeamformerComputePlan;
struct BeamformerComputePlan {
	BeamformerComputePipeline pipeline;

	u32 programs[BeamformerMaxComputeShaderStages];

	u32 dirty_programs;

	BeamformerAcquisitionKind acquisition_kind;
	u32                       acquisition_count;

	u32 rf_size;
	i32 hadamard_order;
	b32 iq_pipeline;

	v3  min_coordinate;
	v3  max_coordinate;
	iv3 output_points;
	i32 average_frames;

	u32 textures[BeamformerComputeTextureKind_Count];
	u32 ubos[BeamformerComputeUBOKind_Count];

	BeamformerFilter filters[BeamformerFilterSlots];

	#define X(k, type, name) type name ##_ubo_data;
	BEAMFORMER_COMPUTE_UBO_LIST
	#undef X

	u128 shader_hashes[BeamformerMaxComputeShaderStages];
	BeamformerShaderDescriptor shader_descriptors[BeamformerMaxComputeShaderStages];

	BeamformerComputePlan *next;
};

typedef struct {
	GLsync upload_syncs[BeamformerMaxRawDataFramesInFlight];
	GLsync compute_syncs[BeamformerMaxRawDataFramesInFlight];

	u8 *buffer;

	u32 ssbo;

	u32 size;
	u32 active_rf_size;

	u32 data_timestamp_query;

	u32 insertion_index;
	u32 compute_index;
} BeamformerRFBuffer;

typedef struct {
	BeamformerRFBuffer rf_buffer;

	BeamformerComputePlan *compute_plans[BeamformerMaxParameterBlockSlots];
	BeamformerComputePlan *compute_plan_freelist;

	/* NOTE(rnp): two interstage ssbos are allocated so that they may be used to
	 * ping pong data between compute stages */
	u32 ping_pong_ssbos[2];
	u32 last_output_ssbo_index;

	u32 ping_pong_ssbo_size;

	f32 processing_progress;
	b32 processing_compute;

	u32 shader_timer_ids[BeamformerMaxComputeShaderStages];

	BeamformerRenderModel unit_cube_model;
} BeamformerComputeContext;

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

typedef struct {
	BeamformerRFBuffer *rf_buffer;
	SharedMemoryRegion *shared_memory;
	ComputeTimingTable *compute_timing_table;
	i32                *compute_worker_sync;
} BeamformerUploadThreadContext;

struct BeamformerFrame {
	u32 texture;
	b32 ready_to_present;

	iv3 dim;
	i32 mips;

	/* NOTE: for use when displaying either prebeamformed frames or on the current frame
	 * when we intend to recompute on the next frame */
	v3  min_coordinate;
	v3  max_coordinate;

	// metadata
	GLenum                    gl_kind;
	u32                       id;
	u32                       compound_count;
	u32                       parameter_block;
	BeamformerAcquisitionKind acquisition_kind;
	BeamformerViewPlaneTag    view_plane_tag;

	BeamformerFrame *next;
};

typedef struct {
	iv2 window_size;

	Arena  arena;
	Arena  ui_backing_store;
	void  *ui;
	u32    ui_dirty_parameter_blocks;

	BeamformerComputeContext compute_context;

	/* TODO(rnp): ideally this would go in the UI but its hard to manage with the UI
	 * destroying itself on hot-reload */
	FrameViewRenderContext frame_view_render_context;

	Stream error_stream;

	BeamformWorkQueue *beamform_work_queue;

	ComputeShaderStats *compute_shader_stats;
	ComputeTimingTable *compute_timing_table;

	SharedMemoryRegion shared_memory;

	BeamformerFrame beamform_frames[BeamformerMaxSavedFrames];
	BeamformerFrame *latest_frame;
	u32 next_render_frame_index;
	u32 display_frame_index;

	/* NOTE: this will only be used when we are averaging */
	u32             averaged_frame_index;
	BeamformerFrame averaged_frames[2];

	GLWorkerThreadContext  upload_worker;
	GLWorkerThreadContext  compute_worker;

	DEBUG_DECL(renderdoc_start_frame_capture_fn *start_frame_capture;)
	DEBUG_DECL(renderdoc_end_frame_capture_fn   *end_frame_capture;)
} BeamformerCtx;
#define BeamformerContextMemory(m) (BeamformerCtx *)align_pointer_up((m), alignof(BeamformerCtx));

typedef struct ShaderReloadContext ShaderReloadContext;
struct ShaderReloadContext {
	BeamformerCtx       *beamformer_context;
	ShaderReloadContext *link;
	s8     header;
	GLenum gl_type;
	i32    reloadable_info_index;
};

#define BEAMFORMER_COMPLETE_COMPUTE_FN(name) void name(iptr user_context, Arena *arena, iptr gl_context)
typedef BEAMFORMER_COMPLETE_COMPUTE_FN(beamformer_complete_compute_fn);

#define BEAMFORMER_RF_UPLOAD_FN(name) void name(BeamformerUploadThreadContext *ctx)
typedef BEAMFORMER_RF_UPLOAD_FN(beamformer_rf_upload_fn);

#define BEAMFORMER_RELOAD_SHADER_FN(name) b32 name(s8 path, ShaderReloadContext *src, \
                                                   Arena arena, s8 shader_name)
typedef BEAMFORMER_RELOAD_SHADER_FN(beamformer_reload_shader_fn);

#endif /* BEAMFORMER_INTERNAL_H */
