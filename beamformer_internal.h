/* See LICENSE for license details. */
#ifndef BEAMFORMER_INTERNAL_H
#define BEAMFORMER_INTERNAL_H

#include "beamformer.h"

#include "util.h"
#include "opengl.h"

#include "generated/beamformer.meta.c"
#include "generated/beamformer_shaders.c"

#include <raylib_extended.h>
#include <rlgl.h>

#include "threads.c"
#include "util_gl.c"
#include "util_os.c"

#define beamformer_info(s) s8("[info] " s "\n")

#define os_path_separator() (s8){.data = &os_system_info()->path_separator_byte, .len = 1}

typedef enum {
	GPUBufferCreateFlags_HostWritable = 1 << 0,
	GPUBufferCreateFlags_MemoryOnly   = 1 << 1,
} GPUBufferCreateFlags;

typedef struct { u64 value[1]; } VulkanHandle;

typedef struct {
	u64          gpu_pointer;
	i64          size;
	VulkanHandle buffer;
} GPUBuffer;

typedef enum {
	GPUVendor_AMD      = 0x1002,
	GPUVendor_NVIDIA   = 0x10DE,
	GPUVendor_Qualcomm = 0x5143,
	GPUVendor_Intel    = 0x8086,
} GPUVendor;

typedef struct {
	s8        name;
	GPUVendor vendor;

	f32 timestamp_period_ns;

	u32 max_compute_shared_memory_size;
	u32 max_msaa_samples;
	u32 max_image_dimension_2D;
	// NOTE(rnp): vulkan compute will output to a buffer so this won't be relevant
	u32 max_image_dimension_3D;

	u64 gpu_heap_size;
	u64 gpu_heap_used;
} GPUInfo;

///////////////////////////
// NOTE: vulkan layer API
DEBUG_EXPORT void vk_load(OSLibrary vulkan, Arena *memory, Stream *error);

DEBUG_EXPORT GPUInfo *vk_gpu_info(void);

DEBUG_EXPORT void vk_buffer_allocate(GPUBuffer *, iz size, GPUBufferCreateFlags flags, OSHandle *export, s8 label);
DEBUG_EXPORT void vk_buffer_release(GPUBuffer *);
DEBUG_EXPORT void vk_buffer_range_upload(GPUBuffer *, void *data, u64 offset, u64 size, b32 non_temporal);
DEBUG_EXPORT u64  vk_round_up_to_sync_size(u64, u64 min);

// NOTE: temporary API
DEBUG_EXPORT b32 vk_buffer_needs_sync(GPUBuffer *);

DEBUG_EXPORT VulkanHandle vk_semaphore_create(OSHandle *export);

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
	// NOTE(rnp): w32 doesn't transfer ownership of these when they are imported
	// into the driver. For now just store them here, this code won't be around for long
	OSHandle     upload_semaphores_handles[BeamformerMaxRawDataFramesInFlight];
	VulkanHandle vk_upload_semaphores[BeamformerMaxRawDataFramesInFlight];
	u32          gl_upload_semaphores[BeamformerMaxRawDataFramesInFlight];

	GLsync       compute_syncs[BeamformerMaxRawDataFramesInFlight];

	u64          uploaded_data_indices[BeamformerMaxRawDataFramesInFlight];

	GPUBuffer buffer;
	OSHandle  export_handle;

	u32 ssbo, memory_object;

	u32 active_rf_size;
	u32 data_timestamp_query;

	u64 insertion_index;
	u64 compute_index;
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
	BeamformerRFBuffer *     rf_buffer;
	BeamformerSharedMemory * shared_memory;
	ComputeTimingTable *     compute_timing_table;
	i32                *     compute_worker_sync;
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
	OSThread handle;

	Arena arena;
	iptr  window_handle;
	iptr  gl_context;
	iptr  user_context;
	i32   sync_variable;
	b32   awake;
} GLWorkerThreadContext;

typedef enum {
	BeamformerState_Uninitialized = 0,
	BeamformerState_Running,
	BeamformerState_ShouldClose,
	BeamformerState_Terminated,
} BeamformerState;

typedef struct {
	BeamformerState state;

	iv2 window_size;

	Arena  arena;
	Arena  ui_backing_store;
	void  *ui;
	u32    ui_dirty_parameter_blocks;

	u64    frame_timestamp;

	BeamformerComputeContext compute_context;

	/* TODO(rnp): ideally this would go in the UI but its hard to manage with the UI
	 * destroying itself on hot-reload */
	FrameViewRenderContext frame_view_render_context;

	Stream error_stream;

	BeamformWorkQueue *beamform_work_queue;

	ComputeShaderStats *compute_shader_stats;
	ComputeTimingTable *compute_timing_table;

	BeamformerSharedMemory *shared_memory;

	BeamformerFrame beamform_frames[BeamformerMaxSavedFrames];
	BeamformerFrame *latest_frame;
	u32 next_render_frame_index;
	u32 display_frame_index;

	/* NOTE: this will only be used when we are averaging */
	u32             averaged_frame_index;
	BeamformerFrame averaged_frames[2];

	GLWorkerThreadContext  upload_worker;
	GLWorkerThreadContext  compute_worker;
} BeamformerCtx;
#define BeamformerContextMemory(m) (BeamformerCtx *)align_pointer_up((m), alignof(BeamformerCtx));

typedef enum {
	BeamformerFileReloadKind_Shader,
	BeamformerFileReloadKind_ComputeShader,
} BeamformerFileReloadKind;

typedef struct BeamformerShaderReloadContext BeamformerShaderReloadContext;
struct BeamformerShaderReloadContext {
	BeamformerShaderReloadContext * link;
	s8     header;
	GLenum gl_type;
	i32    reloadable_info_index;
};

typedef struct {
	BeamformerFileReloadKind kind;
	union {
		BeamformerShaderReloadContext * shader_reload_context;
		BeamformerShaderKind            compute_shader_kind;
	};
} BeamformerFileReloadContext;

#define BEAMFORMER_COMPLETE_COMPUTE_FN(name) void name(iptr user_context, Arena *arena, iptr gl_context)
typedef BEAMFORMER_COMPLETE_COMPUTE_FN(beamformer_complete_compute_fn);

#define BEAMFORMER_RF_UPLOAD_FN(name) void name(BeamformerUploadThreadContext *ctx)
typedef BEAMFORMER_RF_UPLOAD_FN(beamformer_rf_upload_fn);

#define BEAMFORMER_DEBUG_UI_DEINIT_FN(name) void name(BeamformerCtx *ctx)
typedef BEAMFORMER_DEBUG_UI_DEINIT_FN(beamformer_debug_ui_deinit_fn);

#endif /* BEAMFORMER_INTERNAL_H */
