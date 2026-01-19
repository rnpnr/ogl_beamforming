/* See LICENSE for license details. */
#ifndef BEAMFORMER_INTERNAL_H
#define BEAMFORMER_INTERNAL_H

#include "beamformer.h"

#include "util.h"
#include "opengl.h"

#include "generated/beamformer.meta.c"
#include "generated/beamformer_shaders.c"

#include "external/raylib/src/raylib.h"
#include "external/raylib/src/rlgl.h"

#define beamformer_info(s) s8("[info] " s "\n")

#define os_path_separator() (s8){.data = &os_system_info()->path_separator_byte, .len = 1}

typedef struct { u64 value[1]; } VulkanHandle;

typedef enum {
	VulkanTimeline_Graphics,
	VulkanTimeline_Compute,
	VulkanTimeline_Transfer,
	VulkanTimeline_Count,
} VulkanTimeline;

typedef enum {
	VulkanShaderKind_Vertex,
	VulkanShaderKind_Mesh,
	VulkanShaderKind_Fragment,
	VulkanShaderKind_Compute,
	VulkanShaderKind_Count,
} VulkanShaderKind;

typedef enum {
	VulkanImageUsage_None,
	VulkanImageUsage_Colour,
	VulkanImageUsage_DepthStencil,
	VulkanImageUsage_Count,
} VulkanImageUsage;

typedef enum {
	VulkanUsageFlag_ImageSampling       = 1 << 0,
	VulkanUsageFlag_HostReadWrite       = 1 << 1, // NOTE: not valid on images
	/* NOTE: uses:
	 * - image-image copy operations
	 * - buffer-buffer copy operations
	 */
	VulkanUsageFlag_TransferSource      = 1 << 2,
	VulkanUsageFlag_TransferDestination = 1 << 3,
} VulkanUsageFlags;

typedef struct {
	VulkanShaderKind kind;
	s8               text;
	s8               name;
} VulkanPipelineCreateInfo;

typedef struct {
	VulkanHandle buffer;
	u64          gpu_pointer;
	i64          size;

	// NOTE: only used for render models
	u64          index_count;
} GPUBuffer;

typedef struct {
	VulkanHandle image;
	u32          width;
	u32          height;
	u32          samples;
	u32          mip_map_levels;
	// TODO(rnp): this is only here for importing from OpenGL, move it back into handle later
	u64          memory_size;
} GPUImage;

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
	u16 max_msaa_samples;
	u16 subgroup_size;

	u32 max_image_dimension_2D;
	// NOTE(rnp): vulkan compute will output to a buffer so this won't be relevant
	u32 max_image_dimension_3D;

	u64 gpu_heap_size;
	u64 gpu_heap_used;
} GPUInfo;

typedef struct {
	i64               size;
	VulkanUsageFlags  flags;

	// NOTE(rnp): only required if buffer will be used on multiple timelines
	VulkanTimeline   *timelines_used;
	u32               timeline_count;

	s8                label;
} GPUBufferAllocateInfo;

typedef struct {
	GPUBuffer *gpu_buffer;
	u64        offset;
	u64        size;
} GPUMemoryBarrierInfo;

typedef struct {
	GPUBuffer model;
	u32       vertex_count;
	u32       normals_offset;
} RenderModel;

#include "threads.c"
#include "util_os.c"

///////////////////////////
// NOTE: vulkan layer API
DEBUG_IMPORT void vk_load(OSLibrary vulkan, Arena *memory, Stream *error);

DEBUG_IMPORT GPUInfo *vk_gpu_info(void);

DEBUG_IMPORT void vk_buffer_allocate(GPUBuffer *, GPUBufferAllocateInfo *info);
DEBUG_IMPORT void vk_buffer_release(GPUBuffer *);
DEBUG_IMPORT void vk_buffer_range_upload(GPUBuffer *, void *data, u64 offset, u64 size, b32 non_temporal);
DEBUG_IMPORT void vk_buffer_range_download(void *output, GPUBuffer *, u64 source_offset, u64 size, b32 non_temporal);
DEBUG_IMPORT u64  vk_round_up_to_sync_size(u64, u64 min);

// NOTE: images are 2D only, any other use case should just use a buffer and index in the shader
DEBUG_IMPORT void vk_image_allocate(GPUImage *, u32 width, u32 height, u32 mips, u32 samples, VulkanImageUsage usage, VulkanUsageFlags flags, OSHandle *export);
DEBUG_IMPORT void vk_image_release(GPUImage *);

DEBUG_IMPORT void vk_render_model_allocate(GPUBuffer *, void *indices, u64 index_count, u64 model_size, s8 label);
DEBUG_IMPORT void vk_render_model_range_upload(GPUBuffer *, void *data, u64 offset, u64 size, b32 non_temporal);
DEBUG_IMPORT void vk_render_model_release(GPUBuffer *);

/* NOTE: Pipelines do not have bindings. Data should be passed using push constants.
 * In particular the push constants should contain pointers to gpu memory using the
 * BufferDeviceAddress extension. */
// TODO(rnp): change this to accept SPIR-V directly and accept BakeParameters as specialization data
DEBUG_IMPORT VulkanHandle vk_pipeline(VulkanPipelineCreateInfo *infos, u32 count, u32 push_constants_size);
DEBUG_IMPORT b32          vk_pipeline_valid(VulkanHandle);
DEBUG_IMPORT void         vk_pipeline_release(VulkanHandle);

DEBUG_IMPORT b32 vk_buffer_needs_sync(GPUBuffer *);

DEBUG_IMPORT VulkanHandle vk_create_semaphore(OSHandle *export);

DEBUG_IMPORT b32          vk_host_wait_timeline(VulkanTimeline timeline, u64 value, u64 timeout_ns);
DEBUG_IMPORT u64          vk_host_signal_timeline(VulkanTimeline timeline);

DEBUG_IMPORT VulkanHandle vk_command_begin(VulkanTimeline timeline);
DEBUG_IMPORT void         vk_command_bind_pipeline(VulkanHandle command, VulkanHandle pipeline);
DEBUG_IMPORT void         vk_command_buffer_memory_barriers(VulkanHandle command, GPUMemoryBarrierInfo *barriers, u64 count);
DEBUG_IMPORT void         vk_command_dispatch_compute(VulkanHandle command, uv3 dispatch);
DEBUG_IMPORT void         vk_command_push_constants(VulkanHandle command, u32 offset, u32 size, void *values);
DEBUG_IMPORT void         vk_command_timestamp(VulkanHandle command);
DEBUG_IMPORT void         vk_command_wait_timeline(VulkanHandle command, VulkanTimeline timeline, u64 value);
// NOTE: extra semaphores only exist for synchronization with OpenGL and will be removed in the future
DEBUG_IMPORT u64          vk_command_end(VulkanHandle command, VulkanHandle wait_semaphore, VulkanHandle finished_semaphore);

DEBUG_IMPORT void         vk_command_begin_rendering(VulkanHandle command, GPUImage *restrict colour, GPUImage *restrict depth, GPUImage *restrict resolve);
DEBUG_IMPORT void         vk_command_draw(VulkanHandle command, GPUBuffer *model);
DEBUG_IMPORT void         vk_command_scissor(VulkanHandle command, u32 width, u32 height, u32 x_offset, u32 y_offset);
DEBUG_IMPORT void         vk_command_viewport(VulkanHandle command, f32 width, f32 height, f32 x_offset, f32 y_offset, f32 min_depth, f32 max_depth);
DEBUG_IMPORT void         vk_command_end_rendering(VulkanHandle command);

DEBUG_IMPORT void         vk_command_copy_buffer(VulkanHandle command, GPUBuffer *restrict destination, GPUBuffer *restrict source, u64 source_offset, i64 size);

// NOTE: returns array of valid timestamps + 1, first element is the count.
//       Calling thread may stall until results available.
DEBUG_IMPORT u64 *        vk_command_read_timestamps(VulkanTimeline timeline, Arena *arena);

#if BEAMFORMER_RENDERDOC_HOOKS
DEBUG_IMPORT void *       vk_renderdoc_instance_handle(void);
#else
#define vk_renderdoc_instance_handle() ((void *)0)
#endif

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

#include "beamformer_parameters.h"
#include "beamformer_shared_memory.c"

typedef struct {
	BeamformerFilterParameters parameters;
	f32                        time_delay;
	i32                        length;
	GPUBuffer                  buffer;
} BeamformerFilter;

// X(kind, format, elements)
#define BEAMFORMER_COMPUTE_ARRAY_PARAMETERS_LIST \
	X(Hadamard,                    f32, BeamformerMaxChannelCount * BeamformerMaxChannelCount) \
	X(FocalVectors,                v2,  BeamformerMaxChannelCount) \
	X(SparseElements,              i16, BeamformerMaxChannelCount) \
	X(TransmitReceiveOrientations, u16, BeamformerMaxChannelCount) \

typedef enum {
	#define X(k, ...) BeamformerComputeArrayParameterKind_##k,
	BEAMFORMER_COMPUTE_ARRAY_PARAMETERS_LIST
	#undef X
	BeamformerComputeArrayParameterKind_Count
} BeamformerComputeArrayParameterKind;

// NOTE(rnp): only used to calculate offsets, never used directly
#define X(name, type, elements) alignas(64) type name[elements];
typedef struct {BEAMFORMER_COMPUTE_ARRAY_PARAMETERS_LIST} BeamformerComputeArrayParameters;
#undef X

typedef struct {
	uv3 layout;
	uv3 dispatch;
	BeamformerShaderBakeParameters bake;
} BeamformerShaderDescriptor;

typedef struct BeamformerComputePlan BeamformerComputePlan;
struct BeamformerComputePlan {
	BeamformerComputePipeline pipeline;

	VulkanHandle vulkan_pipelines[BeamformerMaxComputeShaderStages];

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

	// TODO(rnp): specialization constants
	v2  xdc_element_pitch;
	m4  xdc_transform;
	// TODO(rnp): probably just compute this everytime
	m4  das_voxel_transform;

	GPUBuffer array_parameters;

	BeamformerFilter filters[BeamformerFilterSlots];

	u128 shader_hashes[BeamformerMaxComputeShaderStages];
	BeamformerShaderDescriptor shader_descriptors[BeamformerMaxComputeShaderStages];

	BeamformerComputePlan *next;
};

typedef struct {
	u64 upload_complete_values[BeamformerMaxRawDataFramesInFlight];
	u64 compute_complete_values[BeamformerMaxRawDataFramesInFlight];

	GPUBuffer buffer;

	u32 active_rf_size;

	u64 timestamp;

	u64 insertion_index;
	u64 compute_index;
} BeamformerRFBuffer;

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
		struct {
			static_assert(BeamformerShaderKind_Count <= U16_MAX, "");
			u16 shader;
			u16 shader_slot;
		};
	};
} ComputeTimingInfo;

typedef struct {
	u32 write_index;
	u32 read_index;
	b32 compute_frame_active;

	u32                  in_flight_shader_count;
	BeamformerShaderKind in_flight_shader_ids[BeamformerMaxComputeShaderStages];

	ComputeTimingInfo buffer[4096];
} ComputeTimingTable;

typedef struct {
	BeamformerRFBuffer *     rf_buffer;
	BeamformerSharedMemory * shared_memory;
	ComputeTimingTable *     compute_timing_table;
	i32                *     compute_worker_sync;
} BeamformerUploadThreadContext;

typedef struct {
	u64 buffer_offset;
	u64 timeline_valid_value;

	iv3 points;
	v3  min_coordinate;
	v3  max_coordinate;

	u32                       id;
	u32                       compound_count;
	BeamformerDataKind        data_kind;
	BeamformerAcquisitionKind acquisition_kind;
	BeamformerViewPlaneTag    view_plane_tag;
} BeamformerFrame;

/* NOTE(rnp): backing storage for beamformed frames. The amount of backlog frames
* is dependant on the currently requested output size. */
typedef struct {
	GPUBuffer   buffer[1];

	u64         next_offset;
	u64         counter;

	BeamformerFrame frames[BeamformerMaxBacklogFrames];
} BeamformerFrameBacklog;

typedef struct {
	BeamformerRFBuffer rf_buffer;

	BeamformerComputePlan *compute_plans[BeamformerMaxParameterBlockSlots];
	BeamformerComputePlan *compute_plan_freelist;

	VulkanHandle compute_internal_pipelines[BeamformerShaderKind_ComputeInternalCount];

	/* NOTE(rnp): used to ping pong data between compute stages.
	 * Half the buffer will be used for reading and the other for writing. */
	GPUBuffer ping_pong_buffer;
	u32 ping_pong_input_index;

	f32 processing_progress;
	b32 processing_compute;

	BeamformerFrameBacklog backlog;
} BeamformerComputeContext;

typedef struct {
	OSThread handle;

	Arena arena;
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

	Stream error_stream;

	BeamformerSharedMemory *shared_memory;

	BeamformerFrame *latest_frame;

	// TODO(rnp): track elsewhere
	b32 render_shader_updated;

	/* NOTE: this will only be used when we are averaging */
	u32             averaged_frame_index;
	BeamformerFrame averaged_frames[2];

	GLWorkerThreadContext  upload_worker;
	GLWorkerThreadContext  compute_worker;

	BeamformerComputeContext compute_context;

	ComputeShaderStats compute_shader_stats[1];
	ComputeTimingTable compute_timing_table[1];

	BeamformWorkQueue  beamform_work_queue[1];
} BeamformerCtx;
#define BeamformerContextMemory(m) (BeamformerCtx *)align_pointer_up((m), alignof(BeamformerCtx));

typedef enum {
	BeamformerFileReloadKind_ComputeInternalShader,
	BeamformerFileReloadKind_ComputeShader,
	BeamformerFileReloadKind_RenderShader,
} BeamformerFileReloadKind;

typedef struct {
	BeamformerShaderKind shader;
	VulkanHandle *       pipeline;
} BeamformerShaderReloadData;

typedef struct {
	BeamformerShaderKind  shader;
	VulkanShaderKind      shader_kind;

	// NOTE(rnp): based on BakeShaders compile time value
	s8                    filename_or_data;

	BeamformerShaderDescriptor *shader_descriptor;

	uv3 layout;
} BeamformerShaderReloadInfo;

typedef struct {
	BeamformerFileReloadKind kind;
	union {
		BeamformerShaderReloadData shader_reload;
	};
} BeamformerFileReloadContext;

#define BEAMFORMER_COMPLETE_COMPUTE_FN(name) void name(BeamformerCtx *ctx, Arena *arena)
typedef BEAMFORMER_COMPLETE_COMPUTE_FN(beamformer_complete_compute_fn);

#define BEAMFORMER_RF_UPLOAD_FN(name) void name(BeamformerUploadThreadContext *ctx)
typedef BEAMFORMER_RF_UPLOAD_FN(beamformer_rf_upload_fn);

#define BEAMFORMER_DEBUG_UI_DEINIT_FN(name) void name(BeamformerCtx *ctx)
typedef BEAMFORMER_DEBUG_UI_DEINIT_FN(beamformer_debug_ui_deinit_fn);

#endif /* BEAMFORMER_INTERNAL_H */
