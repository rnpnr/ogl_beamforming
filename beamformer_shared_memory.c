/* See LICENSE for license details. */
#define BEAMFORMER_SHARED_MEMORY_VERSION (24UL)

typedef enum {
	BeamformerWorkKind_Compute,
	BeamformerWorkKind_ComputeIndirect,
	BeamformerWorkKind_CreateFilter,
	BeamformerWorkKind_ExportBuffer,
	BeamformerWorkKind_UploadBuffer,
} BeamformerWorkKind;

typedef struct {
	BeamformerFilterParameters parameters;
	u8 filter_slot;
	u8 parameter_block;
	static_assert(BeamformerFilterSlots            <= 255, "CreateFilterContext only supports 255 filter slots");
	static_assert(BeamformerMaxParameterBlockSlots <= 255, "CreateFilterContext only supports 255 parameter blocks");
} BeamformerCreateFilterContext;

typedef enum {
	BeamformerExportKind_BeamformedData,
	BeamformerExportKind_Stats,
} BeamformerExportKind;

typedef struct {
	BeamformerExportKind kind;
	u32 size;
} BeamformerExportContext;

#define BEAMFORMER_SHARED_MEMORY_LOCKS \
	X(ScratchSpace)    \
	X(UploadRF)        \
	X(ExportSync)      \
	X(DispatchCompute)

#define X(name) BeamformerSharedMemoryLockKind_##name,
typedef enum {BEAMFORMER_SHARED_MEMORY_LOCKS BeamformerSharedMemoryLockKind_Count} BeamformerSharedMemoryLockKind;
#undef X

typedef struct {
	u32 parameter_block;
} BeamformerComputeWorkContext;

typedef struct {
	BeamformerViewPlaneTag view_plane;
	u32                    parameter_block;
} BeamformerComputeIndirectWorkContext;

/* NOTE: discriminated union based on type */
typedef struct {
	BeamformerWorkKind kind;
	BeamformerSharedMemoryLockKind lock;
	union {
		void                                 *generic;
		BeamformerComputeWorkContext          compute_context;
		BeamformerComputeIndirectWorkContext  compute_indirect_context;
		BeamformerCreateFilterContext         create_filter_context;
		BeamformerExportContext               export_context;
		BeamformerShaderKind                  reload_shader;
	};
} BeamformWork;

typedef struct {
	union {
		u64 queue;
		struct {u32 widx, ridx;};
	};
	BeamformWork work_items[1 << 6];
} BeamformWorkQueue;

#define BEAMFORMER_SHARED_MEMORY_SIZE             (GB(2))
#define BEAMFORMER_SHARED_MEMORY_MAX_SCRATCH_SIZE (BEAMFORMER_SHARED_MEMORY_SIZE - \
                                                   sizeof(BeamformerSharedMemory) - \
                                                   sizeof(BeamformerParameterBlock))

#define X(name, id) BeamformerLiveImagingDirtyFlags_##name = (1 << id),
typedef enum {BEAMFORMER_LIVE_IMAGING_DIRTY_FLAG_LIST} BeamformerLiveImagingDirtyFlags;
#undef X

#define BEAMFORMER_PARAMETER_BLOCK_REGION_LIST \
	X(ComputePipeline,             pipeline)        \
	X(ChannelMapping,              channel_mapping) \
	X(FocalVectors,                focal_vectors)   \
	X(Parameters,                  parameters)      \
	X(SparseElements,              sparse_elements) \
	X(TransmitReceiveOrientations, transmit_receive_orientations)

typedef enum {
	#define X(k, ...) BeamformerParameterBlockRegion_##k,
	BEAMFORMER_PARAMETER_BLOCK_REGION_LIST
	#undef X
	BeamformerParameterBlockRegion_Count
} BeamformerParameterBlockRegions;

typedef union {
	u8 filter_slot;
} BeamformerShaderParameters;

typedef struct {
	BeamformerShaderKind       shaders[BeamformerMaxComputeShaderStages];
	BeamformerShaderParameters parameters[BeamformerMaxComputeShaderStages];
	u32                        shader_count;
	BeamformerDataKind         data_kind;
} BeamformerComputePipeline;

typedef struct {
	alignas(16) union {
		BeamformerParameters parameters;
		struct {
			BeamformerParametersHead parameters_head;
			BeamformerUIParameters   parameters_ui;
		};
	};

	/* NOTE(rnp): signals to the beamformer that a subregion of a block has been updated */
	u32 dirty_regions;
	static_assert(BeamformerParameterBlockRegion_Count <= 32, "only 32 parameter block regions supported");

	BeamformerComputePipeline pipeline;

	alignas(16) i16 channel_mapping[BeamformerMaxChannelCount];
	alignas(16) i16 sparse_elements[BeamformerMaxChannelCount];
	alignas(16) u8  transmit_receive_orientations[BeamformerMaxChannelCount];
	/* NOTE(rnp): interleaved transmit angle, focal depth pairs */
	alignas(16) v2  focal_vectors[BeamformerMaxChannelCount];
} BeamformerParameterBlock;
static_assert(sizeof(BeamformerParameterBlock) % alignof(BeamformerParameterBlock) == 0,
              "sizeof(BeamformerParametersBlock) must be a multiple of its alignment");

#define X(k, field) [BeamformerParameterBlockRegion_##k] = offsetof(BeamformerParameterBlock, field),
read_only global u16 BeamformerParameterBlockRegionOffsets[BeamformerParameterBlockRegion_Count] = {
	BEAMFORMER_PARAMETER_BLOCK_REGION_LIST
};
#undef X

typedef struct {
	u32 version;

	/* NOTE(rnp): causes future library calls to fail.
	 * see note in beamformer_invalidate_shared_memory() */
	b32 invalid;

	/* NOTE(rnp): not used for locking on w32 but we can use these to peek at the status of
	 * the lock without leaving userspace. */
	i32 locks[(u32)BeamformerSharedMemoryLockKind_Count + (u32)BeamformerMaxParameterBlockSlots];

	/* NOTE(rnp): total number of parameter block regions the client has requested.
	 * used to calculate offset to scratch space and to track number of allocated
	 * semaphores on w32. Defaults to 1 but can be changed at runtime */
	u32 reserved_parameter_blocks;

	/* TODO(rnp): this is really sucky. we need a better way to communicate this */
	u64 rf_block_rf_size;

	u64 max_beamformed_data_size;

	BeamformerLiveImagingParameters live_imaging_parameters;
	BeamformerLiveImagingDirtyFlags live_imaging_dirty_flags;

	BeamformWorkQueue external_work_queue;
} BeamformerSharedMemory;

function BeamformWork *
beamform_work_queue_pop(BeamformWorkQueue *q)
{
	BeamformWork *result = 0;

	static_assert(ISPOWEROF2(countof(q->work_items)), "queue capacity must be a power of 2");
	u64 val  = atomic_load_u64(&q->queue);
	u64 mask = countof(q->work_items) - 1;
	u64 widx = val       & mask;
	u64 ridx = val >> 32 & mask;

	if (ridx != widx)
		result = q->work_items + ridx;

	return result;
}

function void
beamform_work_queue_pop_commit(BeamformWorkQueue *q)
{
	atomic_add_u64(&q->queue, 0x100000000ULL);
}

function BeamformWork *
beamform_work_queue_push(BeamformWorkQueue *q)
{
	BeamformWork *result = 0;

	static_assert(ISPOWEROF2(countof(q->work_items)), "queue capacity must be a power of 2");
	u64 val  = atomic_load_u64(&q->queue);
	u64 mask = countof(q->work_items) - 1;
	u64 widx = val        & mask;
	u64 ridx = val >> 32  & mask;
	u64 next = (widx + 1) & mask;

	if (val & 0x80000000)
		atomic_and_u64(&q->queue, ~0x80000000);

	if (next != ridx) {
		result = q->work_items + widx;
		zero_struct(result);
	}

	return result;
}

function void
beamform_work_queue_push_commit(BeamformWorkQueue *q)
{
	atomic_add_u64(&q->queue, 1);
}

#if OS_WINDOWS
// NOTE(rnp): junk needed on w32 to watch a value across processes while yielding
// control back to the kernel. There are user level CPU instructions that allow
// this so why w32 can't do it in kernel mode sounds like shitty design to me.
DEBUG_IMPORT OSW32Semaphore os_w32_shared_memory_semaphores[countof(((BeamformerSharedMemory *)0)->locks)];
#endif

function b32
beamformer_shared_memory_take_lock(BeamformerSharedMemory *sm, i32 lock, u32 timeout_ms)
{
#if OS_WINDOWS
	b32 result = os_w32_semaphore_wait(os_w32_shared_memory_semaphores[lock], timeout_ms);
	if (result) atomic_store_u32(sm->locks + lock, 1);
#else
	b32 result = take_lock(sm->locks + lock, timeout_ms);
#endif
	return result;
}

function void
beamformer_shared_memory_release_lock(BeamformerSharedMemory *sm, i32 lock)
{
	release_lock(sm->locks + lock);
#if OS_WINDOWS
	os_w32_semaphore_release(os_w32_shared_memory_semaphores[lock], 1);
#endif
}

function BeamformerParameterBlock *
beamformer_parameter_block(BeamformerSharedMemory *sm, u32 block)
{
	assert(sm->reserved_parameter_blocks >= block);
	BeamformerParameterBlock *result = (typeof(result))((u8 *)(sm + 1) + block * sizeof(*result));
	return result;
}

function b32
beamformer_parameter_block_dirty(BeamformerSharedMemory *sm, u32 block)
{
	b32 result = beamformer_parameter_block(sm, block)->dirty_regions != 0;
	return result;
}

function BeamformerParameterBlock *
beamformer_parameter_block_lock(BeamformerSharedMemory *sm, u32 block, i32 timeout_ms)
{
	assert(block < BeamformerMaxParameterBlockSlots);
	BeamformerParameterBlock *result = 0;
	if (beamformer_shared_memory_take_lock(sm, BeamformerSharedMemoryLockKind_Count + block, (u32)timeout_ms))
		result = beamformer_parameter_block(sm, block);
	return result;
}

function void
beamformer_parameter_block_unlock(BeamformerSharedMemory *sm, u32 block)
{
	assert(block < BeamformerMaxParameterBlockSlots);
	beamformer_shared_memory_release_lock(sm, BeamformerSharedMemoryLockKind_Count + block);
}

function Arena
beamformer_shared_memory_scratch_arena(BeamformerSharedMemory *sm)
{
	assert(sm->reserved_parameter_blocks > 0);
	BeamformerParameterBlock *last = beamformer_parameter_block(sm, sm->reserved_parameter_blocks);
	Arena result = {.beg = (u8 *)(last + 1), .end = (u8 *)sm + BEAMFORMER_SHARED_MEMORY_SIZE};
	result.beg = arena_aligned_start(result, KB(4));
	return result;
}

function void
mark_parameter_block_region_dirty(BeamformerSharedMemory *sm, u32 block, BeamformerParameterBlockRegions region)
{
	BeamformerParameterBlock *pb = beamformer_parameter_block(sm, block);
	atomic_or_u32(&pb->dirty_regions, 1u << region);
}

function void
post_sync_barrier(BeamformerSharedMemory *sm, BeamformerSharedMemoryLockKind lock)
{
	/* NOTE(rnp): debug: here it is not a bug to release the lock if it
	 * isn't held but elswhere it is */
	DEBUG_DECL(if (sm->locks[lock])) {
		beamformer_shared_memory_release_lock(sm, lock);
	}
}
