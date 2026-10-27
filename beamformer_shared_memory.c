/* See LICENSE for license details. */
#define BEAMFORMER_SHARED_MEMORY_VERSION (38UL)

#define BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE KB(32)
#define BEAMFORMER_SHARED_MEMORY_PARAMETER_PAGE_SIZE KB(1)

#define BEAMFORMER_RF_ALIGN 256

#define BEAMFORMER_HANDLE(kind) struct kind##_T; typedef struct kind##_T *kind

BEAMFORMER_HANDLE(BeamformerSharedMemoryAllocationHandle);

typedef struct {
	u32 generation;

	u32 first_page;
	u32 page_count;

	b32 release_requested;
	u64 last_use_counter;

	BeamformerDataDescriptor data_descriptor;
	u32 transmit_receive_orientation_offset;
	u32 channel_mapping_offset;
} BeamformerParameterSetMetadata;

typedef enum {
	BeamformerSharedMemoryHandleKind_Nil,
	BeamformerSharedMemoryHandleKind_Memory,
	BeamformerSharedMemoryHandleKind_ParameterSet,
	BeamformerSharedMemoryHandleKind_Count,
} BeamformerSharedMemoryHandleKind;

typedef struct {
	u64 size;
	u32 align;
	u32 page_count;
} BeamformerSharedMemoryAllocation;

typedef struct {
	u32 page_count;
	u32 prev_page;
	u32 next_page;
} BeamformerFreePageRun;

typedef enum {
	BeamformerWorkKind_Compute,
	BeamformerWorkKind_WaitThenCompute,
	BeamformerWorkKind_CreateFilter,
	BeamformerWorkKind_ExportBuffer,
} BeamformerWorkKind;

typedef struct {
	BeamformerFilterParameters parameters;
	u8 filter_slot;
	u8 parameter_block;
	static_assert(BeamformerFilterSlots        <= 255, "CreateFilterContext only supports 255 filter slots");
	static_assert(BeamformerMaxParameterBlocks <= 255, "CreateFilterContext only supports 255 parameter blocks");
} BeamformerCreateFilterContext;

typedef enum {
	BeamformerExportKind_BeamformedData,
	BeamformerExportKind_Stats,
} BeamformerExportKind;

typedef struct {
	BeamformerExportKind kind;
	u32 count; /* Number of frames to export */
	BeamformerSharedMemoryAllocationHandle memory;
} BeamformerExportContext;

#define BEAMFORMER_SHARED_MEMORY_LOCKS \
	X(UploadRF)        \
	X(ExportSync)      \
	X(DispatchCompute)

#define X(name) BeamformerSharedMemoryLockKind_##name,
typedef enum {BEAMFORMER_SHARED_MEMORY_LOCKS BeamformerSharedMemoryLockKind_Count} BeamformerSharedMemoryLockKind;
#undef X

typedef struct {
	m4  voxel_transform;
	u64 rf_offset;
	f32 focus_depth;
	f32 steering_angle;
	u16 tx_rx_orientation;
	u16 parameter_set;
	BeamformerViewPlaneTag view_plane;

	static_assert(BeamformerMaxParameterBlocks <= U16_MAX, "");
} BeamformerComputeWorkContext;

/* NOTE: discriminated union based on type */
typedef struct {
	BeamformerWorkKind kind;
	BeamformerSharedMemoryLockKind lock;
	union {
		BeamformerComputeWorkContext  compute_context;
		BeamformerCreateFilterContext create_filter_context;
		BeamformerExportContext       export_context;
		BeamformerShaderKind          reload_shader;
	};
} BeamformWork;

typedef struct {
	union {
		u64 queue;
		struct {u32 widx, ridx;};
	};
	BeamformWork work_items[1 << 6];
} BeamformWorkQueue;

#define BEAMFORMER_PARAMETER_BLOCK_REGION_LIST \
	X(ComputePipeline,             pipeline)        \
	X(ChannelMapping,              channel_mapping) \
	X(FocalVectors,                focal_vectors)   \
	X(Parameters,                  parameters)      \
	X(SparseElements,              sparse_elements) \
	X(TransmitReceiveOrientations, transmit_receive_orientations) \

typedef enum {
	#define X(k, ...) BeamformerParameterBlockRegion_##k,
	BEAMFORMER_PARAMETER_BLOCK_REGION_LIST
	#undef X
	BeamformerParameterBlockRegion_Count
} BeamformerParameterBlockRegions;

typedef enum {
	BeamformerParameterDirtyFlag_Parameters,
	BeamformerParameterDirtyFlag_NotifyUI,
	BeamformerParameterDirtyFlag_Count,
} BeamformerParameterDirtyFlags;

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
	alignas(16) BeamformerParameters parameters;

	/* NOTE(rnp): signals to the beamformer that a subregion of a block has been updated */
	u32 region_update_flags;
	static_assert(BeamformerParameterDirtyFlag_Count <= 32, "");

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
read_only u16 BeamformerParameterBlockRegionOffsets[BeamformerParameterBlockRegion_Count] = {
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
	i32 locks[(u32)BeamformerSharedMemoryLockKind_Count + (u32)BeamformerMaxParameterBlocks];

	/* NOTE(rnp): total number of parameter block regions the client has requested.
	 * used to calculate offset to scratch space and to track number of allocated
	 * semaphores on w32. Defaults to 1 but can be changed at runtime */
	u32 reserved_parameter_blocks;

	/* TODO(rnp): this is really sucky. we need a better way to communicate this */
	BeamformerSharedMemoryAllocationHandle rf_handle;

	// NOTE(rnp): currently this cannot be directly user readable. its interpretation
	// requires beamformer implementation details
	u64 beamformed_frame_buffer_size;

	struct {
		u64 max_rf_data_size;
		b8  cuda;
		b8  hilbert;
	} capabilities;

	BeamformerLiveImagingParameters live_imaging_parameters;
	BeamformerLiveImagingDirtyFlags live_imaging_dirty_flags;

	struct {
		u32 first_free_page;
		u32 page_count;
		u64 offset;
		u64 lock;
	} transient;

	u32 parameter_set_first_free_page;

	u64 parameter_sets_used_masks[(BeamformerMaxParameterSets + 63) / 64];

	BeamformerParameterSetMetadata parameter_sets[BeamformerMaxParameterSets];

	BeamformWorkQueue external_work_queue;
} BeamformerSharedMemory;

function BeamformWork *
beamform_work_queue_pop(BeamformWorkQueue *q)
{
	BeamformWork *result = 0;

	static_assert(IsPowerOfTwo(countof(q->work_items)), "queue capacity must be a power of 2");
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

	static_assert(IsPowerOfTwo(countof(q->work_items)), "queue capacity must be a power of 2");
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
	assert(sm->reserved_parameter_blocks > block);
	BeamformerParameterBlock *result = (typeof(result))((u8 *)(sm + 1) + block * sizeof(*result));
	return result;
}

function b32
beamformer_parameter_block_dirty(BeamformerSharedMemory *sm, u32 block)
{
	b32 result = beamformer_parameter_block(sm, block)->region_update_flags != 0;
	return result;
}

function BeamformerParameterBlock *
beamformer_parameter_block_lock(BeamformerSharedMemory *sm, u32 block, i32 timeout_ms)
{
	assert(block < BeamformerMaxParameterBlocks);
	BeamformerParameterBlock *result = 0;
	if (beamformer_shared_memory_take_lock(sm, BeamformerSharedMemoryLockKind_Count + block, (u32)timeout_ms))
		result = beamformer_parameter_block(sm, block);
	return result;
}

function void
beamformer_parameter_block_unlock(BeamformerSharedMemory *sm, u32 block)
{
	assert(block < BeamformerMaxParameterBlocks);
	beamformer_shared_memory_release_lock(sm, BeamformerSharedMemoryLockKind_Count + block);
}

function void
beamformed_shared_memory_dirty_flag(BeamformerSharedMemory *sm, u32 block, BeamformerParameterDirtyFlags flag)
{
	BeamformerParameterBlock *pb = beamformer_parameter_block(sm, block);
	atomic_or_u32(&pb->region_update_flags, 1u << flag);
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

function u32
beamformer_shared_memory_claim_pages(BeamformerSharedMemory *sm, u64 bytes)
{
	u32 result = -1u;
	return result;
}

function u32
beamformer_shared_memory_release_pages(BeamformerSharedMemory *sm, u32 first_free_page, u32 page, u32 count, u32 page_size)
{
	u32 current = first_free_page;
	BeamformerFreePageRun *pr = (void *)((u8 *)sm + page_size * current);
	while (current < page && page > pr->next_page) {
		current = pr->next_page;
		pr = (void *)((u8 *)sm + page_size * current);
	}

	u32 result = first_free_page;

	BeamformerFreePageRun *to_free = (void *)((u8 *)sm + page_size * page);
	zero_struct(to_free);

	if (current > page) {
		assert(current == first_free_page);
		assert(pr->prev_page == 0);

		result = page;

		to_free->page_count = count;
		if (page + count == current) {
			to_free->page_count += pr->page_count;
			to_free->next_page   = pr->next_page;
		} else {
			to_free->next_page = current;
			pr->prev_page      = page;
		}
	} else {
		BeamformerFreePageRun *next = (void *)((u8 *)sm + page_size * pr->next_page);
		if (current + pr->page_count == page) {
			pr->page_count += count;
			if (current + pr->page_count == pr->next_page) {
				pr->next_page   = next->next_page;
				pr->page_count += next->page_count;
				if (next->next_page) {
					next = (void *)((u8 *)sm + page_size * next->next_page);
					next->prev_page = current;
				}
			}
		} else {
			if (pr->next_page) next->prev_page = page;
			to_free->next_page  = pr->next_page;
			to_free->page_count = count;
			to_free->prev_page  = current;
		}
	}
	return result;
}

function void *
beamformer_shared_memory_make_handle(BeamformerSharedMemoryHandleKind kind, u64 payload)
{
	static_assert(BeamformerSharedMemoryHandleKind_Count <= U16_MAX, "");
	u64 result = ((u64)kind << 48ul) | (payload & 0x0000fffffffffffful);
	return (void *)result;
}

function BeamformerSharedMemoryHandleKind
beamformer_shared_memory_handle_kind(void *handle)
{
	BeamformerSharedMemoryHandleKind result = (BeamformerSharedMemoryHandleKind)((u64)handle >> 48ul);
	return result;
}

function u64
beamformer_shared_memory_handle_payload(void *handle)
{
	u64 result = ((u64)handle & 0x0000fffffffffffful);
	return result;
}

#define beamformer_shared_memory_transient_alloc(sm, ...) beamformer_shared_memory_transient_alloc_(sm, (ArenaAllocateInfo){.align = 8, .count = 1, __VA_ARGS__})
function BeamformerSharedMemoryAllocationHandle
beamformer_shared_memory_transient_alloc_(BeamformerSharedMemory *sm, ArenaAllocateInfo info)
{
	u64 needed_size = AlignUpPowerOfTwo(info.size * info.count + sizeof(BeamformerSharedMemoryAllocation), info.align);

	u64 page_count = DivRoundUp(needed_size, BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE);
	u64 expected = 0;
	spin_wait(!atomic_cas_u64(&sm->transient.lock, &expected, 1));

	BeamformerFreePageRun *pr = sm->transient.first_free_page ? (void *)((u8 *)sm + sm->transient.first_free_page * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE) : 0;
	while (pr) {
		if (pr->page_count >= page_count)
			break;
		pr = pr->next_page ? (void *)((u8 *)sm + pr->next_page * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE) : 0;
	}

	BeamformerSharedMemoryAllocationHandle result = 0;
	if (pr) {
		u64 new_free_page = (u64)((u8 *)pr - (u8 *)sm) / BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE + page_count;

		BeamformerFreePageRun *next = (void *)((u8 *)sm + new_free_page * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE);
		next->page_count = pr->page_count - page_count;
		next->prev_page  = pr->prev_page;
		next->next_page  = pr->next_page;

		if (next->prev_page) {
			BeamformerFreePageRun *prev = (void *)((u8 *)sm + next->prev_page * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE);
			prev->next_page = new_free_page;
		}

		if (next->next_page) {
			BeamformerFreePageRun *nn = (void *)((u8 *)sm + next->next_page * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE);
			nn->prev_page = new_free_page;
		}

		zero_struct(pr);

		BeamformerSharedMemoryAllocation *ma = (void *)pr;
		ma->size       = info.size * info.count;
		ma->align      = info.align;
		ma->page_count = page_count;

		result = beamformer_shared_memory_make_handle(BeamformerSharedMemoryHandleKind_Memory,
		                                              new_free_page - page_count);
	}
	atomic_store_u64(&sm->transient.lock, 0);

	return result;
}

function void
beamformer_shared_memory_allocation_release(BeamformerSharedMemory *sm, BeamformerSharedMemoryAllocationHandle handle)
{
	assert(beamformer_shared_memory_handle_kind(handle) == BeamformerSharedMemoryHandleKind_Memory);
	u64 page = beamformer_shared_memory_handle_payload(handle);
	BeamformerSharedMemoryAllocation *ma = (void *)((u8 *)sm + page * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE);
	sm->transient.first_free_page = beamformer_shared_memory_release_pages(sm, sm->transient.first_free_page,
	                                                                       page, ma->page_count,
	                                                                       BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE);
}

function void *
beamformer_shared_memory_allocation_pointer(BeamformerSharedMemory *sm, BeamformerSharedMemoryAllocationHandle handle)
{
	assert(beamformer_shared_memory_handle_kind(handle) == BeamformerSharedMemoryHandleKind_Memory);
	u64 page = beamformer_shared_memory_handle_payload(handle);
	BeamformerSharedMemoryAllocation *ma = (void *)((u8 *)sm + page * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE);
	void *result = (void *)AlignUpPowerOfTwo((u64)(ma + 1), (u64)ma->align);
	return result;
}

function u64
beamformer_shared_memory_allocation_size(BeamformerSharedMemory *sm, BeamformerSharedMemoryAllocationHandle handle)
{
	assert(beamformer_shared_memory_handle_kind(handle) == BeamformerSharedMemoryHandleKind_Memory);
	u64 page = beamformer_shared_memory_handle_payload(handle);

	BeamformerSharedMemoryAllocation *ma = (void *)((u8 *)sm + page * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE);
	u64 result = ma->size;
	return result;
}
