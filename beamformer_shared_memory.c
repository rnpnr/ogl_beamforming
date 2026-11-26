/* See LICENSE for license details. */
#define BEAMFORMER_SHARED_MEMORY_VERSION (39UL)

#define BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE KB(32)
#define BEAMFORMER_SHARED_MEMORY_PARAMETER_PAGE_SIZE KB(1)

#define BEAMFORMER_RF_ALIGN 256

#define BEAMFORMER_HANDLE(kind) typedef struct {uint64_t v;} kind;

BEAMFORMER_HANDLE(BeamformerSharedMemoryAllocationHandle);

#ifndef BEAMFORMER_LIB_SHARED_MEMORY_INCLUDE
BEAMFORMER_HANDLE(BeamformerParameterSetHandle);
#endif

typedef struct {
	u32 generation;

	u32 first_page;
	u32 page_count;

	b32 release_requested;

	BeamformerDataDescriptor data_description;

	// NOTE(rnp): core determines GPU parameter set needs uploading based on internal
	// generation not matching passed in handle generation. In that case this field
	// will be non zero. Modifications to this field must use atomic_swap. If the reader
	// gets a non-zero handle they are responsible for releasing it.
	BeamformerSharedMemoryAllocationHandle transient_handle;
} BeamformerParameterSetMetadata;

typedef enum {
	BeamformerSharedMemoryHandleKind_Nil,
	BeamformerSharedMemoryHandleKind_Memory,
	BeamformerSharedMemoryHandleKind_ParameterSet,
	BeamformerSharedMemoryHandleKind_Count,
} BeamformerSharedMemoryHandleKind;

typedef struct {
	u32 page_count;
	u32 prev_page;
	u32 next_page;
} BeamformerFreePageRun;

typedef struct {
	u64 size;
	u32 align;
	u32 page_count;
} BeamformerMemoryAllocation;

// NOTE(rnp): to facilitate use in shared memory the base address for
// memory managed by allocator is stored out of band.
typedef struct {
	u32 first_free_page;
	u32 page_count;
	u64 offset;
	u64 lock;
} BeamformerPageAllocatorContext;

typedef enum {
	BeamformerWorkKind_Compute,
	BeamformerWorkKind_WaitThenCompute,
	BeamformerWorkKind_CreateFilter,
	BeamformerWorkKind_ExportBuffer,
} BeamformerWorkKind;

typedef struct {
	BeamformerFilterParameters   parameters;
	BeamformerParameterSetHandle set_handle;
	u8 filter_slot;
	static_assert(BeamformerFilterSlots <= 255, "");
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
	BeamformerPushParameters     parameters;
	BeamformerParameterSetHandle set_handle;
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

typedef union {
	u8 filter_slot;
} BeamformerShaderParameters;

typedef struct {
	BeamformerShaderKind       shaders[BeamformerMaxComputeShaderStages];
	BeamformerShaderParameters parameters[BeamformerMaxComputeShaderStages];
	u32                        shader_count;
} BeamformerComputePipeline;

typedef struct {
	alignas(16) BeamformerParameters parameters;

	BeamformerComputePipeline pipeline;

	alignas(16) i16 channel_mapping[BeamformerMaxChannelCount];
	alignas(16) i16 sparse_elements[BeamformerMaxChannelCount];
	alignas(16) u8  transmit_receive_orientations[BeamformerMaxChannelCount];
	/* NOTE(rnp): interleaved transmit angle, focal depth pairs */
	alignas(16) v2  focal_vectors[BeamformerMaxChannelCount];
} BeamformerParameterBlock;
static_assert(sizeof(BeamformerParameterBlock) % alignof(BeamformerParameterBlock) == 0,
              "sizeof(BeamformerParametersBlock) must be a multiple of its alignment");

typedef struct {
	u32 version;

	/* NOTE(rnp): causes future library calls to fail.
	 * see note in beamformer_invalidate_shared_memory() */
	b32 invalid;

	/* NOTE(rnp): not used for locking on w32 but we can use these to peek at the status of
	 * the lock without leaving userspace. */
	i32 locks[BeamformerSharedMemoryLockKind_Count];

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

	BeamformerPageAllocatorContext transient;

	// NOTE(rnp): persistent storage is allocated in reverse to
	// avoid extra fragmentation. When the persistent area claims
	// pages it does so from the end of the transient area. The
	// free page list is still navigated in the forward direction.
	BeamformerPageAllocatorContext persistent;

	u64 parameter_sets_used_masks[DivRoundUp(BeamformerMaxParameterSets, 64)];

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
beamformer_shared_memory_release_pages(u8 *base, u32 first_free_page, u32 page, u32 count, u32 page_size)
{
	u32 current = first_free_page;
	BeamformerFreePageRun *pr = (void *)(base + page_size * current);
	while (current < page && page > pr->next_page) {
		current = pr->next_page;
		pr = (void *)(base + page_size * current);
	}

	u32 result = first_free_page;

	BeamformerFreePageRun *to_free = (void *)(base + page_size * page);
	zero_struct(to_free);

	if (current > page) {
		assert(current == first_free_page);

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
		BeamformerFreePageRun *next = pr->next_page != -1u ? (void *)(base + page_size * pr->next_page) : 0;
		if (current + pr->page_count == page) {
			pr->page_count += count;
			if (current + pr->page_count == pr->next_page) {
				pr->next_page   = next->next_page;
				pr->page_count += next->page_count;
				if (next->next_page != -1u) {
					next = (void *)(base + page_size * next->next_page);
					next->prev_page = current;
				}
			}
		} else {
			if (next) next->prev_page = page;
			to_free->next_page  = pr->next_page;
			to_free->page_count = count;
			to_free->prev_page  = current;
			pr->next_page       = page;
		}
	}
	return result;
}

function u64
beamformer_shared_memory_make_handle(BeamformerSharedMemoryHandleKind kind, u64 payload)
{
	static_assert(BeamformerSharedMemoryHandleKind_Count <= U16_MAX, "");
	u64 result = ((u64)kind << 48ul) | (payload & 0x0000fffffffffffful);
	return result;
}

function BeamformerSharedMemoryHandleKind
beamformer_shared_memory_handle_kind(u64 handle)
{
	BeamformerSharedMemoryHandleKind result = (BeamformerSharedMemoryHandleKind)(handle >> 48ul);
	return result;
}

function u64
beamformer_shared_memory_handle_payload(u64 handle)
{
	u64 result = handle & 0x0000fffffffffffful;
	return result;
}

function u32
beamformer_parameter_set_index_from_handle(BeamformerSharedMemory *sm, BeamformerParameterSetHandle handle, b32 check_generation)
{
	// NOTE(rnp): beamformer internally doesn't care about the generation. It is used
	// to check for validity in the library code. Furthermore as long as the index is
	// valid (internally it always will be) the library changing the generation, after
	// the previous generation was passed to the beamformer, should not result in the
	// the beamformer getting an index of -1.
	u32 result = -1u;
	if (handle.v && beamformer_shared_memory_handle_kind(handle.v) == BeamformerSharedMemoryHandleKind_ParameterSet) {
		u64 payload    = beamformer_shared_memory_handle_payload(handle.v);
		u32 set_index  = payload >> 32u;
		u32 generation = payload & 0xfffffffful;
		if (set_index < countof(sm->parameter_sets) && (!check_generation || sm->parameter_sets[set_index].generation == generation))
			result = set_index;
	}
	return result;
}

function BeamformerParameterSetMetadata *
beamformer_parameter_set_metadata_from_handle(BeamformerSharedMemory *sm, BeamformerParameterSetHandle handle)
{
	u32 set_index = beamformer_parameter_set_index_from_handle(sm, handle, 1);
	BeamformerParameterSetMetadata *result = 0;
	if (set_index != -1u)
		result = sm->parameter_sets + set_index;
	return result;
}

function b32
beamformer_parameter_set_dirty(BeamformerSharedMemory *sm, BeamformerParameterSetHandle handle)
{
	BeamformerParameterSetMetadata *set_meta = beamformer_parameter_set_metadata_from_handle(sm, handle);
	b32 result = set_meta && set_meta->transient_handle.v != 0;
	return result;
}

function void *
beamformer_shared_memory_allocation_pointer(BeamformerSharedMemory *sm, BeamformerSharedMemoryAllocationHandle handle)
{
	assert(beamformer_shared_memory_handle_kind(handle.v) == BeamformerSharedMemoryHandleKind_Memory);
	u64 page = beamformer_shared_memory_handle_payload(handle.v);
	u8 *base = (u8 *)sm + sm->transient.offset;
	BeamformerMemoryAllocation *ma = (void *)(base + page * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE);
	void *result = (void *)AlignUpPowerOfTwo((u64)(ma + 1), (u64)ma->align);
	return result;
}

function u64
beamformer_shared_memory_allocation_size(BeamformerSharedMemory *sm, BeamformerSharedMemoryAllocationHandle handle)
{
	assert(beamformer_shared_memory_handle_kind(handle.v) == BeamformerSharedMemoryHandleKind_Memory);
	u64 page = beamformer_shared_memory_handle_payload(handle.v);

	u8 *base = (u8 *)sm + sm->transient.offset;
	BeamformerMemoryAllocation *ma = (void *)(base + page * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE);
	u64 result = ma->size;
	return result;
}

function u64
beamformer_parameter_set_persistent_size(u32 channel_count, u32 event_count)
{
	BeamformerParameterSet *ps = 0;
	u64 result = channel_count * sizeof(*ps->channel_mapping);
	// TODO(rnp): event remap table size
	if (0) {
		static_assert(BeamformerMaxAcquisitionCount <= U16_MAX, "");
		result += event_count * sizeof(u16);
	}
	return result;
}

function void
beamformer_shared_memory_allocation_release(BeamformerSharedMemory *sm, BeamformerSharedMemoryAllocationHandle handle)
{
	assert(beamformer_shared_memory_handle_kind(handle.v) == BeamformerSharedMemoryHandleKind_Memory);

	u64 expected = 0;
	spin_while_false(atomic_cas_u64(&sm->transient.lock, &expected, 1));
	{
		u64 page = beamformer_shared_memory_handle_payload(handle.v);
		u8 *base = (u8 *)sm + sm->transient.offset;
		BeamformerMemoryAllocation *ma = (void *)(base + page * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE);
		u32 next_free = beamformer_shared_memory_release_pages(base, sm->transient.first_free_page,
		                                                       page, ma->page_count,
		                                                       BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE);
		sm->transient.first_free_page = next_free;
	}
	atomic_store_u64(&sm->transient.lock, 0);
}

function BeamformerFreePageRun *
beamformer_page_allocator_find_free_page_run(BeamformerPageAllocatorContext *ctx, void *memory,
                                             u32 page_size, u32 page_count, b32 reverse)
{
	BeamformerFreePageRun *result = 0;

	u8 *base = (u8 *)memory + ctx->offset;
	BeamformerFreePageRun *pr = ctx->first_free_page != -1u ? (void *)(base + ctx->first_free_page * page_size) : 0;
	while (pr && pr->next_page != -1u) {
		if (!reverse && pr->page_count >= page_count)
			break;
		pr = (void *)(base + pr->next_page * page_size);
	}

	// NOTE(rnp): reverse allocations only allowed at very end of range
	if (pr && pr->page_count >= page_count && (!reverse || (reverse && pr->next_page == -1u)))
		result = pr;

	return result;
}

function BeamformerMemoryAllocation *
beamformer_page_allocate(BeamformerPageAllocatorContext *ctx, void *memory, u32 page_size,
                         b32 reverse, ArenaAllocateInfo info)
{
	u64 expected = 0;
	spin_while_false(atomic_cas_u64(&ctx->lock, &expected, 1));

	u64 needed_size = AlignUpPowerOfTwo(info.size * info.count + sizeof(BeamformerMemoryAllocation), info.align);
	u32 page_count  = DivRoundUp(needed_size, page_size);

	u8 *base = (u8 *)memory + ctx->offset;
	BeamformerMemoryAllocation *result = 0;
	BeamformerFreePageRun *pr = beamformer_page_allocator_find_free_page_run(ctx, memory, page_size, page_count, reverse);
	if (pr) {
		if (reverse) {
			pr->page_count--;
			result = (void *)((u8 *)pr + pr->page_count * page_size);
			if (pr->page_count == 0) {
				if (pr->next_page != -1u) {
					BeamformerFreePageRun *next= (void *)(base + pr->next_page * page_size);
					next->prev_page = pr->prev_page;
				}
				if (pr->prev_page != -1u) {
					BeamformerFreePageRun *prev = (void *)(base + pr->prev_page * page_size);
					prev->next_page = pr->next_page;
				}
			}
		}

		if (!reverse) {
			u64 new_free_page = (u64)((u8 *)pr - base) / page_size + page_count;

			BeamformerFreePageRun *next = (void *)(base + new_free_page * page_size);
			next->page_count = pr->page_count - page_count;
			next->prev_page  = pr->prev_page;
			next->next_page  = pr->next_page;

			if (next->prev_page != -1u) {
				BeamformerFreePageRun *prev = (void *)(base + next->prev_page * page_size);
				prev->next_page = new_free_page;
			}

			if (next->next_page != -1u) {
				BeamformerFreePageRun *nn = (void *)(base + next->next_page * page_size);
				nn->prev_page = new_free_page;
			}

			result = (void *)pr;

			ctx->first_free_page = new_free_page;
		}

		zero_struct(result);
		result->size       = info.size * info.count;
		result->align      = info.align;
		result->page_count = page_count;
	}

	atomic_store_u64(&ctx->lock, 0);

	return result;
}

function u32
beamformer_shared_memory_persistent_alloc(BeamformerSharedMemory *sm, u64 bytes)
{
	const u32 page_size = BEAMFORMER_SHARED_MEMORY_PARAMETER_PAGE_SIZE;

	ArenaAllocateInfo info = {.align = 1, .count = 1, .size = bytes};
	BeamformerMemoryAllocation *ma = beamformer_page_allocate(&sm->persistent, sm, page_size, 1, info);
	if unlikely(!ma) {
		ma = beamformer_page_allocate(&sm->transient, sm, BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE, 1, info);
		if (ma && sm->persistent.first_free_page != -1u) {
			static_assert((BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE % BEAMFORMER_SHARED_MEMORY_PARAMETER_PAGE_SIZE) == 0, "");
			BeamformerFreePageRun *pr = (void *)((u8 *)sm + sm->persistent.offset + page_size * sm->persistent.first_free_page);
			pr->page_count += ma->page_count * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE
			                  / BEAMFORMER_SHARED_MEMORY_PARAMETER_PAGE_SIZE;
		}

		if (ma) {
			u32 page_count = ma->page_count * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE
			                                / BEAMFORMER_SHARED_MEMORY_PARAMETER_PAGE_SIZE;

			BeamformerFreePageRun *new_pr = (void *)ma;
			zero_struct(new_pr);
			new_pr->prev_page  = -1u;
			new_pr->next_page  = -1u;
			new_pr->page_count = page_count;

			if (sm->persistent.first_free_page != -1u) {
				assert(sm->persistent.first_free_page == 0);
				BeamformerFreePageRun *pr = (void *)((u8 *)sm + sm->persistent.offset);
				new_pr->next_page   = pr->next_page;
				new_pr->page_count += pr->page_count;
			}
			sm->persistent.offset = (u64)((u8 *)new_pr - (u8 *)sm);
			sm->persistent.first_free_page  = 0;
			sm->persistent.page_count      += page_count;
		}

		ma = beamformer_page_allocate(&sm->persistent, sm, page_size, 1, info);
	}

	u32 result = -1u;
	if (ma) {
		u8 *end = (u8 *)sm + sm->persistent.offset + sm->persistent.page_count * page_size;
		result = (end - (u8 *)ma) / page_size - 1;
	}
	return result;
}
