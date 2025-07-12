/* See LICENSE for license details. */
#include "beamformer_work_queue.h"

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

DEBUG_EXPORT BEAMFORM_WORK_QUEUE_PUSH_FN(beamform_work_queue_push)
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

DEBUG_EXPORT BEAMFORM_WORK_QUEUE_PUSH_COMMIT_FN(beamform_work_queue_push_commit)
{
	atomic_add_u64(&q->queue, 1);
}

function void
post_sync_barrier(SharedMemoryRegion *sm, BeamformerSharedMemoryLockKind lock, i32 *locks,
                  os_shared_memory_region_unlock_fn *shared_memory_region_unlock)
{
	/* NOTE(rnp): debug: here it is not a bug to release the lock if it
	 * isn't held but elswhere it is */
	DEBUG_DECL(if (locks[lock])) {
		shared_memory_region_unlock(sm, locks, (i32)lock);
	}
}
