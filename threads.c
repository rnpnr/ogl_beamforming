/* See LICENSE for license details. */
thread_static ThreadContext *thread_context_local = 0;

#define lane_context(ctx)             thread_context_select((ctx))
#define lane_index()                 (thread_context_local->lane_context.index)
#define lane_count()                 (thread_context_local->lane_context.count)
#define lane_sync()                   thread_context_barrier_wait(0, 0, 0)
#define lane_sync_u64(ptr, src_lane)  thread_context_barrier_wait((ptr), sizeof(*(ptr)), (src_lane))
#define lane_range(count)             subrange_n_from_n_m_count(lane_index(), lane_count(), (count))

function void
thread_context_select(ThreadContext *tctx)
{
	thread_context_local = tctx;
}

function void
thread_context_barrier_wait(void *broadcast, u64 broadcast_size, u64 broadcast_lane_index)
{
	ThreadContext *ctx = thread_context_local;
	u64 broadcast_size_clamped = MIN(broadcast_size, sizeof(ctx->lane_context.broadcast_memory[0]));
	if (broadcast && lane_index() == broadcast_lane_index)
		mem_copy(ctx->lane_context.broadcast_memory, broadcast, broadcast_size_clamped);

	os_barrier_enter(ctx->lane_context.barrier);

	if (broadcast && lane_index() != broadcast_lane_index)
		mem_copy(broadcast, ctx->lane_context.broadcast_memory, broadcast_size_clamped);

	if (broadcast)
		os_barrier_enter(ctx->lane_context.barrier);
}
