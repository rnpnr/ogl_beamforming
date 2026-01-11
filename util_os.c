/* See LICENSE for license details. */

// NOTE(rnp): functions which require platform layer support but
// otherwise share implementation

function b32
take_lock(i32 *lock, i32 timeout_ms)
{
	b32 result = 0;
	for (;;) {
		i32 current = 0;
		if (atomic_cas_u32(lock, &current, 1))
			result = 1;
		if (result || !timeout_ms || (!os_wait_on_address(lock, current, (u32)timeout_ms) && timeout_ms != -1))
			break;
	}
	return result;
}

function void
release_lock(i32 *lock)
{
	assert(atomic_load_u32(lock));
	atomic_store_u32(lock, 0);
	os_wake_all_waiters(lock);
}
