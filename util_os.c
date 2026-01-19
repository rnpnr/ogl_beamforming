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

#if BEAMFORMER_RENDERDOC_HOOKS
function void
load_renderdoc_functions(BeamformerInput *input, OSLibrary rdoc)
{
	if ValidHandle(rdoc) {
		renderdoc_get_api_fn *get_api = os_lookup_symbol(rdoc, "RENDERDOC_GetAPI");
		if (get_api) {
			RenderDocAPI *api = 0;
			if (get_api(10600, (void **)&api)) {
				input->renderdoc_start_frame_capture            = RENDERDOC_START_FRAME_CAPTURE(api);
				input->renderdoc_end_frame_capture              = RENDERDOC_END_FRAME_CAPTURE(api);
				input->renderdoc_set_capture_file_path_template = RENDERDOC_SET_CAPTURE_PATH_TEMPLATE(api);
			}
		}
	}
}
#endif
