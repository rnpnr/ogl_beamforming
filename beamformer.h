/* See LICENSE for license details. */
#ifndef BEAMFORMER_H
#define BEAMFORMER_H

///////////////////
// REQUIRED OS API
function void os_barrier_wait(Barrier);
function iptr os_error_handle(void);
function s8   os_path_separator(void);
function OS_READ_WHOLE_FILE_FN(os_read_whole_file);
function OS_SHARED_MEMORY_LOCK_REGION_FN(os_shared_memory_region_lock);
function OS_SHARED_MEMORY_UNLOCK_REGION_FN(os_shared_memory_region_unlock);
function OS_WAKE_WAITERS_FN(os_wake_waiters);
function OS_WRITE_FILE_FN(os_write_file);

#define BEAMFORMER_NAME_STRING "OGL Beamformer"

typedef struct {
	Arena memory;
	f64   dt;
	v2    mouse;
	v2    last_mouse;
	b32   executable_reloaded;
} BeamformerInput;

#define BEAMFORMER_FRAME_STEP_FN(name) void name(BeamformerInput *input)
typedef BEAMFORMER_FRAME_STEP_FN(beamformer_frame_step_fn);

#endif /*BEAMFORMER_H */
