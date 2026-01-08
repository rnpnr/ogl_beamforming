/* See LICENSE for license details. */
#ifndef BEAMFORMER_H
#define BEAMFORMER_H

#include <stdint.h>

#define BEAMFORMER_NAME_STRING "OGL Beamformer"

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

typedef struct {
	void *      memory;
	uint64_t    memory_size;

	uint64_t    timer_ticks;
	uint64_t    timer_frequency;

	float       mouse_x;
	float       mouse_y;
	float       last_mouse_x;
	float       last_mouse_y;

	uint32_t    executable_reloaded;
} BeamformerInput;

#define BEAMFORMER_FRAME_STEP_FN(name) void name(BeamformerInput *input)
typedef BEAMFORMER_FRAME_STEP_FN(beamformer_frame_step_fn);

#define BEAMFORMER_DEBUG_UI_DEINIT_FN(name) void name(void *memory)
typedef BEAMFORMER_DEBUG_UI_DEINIT_FN(beamformer_debug_ui_deinit_fn);

function void beamformer_invalidate_shared_memory(void *memory);

#endif /*BEAMFORMER_H */
