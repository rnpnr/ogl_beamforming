/* See LICENSE for license details. */
#ifndef BEAMFORMER_H
#define BEAMFORMER_H

#include <stdint.h>

#define BEAMFORMER_NAME_STRING "OGL Beamformer"

///////////////////////////////
// COMPILE TIME CONFIGURATION

/* NOTE(rnp): By design the beamformer has very little compile time configuration.
 * The few options it does have are documented here.
 *
 * BEAMFORMER_IMPORT
 * BEAMFORMER_EXPORT
 *   The symbol markup for imported and exported symbols. In a typical
 *   release unity build these are both defined to `static`.
 *
 * BEAMFORMER_DEBUG
 *   Compile the beamformer with handling for hot reloading at runtime.
 *   This requires compiling `beamformer.c` as a dynamic library which the
 *   platform is required to load at runtime.
 *   IMPORTANT: When the platform wants to reload the library at runtime it
 *   MUST NOT unload the old library immediately; the beamformer may still
 *   be executing code in old library. Instead the platform must first call
 *   `beamformer_debug_hot_reload` with the program's memory and the new library
 *   handle. Once that function has returned it is safe to close the old handle.
 *
 * BEAMFORMER_RENDERDOC_HOOKS
 *   Add RenderDoc API calls to capture complete compute frames. As compute is performed
 *   asynchronously from normal rendering it is not possible to capture normally. In this
 *   configuration the beamformer will use the function pointers provided in the
 *   BeamformerInput to make these calls.
 *   IMPORTANT: The renderdoc library will only be visible when the application is started
 *   through RenderDoc. Furthermore the library has startup code which will halt the program
 *   if loaded normally. It must be loaded using platform module loading APIs. For example
 *   GetModuleHandle or dlopen with the RTLD_NOLOAD flag set.
 *
 */

#ifndef BEAMFORMER_IMPORT
 #define BEAMFORMER_IMPORT
#endif

#ifndef BEAMFORMER_EXPORT
 #define BEAMFORMER_EXPORT
#endif

#ifdef BEAMFORMER_DEBUG
  #undef BEAMFORMER_DEBUG
  #define BEAMFORMER_DEBUG (1)
#else
  #define BEAMFORMER_DEBUG (0)
#endif

#ifdef BEAMFORMER_RENDERDOC_HOOKS
  #undef BEAMFORMER_RENDERDOC_HOOKS
  #define BEAMFORMER_RENDERDOC_HOOKS (1)
#else
  #define BEAMFORMER_RENDERDOC_HOOKS (0)
#endif

///////////////////
// REQUIRED OS API
#define BeamformerInvalidHandle (BeamformerLibraryHandle){-1}
typedef struct { uint64_t value[1]; } BeamformerLibraryHandle;

#define BEAMFORMER_OS_ADD_FILE_WATCH_FN(name) void name(char *path, int64_t path_length, void *user_context)
BEAMFORMER_IMPORT BEAMFORMER_OS_ADD_FILE_WATCH_FN(os_add_file_watch);

#define BEAMFORMER_OS_LOOKUP_SYMBOL_FN(name) void *name(BeamformerLibraryHandle library, char *symbol, Stream *error)
BEAMFORMER_IMPORT BEAMFORMER_OS_LOOKUP_SYMBOL_FN(os_lookup_symbol);

function void os_barrier_wait(Barrier);
function iptr os_error_handle(void);
function s8   os_path_separator(void);
function OS_READ_WHOLE_FILE_FN(os_read_whole_file);
function OS_SHARED_MEMORY_LOCK_REGION_FN(os_shared_memory_region_lock);
function OS_SHARED_MEMORY_UNLOCK_REGION_FN(os_shared_memory_region_unlock);
function OS_WAKE_WAITERS_FN(os_wake_waiters);
function OS_WRITE_FILE_FN(os_write_file);

//////////////////////////////
// BEAMFORMER APPLICATION API

typedef enum {
	BeamformerInputEventKind_ButtonPress,
	BeamformerInputEventKind_ButtonRelease,
	BeamformerInputEventKind_ExecutableReload,
	BeamformerInputEventKind_FileEvent,
} BeamformerInputEventKind;

// TODO: not yet used
typedef enum {
	BeamformerButtonID_MouseLeft,
	BeamformerButtonID_MouseRight,
	BeamformerButtonID_MouseMiddle,
	BeamformerButtonID_Count,
} BeamformerButtonID;

typedef struct {
	BeamformerInputEventKind kind;
	union {
		BeamformerButtonID  button_id;
		void *              file_watch_user_context;
	};
} BeamformerInputEvent;

typedef struct {
	void *      memory;
	uint64_t    memory_size;

	uint64_t    timer_ticks;
	uint64_t    timer_frequency;

	float       mouse_x;
	float       mouse_y;
	float       last_mouse_x;
	float       last_mouse_y;

	uint32_t    event_count;

	BeamformerInputEvent event_queue[256];

	/* NOTE(rnp): the beamformer is not allowed to dynamically load libraries
	 * itself. Libraries are optional and the beamformer will not use features
	 * from libraries which have not been provided. */
	BeamformerLibraryHandle cuda_library_handle;

	#if BEAMFORMER_RENDERDOC_HOOKS
	void *renderdoc_start_frame_capture;
	void *renderdoc_end_frame_capture;
	#endif
} BeamformerInput;

BEAMFORMER_EXPORT void beamformer_init(BeamformerInput *);

#define BEAMFORMER_FRAME_STEP_FN(name) void name(BeamformerInput *input)
typedef BEAMFORMER_FRAME_STEP_FN(beamformer_frame_step_fn);

#define BEAMFORMER_DEBUG_UI_DEINIT_FN(name) void name(void *memory)
typedef BEAMFORMER_DEBUG_UI_DEINIT_FN(beamformer_debug_ui_deinit_fn);

function void beamformer_invalidate_shared_memory(void *memory);

#if BEAMFORMER_DEBUG
BEAMFORMER_EXPORT void beamformer_debug_hot_reload(BeamformerLibraryHandle, BeamformerInput *);
#endif

#endif /*BEAMFORMER_H */
