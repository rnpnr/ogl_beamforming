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
 *   This requires compiling `beamformer_core.c` as a dynamic library which the
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
#define OSInvalidHandleValue ((u64)-1)
typedef struct { uint64_t value[1]; } OSBarrier;
typedef struct { uint64_t value[1]; } OSHandle;
typedef struct { uint64_t value[1]; } OSLibrary;
typedef struct { uint64_t value[1]; } OSThread;
typedef struct { uint64_t value[1]; } OSW32Semaphore;

typedef uint64_t os_thread_entry_point_fn(void *user_context);

typedef struct {
	uint64_t timer_frequency;

	uint32_t logical_processor_count;
	uint32_t page_size;

	uint8_t  path_separator_byte;
} OSSystemInfo;

BEAMFORMER_IMPORT OSSystemInfo * os_system_info(void);

BEAMFORMER_IMPORT OSThread       os_create_thread(const char *name, void *user_context, os_thread_entry_point_fn *fn);
BEAMFORMER_IMPORT OSBarrier      os_barrier_alloc(uint32_t thread_count);
BEAMFORMER_IMPORT void           os_barrier_enter(OSBarrier);

/* NOTE(rnp): since the beamformer may spawn threads, which may need to keep time,
 * passing in a single timer value with the rest of the input is insufficient. */
BEAMFORMER_IMPORT uint64_t       os_timer_count(void);

BEAMFORMER_IMPORT void           os_add_file_watch(const char *path, int64_t path_length, void *user_context);
BEAMFORMER_IMPORT int64_t        os_read_entire_file(const char *file, void *buffer, int64_t buffer_capacity);

BEAMFORMER_IMPORT void *         os_lookup_symbol(OSLibrary library, const char *symbol);

/* NOTE(rnp): memory watch timed waiting functions. (-1) is an infinite timeout. the beamformer
 * will use these with the intention of yielding the thread back to the OS. */
BEAMFORMER_IMPORT uint32_t       os_wait_on_address(int32_t *lock, int32_t current, uint32_t timeout_ms);
BEAMFORMER_IMPORT void           os_wake_all_waiters(int32_t *lock);

// NOTE(rnp): eventually logging will just be done internally
BEAMFORMER_IMPORT void           os_console_log(uint8_t *data, int64_t length);
BEAMFORMER_IMPORT void           os_fatal(uint8_t *data, int64_t length);


// NOTE(rnp): for vulkan cross API export on win32 (will be removed eventually)
BEAMFORMER_IMPORT void           os_release_handle(OSHandle handle);

/* NOTE(rnp): this functionality is only needed on win32 to provide cross process
 * synchronization. While posix has equivalent functionality there is no reason to
 * use it over a value located in shared memory. */
#if defined(_WIN32)
BEAMFORMER_IMPORT OSW32Semaphore os_w32_create_semaphore(const char *name, int32_t initial_count, int32_t maximum_count);
BEAMFORMER_IMPORT uint32_t       os_w32_semaphore_wait(OSW32Semaphore, uint32_t timeout_ms);
BEAMFORMER_IMPORT void           os_w32_semaphore_release(OSW32Semaphore, int32_t count);
#endif

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
	/* NOTE(rnp): besides vulkan library code the beamformer will not allocate memory on its
	 * own. Recommended minimum size is 16MB. If shared memory is not provided it is recommended
	 * to increase this to at least 1GB to help facilitate loading of external data files (not yet
	 * implemented). */
	void *      memory;
	uint64_t    memory_size;

	/* NOTE(rnp): beamformer will use this to communicate with external processes. While it
	 * it won't be required in the future it is currently the only way to load data.
	 * Recommended size is 2-4GB. Currently this size will also limit the size of any data
	 * another process wishes to export. The name is required for listing in the UI so that
	 * users of external processes can open the region on their end. */
	void *      shared_memory;
	uint64_t    shared_memory_size;
	uint8_t *   shared_memory_name;
	uint32_t    shared_memory_name_length;

	float       mouse_x;
	float       mouse_y;
	float       last_mouse_x;
	float       last_mouse_y;

	uint32_t    event_count;

	BeamformerInputEvent event_queue[256];

	/* NOTE(rnp): the beamformer is not allowed to dynamically load libraries
	 * itself. Besides Vulkan, which is required, libraries are optional and
	 * the beamformer will not use features from libraries which have not
	 * been provided. */
	OSLibrary cuda_library_handle;
	OSLibrary vulkan_library_handle;

	#if BEAMFORMER_RENDERDOC_HOOKS
	void *renderdoc_start_frame_capture;
	void *renderdoc_end_frame_capture;
	void *renderdoc_set_capture_file_path_template;
	#endif
} BeamformerInput;

BEAMFORMER_EXPORT void beamformer_init(BeamformerInput *);

/* NOTE(rnp): while the platform can also decide to terminate the beamformer,
 * the beamformer itself may indicate that it wants to terminate. If the
 * beamformer itself decides to terminate it is unnecessary to call
 * `beamformer_terminate()` but it will act as a NOP if you do. */
BEAMFORMER_EXPORT uint32_t beamformer_should_close(BeamformerInput *);

/* IMPORTANT(rnp): since the beamformer may be interacting with external hardware
 * it is critical that the platform calls this when it wishes to terminate the
 * beamformer. Otherwise the external hardware may be left in a bad state and require
 * a reboot. The beamformer will not waste time releasing resources unless it was
 * compiled with BEAMFORMER_DEBUG enabled (useful for address sanitizer). */
BEAMFORMER_EXPORT void beamformer_terminate(BeamformerInput *);

#if !BEAMFORMER_DEBUG
BEAMFORMER_EXPORT void beamformer_frame_step(BeamformerInput *);
#endif

#if BEAMFORMER_DEBUG
BEAMFORMER_EXPORT void beamformer_debug_hot_reload(OSLibrary new_library, BeamformerInput *);
#endif

#endif /*BEAMFORMER_H */
