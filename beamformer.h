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
 *   `beamformer_debug_hot_release` with the program's memory, then it may close the
 *   old handle. Then `beamformer_debug_hot_reload` should be called with the new handle
 *   so that the beamformer may resume operation.
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
typedef struct { uint64_t value[1]; } OSWindow;
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

// NOTE(rnp): currently beamformer will only create one window.
// once raylib is removed it may request multiple
BEAMFORMER_IMPORT OSWindow       os_window_create(uint8_t *title, int64_t title_length, int32_t width, int32_t height);
//BEAMFORMER_IMPORT void           os_window_title(OSWindow window, uint8_t *title, int64_t title_length);
//BEAMFORMER_IMPORT void           os_window_destroy(OSWindow window);

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
	BeamformerInputEventKind_MouseScroll,
	BeamformerInputEventKind_WindowResize,
	BeamformerInputEventKind_ExecutableReload,
	BeamformerInputEventKind_FileEvent,
} BeamformerInputEventKind;

typedef enum {
	BeamformerButtonID_Space        = ' ',
	BeamformerButtonID_Apostrophe   = '\'',
	BeamformerButtonID_Comma        = ',',
	BeamformerButtonID_Minus        = '-',
	BeamformerButtonID_Period       = '.',
	BeamformerButtonID_Slash        = '/',
	BeamformerButtonID_0            = '0',
	BeamformerButtonID_1            = '1',
	BeamformerButtonID_2            = '2',
	BeamformerButtonID_3            = '3',
	BeamformerButtonID_4            = '4',
	BeamformerButtonID_5            = '5',
	BeamformerButtonID_6            = '6',
	BeamformerButtonID_7            = '7',
	BeamformerButtonID_8            = '8',
	BeamformerButtonID_9            = '9',
	BeamformerButtonID_Semicolon    = ';',
	BeamformerButtonID_Equal        = '=',
	BeamformerButtonID_A            = 'A',
	BeamformerButtonID_B            = 'B',
	BeamformerButtonID_C            = 'C',
	BeamformerButtonID_D            = 'D',
	BeamformerButtonID_E            = 'E',
	BeamformerButtonID_F            = 'F',
	BeamformerButtonID_G            = 'G',
	BeamformerButtonID_H            = 'H',
	BeamformerButtonID_I            = 'I',
	BeamformerButtonID_J            = 'J',
	BeamformerButtonID_K            = 'K',
	BeamformerButtonID_L            = 'L',
	BeamformerButtonID_M            = 'M',
	BeamformerButtonID_N            = 'N',
	BeamformerButtonID_O            = 'O',
	BeamformerButtonID_P            = 'P',
	BeamformerButtonID_Q            = 'Q',
	BeamformerButtonID_R            = 'R',
	BeamformerButtonID_S            = 'S',
	BeamformerButtonID_T            = 'T',
	BeamformerButtonID_U            = 'U',
	BeamformerButtonID_V            = 'V',
	BeamformerButtonID_W            = 'W',
	BeamformerButtonID_X            = 'X',
	BeamformerButtonID_Y            = 'Y',
	BeamformerButtonID_Z            = 'Z',
	BeamformerButtonID_LeftBracket  = '[',
	BeamformerButtonID_Backslash    = '\\',
	BeamformerButtonID_RightBracket = ']',
	BeamformerButtonID_Grave        = '`',

	BeamformerButtonID_Escape,
	BeamformerButtonID_Enter,
	BeamformerButtonID_Tab,
	BeamformerButtonID_Backspace,
	BeamformerButtonID_Insert,
	BeamformerButtonID_Delete,
	BeamformerButtonID_Right,
	BeamformerButtonID_Left,
	BeamformerButtonID_Down,
	BeamformerButtonID_Up,
	BeamformerButtonID_PageUp,
	BeamformerButtonID_PageDown,
	BeamformerButtonID_Home,
	BeamformerButtonID_End,
	BeamformerButtonID_CapsLock,
	BeamformerButtonID_ScrollLock,
	BeamformerButtonID_NumLock,
	BeamformerButtonID_PrintScreen,
	BeamformerButtonID_Pause,
	BeamformerButtonID_F1,
	BeamformerButtonID_F2,
	BeamformerButtonID_F3,
	BeamformerButtonID_F4,
	BeamformerButtonID_F5,
	BeamformerButtonID_F6,
	BeamformerButtonID_F7,
	BeamformerButtonID_F8,
	BeamformerButtonID_F9,
	BeamformerButtonID_F10,
	BeamformerButtonID_F11,
	BeamformerButtonID_F12,
	BeamformerButtonID_F13,
	BeamformerButtonID_F14,
	BeamformerButtonID_F15,
	BeamformerButtonID_F16,
	BeamformerButtonID_F17,
	BeamformerButtonID_F18,
	BeamformerButtonID_F19,
	BeamformerButtonID_F20,
	BeamformerButtonID_F21,
	BeamformerButtonID_F22,
	BeamformerButtonID_F23,
	BeamformerButtonID_F24,
	BeamformerButtonID_F25,
	BeamformerButtonID_KP0,
	BeamformerButtonID_KP1,
	BeamformerButtonID_KP2,
	BeamformerButtonID_KP3,
	BeamformerButtonID_KP4,
	BeamformerButtonID_KP5,
	BeamformerButtonID_KP6,
	BeamformerButtonID_KP7,
	BeamformerButtonID_KP8,
	BeamformerButtonID_KP9,
	BeamformerButtonID_KPDecimal,
	BeamformerButtonID_KPDivide,
	BeamformerButtonID_KPMultiply,
	BeamformerButtonID_KPSubtract,
	BeamformerButtonID_KPAdd,
	BeamformerButtonID_KPEnter,
	BeamformerButtonID_KPEqual,

	BeamformerButtonID_LeftShift,
	BeamformerButtonID_LeftControl,
	BeamformerButtonID_LeftAlt,
	BeamformerButtonID_LeftSuper,
	BeamformerButtonID_RightShift,
	BeamformerButtonID_RightControl,
	BeamformerButtonID_RightAlt,
	BeamformerButtonID_RightSuper,
	BeamformerButtonID_ModifierFirst = BeamformerButtonID_LeftShift,
	BeamformerButtonID_ModifierLast  = BeamformerButtonID_RightSuper,

	BeamformerButtonID_Menu,

	BeamformerButtonID_MouseLeft,
	BeamformerButtonID_MouseRight,
	BeamformerButtonID_MouseMiddle,

	BeamformerButtonID_Count,
} BeamformerButtonID;

typedef enum {
	BeamformerInputModifier_LeftAlt      = (1 << 0),
	BeamformerInputModifier_RightAlt     = (1 << 1),

	BeamformerInputModifier_LeftControl  = (1 << 2),
	BeamformerInputModifier_RightControl = (1 << 3),

	BeamformerInputModifier_LeftShift    = (1 << 4),
	BeamformerInputModifier_RightShift   = (1 << 5),

	BeamformerInputModifier_LeftMeta     = (1 << 6),
	BeamformerInputModifier_RightMeta    = (1 << 7),

	BeamformerInputModifier_Alt     = BeamformerInputModifier_LeftAlt|BeamformerInputModifier_RightAlt,
	BeamformerInputModifier_Control = BeamformerInputModifier_LeftControl|BeamformerInputModifier_RightControl,
	BeamformerInputModifier_Shift   = BeamformerInputModifier_LeftShift|BeamformerInputModifier_RightShift,
	BeamformerInputModifier_Meta    = BeamformerInputModifier_LeftMeta|BeamformerInputModifier_RightMeta,
	BeamformerInputModifier_Any     = BeamformerInputModifier_Alt|
	                                  BeamformerInputModifier_Control|
	                                  BeamformerInputModifier_Shift|
	                                  BeamformerInputModifier_Meta,
} BeamformerInputModifiers;

typedef struct {
	BeamformerInputEventKind kind;
	BeamformerInputModifiers modifiers;
	union {
		struct {
			BeamformerButtonID button_id;
			// NOTE(rnp): if the button is not also an input key codepoint should be 0
			uint32_t           codepoint;
		};
		struct {float x, y;} scroll;

		struct {
			uint32_t width, height;
			OSWindow window;
		} window_resize;

		void *file_watch_user_context;
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
BEAMFORMER_EXPORT void beamformer_debug_hot_release(BeamformerInput *);
BEAMFORMER_EXPORT void beamformer_debug_hot_reload(OSLibrary new_library, BeamformerInput *);
#endif

#endif /*BEAMFORMER_H */
