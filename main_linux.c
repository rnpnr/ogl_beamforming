/* See LICENSE for license details. */
#include "compiler.h"

#if !OS_LINUX
#error This file is only meant to be compiled for Linux
#endif

#ifndef BEAMFORMER_DEBUG
  #define BEAMFORMER_IMPORT function
  #define BEAMFORMER_EXPORT function
#endif

#include "util.h"
#include "beamformer.h"

#include "os_linux.c"

#define OS_DEBUG_LIB_NAME      "./beamformer.so"
#define OS_DEBUG_LIB_TEMP_NAME "./beamformer_temp.so"

#define OS_CUDA_LIB_NAME       "./external/cuda_toolkit.so"
#define OS_CUDA_LIB_TEMP_NAME  "./external/cuda_toolkit_temp.so"

#define OS_RENDERDOC_SONAME    "librenderdoc.so"

/* TODO(rnp): what do if not X11? */
iptr glfwGetGLXContext(iptr);
function iptr
os_get_native_gl_context(iptr window)
{
	return glfwGetGLXContext(window);
}

iptr glfwGetProcAddress(char *);
function iptr
os_gl_proc_address(char *name)
{
	return glfwGetProcAddress(name);
}

#include "beamformer.c"

#include <dlfcn.h>

BEAMFORMER_IMPORT BEAMFORMER_OS_ADD_FILE_WATCH_FN(os_add_file_watch)
{
	s8 path_str = {.data = (u8 *)path, .len = path_length};
	os_linux_add_file_watch(path_str, user_context, OSLinux_FileWatchKindUser);
}

BEAMFORMER_IMPORT BEAMFORMER_OS_LOOKUP_SYMBOL_FN(os_lookup_symbol)
{
	void *result = 0;
	if ValidHandle(library) {
		result = dlsym((void *)library.value[0], symbol);
		if (!result && error) {
			stream_append_s8s(error, s8("os_lookup_symbol(\""), c_str_to_s8(symbol), s8("\"): "),
			                  c_str_to_s8(dlerror()), s8("\n"));
		}
	}
	return result;
}

function BeamformerLibraryHandle
load_library(char *name, char *temp_name, u32 flags)
{
	if (temp_name && os_copy_file(name, temp_name))
		name = temp_name;

	BeamformerLibraryHandle result = {(u64)dlopen(name, flags)};
	if (result.value[0] == 0) result = BeamformerInvalidHandle;

	if (temp_name) unlink(temp_name);

	return result;
}

#if BEAMFORMER_DEBUG
function void
debug_library_reload(BeamformerInput *input)
{
	local_persist BeamformerLibraryHandle beamformer_library_handle = BeamformerInvalidHandle;
	BeamformerLibraryHandle new_handle = load_library(OS_DEBUG_LIB_NAME, OS_DEBUG_LIB_TEMP_NAME, RTLD_NOW|RTLD_LOCAL);

	if (!ValidHandle(beamformer_library_handle) && !ValidHandle(new_handle))
		os_fatal(s8("[os] failed to load: " OS_DEBUG_LIB_NAME "\n"));

	if ValidHandle(new_handle) {
		beamformer_debug_hot_reload(new_handle, input);

		if ValidHandle(beamformer_library_handle)
			dlclose((void *)beamformer_library_handle.value[0]);
		beamformer_library_handle = new_handle;
	}
}
#endif /* BEAMFORMER_DEBUG */

function void
load_platform_libraries(BeamformerInput *input)
{
	#if BEAMFORMER_DEBUG
		debug_library_reload(input);
		os_linux_add_file_watch(s8(OS_DEBUG_LIB_NAME), (void *)BeamformerInputEventKind_ExecutableReload,
		                        OSLinux_FileWatchKindPlatform);
	#endif

	input->cuda_library_handle = load_library(OS_CUDA_LIB_NAME, OS_CUDA_LIB_TEMP_NAME, RTLD_NOW|RTLD_LOCAL);

	#if BEAMFORMER_RENDERDOC_HOOKS
	local_persist BeamformerLibraryHandle renderdoc_handle = BeamformerInvalidHandle;
	renderdoc_handle = load_library(OS_RENDERDOC_SONAME, 0, RTLD_NOW|RTLD_LOCAL|RTLD_NOLOAD);
	if ValidHandle(renderdoc_handle) {
		renderdoc_get_api_fn *get_api = os_lookup_symbol(renderdoc_handle, "RENDERDOC_GetAPI", 0);
		if (get_api) {
			RenderDocAPI *api = 0;
			if (get_api(10600, (void **)&api)) {
				input->renderdoc_start_frame_capture = RENDERDOC_START_FRAME_CAPTURE(api);
				input->renderdoc_end_frame_capture   = RENDERDOC_END_FRAME_CAPTURE(api);
			}
		}
	}
	#endif
}

function void
dispatch_file_watch_events(BeamformerInput *input, u64 current_time)
{
	OSLinux_FileWatchDirectoryList *fwctx = &os_linux_context.file_watch_list;
	Arena arena = os_linux_context.arena;
	u8 *mem     = arena_alloc(&arena, .size = 4096, .align = 16);
	struct inotify_event *event;

	iz rlen;
	while ((rlen = read(os_linux_context.inotify_handle, mem, 4096)) > 0) {
		for (u8 *data = mem; data < mem + rlen; data += sizeof(*event) + event->len) {
			event = (struct inotify_event *)data;
			for (u32 i = 0; i < fwctx->count; i++) {
				OSLinux_FileWatchDirectory *dir = fwctx->data + i;
				if (event->wd != dir->handle)
					continue;

				s8  file = c_str_to_s8(event->name);
				u64 hash = u64_hash_from_s8(file);
				for (u32 j = 0; j < dir->count; j++) {
					OSLinux_FileWatch *fw = dir->data + j;
					if (fw->hash == hash) {
						// NOTE(rnp): avoid multiple updates in a single frame
						if (fw->update_time < current_time) {
							BeamformerInputEvent input_event = {0};
							if (fw->kind == OSLinux_FileWatchKindPlatform) {
								assert((u64)fw->user_context == BeamformerInputEventKind_ExecutableReload);
								#if BEAMFORMER_DEBUG
									if ((u64)fw->user_context == BeamformerInputEventKind_ExecutableReload)
										debug_library_reload(input);
								#endif
								input_event.kind = (u64)fw->user_context;
							} else {
								input_event.kind = BeamformerInputEventKind_FileEvent;
								input_event.file_watch_user_context = fw->user_context;
							}
							input->event_queue[input->event_count++] = input_event;
						}
						fw->update_time = current_time;
						break;
					}
				}
			}
		}
	}
}

extern i32
main(void)
{
	os_common_init();

	Arena program_memory = os_alloc_arena(MB(16) + KB(16));

	os_linux_context.arena = sub_arena(&program_memory, KB(16), KB(4));
	os_linux_context.inotify_handle = inotify_init1(IN_NONBLOCK|IN_CLOEXEC);


	BeamformerInput *input     = push_struct(&program_memory, BeamformerInput);
	input->memory              = program_memory.beg;
	input->memory_size         = program_memory.end - program_memory.beg;
	input->timer_frequency     = os_get_timer_frequency();
	input->event_queue[input->event_count++] = (BeamformerInputEvent){
		.kind = BeamformerInputEventKind_ExecutableReload,
	};

	load_platform_libraries(input);

	beamformer_init(input);

	struct pollfd fds[1] = {{0}};
	fds[0].fd     = os_linux_context.inotify_handle;
	fds[0].events = POLLIN;

	u64 last_time = os_get_timer_counter();
	while (!WindowShouldClose()) {
		u64 now = os_get_timer_counter();

		poll(fds, countof(fds), 0);
		if (fds[0].revents & POLLIN)
			dispatch_file_watch_events(input, now);

		Vector2 new_mouse = GetMousePosition();
		input->last_mouse_x = input->mouse_x;
		input->last_mouse_y = input->mouse_y;
		input->mouse_x      = new_mouse.x;
		input->mouse_y      = new_mouse.y;
		input->timer_ticks  = now - last_time;
		last_time           = now;

		beamformer_frame_step(input);

		input->event_count  = 0;
	}

	beamformer_invalidate_shared_memory(program_memory.beg);
	beamformer_debug_ui_deinit(program_memory.beg);

	/* NOTE: make sure this will get cleaned up after external
	 * programs release their references */
	shm_unlink(OS_SHARED_MEMORY_NAME);
}
