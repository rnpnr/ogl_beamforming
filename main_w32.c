/* See LICENSE for license details. */
#include "compiler.h"

// NOTE(rnp): for test compilations on linux (we don't use headers from windows) */
#if OS_LINUX
  #undef OS_WINDOWS
  #undef OS_LINUX
  #undef __declspec
  #undef __stdcall
  #define OS_WINDOWS 1
  #define OS_LINUX   0
  #define __declspec(x)
  #define __stdcall
#endif

#if !OS_WINDOWS
#error This file is only meant to be compiled for Win32
#endif

#ifndef BEAMFORMER_DEBUG
  #define BEAMFORMER_IMPORT function
  #define BEAMFORMER_EXPORT function
#endif

#include "util.h"
#include "beamformer.h"

#include "os_win32.c"

#define OS_DEBUG_LIB_NAME      ".\\beamformer.dll"
#define OS_DEBUG_LIB_TEMP_NAME ".\\beamformer_temp.dll"

#define OS_CUDA_LIB_NAME       "external\\cuda_toolkit.dll"
#define OS_CUDA_LIB_TEMP_NAME  "external\\cuda_toolkit_temp.dll"

#define OS_RENDERDOC_SONAME    "renderdoc.dll"

iptr glfwGetWGLContext(iptr);
function iptr
os_get_native_gl_context(iptr window)
{
	return glfwGetWGLContext(window);
}

function iptr
os_gl_proc_address(char *name)
{
	return wglGetProcAddress(name);
}

#include "static.c"

W32(b32)    FreeLibrary(u64);
W32(void *) GetModuleHandleA(c8 *);
W32(void *) GetProcAddress(u64, c8 *);
W32(void *) LoadLibraryA(c8 *);

BEAMFORMER_IMPORT BEAMFORMER_OS_ADD_FILE_WATCH_FN(os_add_file_watch)
{
	s8 path_str = {.data = (u8 *)path, .len = path_length};
	os_w32_add_file_watch(path_str, user_context, OSW32_FileWatchKindUser);
}

BEAMFORMER_IMPORT BEAMFORMER_OS_LOOKUP_SYMBOL_FN(os_lookup_symbol)
{
	void *result = 0;
	if ValidHandle(library) {
		result = GetProcAddress(library.value[0], symbol);
		if (!result && error) {
			stream_append_s8s(error, s8("os_lookup_symbol(\""), c_str_to_s8(symbol), s8("\"): "));
			stream_append_i64(error, GetLastError());
			stream_append_byte(error, '\n');
		}
	}
	return result;
}

#if BEAMFORMER_RENDERDOC_HOOKS
function BeamformerLibraryHandle
get_module(char *name)
{
	BeamformerLibraryHandle result = {(u64)GetModuleHandleA(name)};
	if (result.value[0] == 0) result = BeamformerInvalidHandle;
	return result;
}
#endif

function BeamformerLibraryHandle
load_library(char *name, char *temp_name)
{
	if (temp_name && os_copy_file(name, temp_name))
		name = temp_name;

	BeamformerLibraryHandle result = {(u64)LoadLibraryA(name)};
	if (result.value[0] == 0) result = BeamformerInvalidHandle;

	if (temp_name) DeleteFileA(temp_name);

	return result;
}

#if BEAMFORMER_DEBUG
function void
debug_library_reload(BeamformerInput *input)
{
	local_persist BeamformerLibraryHandle beamformer_library_handle = BeamformerInvalidHandle;
	BeamformerLibraryHandle new_handle = load_library(OS_DEBUG_LIB_NAME, OS_DEBUG_LIB_TEMP_NAME);

	if (!ValidHandle(beamformer_library_handle) && !ValidHandle(new_handle))
		os_fatal(s8("[os] failed to load: " OS_DEBUG_LIB_NAME "\n"));

	if ValidHandle(new_handle) {
		beamformer_debug_hot_reload(new_handle, input);

		if ValidHandle(beamformer_library_handle)
			FreeLibrary(beamformer_library_handle.value[0]);
		beamformer_library_handle = new_handle;
	}
}
#endif /* BEAMFORMER_DEBUG */

function void
load_platform_libraries(BeamformerInput *input)
{
	#if BEAMFORMER_DEBUG
		debug_library_reload(input);
		os_w32_add_file_watch(s8(OS_DEBUG_LIB_NAME), (void *)BeamformerInputEventKind_ExecutableReload,
		                      OSW32_FileWatchKindPlatform);
	#endif

	input->cuda_library_handle = load_library(OS_CUDA_LIB_NAME, OS_CUDA_LIB_TEMP_NAME);

	#if BEAMFORMER_RENDERDOC_HOOKS
	local_persist BeamformerLibraryHandle renderdoc_handle = BeamformerInvalidHandle;
	renderdoc_handle = get_module(OS_RENDERDOC_SONAME);
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
dispatch_file_watch(BeamformerInput *input, Arena arena, u64 current_time, OSW32_FileWatchDirectory *fw_dir)
{
	TempArena save_point = {0};
	i64       offset     = 0;

	w32_file_notify_info *fni = (w32_file_notify_info *)fw_dir->buffer;
	do {
		end_temp_arena(save_point);
		save_point = begin_temp_arena(&arena);

		Stream e = {.data = arena_commit(&arena, KB(1)), .cap = KB(1)};

		if (fni->action != FILE_ACTION_MODIFIED) {
			stream_append_s8(&e, s8("unknown file watch event: "));
			stream_append_u64(&e, fni->action);
			stream_append_byte(&e, '\n');
			os_write_file(os_w32_context.error_handle, stream_to_s8(&e));
			stream_reset(&e, 0);
		}

		s8 file_name = s16_to_s8(&arena, (s16){.data = fni->filename, .len  = fni->filename_size / 2});
		u64 hash = u64_hash_from_s8(file_name);
		for (u32 i = 0; i < fw_dir->count; i++) {
			OSW32_FileWatch *fw = fw_dir->data + i;
			if (fw->hash == hash) {
				// NOTE(rnp): avoid multiple updates in a single frame
				if (fw->update_time < current_time) {
					BeamformerInputEvent input_event = {0};
					if (fw->kind == OSW32_FileWatchKindPlatform) {
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

		offset = fni->next_entry_offset;
		fni    = (w32_file_notify_info *)((u8 *)fni + offset);
	} while (offset);
}

function void
clear_io_queue(BeamformerInput *input, Arena arena, u64 current_time)
{
	iptr handle = os_w32_context.io_completion_handle;
	w32_overlapped *overlapped;
	u32  bytes_read;
	uptr user_data;
	while (GetQueuedCompletionStatus(handle, &bytes_read, &user_data, &overlapped, 0)) {
		w32_io_completion_event *event = (w32_io_completion_event *)user_data;
		switch (event->tag) {
		case W32IOEvent_FileWatch:{
			OSW32_FileWatchDirectory *dir = (OSW32_FileWatchDirectory *)event->context;
			dispatch_file_watch(input, arena, current_time, dir);
			zero_struct(&dir->overlapped);
			ReadDirectoryChangesW(dir->handle, dir->buffer, OSW32_FileWatchDirectoryBufferSize, 0,
			                      FILE_NOTIFY_CHANGE_LAST_WRITE, 0, &dir->overlapped, 0);
		}break;
		}
	}
}

extern i32
main(void)
{
	os_common_init();

	Arena program_memory = os_alloc_arena(MB(16) + MB(2));

	os_w32_context.arena                = sub_arena(&program_memory, MB(2), KB(4));
	os_w32_context.error_handle         = GetStdHandle(STD_ERROR_HANDLE);
	os_w32_context.io_completion_handle = CreateIoCompletionPort(INVALID_FILE, 0, 0, 0);

	BeamformerInput *input     = push_struct(&program_memory, BeamformerInput);
	input->memory              = program_memory.beg;
	input->memory_size         = program_memory.end - program_memory.beg;
	input->timer_frequency     = os_w32_context.timer_frequency;
	input->event_queue[input->event_count++] = (BeamformerInputEvent){
		.kind = BeamformerInputEventKind_ExecutableReload,
	};

	load_platform_libraries(input);

	beamformer_init(input);

	u64 last_time = os_get_timer_counter();
	while (!WindowShouldClose()) {
		u64 now = os_get_timer_counter();

		DeferLoop(os_take_lock(&os_w32_context.arena_lock, -1),
		          os_release_lock(&os_w32_context.arena_lock))
		{
			clear_io_queue(input, os_w32_context.arena, now);
		}

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
}
