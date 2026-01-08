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

function void
dispatch_file_watch(OSW32_FileWatchDirectory *fw_dir, Arena arena)
{
	TempArena save_point = {0};
	i64       offset     = 0;

	w32_file_notify_info *fni = (w32_file_notify_info *)fw_dir->buffer;
	do {
		end_temp_arena(save_point);
		save_point = begin_temp_arena(&arena);

		Stream path = {.data = arena_commit(&arena, KB(1)), .cap = KB(1)};

		if (fni->action != FILE_ACTION_MODIFIED) {
			stream_append_s8(&path, s8("unknown file watch event: "));
			stream_append_u64(&path, fni->action);
			stream_append_byte(&path, '\n');
			os_write_file(os_w32_context.error_handle, stream_to_s8(&path));
			stream_reset(&path, 0);
		}

		stream_append_s8(&path, fw_dir->name);
		stream_append_byte(&path, '\\');

		s8 file_name = s16_to_s8(&arena, (s16){.data = fni->filename,
		                                       .len  = fni->filename_size / 2});
		stream_append_s8(&path, file_name);
		stream_append_byte(&path, 0);
		stream_commit(&path, -1);

		u64 hash = u64_hash_from_s8(file_name);
		for (u32 i = 0; i < fw_dir->count; i++) {
			FileWatch *fw = fw_dir->data + i;
			if (fw->hash == hash) {
				fw->callback(stream_to_s8(&path), fw->user_data, arena);
				break;
			}
		}

		offset = fni->next_entry_offset;
		fni    = (w32_file_notify_info *)((u8 *)fni + offset);
	} while (offset);
}

function void
clear_io_queue(BeamformerInput *input, Arena arena)
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
			dispatch_file_watch(dir, arena);
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
	input->executable_reloaded = 1;
	beamformer_init(input);

	u64 last_time = os_get_timer_counter();
	while (!WindowShouldClose()) {
		DeferLoop(os_take_lock(&os_w32_context.arena_lock, -1),
		          os_release_lock(&os_w32_context.arena_lock))
		{
			clear_io_queue(input, os_w32_context.arena);
		}

		Vector2 new_mouse = GetMousePosition();
		u64 now = os_get_timer_counter();
		input->last_mouse_x = input->mouse_x;
		input->last_mouse_y = input->mouse_y;
		input->mouse_x      = new_mouse.x;
		input->mouse_y      = new_mouse.y;
		input->timer_ticks  = now - last_time;
		last_time           = now;

		beamformer_frame_step(input);

		input->executable_reloaded = 0;
	}

	beamformer_invalidate_shared_memory(program_memory.beg);
	beamformer_debug_ui_deinit(program_memory.beg);
}
