/* See LICENSE for license details. */
#include "compiler.h"

#if !OS_LINUX
#error This file is only meant to be compiled for Linux
#endif

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

#include "static.c"

function void
dispatch_file_watch_events(void)
{
	OSLinux_FileWatchDirectoryList *fwctx = &os_linux_context.file_watch_list;
	Arena arena = os_linux_context.arena;
	u8 *mem     = arena_alloc(&arena, 4096, 16, 1);
	Stream path = stream_alloc(&arena, 256);
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
					FileWatch *fw = dir->data + j;
					if (fw->hash == hash) {
						stream_append_s8s(&path, dir->name, s8("/"), file);
						stream_append_byte(&path, 0);
						stream_commit(&path, -1);
						fw->callback(stream_to_s8(&path), fw->user_data, arena);
						stream_reset(&path, 0);
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
	input->memory              = program_memory;
	input->executable_reloaded = 1;
	beamformer_init(input);

	struct pollfd fds[1] = {{0}};
	fds[0].fd     = os_linux_context.inotify_handle;
	fds[0].events = POLLIN;

	u64 last_time = os_get_timer_counter();
	while (!WindowShouldClose()) {
		poll(fds, countof(fds), 0);
		if (fds[0].revents & POLLIN)
			dispatch_file_watch_events();

		Vector2 new_mouse = GetMousePosition();
		u64 now = os_get_timer_counter();
		input->last_mouse = input->mouse;
		input->mouse      = (v2){{new_mouse.x, new_mouse.y}};
		input->dt         = (f64)(now - last_time) / (f64)os_get_timer_frequency();
		last_time         = now;

		beamformer_frame_step(input);

		input->executable_reloaded = 0;
	}

	beamformer_invalidate_shared_memory(program_memory);
	beamformer_debug_ui_deinit(program_memory);

	/* NOTE: make sure this will get cleaned up after external
	 * programs release their references */
	shm_unlink(OS_SHARED_MEMORY_NAME);
}
