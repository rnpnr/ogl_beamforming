/* See LICENSE for license details. */
#ifndef __linux__
#error This file is only meant to be compiled for Linux
#endif

#include "beamformer.h"

#include "os_unix.c"

#define OS_DEBUG_LIB_NAME      "./beamformer.so"
#define OS_DEBUG_LIB_TEMP_NAME "./beamformer_temp.so"

#define OS_CUDA_LIB_NAME      "./external/cuda_toolkit.so"
#define OS_CUDA_LIB_TEMP_NAME "./external/cuda_toolkit_temp.so"

#define OS_PIPE_NAME "/tmp/beamformer_data_fifo"
#define OS_SMEM_NAME "/ogl_beamformer_parameters"

#define OS_PATH_SEPERATOR "/"

#include "static.c"

static void
dispatch_file_watch_events(Platform *platform, Arena arena)
{
	FileWatchContext *fwctx = &platform->file_watch_context;
	u8 *mem     = alloc_(&arena, 4096, 64, 1);
	Stream path = stream_alloc(&arena, 256);
	struct inotify_event *event;

	size rlen;
	while ((rlen = read(fwctx->handle, mem, 4096)) > 0) {
		for (u8 *data = mem; data < mem + rlen; data += sizeof(*event) + event->len) {
			event = (struct inotify_event *)data;
			for (u32 i = 0; i < fwctx->directory_watch_count; i++) {
				FileWatchDirectory *dir = fwctx->directory_watches + i;
				if (event->wd != dir->handle)
					continue;

				s8  file = cstr_to_s8(event->name);
				u64 hash = s8_hash(file);
				for (u32 i = 0; i < dir->file_watch_count; i++) {
					FileWatch *fw = dir->file_watches + i;
					if (fw->hash == hash) {
						stream_append_s8(&path, dir->name);
						stream_append_byte(&path, '/');
						stream_append_s8(&path, file);
						stream_append_byte(&path, 0);
						path.widx--;
						fw->callback(platform, stream_to_s8(&path),
						             fw->user_data, arena);
						path.widx = 0;
						break;
					}
				}
			}
		}
	}
}

int
main(void)
{
	BeamformerCtx   ctx   = {0};
	BeamformerInput input = {.executable_reloaded = 1};
	Arena temp_memory = os_alloc_arena((Arena){0}, MB(16));
	ctx.error_stream  = stream_alloc(&temp_memory, MB(1));

	ctx.ui_backing_store              = sub_arena(&temp_memory, MB(2), KB(4));
	ctx.platform.compute_worker.arena = sub_arena(&temp_memory, MB(2), KB(4));

	Pipe data_pipe    = os_open_named_pipe(OS_PIPE_NAME);
	input.pipe_handle = data_pipe.file;
	ASSERT(data_pipe.file != INVALID_FILE);

	#define X(name) ctx.platform.name = os_ ## name;
	PLATFORM_FNS
	#undef X

	ctx.platform.file_watch_context.handle = inotify_init1(O_NONBLOCK|O_CLOEXEC);
	ctx.platform.compute_worker.asleep     = 1;
	ctx.platform.error_file_handle         = STDERR_FILENO;

	debug_init(&ctx.platform, (iptr)&input, &temp_memory);
	setup_beamformer(&ctx, &temp_memory);
	os_wake_thread(ctx.platform.compute_worker.sync_handle);

	struct pollfd fds[2] = {{0}, {0}};
	fds[0].fd     = ctx.platform.file_watch_context.handle;
	fds[0].events = POLLIN;
	fds[1].fd     = data_pipe.file;
	fds[1].events = POLLIN;

	while (!ctx.should_exit) {
		poll(fds, 2, 0);
		if (fds[0].revents & POLLIN)
			dispatch_file_watch_events(&ctx.platform, temp_memory);

		input.pipe_data_available = !!(fds[1].revents & POLLIN);
		input.last_mouse          = input.mouse;
		input.mouse.rl            = GetMousePosition();

		beamformer_frame_step(&ctx, &temp_memory, &input);

		input.executable_reloaded = 0;
	}

	/* NOTE: make sure this will get cleaned up after external
	 * programs release their references */
	shm_unlink(OS_SMEM_NAME);

	/* NOTE: garbage code needed for Linux */
	close(data_pipe.file);
	unlink(data_pipe.name);
}
