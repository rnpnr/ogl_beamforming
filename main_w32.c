/* See LICENSE for license details. */
#ifndef _WIN32
#error This file is only meant to be compiled for Win32
#endif

#include "beamformer.h"

#include "os_win32.c"

#define OS_DEBUG_LIB_NAME      "beamformer.dll"
#define OS_DEBUG_LIB_TEMP_NAME "beamformer_temp.dll"

#define OS_CUDA_LIB_NAME      "external\\cuda_toolkit.dll"
#define OS_CUDA_LIB_TEMP_NAME "external\\cuda_toolkit_temp.dll"

#define OS_PIPE_NAME "\\\\.\\pipe\\beamformer_data_fifo"
#define OS_SMEM_NAME "Local\\ogl_beamformer_parameters"

#include "static.c"

int
main(void)
{
	BeamformerCtx   ctx   = {0};
	BeamformerInput input = {.executable_reloaded = 1};
	Arena temp_memory = os_alloc_arena((Arena){0}, 16 * MEGABYTE);
	ctx.error_stream  = stream_alloc(&temp_memory, 1 * MEGABYTE);

	ctx.ui_backing_store = sub_arena(&temp_memory, 2 * MEGABYTE);

	Pipe data_pipe    = os_open_named_pipe(OS_PIPE_NAME);
	input.pipe_handle = data_pipe.file;
	ASSERT(data_pipe.file != INVALID_FILE);

	#define X(name) ctx.platform.name = os_ ## name;
	PLATFORM_FNS
	#undef X

	ctx.platform.file_watch_context.handle = CreateIoCompletionPort(INVALID_FILE, 0, 0, 0);

	setup_beamformer(&ctx, temp_memory);
	debug_init(&ctx.platform, (iptr)&input, &temp_memory);

	while (!(ctx.flags & SHOULD_EXIT)) {
		input.last_mouse = input.mouse;
		input.mouse.rl   = GetMousePosition();

		i32 bytes_available = 0;
		input.pipe_data_available = PeekNamedPipe(p.file, 0, 1 * MEGABYTE, 0,
		                                          &bytes_available, 0) && bytes_available;

		beamformer_frame_step(&ctx, &temp_memory, &input);

		input.executable_reloaded = 0;
	}
}
