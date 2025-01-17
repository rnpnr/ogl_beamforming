/* See LICENSE for license details. */
#include "beamformer.h"

#if defined(__unix__)
	#include "os_unix.c"

	#define OS_DEBUG_LIB_NAME      "./beamformer.so"
	#define OS_DEBUG_LIB_TEMP_NAME "./beamformer_temp.so"

	#define OS_CUDA_LIB_NAME      "./external/cuda_toolkit.so"
	#define OS_CUDA_LIB_TEMP_NAME "./external/cuda_toolkit_temp.so"

	#define OS_PIPE_NAME "/tmp/beamformer_data_fifo"
	#define OS_SMEM_NAME "/ogl_beamformer_parameters"

#elif defined(_WIN32)
	#include "os_win32.c"

	#define OS_DEBUG_LIB_NAME      "beamformer.dll"
	#define OS_DEBUG_LIB_TEMP_NAME "beamformer_temp.dll"

	#define OS_CUDA_LIB_NAME      "external\\cuda_toolkit.dll"
	#define OS_CUDA_LIB_TEMP_NAME "external\\cuda_toolkit_temp.dll"

	#define OS_PIPE_NAME "\\\\.\\pipe\\beamformer_data_fifo"
	#define OS_SMEM_NAME "Local\\ogl_beamformer_parameters"
#else
	#error Unsupported Platform!
#endif

#include "static.c"

int
main(void)
{
	BeamformerCtx   ctx   = {0};
	BeamformerInput input = {0};
	Arena temp_memory = os_alloc_arena((Arena){0}, 16 * MEGABYTE);
	ctx.error_stream  = stream_alloc(&temp_memory, 1 * MEGABYTE);

	ctx.ui_backing_store = sub_arena(&temp_memory, 2 * MEGABYTE);

	Pipe data_pipe    = os_open_named_pipe(OS_PIPE_NAME);
	input.pipe_handle = data_pipe.file;
	ASSERT(data_pipe.file != INVALID_FILE);

	input.executable_reloaded = 1;

	#define X(name) ctx.platform.name = os_ ## name;
	PLATFORM_FNS
	#undef X

	setup_beamformer(&ctx, temp_memory);

	while(!(ctx.flags & SHOULD_EXIT)) {
		do_debug(&input, &ctx.error_stream);
		if (ctx.gl.vendor_id == GL_VENDOR_NVIDIA)
			check_and_load_cuda_lib(&ctx.cuda_lib, &ctx.error_stream);

		if (ctx.flags & RELOAD_SHADERS) {
			ctx.flags &= ~RELOAD_SHADERS;
			reload_shaders(&ctx, temp_memory);
		}

		input.last_mouse = input.mouse;
		input.mouse.rl   = GetMousePosition();
		input.pipe_data_available = os_poll_pipe(&data_pipe);

		beamformer_frame_step(&ctx, &temp_memory, &input);

		input.executable_reloaded = 0;
	}

	/* NOTE: make sure this will get cleaned up after external
	 * programs release their references */
	os_remove_shared_memory(OS_SMEM_NAME);

	/* NOTE: garbage code needed for Linux */
	os_close_named_pipe(data_pipe);
}
