/* See LICENSE for license details. */
#include "beamformer.h"

#include "os_win32.c"

#define OS_DEBUG_LIB_NAME      "beamformer.dll"
#define OS_DEBUG_LIB_TEMP_NAME "beamformer_temp.dll"

#define OS_CUDA_LIB_NAME      "external\\cuda_toolkit.dll"
#define OS_CUDA_LIB_TEMP_NAME "external\\cuda_toolkit_temp.dll"

#define OS_PIPE_NAME "\\\\.\\pipe\\beamformer_data_fifo"
#define OS_SMEM_NAME "Local\\ogl_beamformer_parameters"

#include "static.c"

/* NOTE: for now we use mainCRTStartup because we want the console to start for logging */
#define USE_CONSOLE
i32
#ifndef USE_CONSOLE
/* NOTE: needs -mwindows linker flag */
WinMainCRTStartup(void)
#else
mainCRTStartup(void)
#endif
{
	BeamformerCtx ctx = {0};
	Arena temp_memory = os_alloc_arena((Arena){0}, 16 * MEGABYTE);
	ctx.error_stream  = stream_alloc(&temp_memory, 1 * MEGABYTE);

	#define X(name) ctx.platform.name = os_ ## name;
	PLATFORM_FNS
	#undef X

	setup_beamformer(&ctx, temp_memory);

	while(!(ctx.flags & SHOULD_EXIT)) {
		do_program_step(&ctx, temp_memory);
	}

	/* NOTE: make sure this will get cleaned up after external
	 * programs release their references */
	os_remove_shared_memory(OS_SMEM_NAME);

	/* NOTE: garbage code needed for Linux */
	os_close_named_pipe(ctx.data_pipe);

	#ifdef USE_CONSOLE
	ExitProcess(0);
	unreachable();
	#else
	return TerminateProcess(GetCurrentProcess(), 0);
	#endif
}
