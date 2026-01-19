/* See LICENSE for license details. */

#include "beamformer_internal.h"

/* NOTE(rnp): magic variables to force discrete GPU usage on laptops with multiple devices */
EXPORT i32 NvOptimusEnablement = 1;
EXPORT i32 AmdPowerXpressRequestHighPerformance = 1;

#if !BEAMFORMER_DEBUG
#include "beamformer_core.c"
#else

typedef void beamformer_frame_step_fn(BeamformerInput *);

#define BEAMFORMER_DEBUG_ENTRY_POINTS \
	X(beamformer_debug_ui_deinit)  \
	X(beamformer_complete_compute) \
	X(beamformer_frame_step)       \
	X(beamformer_rf_upload)        \

#define X(name) global name ##_fn *name;
BEAMFORMER_DEBUG_ENTRY_POINTS
#undef X

BEAMFORMER_EXPORT void
beamformer_debug_hot_reload(OSLibrary library, BeamformerInput *input)
{
	BeamformerCtx *ctx = BeamformerContextMemory(input->memory);

	// TODO(rnp): this will deadlock if live imaging is active
	/* NOTE(rnp): spin until compute thread finishes its work (we will probably
	 * never reload while compute is in progress but just incase). */
	spin_wait(atomic_load_u32(&ctx->upload_worker.awake));
	spin_wait(atomic_load_u32(&ctx->compute_worker.awake));

	#define X(name) name = os_lookup_symbol(library, #name);
	BEAMFORMER_DEBUG_ENTRY_POINTS
	#undef X

	s8 info = beamformer_info("reloaded main executable");
	os_console_log(info.data, info.len);
}

#endif /* BEAMFORMER_DEBUG */

function no_return void
fatal(s8 message)
{
	os_fatal(message.data, message.len);
	unreachable();
}

#include "vulkan.c"

// TODO(rnp): this doesn't belong here, but will be removed
// once vulkan migration is complete
void * glfwGetProcAddress(char *);

function void
gl_debug_logger(u32 src, u32 type, u32 id, u32 lvl, i32 len, const char *msg, const void *userctx)
{
	Stream *e = (Stream *)userctx;
	stream_append_s8s(e, s8("[OpenGL] "), (s8){.len = len, .data = (u8 *)msg}, s8("\n"));
	os_console_log(e->data, e->widx);
	stream_reset(e, 0);
}

function void
load_gl(Stream *err)
{
	#define X(name, ret, params) name = (name##_fn *)glfwGetProcAddress(#name);
	OGLProcedureList
	OGLRequiredExtensionProcedureList
	#undef X

	stream_reset(err, 0);
	#define X(name, ret, params) if (!name) stream_append_s8(err, s8("missing required GL function: " #name "\n"));
	OGLProcedureList
	OGLRequiredExtensionProcedureListBase
	#if OS_WINDOWS
	  OGLRequiredExtensionProcedureListW32
	#else
	  OGLRequiredExtensionProcedureListLinux
	#endif
	#undef X

	if (err->widx) fatal(stream_to_s8(err));
}

function void
beamformer_load_cuda_library(BeamformerCtx *ctx, OSLibrary cuda, Arena arena)
{
	/* TODO(rnp): (25.10.30) registering the rf buffer with CUDA is currently
	 * causing a major performance regression. for now we are disabling its use
	 * altogether. it will be reenabled once the issue can be fixed */
	b32 result = 0 && vk_gpu_info()->vendor == GPUVendor_NVIDIA && ValidHandle(cuda);
	if (result) {
		Stream err = arena_stream(arena);

		stream_append_s8(&err, beamformer_info("loading CUDA library functions"));
		#define X(name, symname) cuda_## name = os_lookup_symbol(cuda, symname);
		CUDALibraryProcedureList
		#undef X

		os_console_log(err.data, err.widx);
	}

	#define X(name, symname) if (!cuda_## name) cuda_## name = cuda_ ## name ## _stub;
	CUDALibraryProcedureList
	#undef X
}

function void
worker_thread_sleep(GLWorkerThreadContext *ctx, BeamformerSharedMemory *sm)
{
	for (;;) {
		i32 expected = 0;
		if (atomic_cas_u32(&ctx->sync_variable, &expected, 1) ||
		    atomic_load_u32(&sm->live_imaging_parameters.active))
		{
			break;
		}

		/* TODO(rnp): clean this crap up; we shouldn't need two values to communicate this */
		atomic_store_u32(&ctx->awake, 0);
		os_wait_on_address(&ctx->sync_variable, 1, (u32)-1);
		atomic_store_u32(&ctx->awake, 1);
	}
}

function OS_THREAD_ENTRY_POINT_FN(compute_worker_thread_entry_point)
{
	GLWorkerThreadContext *ctx = user_context;

	BeamformerCtx *beamformer = (BeamformerCtx *)ctx->user_context;

	for (;;) {
		worker_thread_sleep(ctx, beamformer->shared_memory);
		asan_poison_region(ctx->arena.beg, ctx->arena.end - ctx->arena.beg);
		beamformer_complete_compute(beamformer, &ctx->arena);
	}

	unreachable();

	return 0;
}

function OS_THREAD_ENTRY_POINT_FN(beamformer_upload_entry_point)
{
	GLWorkerThreadContext         *ctx = user_context;
	BeamformerUploadThreadContext *up  = (typeof(up))ctx->user_context;

	for (;;) {
		worker_thread_sleep(ctx, up->shared_memory);
		beamformer_rf_upload(up);
	}

	unreachable();

	return 0;
}

BEAMFORMER_EXPORT void
beamformer_init(BeamformerInput *input)
{
	Arena  memory        = arena_from_memory(input->memory, input->memory_size);
	Arena  compute_arena = sub_arena_end(&memory, MB(2), KB(4));
	Arena  upload_arena  = sub_arena_end(&memory, KB(4), KB(4));
	Arena  ui_arena      = sub_arena_end(&memory, MB(2), KB(4));
	Stream error         = arena_stream(sub_arena_end(&memory, MB(1), 1));

	BeamformerCtx *ctx   = push_struct(&memory, BeamformerCtx);

	Arena scratch = {.beg = memory.end - 4096L, .end = memory.end};
	memory.end = scratch.beg;

	ctx->window_size           = (iv2){{1280, 840}};
	ctx->error_stream          = error;
	ctx->ui_backing_store      = ui_arena;
	ctx->compute_worker.arena  = compute_arena;
	ctx->upload_worker.arena   = upload_arena;

	vk_load(input->vulkan_library_handle, &memory, &ctx->error_stream);

	BeamformerComputeContext *cs = &ctx->compute_context;

	// NOTE(rnp): allocate beamformed image ring buffer
	{
		u64 gpu_heap_size = vk_gpu_info()->gpu_heap_size;
		u64 trial_sizes[] = {
			GB(4),
			GB(2),
			GB(1) + MB(512),
			GB(1),
		};

		u32 base_index = 0;
		for EachElement(trial_sizes, it) {
			if (gpu_heap_size >= 2 * trial_sizes[it])
				break;
			base_index++;
		}

		for (u32 i = base_index; i < countof(trial_sizes); i++) {
			// TODO(rnp): it may be better to download data from this using the transfer queue
			VulkanTimeline timelines[] = {VulkanTimeline_Compute, VulkanTimeline_Graphics};
			GPUBufferAllocateInfo allocate_info = {
				.size            = trial_sizes[i],
				.flags           = VulkanUsageFlag_TransferSource|VulkanUsageFlag_HostReadWrite,
				.timeline_count  = countof(timelines),
				.timelines_used  = timelines,
				.label           = s8("BeaformedData"),
			};
			vk_buffer_allocate(cs->backlog.buffer, &allocate_info);
			if (cs->backlog.buffer->size > 0)
				break;
		}
		if (cs->backlog.buffer->size == 0) {
			// NOTE(rnp): if this becomes an issue we may be able to get by in some other way
			fatal(s8("Failed to allocate space for beamformed data\n"));
		}
	}

	beamformer_load_cuda_library(ctx, input->cuda_library_handle, memory);

	SetConfigFlags(FLAG_VSYNC_HINT|FLAG_WINDOW_ALWAYS_RUN);
	InitWindow(ctx->window_size.w, ctx->window_size.h, "OGL Beamformer");
	/* NOTE: do this after initing so that the window starts out floating in tiling wm */
	SetWindowState(FLAG_WINDOW_RESIZABLE);
	SetWindowMinSize(840, ctx->window_size.h);

	load_gl(&ctx->error_stream);

	ctx->shared_memory = input->shared_memory;
	if (!ctx->shared_memory) fatal(s8("Get more ram lol\n"));
	zero_struct(ctx->shared_memory);

	ctx->shared_memory->version = BEAMFORMER_SHARED_MEMORY_VERSION;
	ctx->shared_memory->reserved_parameter_blocks = 1;
	ctx->shared_memory->max_beamformed_data_size = cs->backlog.buffer->size;

	/* TODO(rnp): I'm not sure if its a good idea to pre-reserve a bunch of semaphores
	 * on w32 but thats what we are doing for now */
	#if OS_WINDOWS
	{
		Stream sb = arena_stream(memory);
		stream_append(&sb, input->shared_memory_name, input->shared_memory_name_length);
		stream_append_s8(&sb, s8("_lock_"));
		i32 start_index = sb.widx;
		for EachElement(os_w32_shared_memory_semaphores, it) {
			stream_reset(&sb, start_index);
			stream_append_u64(&sb, it);
			stream_append_byte(&sb, 0);
			os_w32_shared_memory_semaphores[it] = os_w32_create_semaphore((c8 *)sb.data, 1, 1);
			if InvalidHandle(os_w32_shared_memory_semaphores[it])
				fatal(beamformer_info("init: failed to create w32 shared memory semaphore\n"));

			/* NOTE(rnp): hacky garbage because CreateSemaphore will just open an existing
			 * semaphore without any indication. Sometimes the other side of the shared memory
			 * will provide incorrect parameters or will otherwise fail and its faster to
			 * restart this program than to get that application to release the semaphores */
			/* TODO(rnp): figure out something more robust */
			os_w32_semaphore_release(os_w32_shared_memory_semaphores[it], 1);
		}
	}
	#endif

	GLWorkerThreadContext *worker = &ctx->compute_worker;
	/* TODO(rnp): we should lock this down after we have something working */
	worker->user_context = (iptr)ctx;
	worker->handle       = os_create_thread("[compute]", worker, compute_worker_thread_entry_point);

	GLWorkerThreadContext         *upload = &ctx->upload_worker;
	BeamformerUploadThreadContext *upctx  = push_struct(&memory, typeof(*upctx));
	upload->user_context = (iptr)upctx;
	upctx->rf_buffer     = &cs->rf_buffer;
	upctx->shared_memory = ctx->shared_memory;
	upctx->compute_timing_table = ctx->compute_timing_table;
	upctx->compute_worker_sync  = &ctx->compute_worker.sync_variable;
	upload->handle = os_create_thread("[upload]", upload, beamformer_upload_entry_point);

	/* NOTE: set up OpenGL debug logging */
	Stream *gl_error_stream = push_struct(&memory, Stream);
	*gl_error_stream        = stream_alloc(&memory, 1024);
	glDebugMessageCallback(gl_debug_logger, gl_error_stream);
#ifdef _DEBUG
	glEnable(GL_DEBUG_OUTPUT);
#endif

	if (!BakeShaders)
	{
		for EachElement(beamformer_reloadable_compute_shader_info_indices, it) {
			i32   index = beamformer_reloadable_compute_shader_info_indices[it];
			Arena temp  = scratch;
			s8 file = push_s8_from_parts(&temp, os_path_separator(), s8("shaders"),
			                             beamformer_reloadable_shader_files[index][0]);
			BeamformerFileReloadContext *frc = push_struct(&memory, typeof(*frc));
			frc->kind                 = BeamformerFileReloadKind_ComputeShader;
			frc->shader_reload.shader = beamformer_reloadable_shader_kinds[index];
			os_add_file_watch((char *)file.data, file.len, frc);
		}

		for EachElement(beamformer_reloadable_compute_helpers_shader_info_indices, it) {
			i32   index = beamformer_reloadable_compute_helpers_shader_info_indices[it];
			Arena temp  = scratch;
			s8 file = push_s8_from_parts(&temp, os_path_separator(), s8("shaders"),
			                             beamformer_reloadable_shader_files[index][0]);
			BeamformerFileReloadContext *frc = push_struct(&memory, typeof(*frc));
			frc->kind                 = BeamformerFileReloadKind_ComputeShader;
			frc->shader_reload.shader = beamformer_reloadable_shader_kinds[index];
			os_add_file_watch((char *)file.data, file.len, frc);
		}

		for EachElement(beamformer_reloadable_compute_internal_shader_info_indices, it) {
			i32   index = beamformer_reloadable_compute_internal_shader_info_indices[it];
			Arena temp  = scratch;
			s8 file = push_s8_from_parts(&temp, os_path_separator(), s8("shaders"),
			                             beamformer_reloadable_shader_files[index][0]);
			BeamformerFileReloadContext *frc = push_struct(&memory, typeof(*frc));
			frc->kind                   = BeamformerFileReloadKind_ComputeInternalShader;
			frc->shader_reload.shader   = beamformer_reloadable_shader_kinds[index];
			frc->shader_reload.pipeline = cs->compute_internal_pipelines + it;
			os_add_file_watch((char *)file.data, file.len, frc);
		}
	}

	memory.end = scratch.end;
	ctx->arena = memory;
	ctx->state = BeamformerState_Running;
}

BEAMFORMER_EXPORT void
beamformer_terminate(BeamformerInput *input)
{
	/* NOTE(rnp): work around pebkac when the beamformer is closed while we are doing live
	 * imaging. if the verasonics is blocked in an external function (calling the library
	 * to start compute) it is impossible for us to get it to properly shut down which
	 * will sometimes result in us needing to power cycle the system. set the shared memory
	 * into an error state and release dispatch lock so that future calls will error instead
	 * of blocking.
	 */
	BeamformerCtx *          ctx = BeamformerContextMemory(input->memory);
	BeamformerSharedMemory * sm  = input->shared_memory;
	if (ctx->state != BeamformerState_Terminated) {
		if (sm) {
			BeamformerSharedMemoryLockKind lock = BeamformerSharedMemoryLockKind_DispatchCompute;
			atomic_store_u32(&sm->invalid, 1);
			atomic_store_u32(&sm->external_work_queue.ridx, sm->external_work_queue.widx);
			DEBUG_DECL(if (sm->locks[lock])) {
				beamformer_shared_memory_release_lock(sm, (i32)lock);
			}

			atomic_or_u32(&sm->live_imaging_dirty_flags, BeamformerLiveImagingDirtyFlags_StopImaging);
		}

		beamformer_debug_ui_deinit(ctx);

		ctx->state = BeamformerState_Terminated;
	}
}

BEAMFORMER_EXPORT u32
beamformer_should_close(BeamformerInput *input)
{
	BeamformerCtx * ctx = BeamformerContextMemory(input->memory);
	if (ctx->state == BeamformerState_ShouldClose)
		beamformer_terminate(input);
	return ctx->state == BeamformerState_Terminated;
}
