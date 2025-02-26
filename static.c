/* See LICENSE for license details. */
#ifndef _DEBUG

#include "beamformer.c"
#define debug_init(...)

#else
static void *debug_lib;

#define DEBUG_ENTRY_POINTS \
	X(beamformer_frame_step)           \
	X(beamformer_complete_compute)     \
	X(beamform_work_queue_push)        \
	X(beamform_work_queue_push_commit)

#define X(name) static name ##_fn *name;
DEBUG_ENTRY_POINTS
#undef X

static FILE_WATCH_CALLBACK_FN(debug_reload)
{
	BeamformerInput *input = (BeamformerInput *)user_data;
	Stream err             = arena_stream(&tmp);

	/* NOTE(rnp): spin until compute thread finishes its work (we will probably
	 * never reload while compute is in progress but just incase). */
	while (!atomic_load(&platform->compute_worker.asleep));

	os_unload_library(debug_lib);
	debug_lib = os_load_library(OS_DEBUG_LIB_NAME, OS_DEBUG_LIB_TEMP_NAME, &err);

	#define X(name) name = os_lookup_dynamic_symbol(debug_lib, #name, &err);
	DEBUG_ENTRY_POINTS
	#undef X

	os_write_err_msg(s8("Reloaded Main Executable\n"));
	input->executable_reloaded = 1;

	return 1;
}

static void
debug_init(Platform *p, iptr input, Arena *arena)
{
	p->add_file_watch(p, arena, s8(OS_DEBUG_LIB_NAME), debug_reload, input);
	debug_reload(p, (s8){0}, input, *arena);
}

#endif /* _DEBUG */

#define static_path_join(a, b) (a OS_PATH_SEPERATOR b)

static void
gl_debug_logger(u32 src, u32 type, u32 id, u32 lvl, i32 len, const char *msg, const void *userctx)
{
	(void)src; (void)type; (void)id;

	Stream *e = (Stream *)userctx;
	stream_append_s8(e, s8("[GL DEBUG "));
	switch (lvl) {
	case GL_DEBUG_SEVERITY_HIGH:         stream_append_s8(e, s8("HIGH]: "));         break;
	case GL_DEBUG_SEVERITY_MEDIUM:       stream_append_s8(e, s8("MEDIUM]: "));       break;
	case GL_DEBUG_SEVERITY_LOW:          stream_append_s8(e, s8("LOW]: "));          break;
	case GL_DEBUG_SEVERITY_NOTIFICATION: stream_append_s8(e, s8("NOTIFICATION]: ")); break;
	default:                             stream_append_s8(e, s8("INVALID]: "));      break;
	}
	stream_append(e, (char *)msg, len);
	stream_append_byte(e, '\n');
	os_write_err_msg(stream_to_s8(e));
	e->widx = 0;
}

static void
get_gl_params(GLParams *gl, Stream *err)
{
	char *vendor = (char *)glGetString(GL_VENDOR);
	if (!vendor) {
		stream_append_s8(err, s8("Failed to determine GL Vendor\n"));
		os_fatal(stream_to_s8(err));
	}
	/* TODO(rnp): str prefix of */
	switch (vendor[0]) {
	case 'A': gl->vendor_id = GL_VENDOR_AMD;    break;
	case 'I': gl->vendor_id = GL_VENDOR_INTEL;  break;
	case 'N': gl->vendor_id = GL_VENDOR_NVIDIA; break;
	/* NOTE(rnp): freedreno */
	case 'f': gl->vendor_id = GL_VENDOR_ARM;    break;
	/* NOTE(rnp): Microsoft Corporation - weird win32 thing (microsoft is just using mesa for the driver) */
	case 'M': gl->vendor_id = GL_VENDOR_ARM;    break;
	default:
		stream_append_s8(err, s8("Unknown GL Vendor: "));
		stream_append_s8(err, c_str_to_s8(vendor));
		stream_append_byte(err, '\n');
		os_fatal(stream_to_s8(err));
		break;
	}

	glGetIntegerv(GL_MAJOR_VERSION,                 &gl->version_major);
	glGetIntegerv(GL_MINOR_VERSION,                 &gl->version_minor);
	glGetIntegerv(GL_MAX_TEXTURE_SIZE,              &gl->max_2d_texture_dim);
	glGetIntegerv(GL_MAX_3D_TEXTURE_SIZE,           &gl->max_3d_texture_dim);
	glGetIntegerv(GL_MAX_SHADER_STORAGE_BLOCK_SIZE, &gl->max_ssbo_size);
	glGetIntegerv(GL_MAX_UNIFORM_BLOCK_SIZE,        &gl->max_ubo_size);
	glGetIntegerv(GL_MAX_SERVER_WAIT_TIMEOUT,       &gl->max_server_wait_time);

	/* NOTE(rnp): sometimes GL_MINOR_VERSION doesn't actually report the drivers
	 * supported version. Since at this point GL has been fully loaded we can
	 * check that at least one of the GL 4.5 function pointers are available */
	if (gl->version_minor < 5 && glCreateBuffers)
		gl->version_minor = 5;
}

static void
validate_gl_requirements(GLParams *gl, Arena a)
{
	Stream s = arena_stream(&a);

	if (gl->max_ubo_size < sizeof(BeamformerParameters)) {
		stream_append_s8(&s, s8("GPU must support UBOs of at least "));
		stream_append_i64(&s, sizeof(BeamformerParameters));
		stream_append_s8(&s, s8(" bytes!\n"));
	}

	if (gl->version_major < 4 || (gl->version_major == 4 && gl->version_minor < 5))
		stream_append_s8(&s, s8("Only OpenGL Versions 4.5 or newer are supported!\n"));

	if (s.widx) os_fatal(stream_to_s8(&s));
}

static void
dump_gl_params(GLParams *gl, Arena a)
{
	(void)gl; (void)a;
#ifdef _DEBUG
	Stream s = arena_stream(&a);
	stream_append_s8(&s, s8("---- GL Parameters ----\n"));
	switch (gl->vendor_id) {
	case GL_VENDOR_AMD:    stream_append_s8(&s, s8("Vendor: AMD\n"));    break;
	case GL_VENDOR_ARM:    stream_append_s8(&s, s8("Vendor: ARM\n"));    break;
	case GL_VENDOR_INTEL:  stream_append_s8(&s, s8("Vendor: Intel\n"));  break;
	case GL_VENDOR_NVIDIA: stream_append_s8(&s, s8("Vendor: nVidia\n")); break;
	}
	stream_append_s8(&s, s8("Version: "));
	stream_append_i64(&s, gl->version_major);
	stream_append_byte(&s, '.');
	stream_append_i64(&s, gl->version_minor);
	stream_append_s8(&s, s8("\nMax 1D/2D Texture Dimension: "));
	stream_append_i64(&s, gl->max_2d_texture_dim);
	stream_append_s8(&s, s8("\nMax 3D Texture Dimension:    "));
	stream_append_i64(&s, gl->max_3d_texture_dim);
	stream_append_s8(&s, s8("\nMax SSBO Size:               "));
	stream_append_i64(&s, gl->max_ssbo_size);
	stream_append_s8(&s, s8("\nMax UBO Size:                "));
	stream_append_i64(&s, gl->max_ubo_size);
	stream_append_s8(&s, s8("\nMax Server Wait Time [ns]:   "));
	stream_append_i64(&s, gl->max_server_wait_time);
	stream_append_s8(&s, s8("\n-----------------------\n"));
	if (!s.errors)
		os_write_err_msg(stream_to_s8(&s));
#endif
}

static FILE_WATCH_CALLBACK_FN(reload_render_shader)
{
	FragmentShaderCtx *ctx = (FragmentShaderCtx *)user_data;
	Shader updated_fs      = LoadShader(0, (c8 *)path.data);

	if (updated_fs.id) {
		UnloadShader(ctx->shader);
		LABEL_GL_OBJECT(GL_PROGRAM, updated_fs.id, s8("Render Shader"));
		ctx->shader       = updated_fs;
		ctx->db_cutoff_id = GetShaderLocation(updated_fs, "u_db_cutoff");
		ctx->threshold_id = GetShaderLocation(updated_fs, "u_threshold");
		ctx->gen_mipmaps  = 1;
	}

	return 1;
}


static FILE_WATCH_CALLBACK_FN(queue_compute_shader_reload)
{
	ComputeShaderReloadContext *csr = (typeof(csr))user_data;
	BeamformerCtx *ctx = csr->beamformer_ctx;
	BeamformWork *work = beamform_work_queue_push(ctx->beamform_work_queue);
	if (work) {
		work->type = BW_RELOAD_SHADER;
		work->reload_shader_ctx = csr;
		beamform_work_queue_push_commit(ctx->beamform_work_queue);
		if (ctx->platform.compute_worker.asleep &&
		    ctx->beamform_frames[ctx->display_frame_index].ready_to_present)
		{
			BeamformWork *compute = beamform_work_queue_push(ctx->beamform_work_queue);
			if (compute) {
				compute->type  = BW_COMPUTE;
				compute->frame = ctx->beamform_frames + ctx->next_render_frame_index++;
				compute->frame->ready_to_present = 0;
				if (ctx->next_render_frame_index >= ARRAY_COUNT(ctx->beamform_frames))
					ctx->next_render_frame_index = 0;
				beamform_work_queue_push_commit(ctx->beamform_work_queue);
			}
		}
		ctx->platform.wake_thread(ctx->platform.compute_worker.sync_handle);
	}
	return 1;
}

static FILE_WATCH_CALLBACK_FN(load_cuda_lib)
{
	CudaLib *cl = (CudaLib *)user_data;
	b32 result  = os_file_exists((c8 *)path.data);
	if (result) {
		os_write_err_msg(s8("loading CUDA lib: " OS_CUDA_LIB_NAME "\n"));

		Stream err = arena_stream(&tmp);
		os_unload_library(cl->lib);
		cl->lib = os_load_library((c8 *)path.data, OS_CUDA_LIB_TEMP_NAME, &err);
		#define X(name) cl->name = os_lookup_dynamic_symbol(cl->lib, #name, &err);
		CUDA_LIB_FNS
		#undef X
	}

	#define X(name) if (!cl->name) cl->name = name ## _stub;
	CUDA_LIB_FNS
	#undef X

	return result;
}


#define GLFW_VISIBLE 0x00020004
void glfwWindowHint(i32, i32);
iptr glfwCreateWindow(i32, i32, char *, iptr, iptr);
void glfwMakeContextCurrent(iptr);

#include <stdio.h>
static PLATFORM_THREAD_ENTRY_POINT_FN(compute_worker_thread_entry_point)
{
	GLWorkerThreadContext *ctx = (GLWorkerThreadContext *)_ctx;

	glfwMakeContextCurrent(ctx->window_handle);

	for (;;) {
		ctx->asleep = 1;
		os_sleep_thread(ctx->sync_handle);
		ctx->asleep = 0;
		beamformer_complete_compute(ctx->user_context, ctx->arena);
	}

	unreachable();

	return 0;
}

static void
setup_beamformer(BeamformerCtx *ctx, Arena *memory)
{
	ctx->window_size  = (uv2){.w = 1280, .h = 840};

	SetConfigFlags(FLAG_VSYNC_HINT);
	InitWindow(ctx->window_size.w, ctx->window_size.h, "OGL Beamformer");
	/* NOTE: do this after initing so that the window starts out floating in tiling wm */
	SetWindowState(FLAG_WINDOW_RESIZABLE);
	SetWindowMinSize(INFO_COLUMN_WIDTH * 2, ctx->window_size.h);

	/* NOTE: Gather information about the GPU */
	get_gl_params(&ctx->gl, &ctx->error_stream);
	dump_gl_params(&ctx->gl, *memory);
	validate_gl_requirements(&ctx->gl, *memory);

	glfwWindowHint(GLFW_VISIBLE, 0);
	iptr raylib_window_handle = (iptr)GetPlatformWindowHandle();
	GLWorkerThreadContext *worker = &ctx->platform.compute_worker;
	worker->window_handle = glfwCreateWindow(320, 240, "", 0, raylib_window_handle);
	worker->sync_handle   = os_create_sync_object(memory);
	worker->handle        = os_create_thread(*memory, (iptr)worker, s8("[compute]"),
	                                         compute_worker_thread_entry_point);
	/* TODO(rnp): we should lock this down after we have something working */
	worker->user_context  = (iptr)ctx;

	glfwMakeContextCurrent(raylib_window_handle);

	ctx->beamform_work_queue = push_struct(memory, BeamformWorkQueue);

	ctx->fsctx.db        = -50.0f;
	ctx->fsctx.threshold =  40.0f;

	ctx->params = os_open_shared_memory_area(OS_SMEM_NAME, sizeof(*ctx->params));
	/* TODO: properly handle this? */
	ASSERT(ctx->params);

	/* NOTE: default compute shader pipeline */
	ctx->params->compute_stages[0]    = CS_DECODE;
	ctx->params->compute_stages[1]    = CS_DAS;
	ctx->params->compute_stages_count = 2;

	if (ctx->gl.vendor_id == GL_VENDOR_NVIDIA
	    && load_cuda_lib(&ctx->platform, s8(OS_CUDA_LIB_NAME), (iptr)&ctx->cuda_lib, *memory))
	{
		os_add_file_watch(&ctx->platform, memory, s8(OS_CUDA_LIB_NAME), load_cuda_lib,
		                  (iptr)&ctx->cuda_lib);
	} else {
		#define X(name) if (!ctx->cuda_lib.name) ctx->cuda_lib.name = name ## _stub;
		CUDA_LIB_FNS
		#undef X
	}

	/* NOTE: set up OpenGL debug logging */
	glDebugMessageCallback(gl_debug_logger, &ctx->error_stream);
#ifdef _DEBUG
	glEnable(GL_DEBUG_OUTPUT);
#endif

	/* NOTE: allocate space for Uniform Buffer but don't send anything yet */
	glCreateBuffers(1, &ctx->csctx.shared_ubo);
	glNamedBufferStorage(ctx->csctx.shared_ubo, sizeof(BeamformerParameters), 0, GL_DYNAMIC_STORAGE_BIT);
	LABEL_GL_OBJECT(GL_BUFFER, ctx->csctx.shared_ubo, s8("Beamformer_Parameters"));

	#define X(e, sn, f, nh, pretty_name) do if (s8(f).len > 0) {                 \
		ComputeShaderReloadContext *csr = push_struct(memory, typeof(*csr)); \
		csr->beamformer_ctx = ctx;                                           \
		csr->label          = s8("CS_" #e);                                  \
		csr->shader         = sn;                                            \
		csr->needs_header   = nh;                                            \
		csr->path           = s8(static_path_join("shaders", f ".glsl"));    \
		os_add_file_watch(&ctx->platform, memory, csr->path, queue_compute_shader_reload, (iptr)csr); \
		queue_compute_shader_reload(&ctx->platform, csr->path, (iptr)csr, *memory); \
	} while (0);
	COMPUTE_SHADERS
	#undef X
	os_wake_thread(worker->sync_handle);

	s8 render = s8(static_path_join("shaders", "render.glsl"));
	reload_render_shader(&ctx->platform, render, (iptr)&ctx->fsctx, *memory);
	os_add_file_watch(&ctx->platform, memory, render, reload_render_shader, (iptr)&ctx->fsctx);
	ctx->fsctx.gen_mipmaps = 0;

	ctx->ready_for_rf = 1;
}
