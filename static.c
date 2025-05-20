/* See LICENSE for license details. */
#ifndef _DEBUG

#include "beamformer.c"
#define debug_init(...)

#else
global void *debug_lib;

#define DEBUG_ENTRY_POINTS \
	X(beamformer_frame_step)           \
	X(beamformer_complete_compute)     \
	X(beamformer_compute_setup)        \
	X(beamform_work_queue_push)        \
	X(beamform_work_queue_push_commit)

#define X(name) global name ##_fn *name;
DEBUG_ENTRY_POINTS
#undef X

function FILE_WATCH_CALLBACK_FN(debug_reload)
{
	BeamformerInput *input = (BeamformerInput *)user_data;
	Stream err             = arena_stream(tmp);

	/* NOTE(rnp): spin until compute thread finishes its work (we will probably
	 * never reload while compute is in progress but just incase). */
	while (!atomic_load(&os->compute_worker.asleep));

	os_unload_library(debug_lib);
	debug_lib = os_load_library(OS_DEBUG_LIB_NAME, OS_DEBUG_LIB_TEMP_NAME, &err);

	#define X(name) name = os_lookup_dynamic_symbol(debug_lib, #name, &err);
	DEBUG_ENTRY_POINTS
	#undef X

	stream_append_s8(&err, s8("Reloaded Main Executable\n"));
	os->write_file(os->error_handle, stream_to_s8(&err));

	input->executable_reloaded = 1;

	return 1;
}

function void
debug_init(OS *os, iptr input, Arena *arena)
{
	os->add_file_watch(os, arena, s8(OS_DEBUG_LIB_NAME), debug_reload, input);
	debug_reload(os, s8(""), input, *arena);

	Stream err = arena_stream(*arena);
	void *rdoc = os_get_module(OS_RENDERDOC_SONAME, 0);
	if (rdoc) {
		renderdoc_get_api_fn *get_api = os_lookup_dynamic_symbol(rdoc, "RENDERDOC_GetAPI", &err);
		if (get_api) {
			RenderDocAPI *api = 0;
			if (get_api(10600, (void **)&api)) {
				os->start_frame_capture = RENDERDOC_START_FRAME_CAPTURE(api);
				os->end_frame_capture   = RENDERDOC_END_FRAME_CAPTURE(api);
				stream_append_s8(&err, s8("loaded: " OS_RENDERDOC_SONAME "\n"));
			}
		}
	}

	os->write_file(os->error_handle, stream_to_s8(&err));
}

#endif /* _DEBUG */

#define static_path_join(a, b) (a OS_PATH_SEPARATOR b)

struct gl_debug_ctx {
	Stream  stream;
	OS     *os;
};

function void
gl_debug_logger(u32 src, u32 type, u32 id, u32 lvl, i32 len, const char *msg, const void *userctx)
{
	(void)src; (void)type; (void)id;

	struct gl_debug_ctx *ctx = (struct gl_debug_ctx *)userctx;
	Stream *e = &ctx->stream;
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
	ctx->os->write_file(ctx->os->error_handle, stream_to_s8(e));
	stream_reset(e, 0);
}

function void
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
		stream_append_s8s(err, s8("Unknown GL Vendor: "), c_str_to_s8(vendor), s8("\n"));
		os_fatal(stream_to_s8(err));
	}

	#define X(glname, name, suffix) glGetIntegerv(GL_##glname, &gl->name);
	GL_PARAMETERS
	#undef X

	/* NOTE(rnp): sometimes GL_MINOR_VERSION doesn't actually report the drivers
	 * supported version. Since at this point GL has been fully loaded we can
	 * check that at least one of the GL 4.5 function pointers are available */
	if (gl->version_minor < 5 && glCreateBuffers)
		gl->version_minor = 5;
}

function void
validate_gl_requirements(GLParams *gl, Arena a)
{
	Stream s = arena_stream(a);

	if (gl->max_ubo_size < sizeof(BeamformerParameters)) {
		stream_append_s8(&s, s8("GPU must support UBOs of at least "));
		stream_append_i64(&s, sizeof(BeamformerParameters));
		stream_append_s8(&s, s8(" bytes!\n"));
	}

	if (gl->version_major < 4 || (gl->version_major == 4 && gl->version_minor < 5))
		stream_append_s8(&s, s8("Only OpenGL Versions 4.5 or newer are supported!\n"));

	if (s.widx) os_fatal(stream_to_s8(&s));
}

function void
dump_gl_params(GLParams *gl, Arena a, OS *os)
{
	(void)gl; (void)a;
#ifdef _DEBUG
	s8 vendor = s8("vendor:");
	iz max_width = vendor.len;
	#define X(glname, name, suffix) if (s8(#name).len > max_width) max_width = s8(#name ":").len;
	GL_PARAMETERS
	#undef X
	max_width++;

	Stream s = arena_stream(a);
	stream_append_s8s(&s, s8("---- GL Parameters ----\n"), vendor);
	stream_pad(&s, ' ', max_width - vendor.len);
	switch (gl->vendor_id) {
	case GL_VENDOR_AMD:    stream_append_s8(&s, s8("AMD\n"));    break;
	case GL_VENDOR_ARM:    stream_append_s8(&s, s8("ARM\n"));    break;
	case GL_VENDOR_INTEL:  stream_append_s8(&s, s8("Intel\n"));  break;
	case GL_VENDOR_NVIDIA: stream_append_s8(&s, s8("nVidia\n")); break;
	}

	#define X(glname, name, suffix) \
		stream_append_s8(&s, s8(#name ":"));                \
		stream_pad(&s, ' ', max_width - s8(#name ":").len); \
		stream_append_i64(&s, gl->name);                    \
		stream_append_s8(&s, s8(suffix));                   \
		stream_append_byte(&s, '\n');
	GL_PARAMETERS
	#undef X
	stream_append_s8(&s, s8("-----------------------\n"));
	os->write_file(os->error_handle, stream_to_s8(&s));
#endif
}

function FILE_WATCH_CALLBACK_FN(reload_render_shader)
{
	FrameViewRenderContext *ctx = (typeof(ctx))user_data;

	local_persist s8 vertex = s8(""
	"#version 460 core\n"
	"\n"
	"layout(location = 0) in vec2 vertex_position;\n"
	"layout(location = 1) in vec2 vertex_texture_coordinate;\n"
	"\n"
	"layout(location = 0) out vec2 fragment_texture_coordinate;\n"
	"\n"
	"void main()\n"
	"{\n"
	"\tfragment_texture_coordinate = vertex_texture_coordinate;\n"
	"\tgl_Position = vec4(vertex_position, 0, 1);\n"
	"}\n");

	s8 header = push_s8(&tmp, s8(""
	"#version 460 core\n\n"
	"layout(location = 0) in  vec2 fragment_texture_coordinate;\n"
	"layout(location = 0) out vec4 v_out_colour;\n\n"
	"layout(location = " str(FRAME_VIEW_RENDER_DYNAMIC_RANGE_LOC) ") uniform float u_db_cutoff = 60;\n"
	"layout(location = " str(FRAME_VIEW_RENDER_THRESHOLD_LOC)     ") uniform float u_threshold = 40;\n"
	"layout(location = " str(FRAME_VIEW_RENDER_GAMMA_LOC)         ") uniform float u_gamma     = 1;\n"
	"layout(location = " str(FRAME_VIEW_RENDER_LOG_SCALE_LOC)     ") uniform bool  u_log_scale;\n"
	"\n#line 1\n"));

	s8 fragment    = os->read_whole_file(&tmp, (c8 *)path.data);
	fragment.data -= header.len;
	fragment.len  += header.len;
	ASSERT(fragment.data == header.data);
	u32 new_program = load_shader(os, tmp, 0, vertex, fragment, s8(""), path, s8("Render Shader"));
	if (new_program) {
		glDeleteProgram(ctx->shader);
		ctx->shader  = new_program;
		ctx->updated = 1;
	}

	return 1;
}


function FILE_WATCH_CALLBACK_FN(queue_compute_shader_reload)
{
	ComputeShaderReloadContext *csr = (typeof(csr))user_data;
	BeamformerCtx *ctx = csr->beamformer_ctx;
	BeamformWork *work = beamform_work_queue_push(ctx->beamform_work_queue);
	if (work) {
		work->type = BW_RELOAD_SHADER;
		work->reload_shader_ctx = csr;
		beamform_work_queue_push_commit(ctx->beamform_work_queue);
		ctx->os.wake_waiters(&ctx->os.compute_worker.sync_variable);
	}
	return 1;
}

function FILE_WATCH_CALLBACK_FN(load_cuda_lib)
{
	CudaLib *cl = (CudaLib *)user_data;
	b32 result  = os_file_exists((c8 *)path.data);
	if (result) {
		Stream err = arena_stream(tmp);

		stream_append_s8(&err, s8("loading CUDA lib: " OS_CUDA_LIB_NAME "\n"));
		os_unload_library(cl->lib);
		cl->lib = os_load_library((c8 *)path.data, OS_CUDA_LIB_TEMP_NAME, &err);
		#define X(name, symname) cl->name = os_lookup_dynamic_symbol(cl->lib, symname, &err);
		CUDA_LIB_FNS
		#undef X

		os->write_file(os->error_handle, stream_to_s8(&err));
	}

	#define X(name, symname) if (!cl->name) cl->name = cuda_ ## name ## _stub;
	CUDA_LIB_FNS
	#undef X

	return result;
}

#define GLFW_VISIBLE 0x00020004
void glfwWindowHint(i32, i32);
iptr glfwCreateWindow(i32, i32, char *, iptr, iptr);
void glfwMakeContextCurrent(iptr);

function OS_THREAD_ENTRY_POINT_FN(compute_worker_thread_entry_point)
{
	GLWorkerThreadContext *ctx = (GLWorkerThreadContext *)_ctx;

	glfwMakeContextCurrent(ctx->window_handle);
	ctx->gl_context = os_get_native_gl_context(ctx->window_handle);

	beamformer_compute_setup(ctx->user_context, ctx->arena, ctx->gl_context);

	for (;;) {
		for (;;) {
			i32 current = atomic_load(&ctx->sync_variable);
			if (current == 0 && atomic_swap(&ctx->sync_variable, 1) == current)
				break;

			ctx->asleep = 1;
			os_wait_on_value(&ctx->sync_variable, current, -1);
			ctx->asleep = 0;
		}
		beamformer_complete_compute(ctx->user_context, ctx->arena, ctx->gl_context);
	}

	unreachable();

	return 0;
}

function void
setup_beamformer(BeamformerCtx *ctx, Arena *memory)
{
	ctx->window_size  = (uv2){.w = 1280, .h = 840};

	SetConfigFlags(FLAG_VSYNC_HINT|FLAG_WINDOW_ALWAYS_RUN);
	InitWindow(ctx->window_size.w, ctx->window_size.h, "OGL Beamformer");
	/* NOTE: do this after initing so that the window starts out floating in tiling wm */
	SetWindowState(FLAG_WINDOW_RESIZABLE);
	SetWindowMinSize(840, ctx->window_size.h);

	/* NOTE: Gather information about the GPU */
	get_gl_params(&ctx->gl, &ctx->error_stream);
	dump_gl_params(&ctx->gl, *memory, &ctx->os);
	validate_gl_requirements(&ctx->gl, *memory);

	glfwWindowHint(GLFW_VISIBLE, 0);
	iptr raylib_window_handle = (iptr)GetPlatformWindowHandle();
	GLWorkerThreadContext *worker = &ctx->os.compute_worker;
	worker->window_handle = glfwCreateWindow(320, 240, "", 0, raylib_window_handle);
	worker->handle        = os_create_thread(*memory, (iptr)worker, s8("[compute]"),
	                                         compute_worker_thread_entry_point);
	/* TODO(rnp): we should lock this down after we have something working */
	worker->user_context  = (iptr)ctx;

	glfwMakeContextCurrent(raylib_window_handle);

	ctx->beamform_work_queue = push_struct(memory, BeamformWorkQueue);

	ctx->shared_memory = os_create_shared_memory_area(OS_SHARED_MEMORY_NAME, BEAMFORMER_SHARED_MEMORY_SIZE);
	if (!ctx->shared_memory)
		os_fatal(s8("Get more ram lol\n"));
	mem_clear(ctx->shared_memory, 0, sizeof(*ctx->shared_memory));

	ctx->shared_memory->version = BEAMFORMER_PARAMETERS_VERSION;

	/* NOTE: default compute shader pipeline */
	ctx->shared_memory->compute_stages[0]    = CS_DECODE;
	ctx->shared_memory->compute_stages[1]    = CS_DAS;
	ctx->shared_memory->compute_stages_count = 2;

	if (ctx->gl.vendor_id == GL_VENDOR_NVIDIA
	    && load_cuda_lib(&ctx->os, s8(OS_CUDA_LIB_NAME), (iptr)&ctx->cuda_lib, *memory))
	{
		os_add_file_watch(&ctx->os, memory, s8(OS_CUDA_LIB_NAME), load_cuda_lib,
		                  (iptr)&ctx->cuda_lib);
	} else {
		#define X(name, symname) if (!ctx->cuda_lib.name) ctx->cuda_lib.name = cuda_ ## name ## _stub;
		CUDA_LIB_FNS
		#undef X
	}

	/* NOTE: set up OpenGL debug logging */
	struct gl_debug_ctx *gl_debug_ctx = push_struct(memory, typeof(*gl_debug_ctx));
	gl_debug_ctx->stream = stream_alloc(memory, 1024);
	gl_debug_ctx->os     = &ctx->os;
	glDebugMessageCallback(gl_debug_logger, gl_debug_ctx);
#ifdef _DEBUG
	glEnable(GL_DEBUG_OUTPUT);
#endif

	#define X(e, sn, f, nh, pretty_name) do if (s8(f).len > 0) {                 \
		ComputeShaderReloadContext *csr = push_struct(memory, typeof(*csr)); \
		csr->beamformer_ctx = ctx;                                           \
		csr->label          = s8("CS_" #e);                                  \
		csr->shader         = sn;                                            \
		csr->needs_header   = nh;                                            \
		csr->path           = s8(static_path_join("shaders", f ".glsl"));    \
		os_add_file_watch(&ctx->os, memory, csr->path, queue_compute_shader_reload, (iptr)csr); \
		queue_compute_shader_reload(&ctx->os, csr->path, (iptr)csr, *memory); \
	} while (0);
	COMPUTE_SHADERS
	#undef X
	os_wake_waiters(&worker->sync_variable);

	FrameViewRenderContext *fvr = &ctx->frame_view_render_context;
	glGenFramebuffers(1, &fvr->framebuffer);
	LABEL_GL_OBJECT(GL_FRAMEBUFFER, fvr->framebuffer, s8("Frame View Render Framebuffer"));
	f32 vertices[] = {
		-1,  1, 0, 0,
		-1, -1, 0, 1,
		 1, -1, 1, 1,
		-1,  1, 0, 0,
		 1, -1, 1, 1,
		 1,  1, 1, 0,
	};
	glGenVertexArrays(1, &fvr->vao);
	glBindVertexArray(fvr->vao);
	glGenBuffers(1, &fvr->vbo);
	glBindBuffer(GL_ARRAY_BUFFER, fvr->vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
	glVertexAttribPointer(0, 2, GL_FLOAT, 0, 4 * sizeof(f32), 0);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(1, 2, GL_FLOAT, 0, 4 * sizeof(f32), (void *)(2 * sizeof(f32)));
	glEnableVertexAttribArray(1);
	glBindVertexArray(0);

	s8 render = s8(static_path_join("shaders", "render.glsl"));
	reload_render_shader(&ctx->os, render, (iptr)fvr, *memory);
	os_add_file_watch(&ctx->os, memory, render, reload_render_shader, (iptr)fvr);
}
