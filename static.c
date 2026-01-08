/* See LICENSE for license details. */

/* NOTE(rnp): magic variables to force discrete GPU usage on laptops with multiple devices */
EXPORT i32 NvOptimusEnablement = 1;
EXPORT i32 AmdPowerXpressRequestHighPerformance = 1;

#include "beamformer_internal.h"

#ifndef _DEBUG

#include "beamformer.c"
#define debug_init(...)

#else

global void *debug_lib;

#define DEBUG_ENTRY_POINTS \
	X(beamformer_debug_ui_deinit)  \
	X(beamformer_complete_compute) \
	X(beamformer_frame_step)       \
	X(beamformer_reload_shader)    \
	X(beamformer_rf_upload)

#define X(name) global name ##_fn *name;
DEBUG_ENTRY_POINTS
#undef X

struct debug_context {
	BeamformerInput *input;
	b32 *compute_worker_asleep;
	b32 *upload_worker_asleep;
};

function FILE_WATCH_CALLBACK_FN(debug_reload)
{
	struct debug_context *ctx = (struct debug_context *)user_data;
	Stream err = arena_stream(arena);

	/* NOTE(rnp): spin until compute thread finishes its work (we will probably
	 * never reload while compute is in progress but just incase). */
	spin_wait(!atomic_load_u32(ctx->compute_worker_asleep));
	spin_wait(!atomic_load_u32(ctx->upload_worker_asleep));

	os_unload_library(debug_lib);
	debug_lib = os_load_library(OS_DEBUG_LIB_NAME, OS_DEBUG_LIB_TEMP_NAME, &err);

	#define X(name) name = os_lookup_dynamic_symbol(debug_lib, #name, &err);
	DEBUG_ENTRY_POINTS
	#undef X

	stream_append_s8(&err, s8("Reloaded Main Executable\n"));
	os_write_file(os_error_handle(), stream_to_s8(&err));

	ctx->input->executable_reloaded = 1;

	return 1;
}

function void
debug_init(BeamformerCtx *ctx, BeamformerInput *input, Arena *arena)
{
	struct debug_context *dctx = push_struct(arena, struct debug_context);
	dctx->input = input;
	dctx->compute_worker_asleep = &ctx->compute_worker.asleep;
	dctx->upload_worker_asleep  = &ctx->upload_worker.asleep;
	os_add_file_watch(s8(OS_DEBUG_LIB_NAME), debug_reload, (iptr)dctx);
	debug_reload(s8(""), (iptr)dctx, *arena);

	Stream err = arena_stream(*arena);
	void *rdoc = os_get_module(OS_RENDERDOC_SONAME, 0);
	if (rdoc) {
		renderdoc_get_api_fn *get_api = os_lookup_dynamic_symbol(rdoc, "RENDERDOC_GetAPI", &err);
		if (get_api) {
			RenderDocAPI *api = 0;
			if (get_api(10600, (void **)&api)) {
				ctx->start_frame_capture = RENDERDOC_START_FRAME_CAPTURE(api);
				ctx->end_frame_capture   = RENDERDOC_END_FRAME_CAPTURE(api);
				stream_append_s8(&err, s8("loaded: " OS_RENDERDOC_SONAME "\n"));
			}
		}
	}

	os_write_file(os_error_handle(), stream_to_s8(&err));
}

#endif /* _DEBUG */

function void
gl_debug_logger(u32 src, u32 type, u32 id, u32 lvl, i32 len, const char *msg, const void *userctx)
{
	Stream *e = (Stream *)userctx;
	stream_append_s8s(e, s8("[OpenGL] "), (s8){.len = len, .data = (u8 *)msg}, s8("\n"));
	os_write_file(os_error_handle(), stream_to_s8(e));
	stream_reset(e, 0);
}

function void
load_gl(Stream *err)
{
	#define X(name, ret, params) name = (name##_fn *)os_gl_proc_address(#name);
	OGLProcedureList
	#undef X

	/* NOTE: Gather information about the GPU */
	{
		char *vendor = (char *)glGetString(GL_VENDOR);
		if (!vendor) {
			stream_append_s8(err, s8("Failed to determine GL Vendor\n"));
			os_fatal(stream_to_s8(err));
		}
		/* TODO(rnp): str prefix of */
		switch (vendor[0]) {
		case 'A': gl_parameters.vendor_id = GLVendor_AMD;    break;
		case 'I': gl_parameters.vendor_id = GLVendor_Intel;  break;
		case 'N': gl_parameters.vendor_id = GLVendor_NVIDIA; break;
		/* NOTE(rnp): freedreno */
		case 'f': gl_parameters.vendor_id = GLVendor_ARM;    break;
		/* NOTE(rnp): Microsoft Corporation - weird win32 thing (microsoft is just using mesa for the driver) */
		case 'M': gl_parameters.vendor_id = GLVendor_ARM;    break;
		default:
			stream_append_s8s(err, s8("Unknown GL Vendor: "), c_str_to_s8(vendor), s8("\n"));
			os_fatal(stream_to_s8(err));
		}

		#define X(glname, name, suffix) glGetIntegerv(GL_##glname, &gl_parameters.name);
		GL_PARAMETERS
		#undef X
	}

#ifdef _DEBUG
	{
		s8 vendor = s8("vendor:");
		i32 max_width = (i32)vendor.len;
		#define X(glname, name, suffix) if (s8(#name).len > max_width) max_width = (i32)s8(#name ":").len;
		GL_PARAMETERS
		#undef X
		max_width++;

		stream_append_s8s(err, s8("---- GL Parameters ----\n"), vendor);
		stream_pad(err, ' ', max_width - (i32)vendor.len);
		switch (gl_parameters.vendor_id) {
		case GLVendor_AMD:    stream_append_s8(err, s8("AMD"));    break;
		case GLVendor_ARM:    stream_append_s8(err, s8("ARM"));    break;
		case GLVendor_Intel:  stream_append_s8(err, s8("Intel"));  break;
		case GLVendor_NVIDIA: stream_append_s8(err, s8("nVidia")); break;
		}
		stream_append_byte(err, '\n');

		#define X(glname, name, suffix) \
			stream_append_s8(err, s8(#name ":"));                     \
			stream_pad(err, ' ', max_width - (i32)s8(#name ":").len); \
			stream_append_i64(err, gl_parameters.name);               \
			stream_append_s8(err, s8(suffix "\n"));
		GL_PARAMETERS
		#undef X
		stream_append_s8(err, s8("-----------------------\n"));
		os_write_file(os_error_handle(), stream_to_s8(err));
	}
#endif

	{
		stream_reset(err, 0);
		if (gl_parameters.max_ubo_size < (i32)sizeof(BeamformerParameters)) {
			stream_append_s8(err, s8("GPU must support UBOs of at least "));
			stream_append_i64(err, sizeof(BeamformerParameters));
			stream_append_s8(err, s8(" bytes!\n"));
		}

		#define X(name, ret, params) if (!name) stream_append_s8(err, s8("missing required GL function: " #name "\n"));
		OGLProcedureList
		#undef X

		if (err->widx) os_fatal(stream_to_s8(err));
	}
}

function FILE_WATCH_CALLBACK_FN(reload_shader)
{
	ShaderReloadContext  *ctx  = (typeof(ctx))user_data;
	BeamformerShaderKind  kind = beamformer_reloadable_shader_kinds[ctx->reloadable_info_index];
	return beamformer_reload_shader(path, ctx, arena, beamformer_shader_names[kind]);
}

typedef struct {
	BeamformerCtx        *beamformer;
	BeamformerShaderKind  shader;
} BeamformerShaderReloadIndirectContext;

function FILE_WATCH_CALLBACK_FN(reload_shader_indirect)
{
	BeamformerShaderReloadIndirectContext *rsi = (typeof(rsi))user_data;
	BeamformerCtx *ctx = rsi->beamformer;
	BeamformWork *work = beamform_work_queue_push(ctx->beamform_work_queue);
	if (work) {
		work->kind = BeamformerWorkKind_ReloadShader,
		work->reload_shader = rsi->shader;
		beamform_work_queue_push_commit(ctx->beamform_work_queue);
		os_wake_waiters(&ctx->compute_worker.sync_variable);
	}
	return 1;
}

function FILE_WATCH_CALLBACK_FN(load_cuda_library)
{
	local_persist void *cuda_library_handle;

	/* TODO(rnp): (25.10.30) registering the rf buffer with CUDA is currently
	 * causing a major performance regression. for now we are disabling its use
	 * altogether. it will be reenabled once the issue can be fixed */
	b32 result = 0 && gl_parameters.vendor_id == GLVendor_NVIDIA && os_file_exists((c8 *)path.data);
	if (result) {
		Stream err = arena_stream(arena);

		stream_append_s8(&err, s8("loading CUDA library: " OS_CUDA_LIB_NAME "\n"));
		os_unload_library(cuda_library_handle);
		cuda_library_handle = os_load_library((c8 *)path.data, OS_CUDA_LIB_TEMP_NAME, &err);
		#define X(name, symname) cuda_## name = os_lookup_dynamic_symbol(cuda_library_handle, symname, &err);
		CUDALibraryProcedureList
		#undef X

		os_write_file(os_error_handle(), stream_to_s8(&err));
	}

	#define X(name, symname) if (!cuda_## name) cuda_## name = cuda_ ## name ## _stub;
	CUDALibraryProcedureList
	#undef X

	return result;
}

function BeamformerRenderModel
render_model_from_arrays(f32 *vertices, f32 *normals, i32 vertices_size, u16 *indices, i32 index_count)
{
	BeamformerRenderModel result = {0};

	i32 buffer_size    = vertices_size * 2 + index_count * (i32)sizeof(u16);
	i32 indices_offset = vertices_size * 2;
	i32 indices_size   = index_count * (i32)sizeof(u16);

	result.elements        = index_count;
	result.elements_offset = indices_offset;

	glCreateBuffers(1, &result.buffer);
	glNamedBufferStorage(result.buffer, buffer_size, 0, GL_DYNAMIC_STORAGE_BIT);
	glNamedBufferSubData(result.buffer, 0,              vertices_size, vertices);
	glNamedBufferSubData(result.buffer, vertices_size,  vertices_size, normals);
	glNamedBufferSubData(result.buffer, indices_offset, indices_size,  indices);

	glCreateVertexArrays(1, &result.vao);
	glVertexArrayVertexBuffer(result.vao, 0, result.buffer, 0,             3 * sizeof(f32));
	glVertexArrayVertexBuffer(result.vao, 1, result.buffer, vertices_size, 3 * sizeof(f32));
	glVertexArrayElementBuffer(result.vao, result.buffer);

	glEnableVertexArrayAttrib(result.vao, 0);
	glEnableVertexArrayAttrib(result.vao, 1);

	glVertexArrayAttribFormat(result.vao, 0, 3, GL_FLOAT, 0, 0);
	glVertexArrayAttribFormat(result.vao, 1, 3, GL_FLOAT, 0, (u32)vertices_size);

	glVertexArrayAttribBinding(result.vao, 0, 0);
	glVertexArrayAttribBinding(result.vao, 1, 0);

	return result;
}

#define GLFW_VISIBLE 0x00020004
void glfwWindowHint(i32, i32);
iptr glfwCreateWindow(i32, i32, char *, iptr, iptr);
void glfwMakeContextCurrent(iptr);

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
		atomic_store_u32(&ctx->asleep, 1);
		os_wait_on_value(&ctx->sync_variable, 1, (u32)-1);
		atomic_store_u32(&ctx->asleep, 0);
	}
}

function OS_THREAD_ENTRY_POINT_FN(compute_worker_thread_entry_point)
{
	GLWorkerThreadContext *ctx = (GLWorkerThreadContext *)_ctx;

	glfwMakeContextCurrent(ctx->window_handle);
	ctx->gl_context = os_get_native_gl_context(ctx->window_handle);

	BeamformerCtx *beamformer = (BeamformerCtx *)ctx->user_context;
	glCreateQueries(GL_TIME_ELAPSED, countof(beamformer->compute_context.shader_timer_ids),
	                beamformer->compute_context.shader_timer_ids);

	for (;;) {
		worker_thread_sleep(ctx, beamformer->shared_memory.region);
		asan_poison_region(ctx->arena.beg, ctx->arena.end - ctx->arena.beg);
		beamformer_complete_compute(ctx->user_context, &ctx->arena, ctx->gl_context);
	}

	unreachable();

	return 0;
}

function OS_THREAD_ENTRY_POINT_FN(beamformer_upload_entry_point)
{
	GLWorkerThreadContext *ctx = (GLWorkerThreadContext *)_ctx;
	glfwMakeContextCurrent(ctx->window_handle);
	ctx->gl_context = os_get_native_gl_context(ctx->window_handle);

	BeamformerUploadThreadContext *up = (typeof(up))ctx->user_context;
	glCreateQueries(GL_TIMESTAMP, 1, &up->rf_buffer->data_timestamp_query);
	/* NOTE(rnp): start this here so we don't have to worry about it being started or not */
	glQueryCounter(up->rf_buffer->data_timestamp_query, GL_TIMESTAMP);

	for (;;) {
		worker_thread_sleep(ctx, up->shared_memory->region);
		beamformer_rf_upload(up);
	}

	unreachable();

	return 0;
}

function void
beamformer_init(BeamformerInput *input)
{
	Arena  memory        = input->memory;
	Arena  compute_arena = sub_arena_end(&memory, MB(2), KB(4));
	Arena  upload_arena  = sub_arena_end(&memory, KB(4), KB(4));
	Arena  ui_arena      = sub_arena_end(&memory, MB(2), KB(4));
	Stream error         = arena_stream(sub_arena_end(&memory, MB(1), 1));
	BeamformerCtx *ctx   = push_struct(&memory, BeamformerCtx);

	Arena scratch = {.beg = memory.end - 4096L, .end = memory.end};
	memory.end = scratch.beg;

	ctx->window_size = (iv2){{1280, 840}};
	ctx->error_stream = error;
	ctx->ui_backing_store = ui_arena;
	input->executable_reloaded = 1;

	ctx->compute_worker.arena  = compute_arena;
	ctx->compute_worker.asleep = 1;
	ctx->upload_worker.arena   = upload_arena;
	ctx->upload_worker.asleep  = 1;

	debug_init(ctx, input, &memory);

	SetConfigFlags(FLAG_VSYNC_HINT|FLAG_WINDOW_ALWAYS_RUN);
	InitWindow(ctx->window_size.w, ctx->window_size.h, "OGL Beamformer");
	/* NOTE: do this after initing so that the window starts out floating in tiling wm */
	SetWindowState(FLAG_WINDOW_RESIZABLE);
	SetWindowMinSize(840, ctx->window_size.h);

	glfwWindowHint(GLFW_VISIBLE, 0);
	iptr raylib_window_handle = (iptr)GetPlatformWindowHandle();

	load_gl(&ctx->error_stream);

	ctx->beamform_work_queue  = push_struct(&memory, BeamformWorkQueue);
	ctx->compute_shader_stats = push_struct(&memory, ComputeShaderStats);
	ctx->compute_timing_table = push_struct(&memory, ComputeTimingTable);

	/* TODO(rnp): I'm not sure if its a good idea to pre-reserve a bunch of semaphores
	 * on w32 but thats what we are doing for now */
	u32 lock_count = (u32)BeamformerSharedMemoryLockKind_Count + (u32)BeamformerMaxParameterBlockSlots;
	ctx->shared_memory = os_create_shared_memory_area(&memory, OS_SHARED_MEMORY_NAME, lock_count,
	                                                  BEAMFORMER_SHARED_MEMORY_SIZE);
	BeamformerSharedMemory *sm = ctx->shared_memory.region;
	if (!sm) os_fatal(s8("Get more ram lol\n"));
	mem_clear(sm, 0, sizeof(*sm));

	sm->version = BEAMFORMER_SHARED_MEMORY_VERSION;
	sm->reserved_parameter_blocks = 1;

	BeamformerComputeContext *cs = &ctx->compute_context;

	GLWorkerThreadContext *worker = &ctx->compute_worker;
	/* TODO(rnp): we should lock this down after we have something working */
	worker->user_context  = (iptr)ctx;
	worker->window_handle = glfwCreateWindow(1, 1, "", 0, raylib_window_handle);
	worker->handle        = os_create_thread((iptr)worker, compute_worker_thread_entry_point);
	os_set_thread_name(worker->handle, s8("[compute]"));

	GLWorkerThreadContext         *upload = &ctx->upload_worker;
	BeamformerUploadThreadContext *upctx  = push_struct(&memory, typeof(*upctx));
	upload->user_context = (iptr)upctx;
	upctx->rf_buffer     = &cs->rf_buffer;
	upctx->shared_memory = &ctx->shared_memory;
	upctx->compute_timing_table = ctx->compute_timing_table;
	upctx->compute_worker_sync  = &ctx->compute_worker.sync_variable;
	upload->window_handle = glfwCreateWindow(1, 1, "", 0, raylib_window_handle);
	upload->handle        = os_create_thread((iptr)upload, beamformer_upload_entry_point);
	os_set_thread_name(worker->handle, s8("[upload]"));

	glfwMakeContextCurrent(raylib_window_handle);

	if (load_cuda_library(s8(OS_CUDA_LIB_NAME), 0, memory))
		os_add_file_watch(s8(OS_CUDA_LIB_NAME), load_cuda_library, 0);

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
			s8 file = push_s8_from_parts(&temp, s8(OS_PATH_SEPARATOR), s8("shaders"),
			                             beamformer_reloadable_shader_files[index]);

			BeamformerShaderReloadIndirectContext *rsi = push_struct(&memory, typeof(*rsi));
			rsi->beamformer = ctx;
			rsi->shader     = beamformer_reloadable_shader_kinds[index];
			os_add_file_watch(file, reload_shader_indirect, (iptr)rsi);
			reload_shader_indirect(file, (iptr)rsi, memory);
		}
		os_wake_waiters(&worker->sync_variable);
	}

	FrameViewRenderContext *fvr = &ctx->frame_view_render_context;
	glCreateFramebuffers(countof(fvr->framebuffers), fvr->framebuffers);
	LABEL_GL_OBJECT(GL_FRAMEBUFFER, fvr->framebuffers[0], s8("Frame View Framebuffer"));
	LABEL_GL_OBJECT(GL_FRAMEBUFFER, fvr->framebuffers[1], s8("Frame View Resolving Framebuffer"));

	glCreateRenderbuffers(countof(fvr->renderbuffers), fvr->renderbuffers);
	i32 msaa_samples = gl_parameters.vendor_id == GLVendor_ARM? 4 : 8;
	glNamedRenderbufferStorageMultisample(fvr->renderbuffers[0], msaa_samples, GL_RGBA8,
	                                      FRAME_VIEW_RENDER_TARGET_SIZE);
	glNamedRenderbufferStorageMultisample(fvr->renderbuffers[1], msaa_samples, GL_DEPTH_COMPONENT24,
	                                      FRAME_VIEW_RENDER_TARGET_SIZE);

	static_assert(countof(beamformer_reloadable_render_shader_info_indices) == 1,
	              "only a single render shader is currently handled");
	i32 render_rsi_index = beamformer_reloadable_render_shader_info_indices[0];

	Arena *arena = BakeShaders? &scratch : &memory;
	ShaderReloadContext *render_3d = push_struct(arena, typeof(*render_3d));
	render_3d->beamformer_context    = ctx;
	render_3d->reloadable_info_index = render_rsi_index;
	render_3d->gl_type = GL_FRAGMENT_SHADER;
	render_3d->header  = s8(""
	"layout(location = 0) in  vec3 normal;\n"
	"layout(location = 1) in  vec3 texture_coordinate;\n\n"
	"layout(location = 2) in  vec3 test_texture_coordinate;\n\n"
	"layout(location = 0) out vec4 out_colour;\n\n"
	"layout(location = " str(FRAME_VIEW_DYNAMIC_RANGE_LOC) ") uniform float u_db_cutoff = 60;\n"
	"layout(location = " str(FRAME_VIEW_THRESHOLD_LOC)     ") uniform float u_threshold = 40;\n"
	"layout(location = " str(FRAME_VIEW_GAMMA_LOC)         ") uniform float u_gamma     = 1;\n"
	"layout(location = " str(FRAME_VIEW_LOG_SCALE_LOC)     ") uniform bool  u_log_scale;\n"
	"layout(location = " str(FRAME_VIEW_BB_COLOUR_LOC)     ") uniform vec4  u_bb_colour   = vec4(" str(FRAME_VIEW_BB_COLOUR) ");\n"
	"layout(location = " str(FRAME_VIEW_BB_FRACTION_LOC)   ") uniform float u_bb_fraction = " str(FRAME_VIEW_BB_FRACTION) ";\n"
	"layout(location = " str(FRAME_VIEW_SOLID_BB_LOC)      ") uniform bool  u_solid_bb;\n"
	"\n"
	"layout(binding = 0) uniform sampler3D u_texture;\n");

	render_3d->link = push_struct(arena, typeof(*render_3d));
	render_3d->link->reloadable_info_index = -1;
	render_3d->link->gl_type = GL_VERTEX_SHADER;
	render_3d->link->link    = render_3d;
	render_3d->link->header  = s8(""
	"layout(location = 0) in vec3 v_position;\n"
	"layout(location = 1) in vec3 v_normal;\n"
	"\n"
	"layout(location = 0) out vec3 f_normal;\n"
	"layout(location = 1) out vec3 f_texture_coordinate;\n"
	"layout(location = 2) out vec3 f_orig_texture_coordinate;\n"
	"\n"
	"layout(location = " str(FRAME_VIEW_MODEL_MATRIX_LOC)  ") uniform mat4  u_model;\n"
	"layout(location = " str(FRAME_VIEW_VIEW_MATRIX_LOC)   ") uniform mat4  u_view;\n"
	"layout(location = " str(FRAME_VIEW_PROJ_MATRIX_LOC)   ") uniform mat4  u_projection;\n"
	"\n"
	"\n"
	"void main()\n"
	"{\n"
	"\tvec3 pos = v_position;\n"
	"\tf_orig_texture_coordinate = (2 * v_position + 1) / 2;\n"
	//"\tif (v_position.y == -1) pos.x = clamp(v_position.x, -u_clip_fraction, u_clip_fraction);\n"
	"\tvec3 tex_coord = (2 * pos + 1) / 2;\n"
	"\tf_texture_coordinate = tex_coord.xzy;\n"
	//"\tf_texture_coordinate = u_swizzle? tex_coord.xzy : tex_coord;\n"
	//"\tf_normal    = normalize(mat3(u_model) * v_normal);\n"
	"\tf_normal    = v_normal;\n"
	"\tgl_Position = u_projection * u_view * u_model * vec4(pos, 1);\n"
	"}\n");

	s8 render_file = {0};
	if (!BakeShaders) {
		render_file = push_s8_from_parts(&scratch, s8(OS_PATH_SEPARATOR), s8("shaders"),
		                                 beamformer_reloadable_shader_files[render_rsi_index]);
		os_add_file_watch(render_file, reload_shader, (iptr)render_3d);
	}
	reload_shader(render_file, (iptr)render_3d, memory);

	f32 unit_cube_vertices[] = {
		 0.5f,  0.5f, -0.5f,
		 0.5f,  0.5f, -0.5f,
		 0.5f,  0.5f, -0.5f,
		 0.5f, -0.5f, -0.5f,
		 0.5f, -0.5f, -0.5f,
		 0.5f, -0.5f, -0.5f,
		 0.5f,  0.5f,  0.5f,
		 0.5f,  0.5f,  0.5f,
		 0.5f,  0.5f,  0.5f,
		 0.5f, -0.5f,  0.5f,
		 0.5f, -0.5f,  0.5f,
		 0.5f, -0.5f,  0.5f,
		-0.5f,  0.5f, -0.5f,
		-0.5f,  0.5f, -0.5f,
		-0.5f,  0.5f, -0.5f,
		-0.5f, -0.5f, -0.5f,
		-0.5f, -0.5f, -0.5f,
		-0.5f, -0.5f, -0.5f,
		-0.5f,  0.5f,  0.5f,
		-0.5f,  0.5f,  0.5f,
		-0.5f,  0.5f,  0.5f,
		-0.5f, -0.5f,  0.5f,
		-0.5f, -0.5f,  0.5f,
		-0.5f, -0.5f,  0.5f
	};
	f32 unit_cube_normals[] = {
		 0.0f,  0.0f, -1.0f,
		 0.0f,  1.0f,  0.0f,
		 1.0f,  0.0f,  0.0f,
		 0.0f,  0.0f, -1.0f,
		 0.0f, -1.0f,  0.0f,
		 1.0f,  0.0f,  0.0f,
		 0.0f,  0.0f,  1.0f,
		 0.0f,  1.0f,  0.0f,
		 1.0f,  0.0f,  0.0f,
		 0.0f,  0.0f,  1.0f,
		 0.0f, -1.0f,  0.0f,
		 1.0f,  0.0f,  0.0f,
		 0.0f,  0.0f, -1.0f,
		 0.0f,  1.0f,  0.0f,
		-1.0f,  0.0f,  0.0f,
		 0.0f,  0.0f, -1.0f,
		 0.0f, -1.0f,  0.0f,
		-1.0f,  0.0f,  0.0f,
		 0.0f,  0.0f,  1.0f,
		 0.0f,  1.0f,  0.0f,
		-1.0f,  0.0f,  0.0f,
		 0.0f,  0.0f,  1.0f,
		 0.0f, -1.0f,  0.0f,
		-1.0f,  0.0f,  0.0f
	};
	u16 unit_cube_indices[] = {
		1,  13, 19,
		1,  19, 7,
		9,  6,  18,
		9,  18, 21,
		23, 20, 14,
		23, 14, 17,
		16, 4,  10,
		16, 10, 22,
		5,  2,  8,
		5,  8,  11,
		15, 12, 0,
		15, 0,  3
	};

	cs->unit_cube_model = render_model_from_arrays(unit_cube_vertices, unit_cube_normals,
	                                               sizeof(unit_cube_vertices),
	                                               unit_cube_indices, countof(unit_cube_indices));

	memory.end = scratch.end;
	ctx->arena = memory;
}

function void
beamformer_invalidate_shared_memory(Arena memory)
{
	/* NOTE(rnp): work around pebkac when the beamformer is closed while we are doing live
	 * imaging. if the verasonics is blocked in an external function (calling the library
	 * to start compute) it is impossible for us to get it to properly shut down which
	 * will sometimes result in us needing to power cycle the system. set the shared memory
	 * into an error state and release dispatch lock so that future calls will error instead
	 * of blocking.
	 */
	BeamformerCtx *ctx = BeamformerContextMemory(memory);
	BeamformerSharedMemory *sm = ctx->shared_memory.region;
	BeamformerSharedMemoryLockKind lock = BeamformerSharedMemoryLockKind_DispatchCompute;
	atomic_store_u32(&sm->invalid, 1);
	atomic_store_u32(&sm->external_work_queue.ridx, sm->external_work_queue.widx);
	DEBUG_DECL(if (sm->locks[lock])) {
		os_shared_memory_region_unlock(&ctx->shared_memory, sm->locks, (i32)lock);
	}

	atomic_or_u32(&sm->live_imaging_dirty_flags, BeamformerLiveImagingDirtyFlags_StopImaging);
}
