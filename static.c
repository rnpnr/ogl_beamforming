/* See LICENSE for license details. */
#ifndef _DEBUG

#include "beamformer.c"
#define debug_init(...)

#else
static void *debug_lib;

static beamformer_frame_step_fn *beamformer_frame_step;

static FILE_WATCH_CALLBACK_FN(debug_reload)
{
	BeamformerInput *input = (BeamformerInput *)user_data;
	Stream err             = arena_stream(&tmp);

	os_unload_library(debug_lib);
	debug_lib = os_load_library(OS_DEBUG_LIB_NAME, OS_DEBUG_LIB_TEMP_NAME, &err);
	beamformer_frame_step = os_lookup_dynamic_symbol(debug_lib, "beamformer_frame_step", &err);
	os_write_err_msg(s8("Reloaded Main Executable\n"));
	input->executable_reloaded = 1;

	return 1;
}

static void
debug_init(Platform *p, iptr input, Arena *arena)
{
	p->add_file_watch(&p->file_watch_context, arena, s8(OS_DEBUG_LIB_NAME), debug_reload, input);
	debug_reload((s8){0}, input, *arena);
}

#endif /* _DEBUG */

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
	stream_append_s8(e, (s8){.len = len, .data = (u8 *)msg});
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
	switch (vendor[0]) {
	case 'A': gl->vendor_id = GL_VENDOR_AMD;    break;
	case 'I': gl->vendor_id = GL_VENDOR_INTEL;  break;
	case 'N': gl->vendor_id = GL_VENDOR_NVIDIA; break;
	/* NOTE(rnp): freedreno - might need different handling on win32 but this is fine for now */
	case 'f': gl->vendor_id = GL_VENDOR_ARM;    break;
	default:
		stream_append_s8(err, s8("Unknown GL Vendor: "));
		stream_append_s8(err, cstr_to_s8(vendor));
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
}

static void
validate_gl_requirements(GLParams *gl)
{
	ASSERT(gl->max_ubo_size >= sizeof(BeamformerParameters));
	/* NOTE: nVidia's driver seems to misreport the version */
	b32 invalid = 0;
	if (gl->version_major < 4)
		invalid = 1;

	switch (gl->vendor_id) {
	case GL_VENDOR_AMD:
	case GL_VENDOR_ARM:
	case GL_VENDOR_INTEL:
		if (gl->version_major == 4 && gl->version_minor < 5)
			invalid = 1;
		break;
	case GL_VENDOR_NVIDIA:
		if (gl->version_major == 4 && gl->version_minor < 3)
			invalid = 1;
		break;
	}

	if (invalid)
		os_fatal(s8("Only OpenGL Versions 4.5 or newer are supported!\n"));
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
	stream_append_s8(&s, s8("\nMax 3D Texture Dimension: "));
	stream_append_i64(&s, gl->max_3d_texture_dim);
	stream_append_s8(&s, s8("\nMax SSBO Size: "));
	stream_append_i64(&s, gl->max_ssbo_size);
	stream_append_s8(&s, s8("\nMax UBO Size: "));
	stream_append_i64(&s, gl->max_ubo_size);
	stream_append_s8(&s, s8("\n-----------------------\n"));
	if (!s.errors)
		os_write_err_msg(stream_to_s8(&s));
#endif
}

static u32
compile_shader(Arena a, u32 type, s8 shader)
{
	u32 sid = glCreateShader(type);
	glShaderSource(sid, 1, (const char **)&shader.data, (int *)&shader.len);
	glCompileShader(sid);

	i32 res = 0;
	glGetShaderiv(sid, GL_COMPILE_STATUS, &res);

	if (res == GL_FALSE) {
		char *stype;
		switch (type) {
		case GL_COMPUTE_SHADER:  stype = "Compute";  break;
		case GL_FRAGMENT_SHADER: stype = "Fragment"; break;
		}

		TraceLog(LOG_WARNING, "SHADER: [ID %u] %s shader failed to compile", sid, stype);
		i32 len = 0;
		glGetShaderiv(sid, GL_INFO_LOG_LENGTH, &len);
		s8 err = s8alloc(&a, len);
		glGetShaderInfoLog(sid, len, (int *)&err.len, (char *)err.data);
		TraceLog(LOG_WARNING, "SHADER: [ID %u] Compile error: %s", sid, (char *)err.data);
		glDeleteShader(sid);

		sid = 0;
	}

	return sid;
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

struct compute_shader_reload_ctx {
	BeamformerCtx *ctx;
	s8             label;
	u32            shader;
	b32            needs_header;
};

static FILE_WATCH_CALLBACK_FN(reload_compute_shader)
{
	struct compute_shader_reload_ctx *ctx = (struct compute_shader_reload_ctx *)user_data;
	ComputeShaderCtx *cs = &ctx->ctx->csctx;

	b32 result = 1;

	/* NOTE: arena works as stack (since everything here is 1 byte aligned) */
	s8 header_in_arena = {.data = tmp.beg};
	if (ctx->needs_header)
		header_in_arena = push_s8(&tmp, s8(COMPUTE_SHADER_HEADER));

	size fs = os_get_file_stats((c8 *)path.data).filesize;

	s8 shader_text    = os_read_file(&tmp, (c8 *)path.data, fs);
	shader_text.data -= header_in_arena.len;
	shader_text.len  += header_in_arena.len;
	ASSERT(shader_text.data == header_in_arena.data);

	u32 shader_id  = compile_shader(tmp, GL_COMPUTE_SHADER, shader_text);
	if (shader_id) {
		glDeleteProgram(cs->programs[ctx->shader]);
		cs->programs[ctx->shader] = rlLoadComputeShaderProgram(shader_id);
		glUseProgram(cs->programs[ctx->shader]);
		glBindBufferBase(GL_UNIFORM_BUFFER, 0, cs->shared_ubo);
		LABEL_GL_OBJECT(GL_PROGRAM, cs->programs[ctx->shader], ctx->label);

		TraceLog(LOG_INFO, "%s loaded", path.data);

		ctx->ctx->flags |= START_COMPUTE;
	} else {
		result = 0;
	}

	glDeleteShader(shader_id);

	return result;
}

static FILE_WATCH_CALLBACK_FN(load_cuda_lib)
{
	CudaLib *cl = (CudaLib *)user_data;
	b32 result  = 0;
	size fs = os_get_file_stats((c8 *)path.data).filesize;
	if (fs > 0) {
		TraceLog(LOG_INFO, "Loading CUDA lib: %s", OS_CUDA_LIB_NAME);

		Stream err = arena_stream(&tmp);
		os_unload_library(cl->lib);
		cl->lib = os_load_library((c8 *)path.data, OS_CUDA_LIB_TEMP_NAME, &err);
		#define X(name) cl->name = os_lookup_dynamic_symbol(cl->lib, #name, &err);
		CUDA_LIB_FNS
		#undef X

		result = 1;
	}

	#define X(name) if (!cl->name) cl->name = name ## _stub;
	CUDA_LIB_FNS
	#undef X

	return result;
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
	validate_gl_requirements(&ctx->gl);

	ctx->fsctx.db        = -50.0f;
	ctx->fsctx.threshold =  40.0f;

	ctx->params = os_open_shared_memory_area(OS_SMEM_NAME, sizeof(*ctx->params));
	/* TODO: properly handle this? */
	ASSERT(ctx->params);

	/* NOTE: default compute shader pipeline */
	ctx->params->compute_stages[0]    = CS_HADAMARD;
	ctx->params->compute_stages[1]    = CS_DAS;
	ctx->params->compute_stages[2]    = CS_MIN_MAX;
	ctx->params->compute_stages_count = 3;

	if (ctx->gl.vendor_id == GL_VENDOR_NVIDIA
	    && load_cuda_lib(s8(OS_CUDA_LIB_NAME), (iptr)&ctx->cuda_lib, *memory))
	{
		os_add_file_watch(&ctx->platform.file_watch_context, memory,
		                  s8(OS_CUDA_LIB_NAME), load_cuda_lib, (iptr)&ctx->cuda_lib);
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

	glGenQueries(ARRAY_COUNT(ctx->csctx.timer_fences) * CS_LAST, (u32 *)ctx->csctx.timer_ids);
	glGenQueries(ARRAY_COUNT(ctx->partial_compute_ctx.timer_ids), ctx->partial_compute_ctx.timer_ids);

	#define X(e, sn, f, nh, pretty_name) do if (s8(f).len > 0) {                       \
		struct compute_shader_reload_ctx *csr = push_struct(memory, typeof(*csr)); \
		csr->ctx          = ctx;                                                   \
		csr->label        = s8("CS_" #e);                                          \
		csr->shader       = sn;                                                    \
		csr->needs_header = nh;                                                    \
		reload_compute_shader(s8("shaders/" f ".glsl"), (iptr)csr, *memory);       \
		os_add_file_watch(&ctx->platform.file_watch_context, memory,               \
		                  s8("shaders/" f ".glsl"), reload_compute_shader,         \
		                  (iptr)csr);                                              \
	} while (0);
	COMPUTE_SHADERS
	#undef X

	reload_render_shader(s8("shaders/render.glsl"), (iptr)&ctx->fsctx, *memory);
	os_add_file_watch(&ctx->platform.file_watch_context, memory, s8("shaders/render.glsl"),
	                  reload_render_shader, (iptr)&ctx->fsctx);

	/* TODO(rnp): remove this */
	ComputeShaderCtx *csctx = &ctx->csctx;
	#define X(idx, name) csctx->name##_id = glGetUniformLocation(csctx->programs[idx], "u_" #name);
	CS_UNIFORMS
	#undef X
}
