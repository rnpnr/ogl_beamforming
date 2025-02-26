/* See LICENSE for license details. */
#include "beamformer.h"

static f32 dt_for_frame;
static u32 cycle_t;

static size
decoded_data_size(ComputeShaderCtx *cs)
{
	uv4  dim    = cs->dec_data_dim;
	size result = 2 * sizeof(f32) * dim.x * dim.y * dim.z;
	return result;
}

static uv3
make_valid_test_dim(uv3 in)
{
	uv3 result;
	result.x = MAX(in.x, 1);
	result.y = MAX(in.y, 1);
	result.z = MAX(in.z, 1);
	return result;
}

static BeamformFrameIterator
beamform_frame_iterator(BeamformerCtx *ctx, i32 start_index, i32 stop_index)
{
	ASSERT(start_index < ARRAY_COUNT(ctx->beamform_frames));
	ASSERT(stop_index  < ARRAY_COUNT(ctx->beamform_frames));
	ASSERT(stop_index >= 0 || start_index >= 0);

	u32 needed_frames;
	if (stop_index < 0 || start_index < 0)
		needed_frames = ARRAY_COUNT(ctx->beamform_frames);
	else
		needed_frames = (u32)(stop_index - start_index) % ARRAY_COUNT(ctx->beamform_frames);

	if (start_index < 0)
		start_index = stop_index;

	BeamformFrameIterator result;
	result.frames        = ctx->beamform_frames;
	result.offset        = start_index;
	result.capacity      = ARRAY_COUNT(ctx->beamform_frames);
	result.cursor        = 0;
	result.needed_frames = needed_frames;
	return result;
}

static BeamformFrame *
frame_next_backwards(BeamformFrameIterator *bfi)
{
	BeamformFrame *result = 0;
	if (bfi->cursor != bfi->needed_frames) {
		u32 index = (bfi->offset - bfi->cursor++) % bfi->capacity;
		result    = bfi->frames + index;
	}
	return result;
}

static BeamformFrame *
frame_next_forwards(BeamformFrameIterator *bfi)
{
	BeamformFrame *result = 0;
	if (bfi->cursor != bfi->needed_frames) {
		u32 index = (bfi->offset + bfi->cursor++) % bfi->capacity;
		result    = bfi->frames + index;
	}
	return result;
}

static void
alloc_beamform_frame(GLParams *gp, BeamformFrame *out, uv3 out_dim, u32 frame_index, s8 name)
{
	out->dim.x = CLAMP(round_down_power_of_2(ORONE(out_dim.x)), 1, gp->max_3d_texture_dim);
	out->dim.y = CLAMP(round_down_power_of_2(ORONE(out_dim.y)), 1, gp->max_3d_texture_dim);
	out->dim.z = CLAMP(round_down_power_of_2(ORONE(out_dim.z)), 1, gp->max_3d_texture_dim);

	/* NOTE: allocate storage for beamformed output data;
	 * this is shared between compute and fragment shaders */
	u32 max_dim = MAX(out->dim.x, MAX(out->dim.y, out->dim.z));
	out->mips   = ctz_u32(max_dim) + 1;

	/* TODO(rnp): arena?? */
	u8 buf[256];
	Stream label = {.data = buf, .cap = ARRAY_COUNT(buf)};
	stream_append_s8(&label, name);
	stream_append_byte(&label, '[');
	stream_append_u64(&label, frame_index);
	stream_append_s8(&label, s8("]"));

	glDeleteTextures(1, &out->texture);
	glCreateTextures(GL_TEXTURE_3D, 1, &out->texture);
	glTextureStorage3D(out->texture, out->mips, GL_RG32F, out->dim.x, out->dim.y, out->dim.z);
	LABEL_GL_OBJECT(GL_TEXTURE, out->texture, stream_to_s8(&label));

	glDeleteQueries(ARRAY_COUNT(out->timer_ids), out->timer_ids);
	glCreateQueries(GL_TIME_ELAPSED, ARRAY_COUNT(out->timer_ids), out->timer_ids);
}

static void
alloc_output_image(BeamformerCtx *ctx, uv3 output_dim)
{
	uv3 try_dim = make_valid_test_dim(output_dim);
	if (!uv3_equal(try_dim, ctx->averaged_frame.dim)) {
		alloc_beamform_frame(&ctx->gl, &ctx->averaged_frame, try_dim, 0,
		                     s8("Beamformed_Averaged_Data"));
		uv3 odim = ctx->averaged_frame.dim;

		UnloadRenderTexture(ctx->fsctx.output);
		/* TODO(rnp): sometimes when accepting data on w32 something happens
		 * and the program will stall in vprintf in TraceLog(...) here.
		 * for now do this to avoid the problem */
		SetTraceLogLevel(LOG_NONE);
		/* TODO: select odim.x vs odim.y */
		ctx->fsctx.output = LoadRenderTexture(odim.x, odim.z);
		SetTraceLogLevel(LOG_INFO);
		LABEL_GL_OBJECT(GL_FRAMEBUFFER, ctx->fsctx.output.id, s8("Rendered_View"));
		GenTextureMipmaps(&ctx->fsctx.output.texture);
		//SetTextureFilter(ctx->fsctx.output.texture, TEXTURE_FILTER_ANISOTROPIC_8X);
		//SetTextureFilter(ctx->fsctx.output.texture, TEXTURE_FILTER_TRILINEAR);
		SetTextureFilter(ctx->fsctx.output.texture, TEXTURE_FILTER_BILINEAR);

		/* NOTE(rnp): work around raylib's janky texture sampling */
		i32 id = ctx->fsctx.output.texture.id;
		glTextureParameteri(id, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
		glTextureParameteri(id, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);

		f32 border_color[] = {0, 0, 0, 1};
		glTextureParameterfv(id, GL_TEXTURE_BORDER_COLOR, border_color);
	}
}

static void
alloc_shader_storage(BeamformerCtx *ctx, Arena a)
{
	ComputeShaderCtx *cs     = &ctx->csctx;
	BeamformerParameters *bp = &ctx->params->raw;

	uv4 dec_data_dim = bp->dec_data_dim;
	u32 rf_raw_size  = ctx->params->raw_data_size;
	cs->dec_data_dim = dec_data_dim;
	cs->rf_raw_size  = rf_raw_size;

	glDeleteBuffers(ARRAY_COUNT(cs->rf_data_ssbos), cs->rf_data_ssbos);
	glCreateBuffers(ARRAY_COUNT(cs->rf_data_ssbos), cs->rf_data_ssbos);

	i32 storage_flags = GL_DYNAMIC_STORAGE_BIT;
	switch (ctx->gl.vendor_id) {
	case GL_VENDOR_AMD:
	case GL_VENDOR_ARM:
	case GL_VENDOR_INTEL:
		if (cs->raw_data_ssbo)
			glUnmapNamedBuffer(cs->raw_data_ssbo);
		storage_flags |= GL_MAP_WRITE_BIT|GL_MAP_PERSISTENT_BIT;
	case GL_VENDOR_NVIDIA:
		/* NOTE: register_cuda_buffers will handle the updated ssbo */
		break;
	}

	glDeleteBuffers(1, &cs->raw_data_ssbo);
	glCreateBuffers(1, &cs->raw_data_ssbo);
	glNamedBufferStorage(cs->raw_data_ssbo, rf_raw_size, 0, storage_flags);
	LABEL_GL_OBJECT(GL_BUFFER, cs->raw_data_ssbo, s8("Raw_RF_SSBO"));

	size rf_decoded_size = decoded_data_size(cs);
	Stream label = stream_alloc(&a, 256);
	stream_append_s8(&label, s8("Decoded_RF_SSBO_"));
	u32 s_widx = label.widx;
	for (u32 i = 0; i < ARRAY_COUNT(cs->rf_data_ssbos); i++) {
		glNamedBufferStorage(cs->rf_data_ssbos[i], rf_decoded_size, 0, 0);
		stream_append_u64(&label, i);
		s8 rf_label = stream_to_s8(&label);
		LABEL_GL_OBJECT(GL_BUFFER, cs->rf_data_ssbos[i], rf_label);
		label.widx = s_widx;
	}

	i32 map_flags = GL_MAP_WRITE_BIT|GL_MAP_PERSISTENT_BIT|GL_MAP_UNSYNCHRONIZED_BIT;
	switch (ctx->gl.vendor_id) {
	case GL_VENDOR_AMD:
	case GL_VENDOR_ARM:
	case GL_VENDOR_INTEL:
		cs->raw_data_arena.beg = glMapNamedBufferRange(cs->raw_data_ssbo, 0,
		                                               rf_raw_size, map_flags);
		cs->raw_data_arena.end = cs->raw_data_arena.beg + rf_raw_size;
		break;
	case GL_VENDOR_NVIDIA:
		cs->raw_data_arena = ctx->platform.alloc_arena(cs->raw_data_arena, rf_raw_size);
		ctx->cuda_lib.register_cuda_buffers(cs->rf_data_ssbos, ARRAY_COUNT(cs->rf_data_ssbos),
		                                    cs->raw_data_ssbo);
		ctx->cuda_lib.init_cuda_configuration(bp->rf_raw_dim.E, bp->dec_data_dim.E,
		                                      bp->channel_mapping);
		break;
	}

	/* NOTE: store hadamard in GPU once; it won't change for a particular imaging session */
	size hadamard_elements = dec_data_dim.z * dec_data_dim.z;
	i32  *hadamard         = alloc(&a, i32, hadamard_elements);
	i32  *tmp              = alloc(&a, i32, hadamard_elements);
	fill_hadamard_transpose(hadamard, tmp, dec_data_dim.z);
	glDeleteTextures(1, &cs->hadamard_texture);
	glCreateTextures(GL_TEXTURE_2D, 1, &cs->hadamard_texture);
	glTextureStorage2D(cs->hadamard_texture, 1, GL_R8I, dec_data_dim.z, dec_data_dim.z);
	glTextureSubImage2D(cs->hadamard_texture, 0, 0, 0, dec_data_dim.z, dec_data_dim.z,
	                    GL_RED_INTEGER, GL_INT, hadamard);
	LABEL_GL_OBJECT(GL_TEXTURE, cs->hadamard_texture, s8("Hadamard_Matrix"));
}

static BeamformWork *
beamform_work_queue_pop(BeamformWorkQueue *q)
{
	BeamformWork *result = 0;

	static_assert(ISPOWEROF2(ARRAY_COUNT(q->work_items)), "queue capacity must be a power of 2");
	u64 val  = atomic_load(&q->queue);
	u64 mask = ARRAY_COUNT(q->work_items) - 1;
	u32 widx = val       & mask;
	u32 ridx = val >> 32 & mask;

	if (ridx != widx)
		result = q->work_items + ridx;

	return result;
}

static void
beamform_work_queue_pop_commit(BeamformWorkQueue *q)
{
	atomic_add(&q->queue, 0x100000000ULL);
}

DEBUG_EXPORT BEAMFORM_WORK_QUEUE_PUSH_FN(beamform_work_queue_push)
{
	BeamformWork *result = 0;

	static_assert(ISPOWEROF2(ARRAY_COUNT(q->work_items)), "queue capacity must be a power of 2");
	u64 val  = atomic_load(&q->queue);
	u64 mask = ARRAY_COUNT(q->work_items) - 1;
	u32 widx = val       & mask;
	u32 ridx = val >> 32 & mask;
	u32 next = (widx + 1) & mask;

	if (val & 0x80000000)
		atomic_and(&q->queue, ~0x80000000);

	if (next != ridx) {
		result = q->work_items + widx;
		zero_struct(result);
	}

	return result;
}

DEBUG_EXPORT BEAMFORM_WORK_QUEUE_PUSH_COMMIT_FN(beamform_work_queue_push_commit)
{
	atomic_add(&q->queue, 1);
}

static void
export_frame(BeamformerCtx *ctx, iptr handle, BeamformFrame *frame)
{
	uv3 dim            = frame->dim;
	size out_size      = dim.x * dim.y * dim.z * 2 * sizeof(f32);
	ctx->export_buffer = ctx->platform.alloc_arena(ctx->export_buffer, out_size);
	glGetTextureImage(frame->texture, 0, GL_RG, GL_FLOAT, out_size, ctx->export_buffer.beg);
	s8 raw = {.len = out_size, .data = ctx->export_buffer.beg};
	if (!ctx->platform.write_file(handle, raw))
		ctx->platform.write_file(ctx->platform.error_file_handle, s8("failed to export frame\n"));
	ctx->platform.close(handle);
}

static void
do_sum_shader(ComputeShaderCtx *cs, u32 *in_textures, u32 in_texture_count, f32 in_scale,
              u32 out_texture, uv3 out_data_dim)
{
	/* NOTE: zero output before summing */
	glClearTexImage(out_texture, 0, GL_RED, GL_FLOAT, 0);

	glBindImageTexture(0, out_texture, 0, GL_TRUE, 0, GL_READ_WRITE, GL_RG32F);
	glUniform1f(cs->sum_prescale_id, in_scale);
	for (u32 i = 0; i < in_texture_count; i++) {
		glBindImageTexture(1, in_textures[i], 0, GL_TRUE, 0, GL_READ_ONLY, GL_RG32F);
		glDispatchCompute(ORONE(out_data_dim.x / 32),
		                  ORONE(out_data_dim.y),
		                  ORONE(out_data_dim.z / 32));
		glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
	}
}

struct compute_cursor {
	iv3 cursor;
	iv3 dispatch;
	iv3 target;
	u32 points_per_dispatch;
	u32 completed_points;
	u32 total_points;
};

static struct compute_cursor
start_compute_cursor(uv3 dim, u32 max_points)
{
	struct compute_cursor result = {0};
	u32 invocations_per_dispatch = DAS_LOCAL_SIZE_X * DAS_LOCAL_SIZE_Y * DAS_LOCAL_SIZE_Z;

	result.dispatch.y = MIN(max_points / invocations_per_dispatch, MAX(dim.y / DAS_LOCAL_SIZE_Y, 1));

	u32 remaining     = max_points / result.dispatch.y;
	result.dispatch.x = MIN(remaining / invocations_per_dispatch, MAX(dim.x / DAS_LOCAL_SIZE_X, 1));
	result.dispatch.z = MIN(remaining / (invocations_per_dispatch * result.dispatch.x),
	                        MAX(dim.z / DAS_LOCAL_SIZE_Z, 1));

	result.target.x = MAX(dim.x / result.dispatch.x / DAS_LOCAL_SIZE_X, 1);
	result.target.y = MAX(dim.y / result.dispatch.y / DAS_LOCAL_SIZE_Y, 1);
	result.target.z = MAX(dim.z / result.dispatch.z / DAS_LOCAL_SIZE_Z, 1);

	result.points_per_dispatch = 1;
	result.points_per_dispatch *= result.dispatch.x * DAS_LOCAL_SIZE_X;
	result.points_per_dispatch *= result.dispatch.y * DAS_LOCAL_SIZE_Y;
	result.points_per_dispatch *= result.dispatch.z * DAS_LOCAL_SIZE_Z;

	result.total_points = dim.x * dim.y * dim.z;

	return result;
}

static iv3
step_compute_cursor(struct compute_cursor *cursor)
{
	cursor->cursor.x += 1;
	if (cursor->cursor.x >= cursor->target.x) {
		cursor->cursor.x  = 0;
		cursor->cursor.y += 1;
		if (cursor->cursor.y >= cursor->target.y) {
			cursor->cursor.y  = 0;
			cursor->cursor.z += 1;
		}
	}

	cursor->completed_points += cursor->points_per_dispatch;

	iv3 result = cursor->cursor;
	result.x *= cursor->dispatch.x * DAS_LOCAL_SIZE_X;
	result.y *= cursor->dispatch.y * DAS_LOCAL_SIZE_Y;
	result.z *= cursor->dispatch.z * DAS_LOCAL_SIZE_Z;

	return result;
}

static b32
compute_cursor_finished(struct compute_cursor *cursor)
{
	b32 result = cursor->completed_points >= cursor->total_points;
	return result;
}

static void
do_compute_shader(BeamformerCtx *ctx, Arena arena, BeamformFrame *frame, enum compute_shaders shader)
{
	ComputeShaderCtx *csctx = &ctx->csctx;

	glUseProgram(csctx->programs[shader]);

	u32 output_ssbo_idx = !csctx->last_output_ssbo_index;
	u32 input_ssbo_idx  = csctx->last_output_ssbo_index;

	switch (shader) {
	case CS_DECODE:
	case CS_DECODE_FLOAT:
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, csctx->raw_data_ssbo);
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, csctx->rf_data_ssbos[output_ssbo_idx]);
		glBindImageTexture(0, csctx->hadamard_texture, 0, GL_FALSE, 0, GL_READ_ONLY, GL_R8I);
		glDispatchCompute(ORONE(csctx->dec_data_dim.x / 32),
		                  ORONE(csctx->dec_data_dim.y / 32),
		                  ORONE(csctx->dec_data_dim.z));
		csctx->last_output_ssbo_index = !csctx->last_output_ssbo_index;
		break;
	case CS_CUDA_DECODE:
		ctx->cuda_lib.cuda_decode(0, output_ssbo_idx, 0);
		csctx->last_output_ssbo_index = !csctx->last_output_ssbo_index;
		break;
	case CS_CUDA_HILBERT:
		ctx->cuda_lib.cuda_hilbert(input_ssbo_idx, output_ssbo_idx);
		csctx->last_output_ssbo_index = !csctx->last_output_ssbo_index;
		break;
	case CS_DEMOD:
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, csctx->rf_data_ssbos[input_ssbo_idx]);
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, csctx->rf_data_ssbos[output_ssbo_idx]);
		glDispatchCompute(ORONE(csctx->dec_data_dim.x / 32),
		                  ORONE(csctx->dec_data_dim.y / 32),
		                  ORONE(csctx->dec_data_dim.z));
		csctx->last_output_ssbo_index = !csctx->last_output_ssbo_index;
		break;
	case CS_MIN_MAX: {
		u32 texture = frame->texture;
		for (u32 i = 1; i < frame->mips; i++) {
			glBindImageTexture(0, texture, i - 1, GL_TRUE, 0, GL_READ_ONLY,  GL_RG32F);
			glBindImageTexture(1, texture, i - 0, GL_TRUE, 0, GL_WRITE_ONLY, GL_RG32F);
			glUniform1i(csctx->mips_level_id, i);

			u32 width  = frame->dim.x >> i;
			u32 height = frame->dim.y >> i;
			u32 depth  = frame->dim.z >> i;
			glDispatchCompute(ORONE(width / 32), ORONE(height), ORONE(depth / 32));
			glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
		}
	} break;
	case CS_DAS: {
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, csctx->rf_data_ssbos[input_ssbo_idx]);
		glBindImageTexture(0, frame->texture, 0, GL_TRUE, 0, GL_WRITE_ONLY, GL_RG32F);

		#if 1
		/* TODO(rnp): compute max_points_per_dispatch based on something like a
		 * transmit_count * channel_count product */
		u32 max_points_per_dispatch = KB(64);
		struct compute_cursor cursor = start_compute_cursor(frame->dim, max_points_per_dispatch);
		f32 percent_per_step = (f32)cursor.points_per_dispatch / (f32)cursor.total_points;
		csctx->processing_progress = -percent_per_step;
		for (iv3 offset = {0};
		     !compute_cursor_finished(&cursor);
		     offset = step_compute_cursor(&cursor))
		{
			csctx->processing_progress += percent_per_step;
			/* IMPORTANT(rnp): prevents OS from coalescing and killing our shader */
			glFinish();
			glUniform3iv(csctx->voxel_offset_id, 1, offset.E);
			glDispatchCompute(cursor.dispatch.x, cursor.dispatch.y, cursor.dispatch.z);
		}
		#else
		/* NOTE(rnp): use this for testing tiling code. The performance of the above path
		 * should be the same as this path if everything is working correctly */
		iv3 compute_dim_offset = {0};
		glUniform3iv(csctx->voxel_offset_id, 1, compute_dim_offset.E);
		glDispatchCompute(ORONE(frame->dim.x / 32),
		                  ORONE(frame->dim.y),
		                  ORONE(frame->dim.z / 32));
		#endif
	} break;
	case CS_SUM: {
		u32 frame_count  = 0;
		u32 *in_textures = alloc(&arena, u32, MAX_BEAMFORMED_SAVED_FRAMES);
		BeamformFrameIterator bfi = beamform_frame_iterator(ctx, ctx->display_frame_index,
		                                                    ctx->params->raw.output_points.w);
		for (BeamformFrame *frame = frame_next_backwards(&bfi);
		     frame;
		     frame = frame_next_backwards(&bfi))
		{
			in_textures[frame_count++] = frame->texture;
		}
		do_sum_shader(csctx, in_textures, frame_count, 1 / (f32)frame_count,
		              ctx->averaged_frame.texture, ctx->averaged_frame.dim);
	} break;
	default: ASSERT(0);
	}
}

static u32
compile_shader(Platform *platform, Arena a, u32 type, s8 shader, s8 name)
{
	u32 sid = glCreateShader(type);
	glShaderSource(sid, 1, (const char **)&shader.data, (int *)&shader.len);
	glCompileShader(sid);

	i32 res = 0;
	glGetShaderiv(sid, GL_COMPILE_STATUS, &res);

	if (res == GL_FALSE) {
		Stream buf = arena_stream(&a);
		stream_append_s8(&buf, name);
		stream_append_s8(&buf, s8(": failed to compile\n"));

		i32 len = 0, out_len = 0;
		glGetShaderiv(sid, GL_INFO_LOG_LENGTH, &len);
		glGetShaderInfoLog(sid, len, &out_len, (char *)(buf.data + buf.widx));
		buf.widx += out_len;
		glDeleteShader(sid);
		platform->write_file(platform->error_file_handle, stream_to_s8(&buf));

		sid = 0;
	}

	return sid;
}

static u32
link_program(Platform *platform, Arena a, u32 shader_id)
{
	i32 success = 0;
	u32 result  = glCreateProgram();
	glAttachShader(result, shader_id);
	glLinkProgram(result);
	glGetProgramiv(result, GL_LINK_STATUS, &success);
	if (success == GL_FALSE) {
		i32 len    = 0;
		Stream buf = arena_stream(&a);
		stream_append_s8(&buf, s8("shader link error: "));
		glGetProgramInfoLog(result, buf.cap - buf.widx, &len, (c8 *)(buf.data + buf.widx));
		buf.widx = len;
		stream_append_byte(&buf, '\n');
		platform->write_file(platform->error_file_handle, stream_to_s8(&buf));
		glDeleteProgram(result);
		result = 0;
	}
	return result;
}

static void
reload_compute_shader(BeamformerCtx *ctx, s8 path, ComputeShaderReloadContext *csr, Arena tmp)
{
	ComputeShaderCtx *cs = &ctx->csctx;

	/* NOTE: arena works as stack (since everything here is 1 byte aligned) */
	s8 header = {.data = tmp.beg};
	if (csr->needs_header) {
		header = push_s8(&tmp, s8(COMPUTE_SHADER_HEADER));
		if (csr->shader == CS_DECODE_FLOAT) {
			s8 extra = push_s8(&tmp, s8(COMPUTE_FLOAT_DECODE_HEADER));
			ASSERT(extra.data == header.data + header.len);
			header.len += extra.len;
		}
	}

	s8 shader_text = ctx->platform.read_whole_file(&tmp, (c8 *)path.data);
	shader_text.data -= header.len;
	shader_text.len  += header.len;

	if (shader_text.data == header.data) {
		u32 shader_id  = compile_shader(&ctx->platform, tmp, GL_COMPUTE_SHADER, shader_text, path);
		if (shader_id) {
			u32 new_program = link_program(&ctx->platform, tmp, shader_id);
			if (new_program) {
				Stream buf = arena_stream(&tmp);
				stream_append_s8(&buf, s8("loaded: "));
				stream_append_s8(&buf, path);
				stream_append_byte(&buf, '\n');
				ctx->platform.write_file(ctx->platform.error_file_handle,
				                         stream_to_s8(&buf));
				glDeleteProgram(cs->programs[csr->shader]);
				cs->programs[csr->shader] = new_program;
				glUseProgram(cs->programs[csr->shader]);
				glBindBufferBase(GL_UNIFORM_BUFFER, 0, cs->shared_ubo);
				LABEL_GL_OBJECT(GL_PROGRAM, cs->programs[csr->shader], csr->label);
			}
		}

		glDeleteShader(shader_id);
	} else {
		Stream buf = arena_stream(&tmp);
		stream_append_s8(&buf, s8("failed to load: "));
		stream_append_s8(&buf, path);
		stream_append_byte(&buf, '\n');
		ctx->platform.write_file(ctx->platform.error_file_handle, stream_to_s8(&buf));
		/* TODO(rnp): return an error and don't let the work item calling this function
		 * call pop off the queue; store a retry count and only fail after multiple tries */
	}
}

DEBUG_EXPORT BEAMFORMER_COMPLETE_COMPUTE_FN(beamformer_complete_compute)
{
	BeamformerCtx *ctx   = (BeamformerCtx *)user_context;
	BeamformWorkQueue *q = ctx->beamform_work_queue;
	BeamformWork *work   = beamform_work_queue_pop(q);
	ComputeShaderCtx *cs = &ctx->csctx;

	BeamformerParameters *bp = &ctx->params->raw;

	while (work) {
		switch (work->type) {
		case BW_RELOAD_SHADER: {
			ComputeShaderReloadContext *csr = work->reload_shader_ctx;
			reload_compute_shader(ctx, csr->path, csr, arena);
			if (csr->shader == CS_DECODE) {
				csr->shader = CS_DECODE_FLOAT;
				reload_compute_shader(ctx, csr->path, csr, arena);
				csr->shader = CS_DECODE;
			}

			/* TODO(rnp): remove this */
			#define X(idx, name) cs->name##_id = glGetUniformLocation(cs->programs[idx], "u_" #name);
			CS_UNIFORMS
			#undef X
		} break;
		case BW_LOAD_RF_DATA: {
			if (cs->rf_raw_size != ctx->params->raw_data_size ||
			    !uv4_equal(cs->dec_data_dim, bp->dec_data_dim))
			{
				alloc_shader_storage(ctx, arena);
			}

			void *rf_data_buf = cs->raw_data_arena.beg;
			size rlen = ctx->platform.read_file(work->file_handle, rf_data_buf,
			                                    cs->rf_raw_size);
			if (rlen != cs->rf_raw_size) {
				stream_append_s8(&ctx->error_stream, s8("Partial Read Occurred: "));
				stream_append_i64(&ctx->error_stream, rlen);
				stream_append_byte(&ctx->error_stream, '/');
				stream_append_i64(&ctx->error_stream, cs->rf_raw_size);
				stream_append_byte(&ctx->error_stream, '\n');
				ctx->platform.write_file(ctx->platform.error_file_handle,
				                         stream_to_s8(&ctx->error_stream));
				ctx->error_stream.widx = 0;
			} else {
				switch (ctx->gl.vendor_id) {
				case GL_VENDOR_AMD:
				case GL_VENDOR_ARM:
				case GL_VENDOR_INTEL:
					break;
				case GL_VENDOR_NVIDIA:
					glNamedBufferSubData(cs->raw_data_ssbo, 0, rlen, rf_data_buf);
				}
			}
			ctx->ready_for_rf = 1;
		} break;
		case BW_COMPUTE: {
			atomic_store(&cs->processing_compute, 1);
			BeamformFrame *frame = work->frame;
			if (ctx->params->upload) {
				glNamedBufferSubData(cs->shared_ubo, 0, sizeof(ctx->params->raw),
				                     &ctx->params->raw);
				ctx->params->upload = 0;
			}

			if (cs->programs[CS_DAS])
				glProgramUniform1ui(cs->programs[CS_DAS], cs->cycle_t_id, cycle_t++);

			uv3 try_dim = ctx->params->raw.output_points.xyz;
			if (!uv3_equal(try_dim, frame->dim)) {
				size frame_index = frame - ctx->beamform_frames;
				alloc_beamform_frame(&ctx->gl, frame, try_dim, frame_index,
				                     s8("Beamformed_Data"));
			}

			frame->in_flight      = 1;
			frame->min_coordinate = ctx->params->raw.output_min_coordinate;
			frame->max_coordinate = ctx->params->raw.output_max_coordinate;

			u32 stage_count = ctx->params->compute_stages_count;
			enum compute_shaders *stages = ctx->params->compute_stages;
			for (u32 i = 0; i < stage_count; i++) {
				frame->timer_active[stages[i]] = 1;
				glBeginQuery(GL_TIME_ELAPSED, frame->timer_ids[stages[i]]);
				do_compute_shader(ctx, arena, frame, stages[i]);
				glEndQuery(GL_TIME_ELAPSED);
			}
			/* NOTE(rnp): block until work completes so that we can record timings */
			glFinish();
			cs->processing_progress = 1;

			for (u32 i = 0; i < ARRAY_COUNT(frame->timer_ids); i++) {
				u64 ns = 0;
				if (frame->timer_active[i]) {
					glGetQueryObjectui64v(frame->timer_ids[i], GL_QUERY_RESULT, &ns);
					frame->timer_active[i] = 0;
				}
				frame->compute_times[i] = (f32)ns / 1e9;
			}

			frame->ready_to_present = 1;
			cs->processing_compute  = 0;
		} break;
		case BW_SAVE_FRAME: {
			BeamformFrame *frame = work->output_frame_ctx.frame;
			ASSERT(frame->ready_to_present);
			export_frame(ctx, work->output_frame_ctx.file_handle, frame);
		} break;
		}

		beamform_work_queue_pop_commit(q);
		work = beamform_work_queue_pop(q);
	}
}

#include "ui.c"

DEBUG_EXPORT BEAMFORMER_FRAME_STEP_FN(beamformer_frame_step)
{
	dt_for_frame = GetFrameTime();

	if (IsWindowResized()) {
		ctx->window_size.h = GetScreenHeight();
		ctx->window_size.w = GetScreenWidth();
	}

	if (input->executable_reloaded) {
		ui_init(ctx, ctx->ui_backing_store);
	}

	if (ctx->start_compute && !input->pipe_data_available) {
		if (ctx->beamform_frames[ctx->display_frame_index].ready_to_present) {
			BeamformWork *work = beamform_work_queue_push(ctx->beamform_work_queue);
			if (work) {
				/* TODO(rnp): cleanup all the duplicates of this */
				work->type  = BW_COMPUTE;
				work->frame = ctx->beamform_frames + ctx->next_render_frame_index++;
				work->frame->ready_to_present = 0;
				if (ctx->next_render_frame_index >= ARRAY_COUNT(ctx->beamform_frames))
					ctx->next_render_frame_index = 0;
				beamform_work_queue_push_commit(ctx->beamform_work_queue);
			}
		}
		ctx->platform.wake_thread(ctx->platform.compute_worker.sync_handle);
		ctx->start_compute = 0;
	}

	BeamformerParameters *bp = &ctx->params->raw;
	if (ctx->ready_for_rf && input->pipe_data_available) {
		BeamformWork *work = beamform_work_queue_push(ctx->beamform_work_queue);
		if (work) {
			ctx->start_compute = 1;
			ctx->ready_for_rf  = 0;

			work->type        = BW_LOAD_RF_DATA;
			work->file_handle = input->pipe_handle;
			beamform_work_queue_push_commit(ctx->beamform_work_queue);

			BeamformWork *compute = beamform_work_queue_push(ctx->beamform_work_queue);
			if (compute) {
				compute->type  = BW_COMPUTE;
				compute->frame = ctx->beamform_frames + ctx->next_render_frame_index++;
				compute->frame->ready_to_present = 0;
				if (ctx->next_render_frame_index >= ARRAY_COUNT(ctx->beamform_frames))
					ctx->next_render_frame_index = 0;
				beamform_work_queue_push_commit(ctx->beamform_work_queue);

				if (ctx->params->export_next_frame) {
					BeamformWork *export = beamform_work_queue_push(ctx->beamform_work_queue);
					if (export) {
						/* TODO: we don't really want the beamformer opening/closing files */
						iptr f = ctx->platform.open_for_write(ctx->params->export_pipe_name);
						export->type = BW_SAVE_FRAME;
						export->output_frame_ctx.file_handle = f;
						export->output_frame_ctx.frame       = compute->frame;
						beamform_work_queue_push_commit(ctx->beamform_work_queue);
					}
					ctx->params->export_next_frame = 0;
				}
			}

			if (ctx->params->upload) {
				/* TODO(rnp): clean this up */
				ctx->ui->read_params = 1;
			}

			alloc_output_image(ctx, bp->output_points.xyz);
		}
	}

	BeamformFrameIterator bfi = beamform_frame_iterator(ctx, ctx->display_frame_index,
	                                                    ctx->next_render_frame_index);
	for (BeamformFrame *frame = frame_next_forwards(&bfi);
	     frame;
	     frame = frame_next_forwards(&bfi))
	{
		if (frame->in_flight && frame->ready_to_present) {
			frame->in_flight         = 0;
			ctx->display_frame_index = (bfi.offset + bfi.cursor - 1) % bfi.capacity;
			ctx->fsctx.gen_mipmaps   = 1;
		}
	}

	if (ctx->start_compute) {
		ctx->start_compute = 0;
		ctx->platform.wake_thread(ctx->platform.compute_worker.sync_handle);
	}

	/* NOTE: draw output image texture using render fragment shader */
	BeamformFrame *frame_to_draw = 0;
	BeginTextureMode(ctx->fsctx.output);
		ClearBackground(PINK);
		BeginShaderMode(ctx->fsctx.shader);
			FragmentShaderCtx *fs = &ctx->fsctx;
			glUseProgram(fs->shader.id);
			u32 out_texture = 0;
			if (bp->output_points.w > 1) {
				frame_to_draw = &ctx->averaged_frame;
				out_texture   = ctx->averaged_frame.texture;
			} else {
				frame_to_draw = ctx->beamform_frames + ctx->display_frame_index;
				out_texture   = frame_to_draw->ready_to_present ? frame_to_draw->texture : 0;
			}
			glBindTextureUnit(0, out_texture);
			glUniform1f(fs->db_cutoff_id, fs->db);
			glUniform1f(fs->threshold_id, fs->threshold);
			DrawTexture(fs->output.texture, 0, 0, WHITE);
		EndShaderMode();
	EndTextureMode();

	/* NOTE: regenerate mipmaps only when the output has actually changed */
	if (ctx->fsctx.gen_mipmaps) {
		/* NOTE: shut up raylib's reporting on mipmap gen */
		SetTraceLogLevel(LOG_NONE);
		GenTextureMipmaps(&ctx->fsctx.output.texture);
		SetTraceLogLevel(LOG_INFO);
		ctx->fsctx.gen_mipmaps = 0;
	}

	draw_ui(ctx, input, frame_to_draw);

	if (WindowShouldClose())
		ctx->should_exit = 1;
}
