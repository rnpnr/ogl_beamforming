/* See LICENSE for license details. */
#include "beamformer.h"

static f32 dt_for_frame;
static f32 cycle_t;

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
		/* TODO: select odim.x vs odim.y */
		ctx->fsctx.output = LoadRenderTexture(odim.x, odim.z);
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
	uv4 dec_data_dim         = bp->dec_data_dim;
	uv2 rf_raw_dim           = bp->rf_raw_dim;
	ctx->csctx.dec_data_dim  = dec_data_dim;
	ctx->csctx.rf_raw_dim    = rf_raw_dim;
	size rf_raw_size         = rf_raw_dim.x * rf_raw_dim.y * sizeof(i16);
	size rf_decoded_size     = decoded_data_size(cs);

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

	size full_rf_buf_size = ARRAY_COUNT(cs->raw_data_fences) * rf_raw_size;
	glDeleteBuffers(1, &cs->raw_data_ssbo);
	glCreateBuffers(1, &cs->raw_data_ssbo);
	glNamedBufferStorage(cs->raw_data_ssbo, full_rf_buf_size, 0, storage_flags);
	LABEL_GL_OBJECT(GL_BUFFER, cs->raw_data_ssbo, s8("Raw_Data_SSBO"));

	Stream label = stream_alloc(&a, 256);
	stream_append_s8(&label, s8("RF_SSBO_"));
	u32 s_widx  = label.widx;
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
		                                               full_rf_buf_size, map_flags);
		break;
	case GL_VENDOR_NVIDIA:
		cs->raw_data_arena = ctx->platform.alloc_arena(cs->raw_data_arena, full_rf_buf_size);
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

	if (ridx != widx) {
		result = q->work_items + ridx;
		atomic_add(&q->queue, 0x100000000ULL);
	}

	return result;
}

DEBUG_EXPORT BEAMFORMER_WORK_QUEUE_PUSH_FN(beamformer_work_queue_push)
{
	BeamformWork *result = 0;

	static_assert(ISPOWEROF2(ARRAY_COUNT(q->work_items)), "queue capacity must be a power of 2");
	u64 val  = atomic_load(&q->queue);
	u64 mask = ARRAY_COUNT(q->work_items) - 1;
	u32 widx = val       & mask;
	u32 ridx = val >> 32 & mask;
	u32 next = (widx + 1) & mask;

	if (val & 0x80000000) {
		atomic_and(&q->queue, ~0x80000000);
	}

	if (next != ridx) {
		result = q->work_items + widx;
		zero_struct(result);
		atomic_add(&q->queue, 1);
	}

	return result;
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
};

static struct compute_cursor
start_compute_cursor(uv3 dim, u32 max_points)
{
	struct compute_cursor result = {0};
	u32 invocations_per_dispatch = DAS_LOCAL_SIZE_X * DAS_LOCAL_SIZE_Y * DAS_LOCAL_SIZE_Z;

	result.dispatch.y = MAX(MIN(max_points / invocations_per_dispatch, dim.y), 1);

	u32 remaining     = max_points / result.dispatch.y;
	result.dispatch.x = MAX(remaining / invocations_per_dispatch, 1);
	result.dispatch.z = MAX(remaining / (invocations_per_dispatch * result.dispatch.x), 1);

	result.target.x = MAX(dim.x / result.dispatch.x / DAS_LOCAL_SIZE_X, 1);
	result.target.y = MAX(dim.y / result.dispatch.y / DAS_LOCAL_SIZE_Y, 1);
	result.target.z = MAX(dim.z / result.dispatch.z / DAS_LOCAL_SIZE_Z, 1);

	return result;
}

static iv3
step_compute_cursor(struct compute_cursor *cursor)
{
	iv3 result = cursor->cursor;
	result.x *= DAS_LOCAL_SIZE_X;
	result.y *= DAS_LOCAL_SIZE_Y;
	result.z *= DAS_LOCAL_SIZE_Z;

	cursor->cursor.x += 1;
	if (cursor->cursor.x >= cursor->target.x) {
		cursor->cursor.x  = 0;
		cursor->cursor.y += 1;
		if (cursor->cursor.y >= cursor->target.y) {
			cursor->cursor.y  = 0;
			cursor->cursor.z += 1;
		}
	}

	return result;
}

static b32
compute_cursor_finished(struct compute_cursor *cursor)
{
	b32 result = cursor->cursor.z > cursor->target.z;
	return result;
}

static void
do_compute_shader(BeamformerCtx *ctx, Arena arena, BeamformFrame *frame, u32 raw_data_index,
                  enum compute_shaders shader)
{
	ComputeShaderCtx *csctx = &ctx->csctx;
	uv2  rf_raw_dim         = ctx->params->raw.rf_raw_dim;
	size rf_raw_size        = rf_raw_dim.x * rf_raw_dim.y * sizeof(i16);

	glUseProgram(csctx->programs[shader]);

	u32 output_ssbo_idx = !csctx->last_output_ssbo_index;
	u32 input_ssbo_idx  = csctx->last_output_ssbo_index;

	switch (shader) {
	case CS_HADAMARD:
		glBindBufferRange(GL_SHADER_STORAGE_BUFFER, 1, csctx->raw_data_ssbo,
		                  raw_data_index * rf_raw_size, rf_raw_size);

		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, csctx->rf_data_ssbos[output_ssbo_idx]);
		glBindImageTexture(0, csctx->hadamard_texture, 0, GL_FALSE, 0, GL_READ_ONLY, GL_R8I);
		glDispatchCompute(ORONE(csctx->dec_data_dim.x / 32),
		                  ORONE(csctx->dec_data_dim.y / 32),
		                  ORONE(csctx->dec_data_dim.z));
		csctx->raw_data_fences[raw_data_index] = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
		csctx->last_output_ssbo_index = !csctx->last_output_ssbo_index;
		break;
	case CS_CUDA_DECODE:
		ctx->cuda_lib.cuda_decode(raw_data_index * rf_raw_size, output_ssbo_idx, 0);
		csctx->raw_data_fences[raw_data_index] = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
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
		/* TODO(rnp): compute max_points_per_dispatch baseed on something like a
		 * transmit_count * channel_count product */
		u32 max_points_per_dispatch = KB(64);
		struct compute_cursor cursor = start_compute_cursor(frame->dim, max_points_per_dispatch);
		for (iv3 offset = step_compute_cursor(&cursor);
		     !compute_cursor_finished(&cursor);
		     offset = step_compute_cursor(&cursor))
		{
			/* IMPORTANT(rnp): prevents OS from coalescing and killing our shader */
			glFinish();
			glUniform3iv(csctx->voxel_offset_id, 1, offset.E);
			glDispatchCompute(cursor.dispatch.x, cursor.dispatch.y, cursor.dispatch.z);
		}
		#else
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
compile_shader(Platform *platform, Arena a, u32 type, s8 shader)
{
	u32 sid = glCreateShader(type);
	glShaderSource(sid, 1, (const char **)&shader.data, (int *)&shader.len);
	glCompileShader(sid);

	i32 res = 0;
	glGetShaderiv(sid, GL_COMPILE_STATUS, &res);

	if (res == GL_FALSE) {
		Stream buf = arena_stream(&a);
		switch (type) {
		case GL_COMPUTE_SHADER:  stream_append_s8(&buf, s8("Compute"));  break;
		case GL_FRAGMENT_SHADER: stream_append_s8(&buf, s8("Fragment")); break;
		}
		stream_append_s8(&buf, s8(" shader failed to compile: "));

		i32 len = 0;
		glGetShaderiv(sid, GL_INFO_LOG_LENGTH, &len);
		s8 err = s8alloc(&a, len);
		glGetShaderInfoLog(sid, len, (int *)&err.len, (char *)err.data);
		stream_append_s8(&buf, err);
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
	s8 header_in_arena = {.data = tmp.beg};
	if (csr->needs_header)
		header_in_arena = push_s8(&tmp, s8(COMPUTE_SHADER_HEADER));

	s8 shader_text    = ctx->platform.read_whole_file(&tmp, (c8 *)path.data);
	shader_text.data -= header_in_arena.len;
	shader_text.len  += header_in_arena.len;

	if (shader_text.data == header_in_arena.data) {
		u32 shader_id  = compile_shader(&ctx->platform, tmp, GL_COMPUTE_SHADER, shader_text);
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

	if (ctx->csctx.programs[CS_DAS])
		glProgramUniform1f(ctx->csctx.programs[CS_DAS], ctx->csctx.cycle_t_id, cycle_t);

	while (work) {
		switch (work->type) {
		case BW_RELOAD_SHADER: {
			ComputeShaderReloadContext *csr = work->reload_shader_ctx;
			reload_compute_shader(ctx, csr->path, csr, arena);

			/* TODO(rnp): remove this */
			#define X(idx, name) cs->name##_id = glGetUniformLocation(cs->programs[idx], "u_" #name);
			CS_UNIFORMS
			#undef X
		} break;
		case BW_LOAD_RF_DATA: {
			iptr file_handle = work->file_handle;
			if (!uv4_equal(cs->dec_data_dim, bp->dec_data_dim)) {
				alloc_shader_storage(ctx, arena);
			}

			u32 raw_index = cs->raw_data_index;
			if (cs->raw_data_fences[raw_index]) {
				GLsync fence = cs->raw_data_fences[raw_index];
				i32 status   = glClientWaitSync(fence, 0, 0);
				if (status != GL_ALREADY_SIGNALED) {
					ctx->platform.write_file(ctx->platform.error_file_handle,
					                         s8("stall while loading RF data\n"));
					u64 timeout = ctx->gl.max_server_wait_time;
					for (;;) {
						status = glClientWaitSync(fence, 0, timeout);
						if (status == GL_CONDITION_SATISFIED ||
						    status == GL_ALREADY_SIGNALED)
						{
							break;
						}
					}
				}
				glDeleteSync(cs->raw_data_fences[raw_index]);
				cs->raw_data_fences[raw_index] = 0;
			}

			uv2  rf_raw_dim   = cs->rf_raw_dim;
			size rf_raw_size  = rf_raw_dim.x * rf_raw_dim.y * sizeof(i16);
			void *rf_data_buf = cs->raw_data_arena.beg + raw_index * rf_raw_size;

			size rlen = ctx->platform.read_file(file_handle, rf_data_buf, rf_raw_size);
			if (rlen != rf_raw_size) {
				stream_append_s8(&ctx->error_stream, s8("Partial Read Occurred: "));
				stream_append_i64(&ctx->error_stream, rlen);
				stream_append_byte(&ctx->error_stream, '/');
				stream_append_i64(&ctx->error_stream, rf_raw_size);
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
					glNamedBufferSubData(cs->raw_data_ssbo, raw_index * rlen,
					                     rlen, rf_data_buf);
				}
			}
			ctx->ready_for_rf = 1;
		} break;
		case BW_COMPLETE_FRAME: {
			BeamformFrame *frame = work->frame;
			for (u32 i = 0; i < ARRAY_COUNT(frame->timer_ids); i++) {
				u64 ns = 0;
				if (frame->timer_active[i]) {
					glGetQueryObjectui64v(frame->timer_ids[i], GL_QUERY_RESULT, &ns);
					frame->timer_active[i] = 0;
				}
				frame->compute_times[i] = (f32)ns / 1e9;
			}
			frame->ready_to_present = 1;
		} break;
		case BW_COMPUTE: {
			atomic_store(&cs->processing_compute, 1);
			BeamformFrame *frame = work->frame;
			if (ctx->params->upload) {
				glNamedBufferSubData(cs->shared_ubo, 0, sizeof(ctx->params->raw),
				                     &ctx->params->raw);
				ctx->params->upload = 0;
			}

			uv3 try_dim = ctx->params->raw.output_points.xyz;
			if (!uv3_equal(try_dim, frame->dim)) {
				size frame_index = frame - ctx->beamform_frames;
				alloc_beamform_frame(&ctx->gl, frame, try_dim, frame_index,
				                     s8("Beamformed_Data"));
			}

			frame->ready_to_present = 0;
			frame->min_coordinate   = ctx->params->raw.output_min_coordinate;
			frame->max_coordinate   = ctx->params->raw.output_max_coordinate;

			u32 stage_count = ctx->params->compute_stages_count;
			enum compute_shaders *stages = ctx->params->compute_stages;
			for (u32 i = 0; i < stage_count; i++) {
				frame->timer_active[stages[i]] = 1;
				glBeginQuery(GL_TIME_ELAPSED, frame->timer_ids[stages[i]]);
				do_compute_shader(ctx, arena, frame, cs->raw_data_index, stages[i]);
				glEndQuery(GL_TIME_ELAPSED);
			}

			frame->in_flight = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
			cs->processing_compute = 0;
		} break;
		case BW_SAVE_FRAME: {
			BeamformFrame *frame = ctx->beamform_frames + ctx->display_frame_index;
			if (frame->ready_to_present)
				export_frame(ctx, work->file_handle, frame);
		} break;
		}

		work = beamform_work_queue_pop(q);
	}
}

#include "ui.c"

DEBUG_EXPORT BEAMFORMER_FRAME_STEP_FN(beamformer_frame_step)
{
	dt_for_frame = GetFrameTime();

	cycle_t += dt_for_frame;
	if (cycle_t > 1) cycle_t -= 1;

	if (IsWindowResized()) {
		ctx->window_size.h = GetScreenHeight();
		ctx->window_size.w = GetScreenWidth();
	}

	if (input->executable_reloaded) {
		ui_init(ctx, ctx->ui_backing_store);
	}

	if (ctx->start_compute && !input->pipe_data_available) {
		if (ctx->beamform_frames[ctx->display_frame_index].ready_to_present) {
			BeamformWork *work = beamformer_work_queue_push(ctx->beamform_work_queue);
			if (work) {
				work->type  = BW_COMPUTE;
				work->frame = ctx->beamform_frames + ctx->next_render_frame_index++;
				if (ctx->next_render_frame_index >= ARRAY_COUNT(ctx->beamform_frames))
					ctx->next_render_frame_index = 0;
			}
		}
		ctx->platform.wake_thread(ctx->platform.compute_worker.sync_handle);
		ctx->start_compute = 0;
	}

	BeamformerParameters *bp = &ctx->params->raw;
	if (ctx->ready_for_rf && input->pipe_data_available) {
		BeamformWork *work = beamformer_work_queue_push(ctx->beamform_work_queue);
		if (work) {
			ctx->start_compute = 1;
			ctx->ready_for_rf  = 0;

			work->type        = BW_LOAD_RF_DATA;
			work->file_handle = input->pipe_handle;

			BeamformWork *compute = beamformer_work_queue_push(ctx->beamform_work_queue);
			if (compute) {
				compute->type  = BW_COMPUTE;
				compute->frame = ctx->beamform_frames + ctx->next_render_frame_index++;
				if (ctx->next_render_frame_index >= ARRAY_COUNT(ctx->beamform_frames))
					ctx->next_render_frame_index = 0;
			}

			if (ctx->params->upload) {
				/* TODO(rnp): clean this up */
				ctx->ui->read_params = 1;
			}

			if (ctx->params->export_next_frame) {
				BeamformWork *export = beamformer_work_queue_push(ctx->beamform_work_queue);
				if (export) {
					/* TODO: we don't really want the beamformer opening/closing files */
					iptr f = ctx->platform.open_for_write(ctx->params->export_pipe_name);
					export->type        = BW_SAVE_FRAME;
					export->file_handle = f;
				}
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
		if (frame->in_flight) {
			i32 status = glClientWaitSync(frame->in_flight, 0, 0);
			if (status == GL_ALREADY_SIGNALED || status == GL_CONDITION_SATISFIED) {
				glDeleteSync(frame->in_flight);
				frame->in_flight        = 0;
				frame->ready_to_present = 1;
				/* NOTE(rnp): frame is ready to present but the frame times
				 * can only by read in the work thread; they may show up late
				 * on the ui but that is ok! */
				/* TODO(rnp): high priority queue or work item */
				BeamformWork *work = beamformer_work_queue_push(ctx->beamform_work_queue);
				if (work) {
					work->frame = frame;
					work->type  = BW_COMPLETE_FRAME;
					ctx->start_compute = 1;
				}
				ctx->display_frame_index = (bfi.offset + bfi.cursor - 1) % bfi.capacity;
				ctx->fsctx.gen_mipmaps   = 1;
			}
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
