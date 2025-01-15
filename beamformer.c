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

static uv4
make_valid_test_dim(uv4 in)
{
	uv4 result;
	result.x = MAX(in.x, 1);
	result.y = MAX(in.y, 1);
	result.z = MAX(in.z, 1);
	result.w = 1;
	return result;
}

static BeamformFrameIterator
beamform_frame_iterator(BeamformerCtx *ctx)
{
	BeamformFrameIterator result;
	result.frames        = ctx->beamform_frames;
	result.offset        = ctx->displayed_frame_index;
	result.capacity      = ARRAY_COUNT(ctx->beamform_frames);
	result.cursor        = 0;
	result.needed_frames = ORONE(ctx->params->raw.output_points.w);
	return result;
}

static BeamformFrame *
frame_next(BeamformFrameIterator *bfi)
{
	BeamformFrame *result = 0;
	if (bfi->cursor != bfi->needed_frames) {
		u32 index = (bfi->offset - bfi->cursor++) % bfi->capacity;
		result    = bfi->frames + index;
	}
	return result;
}

static void
alloc_beamform_frame(GLParams *gp, BeamformFrame *out, uv4 out_dim, u32 frame_index, s8 name)
{
	glDeleteTextures(out->dim.w, out->textures);

	out->dim.x = CLAMP(round_down_power_of_2(ORONE(out_dim.x)), 1, gp->max_3d_texture_dim);
	out->dim.y = CLAMP(round_down_power_of_2(ORONE(out_dim.y)), 1, gp->max_3d_texture_dim);
	out->dim.z = CLAMP(round_down_power_of_2(ORONE(out_dim.z)), 1, gp->max_3d_texture_dim);
	out->dim.w = CLAMP(out_dim.w, 0, MAX_MULTI_XDC_COUNT);

	/* NOTE: allocate storage for beamformed output data;
	 * this is shared between compute and fragment shaders */
	u32 max_dim = MAX(out->dim.x, MAX(out->dim.y, out->dim.z));
	out->mips   = ctz_u32(max_dim) + 1;

	u8 buf[256];
	Stream label = {.data = buf, .cap = ARRAY_COUNT(buf)};
	stream_append_s8(&label, name);
	stream_append_byte(&label, '[');
	stream_append_u64(&label, frame_index);
	stream_append_s8(&label, s8("]["));
	u32 sidx = label.widx;

	glCreateTextures(GL_TEXTURE_3D, out->dim.w, out->textures);
	for (u32 i = 0; i < out->dim.w; i++) {
		glTextureStorage3D(out->textures[i], out->mips, GL_RG32F,
		                   out->dim.x, out->dim.y, out->dim.z);
		stream_append_u64(&label, i);
		stream_append_byte(&label, ']');
		LABEL_GL_OBJECT(GL_TEXTURE, out->textures[i], stream_to_s8(&label));
		label.widx = sidx;
	}
}

static void
alloc_output_image(BeamformerCtx *ctx, uv4 output_dim)
{
	uv4 try_dim = make_valid_test_dim(output_dim);
	if (!uv4_equal(try_dim, ctx->averaged_frame.dim)) {
		alloc_beamform_frame(&ctx->gl, &ctx->averaged_frame, try_dim, 0,
		                     s8("Beamformed_Averaged_Data"));
		uv4 odim = ctx->averaged_frame.dim;

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
	cs->hadamard_dim       = (uv2){.x = dec_data_dim.z, .y = dec_data_dim.z};
	size hadamard_elements = dec_data_dim.z * dec_data_dim.z;
	i32  *hadamard         = alloc(&a, i32, hadamard_elements);
	i32  *tmp              = alloc(&a, i32, hadamard_elements);
	fill_hadamard_transpose(hadamard, tmp, dec_data_dim.z);
	glDeleteBuffers(1, &cs->hadamard_ssbo);
	glCreateBuffers(1, &cs->hadamard_ssbo);
	glNamedBufferStorage(cs->hadamard_ssbo, hadamard_elements * sizeof(i32), hadamard, 0);
	LABEL_GL_OBJECT(GL_BUFFER, cs->hadamard_ssbo, s8("Hadamard_SSBO"));
}

static BeamformWork *
beamform_work_queue_pop(BeamformWorkQueue *q)
{
	BeamformWork *result = q->first;
	if (result) {
		switch (result->type) {
		case BW_FULL_COMPUTE:
		case BW_RECOMPUTE:
		case BW_PARTIAL_COMPUTE:
			/* NOTE: only one compute is allowed per frame */
			if (q->did_compute_this_frame) {
				result = 0;
			} else {
				q->compute_in_flight--;
				q->did_compute_this_frame = 1;
				ASSERT(q->compute_in_flight >= 0);
			}
			break;
		}
	}
	/* NOTE: only do this once we have determined if we are doing the work */
	if (result) {
		q->first = result->next;
		if (result == q->last) {
			ASSERT(result->next == 0);
			q->last = 0;
		}
	}

	return result;
}

static BeamformWork *
beamform_work_queue_push(BeamformerCtx *ctx, Arena *a, enum beamform_work work_type)
{
	/* TODO: we should have a sub arena specifically for this purpose */

	BeamformWorkQueue *q = &ctx->beamform_work_queue;
	ComputeShaderCtx *cs = &ctx->csctx;

	BeamformWork *result = q->next_free;
	if (result) q->next_free = result->next;
	else        result = alloc(a, typeof(*result), 1);

	if (result) {
		result->type = work_type;
		result->next = 0;

		switch (work_type) {
		case BW_FULL_COMPUTE:
			if (q->compute_in_flight >= ARRAY_COUNT(cs->raw_data_fences)) {
				result->next = q->next_free;
				q->next_free = result;
				result       = 0;
				break;
			}
			cs->raw_data_index++;
			if (cs->raw_data_index >= ARRAY_COUNT(cs->raw_data_fences))
				cs->raw_data_index = 0;
			/* FALLTHROUGH */
		case BW_RECOMPUTE: {
			i32 raw_index = cs->raw_data_index;
			result->compute_ctx.raw_data_ssbo_index = raw_index;
			/* NOTE: if this times out it means the command queue is more than 3
			 * frames behind. In that case we need to re-evaluate the buffer size */
			if (cs->raw_data_fences[raw_index]) {
				i32 result = glClientWaitSync(cs->raw_data_fences[raw_index], 0,
				                              10000);
				if (result == GL_TIMEOUT_EXPIRED) {
					//ASSERT(0);
				}
				glDeleteSync(cs->raw_data_fences[raw_index]);
				cs->raw_data_fences[raw_index] = NULL;
			}
			ctx->displayed_frame_index++;
			if (ctx->displayed_frame_index >= ARRAY_COUNT(ctx->beamform_frames))
				ctx->displayed_frame_index = 0;
			result->compute_ctx.frame = ctx->beamform_frames + ctx->displayed_frame_index;
			result->compute_ctx.first_pass = 1;

			BeamformFrameIterator bfi = beamform_frame_iterator(ctx);
			for (BeamformFrame *frame = frame_next(&bfi); frame; frame = frame_next(&bfi)) {
				uv4 try_dim = ctx->params->raw.output_points;
				try_dim.w   = ctx->params->raw.xdc_count;
				if (!uv4_equal(frame->dim, try_dim)) {
					u32 index = (bfi.offset - bfi.cursor) % bfi.capacity;
					alloc_beamform_frame(&ctx->gl, frame, try_dim, index,
					                     s8("Beamformed_Data"));
				}
			}
		} /* FALLTHROUGH */
		case BW_PARTIAL_COMPUTE:
			q->compute_in_flight++;
		case BW_SAVE_FRAME:
		case BW_SEND_FRAME:
		case BW_SSBO_COPY:
			break;
		}

		if (result) {
			if (q->last) q->last = q->last->next = result;
			else         q->last = q->first      = result;
		}
	}

	return result;
}

static m4
v3_to_xdc_space(v3 direction, v3 origin, v3 corner1, v3 corner2)
{
	v3 edge1      = sub_v3(corner1, origin);
	v3 edge2      = sub_v3(corner2, origin);
	v3 xdc_normal = cross(edge1, edge2);
	if (xdc_normal.z < 0)
		xdc_normal = cross(edge2, edge1);
	ASSERT(xdc_normal.z >= 0);

	v3 e1 = normalize_v3(sub_v3(direction, xdc_normal));
	v3 e2 = {.y = 1};
	v3 e3 = normalize_v3(cross(e2, e1));
	v4 e4 = {.x = -origin.x, .y = -origin.y, .z = -origin.z, .w = 1};

	m4 result = {
		.c[0] = (v4){.x = e3.x, .y = e2.x, .z = e1.x, .w = 0},
		.c[1] = (v4){.x = e3.y, .y = e2.y, .z = e1.y, .w = 0},
		.c[2] = (v4){.x = e3.z, .y = e2.z, .z = e1.z, .w = 0},
		.c[3] = e4,
	};

	return result;
}

static v4
f32_4_to_v4(f32 *in)
{
	v4 result;
	store_f32x4(load_f32x4(in), result.E);
	return result;
}

static void
export_frame(BeamformerCtx *ctx, iptr handle, BeamformFrame *frame)
{
	uv3 dim            = frame->dim.xyz;
	size out_size      = dim.x * dim.y * dim.z * 2 * sizeof(f32);
	ctx->export_buffer = ctx->platform.alloc_arena(ctx->export_buffer, out_size);
	u32 texture        = frame->textures[frame->dim.w - 1];
	glGetTextureImage(texture, 0, GL_RG, GL_FLOAT, out_size, ctx->export_buffer.beg);
	s8 raw = {.len = out_size, .data = ctx->export_buffer.beg};
	if (!ctx->platform.write_file(handle, raw))
		TraceLog(LOG_WARNING, "failed to export frame\n");
	ctx->platform.close(handle);
}

static void
do_sum_shader(ComputeShaderCtx *cs, u32 *in_textures, u32 in_texture_count, f32 in_scale,
              u32 out_texture, uv4 out_data_dim)
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

static void
do_beamform_shader(ComputeShaderCtx *cs, BeamformerParameters *bp, BeamformFrame *frame,
                   u32 rf_ssbo, iv3 dispatch_dim, iv3 compute_dim_offset, i32 compute_pass)
{
	glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, rf_ssbo);
	glUniform3iv(cs->volume_export_dim_offset_id, 1, compute_dim_offset.E);
	glUniform1i(cs->volume_export_pass_id, compute_pass);

	for (u32 i = 0; i < frame->dim.w; i++) {
		u32 texture = frame->textures[i];
		m4 xdc_transform = v3_to_xdc_space((v3){.z = 1},
		                                   f32_4_to_v4(bp->xdc_origin  + (4 * i)).xyz,
		                                   f32_4_to_v4(bp->xdc_corner1 + (4 * i)).xyz,
		                                   f32_4_to_v4(bp->xdc_corner2 + (4 * i)).xyz);
		glBindImageTexture(0, texture, 0, GL_TRUE, 0, GL_WRITE_ONLY, GL_RG32F);
		glUniform1i(cs->xdc_index_id, i);
		glUniformMatrix4fv(cs->xdc_transform_id, 1, GL_FALSE, xdc_transform.E);
		glDispatchCompute(ORONE(dispatch_dim.x / 32),
		                  ORONE(dispatch_dim.y),
		                  ORONE(dispatch_dim.z / 32));
	}
}

static b32
do_partial_compute_step(BeamformerCtx *ctx, BeamformFrame *frame)
{
	ComputeShaderCtx  *cs = &ctx->csctx;
	PartialComputeCtx *pc = &ctx->partial_compute_ctx;

	b32 done = 0;

	/* NOTE: we start this elsewhere on the first dispatch so that we can include
	 * times such as decoding/demodulation/etc. */
	if (!pc->timer_active) {
		glQueryCounter(pc->timer_ids[0], GL_TIMESTAMP);
		pc->timer_active = 1;
	}

	glBeginQuery(GL_TIME_ELAPSED, cs->timer_ids[cs->timer_index][pc->shader]);
	cs->timer_active[cs->timer_index][pc->shader] = 1;

	glUseProgram(cs->programs[pc->shader]);

	/* NOTE: We must tile this otherwise GL will kill us for taking too long */
	/* TODO: this could be based on multiple dimensions */
	i32 dispatch_count = frame->dim.z / 32;
	iv3 dim_offset = {.z = !!dispatch_count * 32 * pc->dispatch_index++};
	iv3 dispatch_dim = {.x = frame->dim.x, .y = frame->dim.y, .z = 1};
	do_beamform_shader(cs, &ctx->params->raw, frame, pc->rf_data_ssbo, dispatch_dim, dim_offset, 1);

	if (pc->dispatch_index >= dispatch_count) {
		pc->dispatch_index  = 0;
		done                = 1;
	}

	glQueryCounter(pc->timer_ids[1], GL_TIMESTAMP);

	glEndQuery(GL_TIME_ELAPSED);

	return done;
}

static void
do_compute_shader(BeamformerCtx *ctx, Arena arena, BeamformFrame *frame, u32 raw_data_index,
                  enum compute_shaders shader)
{
	ComputeShaderCtx *csctx = &ctx->csctx;
	uv2  rf_raw_dim         = ctx->params->raw.rf_raw_dim;
	size rf_raw_size        = rf_raw_dim.x * rf_raw_dim.y * sizeof(i16);

	glBeginQuery(GL_TIME_ELAPSED, csctx->timer_ids[csctx->timer_index][shader]);
	csctx->timer_active[csctx->timer_index][shader] = 1;

	glUseProgram(csctx->programs[shader]);

	u32 output_ssbo_idx = !csctx->last_output_ssbo_index;
	u32 input_ssbo_idx  = csctx->last_output_ssbo_index;

	switch (shader) {
	case CS_HADAMARD:
		glBindBufferRange(GL_SHADER_STORAGE_BUFFER, 1, csctx->raw_data_ssbo,
		                  raw_data_index * rf_raw_size, rf_raw_size);

		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, csctx->rf_data_ssbos[output_ssbo_idx]);
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 3, csctx->hadamard_ssbo);
		glDispatchCompute(ORONE(csctx->dec_data_dim.x / 32),
		                  ORONE(csctx->dec_data_dim.y / 32),
		                  ORONE(csctx->dec_data_dim.z));
		csctx->raw_data_fences[raw_data_index] = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
		csctx->last_output_ssbo_index = !csctx->last_output_ssbo_index;
		break;
	case CS_CUDA_DECODE:
		ctx->cuda_lib.cuda_decode(raw_data_index * rf_raw_size, output_ssbo_idx,
		                          ctx->params->raw.channel_offset);
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
		u32 texture = frame->textures[frame->dim.w - 1];
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
		u32 rf_ssbo      = csctx->rf_data_ssbos[input_ssbo_idx];
		iv3 dispatch_dim = {.x = frame->dim.x, .y = frame->dim.y, .z = frame->dim.z};
		do_beamform_shader(csctx, &ctx->params->raw, frame, rf_ssbo, dispatch_dim, (iv3){0}, 0);
		if (frame->dim.w > 1) {
			glUseProgram(csctx->programs[CS_SUM]);
			u32 input_texture_count = frame->dim.w - 1;
			do_sum_shader(csctx, frame->textures, input_texture_count,
			              1 / (f32)input_texture_count, frame->textures[frame->dim.w - 1],
			              frame->dim);
		}
	} break;
	case CS_SUM: {
		u32 frame_count  = 0;
		u32 *in_textures = alloc(&arena, u32, MAX_BEAMFORMED_SAVED_FRAMES);
		BeamformFrameIterator bfi = beamform_frame_iterator(ctx);
		for (BeamformFrame *frame = frame_next(&bfi); frame; frame = frame_next(&bfi)) {
			ASSERT(frame->dim.w);
			in_textures[frame_count++] = frame->textures[frame->dim.w - 1];
		}
		do_sum_shader(csctx, in_textures, frame_count, 1 / (f32)frame_count,
		              ctx->averaged_frame.textures[0], ctx->averaged_frame.dim);
	} break;
	default: ASSERT(0);
	}

	glEndQuery(GL_TIME_ELAPSED);
}

static BeamformFrame *
start_beamform_compute_work(BeamformWork *work, ComputeShaderCtx *cs, BeamformerParametersFull *bpf)
{
	BeamformFrame *result = work->compute_ctx.frame;
	if (bpf->upload) {
		glNamedBufferSubData(cs->shared_ubo, 0, sizeof(bpf->raw), &bpf->raw);
		bpf->upload = 0;
	}

	result->min_coordinate = bpf->raw.output_min_coordinate;
	result->max_coordinate = bpf->raw.output_max_coordinate;

	return result;
}

static void
do_beamform_work(BeamformerCtx *ctx, Arena *a)
{
	BeamformWorkQueue *q = &ctx->beamform_work_queue;
	BeamformWork *work   = beamform_work_queue_pop(q);
	ComputeShaderCtx *cs = &ctx->csctx;

	while (work) {
		switch (work->type) {
		case BW_PARTIAL_COMPUTE: {
			BeamformFrame *frame = work->compute_ctx.frame;

			if (work->compute_ctx.first_pass) {
				start_beamform_compute_work(work, cs, ctx->params);

				PartialComputeCtx *pc = &ctx->partial_compute_ctx;
				pc->runtime      = 0;
				pc->timer_active = 1;
				glQueryCounter(pc->timer_ids[0], GL_TIMESTAMP);
				glDeleteBuffers(1, &pc->rf_data_ssbo);
				glCreateBuffers(1, &pc->rf_data_ssbo);
				glNamedBufferStorage(pc->rf_data_ssbo, decoded_data_size(cs), 0, 0);
				LABEL_GL_OBJECT(GL_BUFFER, pc->rf_data_ssbo, s8("Volume_RF_SSBO"));

				/* TODO: maybe we should have some concept of compute shader
				 * groups, then we could define a group that does the decoding
				 * and filtering and apply that group directly here. For now
				 * we will do this dumb thing */
				u32 stage_count = ctx->params->compute_stages_count;
				enum compute_shaders *stages = ctx->params->compute_stages;
				for (u32 i = 0; i < stage_count; i++) {
					if (stages[i] == CS_DAS) {
						ctx->partial_compute_ctx.shader = stages[i];
						break;
					}
					do_compute_shader(ctx, *a, frame,
					                  work->compute_ctx.raw_data_ssbo_index,
					                  stages[i]);
				}
				u32 output_ssbo = pc->rf_data_ssbo;
				u32 input_ssbo  = cs->rf_data_ssbos[cs->last_output_ssbo_index];
				size rf_size    = decoded_data_size(cs);
				glCopyNamedBufferSubData(input_ssbo, output_ssbo, 0, 0, rf_size);
			}

			b32 done = do_partial_compute_step(ctx, frame);
			if (!done) {
				BeamformWork *new;
				/* NOTE: this push must not fail */
				new = beamform_work_queue_push(ctx, a, BW_PARTIAL_COMPUTE);
				new->compute_ctx.first_pass    = 0;
				new->compute_ctx.frame         = frame;
				new->compute_ctx.export_handle = work->compute_ctx.export_handle;
			} else if (work->compute_ctx.export_handle != INVALID_FILE) {
				export_frame(ctx, work->compute_ctx.export_handle, frame);
				work->compute_ctx.export_handle = INVALID_FILE;
				/* NOTE: do not waste a bunch of GPU space holding onto the volume
				 * texture if it was just for export */
				glDeleteTextures(frame->dim.w, frame->textures);
				mem_clear(frame, 0, sizeof(*frame));
			}
		} break;
		case BW_FULL_COMPUTE:
		case BW_RECOMPUTE: {
			BeamformFrame *frame = start_beamform_compute_work(work, cs, ctx->params);

			u32 stage_count = ctx->params->compute_stages_count;
			enum compute_shaders *stages = ctx->params->compute_stages;
			for (u32 i = 0; i < stage_count; i++)
				do_compute_shader(ctx, *a, frame, work->compute_ctx.raw_data_ssbo_index,
					          stages[i]);

			if (work->compute_ctx.export_handle != INVALID_FILE) {
				export_frame(ctx, work->compute_ctx.export_handle, frame);
				work->compute_ctx.export_handle = INVALID_FILE;
			}

			ctx->fsctx.gen_mipmaps = 1;
		} break;
		}


		work->next   = q->next_free;
		q->next_free = work;
		work = beamform_work_queue_pop(q);
	}

	if (q->did_compute_this_frame) {
		u32 tidx = ctx->csctx.timer_index;
		glDeleteSync(ctx->csctx.timer_fences[tidx]);
		ctx->csctx.timer_fences[tidx] = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
		ctx->csctx.timer_index = (tidx + 1) % ARRAY_COUNT(ctx->csctx.timer_fences);
	}
}

static void
check_compute_timers(ComputeShaderCtx *cs, PartialComputeCtx *pc, BeamformerParametersFull *bp)
{
	/* NOTE: volume generation running timer */
	if (pc->timer_active) {
		u64 start_ns = 0, end_ns = 0;
		glGetQueryObjectui64v(pc->timer_ids[0], GL_QUERY_RESULT, &start_ns);
		glGetQueryObjectui64v(pc->timer_ids[1], GL_QUERY_RESULT, &end_ns);
		u64 elapsed_ns    = end_ns - start_ns;
		pc->runtime      += (f32)elapsed_ns * 1e-9;
		pc->timer_active  = 0;
	}

	/* NOTE: main timers for display portion of the program */
	u32 last_idx = (cs->timer_index - 1) % ARRAY_COUNT(cs->timer_fences);
	if (!cs->timer_fences[last_idx])
		return;

	i32 status = glClientWaitSync(cs->timer_fences[last_idx], 0, 0);
	if (status == GL_TIMEOUT_EXPIRED || status == GL_WAIT_FAILED)
		return;
	glDeleteSync(cs->timer_fences[last_idx]);
	cs->timer_fences[last_idx] = NULL;

	for (u32 i = 0; i < bp->compute_stages_count; i++) {
		u64 ns = 0;
		i32 idx = bp->compute_stages[i];
		if (cs->timer_active[last_idx][idx]) {
			glGetQueryObjectui64v(cs->timer_ids[last_idx][idx], GL_QUERY_RESULT, &ns);
			cs->timer_active[last_idx][idx] = 0;
		}
		cs->last_frame_time[idx] = (f32)ns / 1e9;
	}
}

#include "ui.c"

DEBUG_EXPORT BEAMFORMER_FRAME_STEP_FN(beamformer_frame_step)
{
	dt_for_frame = GetFrameTime();

	cycle_t += dt_for_frame;
	if (cycle_t > 1) cycle_t -= 1;
	glProgramUniform1f(ctx->csctx.programs[CS_DAS], ctx->csctx.cycle_t_id, cycle_t);

	if (IsWindowResized()) {
		ctx->window_size.h = GetScreenHeight();
		ctx->window_size.w = GetScreenWidth();
	}

	if (input->executable_reloaded) {
		ui_init(ctx, ctx->ui_backing_store);
	}

	if (ctx->flags & START_COMPUTE) {
		if (ui_can_start_compute(ctx))
			ui_start_compute(ctx);
		ctx->flags &= ~START_COMPUTE;
	}

	/* NOTE: Store the compute time for the last frame. */
	check_compute_timers(&ctx->csctx, &ctx->partial_compute_ctx, ctx->params);

	BeamformerParameters *bp = &ctx->params->raw;
	/* NOTE: Check for and Load RF Data into GPU */
	if (input->pipe_data_available) {
		BeamformWork *work = beamform_work_queue_push(ctx, arena, BW_FULL_COMPUTE);
		/* NOTE: we can only read in the new data if we get back a work item.
		 * otherwise we have too many frames in flight and should wait until the
		 * next frame to try again */
		if (work) {
			ComputeShaderCtx *cs = &ctx->csctx;
			if (!uv4_equal(cs->dec_data_dim, bp->dec_data_dim)) {
				alloc_shader_storage(ctx, *arena);
				/* TODO: we may need to invalidate all queue items here */
			}

			if (ctx->params->export_next_frame) {
				/* TODO: we don't really want the beamformer opening/closing files */
				iptr f = ctx->platform.open_for_write(ctx->params->export_pipe_name);
				work->compute_ctx.export_handle = f;
				ctx->params->export_next_frame  = 0;
			} else {
				work->compute_ctx.export_handle = INVALID_FILE;
			}

			b32 output_3d = bp->output_points.x > 1 && bp->output_points.y > 1 &&
			                bp->output_points.z > 1;

			if (output_3d) {
				work->type = BW_PARTIAL_COMPUTE;
				BeamformFrame *frame = &ctx->partial_compute_ctx.frame;
				uv4 out_dim = ctx->params->raw.output_points;
				out_dim.w   = ctx->params->raw.xdc_count;
				alloc_beamform_frame(&ctx->gl, frame, out_dim, 0, s8("Beamformed_Volume"));
				work->compute_ctx.frame = frame;
			}

			u32  raw_index    = work->compute_ctx.raw_data_ssbo_index;
			uv2  rf_raw_dim   = cs->rf_raw_dim;
			size rf_raw_size  = rf_raw_dim.x * rf_raw_dim.y * sizeof(i16);
			void *rf_data_buf = cs->raw_data_arena.beg + raw_index * rf_raw_size;

			alloc_output_image(ctx, bp->output_points);

			size rlen = ctx->platform.read_pipe(input->pipe_handle, rf_data_buf, rf_raw_size);
			if (rlen != rf_raw_size) {
				stream_append_s8(&ctx->error_stream, s8("Partial Read Occurred: "));
				stream_append_i64(&ctx->error_stream, rlen);
				stream_append_byte(&ctx->error_stream, '/');
				stream_append_i64(&ctx->error_stream, rf_raw_size);
				stream_append_s8(&ctx->error_stream, s8("\n\0"));
				TraceLog(LOG_WARNING, (c8 *)stream_to_s8(&ctx->error_stream).data);
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
		}
	}

	ctx->beamform_work_queue.did_compute_this_frame = 0;
	do_beamform_work(ctx, arena);

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
				out_texture   = ctx->averaged_frame.textures[0];
			} else {
				frame_to_draw = ctx->beamform_frames + ctx->displayed_frame_index;
				/* NOTE: verify we have actually beamformed something yet */
				if (frame_to_draw->dim.w)
					out_texture = frame_to_draw->textures[frame_to_draw->dim.w - 1];
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
		ctx->flags |= SHOULD_EXIT;
}
