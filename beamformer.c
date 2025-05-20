/* See LICENSE for license details. */
/* TODO(rnp):
 * [ ]: refactor: compute shader timers should be generated based on the pipeline stage limit
 * [ ]: reinvestigate ring buffer raw_data_ssbo
 *      - to minimize latency the main thread should manage the subbuffer upload so that the
 *        compute thread can just keep computing. This way we can keep the copmute thread busy
 *        with work while we image.
 *      - In particular we will potentially need multiple GPUComputeContexts so that we
 *        can overwrite one while the other is in use.
 *      - make use of glFenceSync to guard buffer uploads
 * [ ]: bug: re-beamform on shader reload
 * [ ]: need to keep track of gpu memory in some way
 *      - want to be able to store more than 16 2D frames but limit 3D frames
 *      - maybe keep track of how much gpu memory is committed for beamformed images
 *        and use that to determine when to loop back over existing textures
 *      - to do this maybe use a circular linked list instead of a flat array
 *      - then have a way of querying how many frames are available for a specific point count
 * [ ]: bug: reinit cuda on hot-reload
 * [ ]: bug: parameters version should not be in the middle of the shared memory
 */

#include "beamformer.h"
#include "beamformer_work_queue.c"

global f32 dt_for_frame;
global u32 cycle_t;

#ifndef _DEBUG
#define start_renderdoc_capture(...)
#define end_renderdoc_capture(...)
#else
global renderdoc_start_frame_capture_fn *start_frame_capture;
global renderdoc_end_frame_capture_fn   *end_frame_capture;
#define start_renderdoc_capture(gl) if (start_frame_capture) start_frame_capture(gl, 0)
#define end_renderdoc_capture(gl)   if (end_frame_capture)   end_frame_capture(gl, 0)
#endif

typedef struct {
	BeamformComputeFrame *frames;
	u32 capacity;
	u32 offset;
	u32 cursor;
	u32 needed_frames;
} ComputeFrameIterator;

function uv3
make_valid_test_dim(u32 in[3])
{
	uv3 result;
	result.E[0] = MAX(in[0], 1);
	result.E[1] = MAX(in[1], 1);
	result.E[2] = MAX(in[2], 1);
	return result;
}

function ComputeFrameIterator
compute_frame_iterator(BeamformerCtx *ctx, u32 start_index, u32 needed_frames)
{
	start_index = start_index % ARRAY_COUNT(ctx->beamform_frames);

	ComputeFrameIterator result;
	result.frames        = ctx->beamform_frames;
	result.offset        = start_index;
	result.capacity      = ARRAY_COUNT(ctx->beamform_frames);
	result.cursor        = 0;
	result.needed_frames = needed_frames;
	return result;
}

function BeamformComputeFrame *
frame_next(ComputeFrameIterator *bfi)
{
	BeamformComputeFrame *result = 0;
	if (bfi->cursor != bfi->needed_frames) {
		u32 index = (bfi->offset + bfi->cursor++) % bfi->capacity;
		result    = bfi->frames + index;
	}
	return result;
}

function void
alloc_beamform_frame(GLParams *gp, BeamformFrame *out, ComputeShaderStats *out_stats,
                     uv3 out_dim, s8 name, Arena arena)
{
	out->dim.x = MAX(1, round_down_power_of_2(ORONE(out_dim.x)));
	out->dim.y = MAX(1, round_down_power_of_2(ORONE(out_dim.y)));
	out->dim.z = MAX(1, round_down_power_of_2(ORONE(out_dim.z)));

	if (gp) {
		out->dim.x = MIN(out->dim.x, gp->max_3d_texture_dim);
		out->dim.y = MIN(out->dim.y, gp->max_3d_texture_dim);
		out->dim.z = MIN(out->dim.z, gp->max_3d_texture_dim);
	}

	/* NOTE: allocate storage for beamformed output data;
	 * this is shared between compute and fragment shaders */
	u32 max_dim = MAX(out->dim.x, MAX(out->dim.y, out->dim.z));
	out->mips   = ctz_u32(max_dim) + 1;

	Stream label = arena_stream(arena);
	stream_append_s8(&label, name);
	stream_append_byte(&label, '[');
	stream_append_hex_u64(&label, out->id);
	stream_append_byte(&label, ']');

	glDeleteTextures(1, &out->texture);
	glCreateTextures(GL_TEXTURE_3D, 1, &out->texture);
	glTextureStorage3D(out->texture, out->mips, GL_RG32F, out->dim.x, out->dim.y, out->dim.z);
	LABEL_GL_OBJECT(GL_TEXTURE, out->texture, stream_to_s8(&label));

	if (out_stats) {
		glDeleteQueries(ARRAY_COUNT(out_stats->timer_ids), out_stats->timer_ids);
		glCreateQueries(GL_TIME_ELAPSED, ARRAY_COUNT(out_stats->timer_ids), out_stats->timer_ids);
	}
}

function void
alloc_shader_storage(BeamformerCtx *ctx, u32 rf_raw_size, Arena a)
{
	ComputeShaderCtx     *cs = &ctx->csctx;
	BeamformerParameters *bp = &ctx->shared_memory->parameters;

	cs->dec_data_dim = uv4_from_u32_array(bp->dec_data_dim);
	cs->rf_raw_size  = rf_raw_size;

	glDeleteBuffers(ARRAY_COUNT(cs->rf_data_ssbos), cs->rf_data_ssbos);
	glCreateBuffers(ARRAY_COUNT(cs->rf_data_ssbos), cs->rf_data_ssbos);

	i32 storage_flags = GL_DYNAMIC_STORAGE_BIT;
	glDeleteBuffers(1, &cs->raw_data_ssbo);
	glCreateBuffers(1, &cs->raw_data_ssbo);
	glNamedBufferStorage(cs->raw_data_ssbo, rf_raw_size, 0, storage_flags);
	LABEL_GL_OBJECT(GL_BUFFER, cs->raw_data_ssbo, s8("Raw_RF_SSBO"));

	iz rf_decoded_size = 2 * sizeof(f32) * cs->dec_data_dim.x * cs->dec_data_dim.y * cs->dec_data_dim.z;
	Stream label = arena_stream(a);
	stream_append_s8(&label, s8("Decoded_RF_SSBO_"));
	u32 s_widx = label.widx;
	for (u32 i = 0; i < ARRAY_COUNT(cs->rf_data_ssbos); i++) {
		glNamedBufferStorage(cs->rf_data_ssbos[i], rf_decoded_size, 0, 0);
		stream_append_u64(&label, i);
		LABEL_GL_OBJECT(GL_BUFFER, cs->rf_data_ssbos[i], stream_to_s8(&label));
		stream_reset(&label, s_widx);
	}

	/* NOTE(rnp): these are stubs when CUDA isn't supported */
	ctx->cuda_lib.register_buffers(cs->rf_data_ssbos, countof(cs->rf_data_ssbos), cs->raw_data_ssbo);
	ctx->cuda_lib.init(bp->rf_raw_dim, bp->dec_data_dim);

	u32  order    = cs->dec_data_dim.z;
	i32 *hadamard = make_hadamard_transpose(&a, order);
	if (hadamard) {
		glDeleteTextures(1, &cs->hadamard_texture);
		glCreateTextures(GL_TEXTURE_2D, 1, &cs->hadamard_texture);
		glTextureStorage2D(cs->hadamard_texture, 1, GL_R8I, order, order);
		glTextureSubImage2D(cs->hadamard_texture, 0, 0, 0,  order, order, GL_RED_INTEGER,
		                    GL_INT, hadamard);
		LABEL_GL_OBJECT(GL_TEXTURE, cs->hadamard_texture, s8("Hadamard_Matrix"));
	}
}

function b32
fill_frame_compute_work(BeamformerCtx *ctx, BeamformWork *work, ImagePlaneTag plane)
{
	b32 result = 0;
	if (work) {
		result = 1;
		u32 frame_id    = atomic_inc(&ctx->next_render_frame_index, 1);
		u32 frame_index = frame_id % ARRAY_COUNT(ctx->beamform_frames);
		work->type      = BW_COMPUTE;
		work->frame     = ctx->beamform_frames + frame_index;
		work->frame->ready_to_present = 0;
		work->frame->frame.id = frame_id;
		work->frame->image_plane_tag = plane;
	}
	return result;
}

function void
export_frame(BeamformerCtx *ctx, iptr handle, BeamformFrame *frame)
{
	uv3 dim            = frame->dim;
	iz  out_size       = dim.x * dim.y * dim.z * 2 * sizeof(f32);
	ctx->export_buffer = ctx->os.alloc_arena(ctx->export_buffer, out_size);
	glGetTextureImage(frame->texture, 0, GL_RG, GL_FLOAT, out_size, ctx->export_buffer.beg);
	s8 raw = {.len = out_size, .data = ctx->export_buffer.beg};
	if (!ctx->os.write_file(handle, raw))
		ctx->os.write_file(ctx->os.error_handle, s8("failed to export frame\n"));
	ctx->os.close(handle);
}

function void
do_sum_shader(ComputeShaderCtx *cs, u32 *in_textures, u32 in_texture_count, f32 in_scale,
              u32 out_texture, uv3 out_data_dim)
{
	/* NOTE: zero output before summing */
	glClearTexImage(out_texture, 0, GL_RED, GL_FLOAT, 0);
	glMemoryBarrier(GL_TEXTURE_UPDATE_BARRIER_BIT);

	glBindImageTexture(0, out_texture, 0, GL_TRUE, 0, GL_READ_WRITE, GL_RG32F);
	glUniform1f(CS_SUM_PRESCALE_UNIFORM_LOC, in_scale);
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

function struct compute_cursor
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

function iv3
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

function b32
compute_cursor_finished(struct compute_cursor *cursor)
{
	b32 result = cursor->completed_points >= cursor->total_points;
	return result;
}

function void
do_compute_shader(BeamformerCtx *ctx, Arena arena, BeamformComputeFrame *frame, ComputeShaderID shader)
{
	ComputeShaderCtx *csctx = &ctx->csctx;

	glUseProgram(csctx->programs[shader]);

	u32 output_ssbo_idx = !csctx->last_output_ssbo_index;
	u32 input_ssbo_idx  = csctx->last_output_ssbo_index;

	switch (shader) {
	case CS_DECODE:
	case CS_DECODE_FLOAT:
	case CS_DECODE_FLOAT_COMPLEX:
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, csctx->raw_data_ssbo);
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, csctx->rf_data_ssbos[output_ssbo_idx]);
		glBindImageTexture(0, csctx->hadamard_texture, 0, GL_FALSE, 0, GL_READ_ONLY, GL_R8I);
		glBindImageTexture(1, csctx->channel_mapping_texture, 0, GL_FALSE, 0, GL_READ_ONLY, GL_R16I);
		glDispatchCompute(ORONE(csctx->dec_data_dim.x / 32),
		                  ORONE(csctx->dec_data_dim.y / 32),
		                  ORONE(csctx->dec_data_dim.z));
		csctx->last_output_ssbo_index = !csctx->last_output_ssbo_index;
		break;
	case CS_CUDA_DECODE:
		ctx->cuda_lib.decode(0, output_ssbo_idx, 0);
		csctx->last_output_ssbo_index = !csctx->last_output_ssbo_index;
		break;
	case CS_CUDA_HILBERT:
		ctx->cuda_lib.hilbert(input_ssbo_idx, output_ssbo_idx);
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
		u32 texture = frame->frame.texture;
		for (u32 i = 1; i < frame->frame.mips; i++) {
			glBindImageTexture(0, texture, i - 1, GL_TRUE, 0, GL_READ_ONLY,  GL_RG32F);
			glBindImageTexture(1, texture, i - 0, GL_TRUE, 0, GL_WRITE_ONLY, GL_RG32F);
			glUniform1i(CS_MIN_MAX_MIPS_LEVEL_UNIFORM_LOC, i);

			u32 width  = frame->frame.dim.x >> i;
			u32 height = frame->frame.dim.y >> i;
			u32 depth  = frame->frame.dim.z >> i;
			glDispatchCompute(ORONE(width / 32), ORONE(height), ORONE(depth / 32));
			glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
		}
	} break;
	case CS_DAS: {
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, csctx->rf_data_ssbos[input_ssbo_idx]);
		glBindImageTexture(0, frame->frame.texture, 0, GL_TRUE, 0, GL_WRITE_ONLY, GL_RG32F);
		glBindImageTexture(1, csctx->sparse_elements_texture, 0, GL_FALSE, 0, GL_READ_ONLY, GL_R16I);
		glBindImageTexture(2, csctx->focal_vectors_texture, 0, GL_FALSE, 0, GL_READ_ONLY, GL_RG32F);

		glUniform1ui(DAS_CYCLE_T_UNIFORM_LOC, cycle_t++);

		#if 1
		/* TODO(rnp): compute max_points_per_dispatch based on something like a
		 * transmit_count * channel_count product */
		u32 max_points_per_dispatch = KB(64);
		struct compute_cursor cursor = start_compute_cursor(frame->frame.dim, max_points_per_dispatch);
		f32 percent_per_step = (f32)cursor.points_per_dispatch / (f32)cursor.total_points;
		csctx->processing_progress = -percent_per_step;
		for (iv3 offset = {0};
		     !compute_cursor_finished(&cursor);
		     offset = step_compute_cursor(&cursor))
		{
			csctx->processing_progress += percent_per_step;
			/* IMPORTANT(rnp): prevents OS from coalescing and killing our shader */
			glFinish();
			glUniform3iv(DAS_VOXEL_OFFSET_UNIFORM_LOC, 1, offset.E);
			glDispatchCompute(cursor.dispatch.x, cursor.dispatch.y, cursor.dispatch.z);
		}
		#else
		/* NOTE(rnp): use this for testing tiling code. The performance of the above path
		 * should be the same as this path if everything is working correctly */
		iv3 compute_dim_offset = {0};
		glUniform3iv(csctx->voxel_offset_id, 1, compute_dim_offset.E);
		glDispatchCompute(ORONE(frame->frame.dim.x / 32),
		                  ORONE(frame->frame.dim.y),
		                  ORONE(frame->frame.dim.z / 32));
		#endif
		glMemoryBarrier(GL_TEXTURE_UPDATE_BARRIER_BIT|GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
	} break;
	case CS_SUM: {
		u32 aframe_index = ctx->averaged_frame_index % ARRAY_COUNT(ctx->averaged_frames);
		BeamformComputeFrame *aframe = ctx->averaged_frames + aframe_index;
		aframe->ready_to_present     = 0;
		aframe->frame.id             = ctx->averaged_frame_index;
		/* TODO(rnp): hack we need a better way of specifying which frames to sum;
		 * this is fine for rolling averaging but what if we want to do something else */
		ASSERT(frame >= ctx->beamform_frames);
		ASSERT(frame < ctx->beamform_frames + ARRAY_COUNT(ctx->beamform_frames));
		u32 base_index   = (u32)(frame - ctx->beamform_frames);
		u32 to_average   = ctx->shared_memory->parameters.output_points[3];
		u32 frame_count  = 0;
		u32 *in_textures = push_array(&arena, u32, MAX_BEAMFORMED_SAVED_FRAMES);
		ComputeFrameIterator cfi = compute_frame_iterator(ctx, 1 + base_index - to_average,
		                                                  to_average);
		for (BeamformComputeFrame *it = frame_next(&cfi); it; it = frame_next(&cfi))
			in_textures[frame_count++] = it->frame.texture;

		ASSERT(to_average == frame_count);

		do_sum_shader(csctx, in_textures, frame_count, 1 / (f32)frame_count,
		              aframe->frame.texture, aframe->frame.dim);
		aframe->frame.min_coordinate = frame->frame.min_coordinate;
		aframe->frame.max_coordinate = frame->frame.max_coordinate;
		aframe->frame.compound_count = frame->frame.compound_count;
		aframe->frame.das_shader_id  = frame->frame.das_shader_id;
	} break;
	default: ASSERT(0);
	}
}

function s8
push_compute_shader_header(Arena *a, b32 parameters, ComputeShaderID shader)
{
	Stream sb = arena_stream(*a);

	stream_append_s8(&sb, s8("#version 460 core\n\n"));

	#define X(name, type, size, gltype, glsize, comment) "\t" #gltype " " #name #glsize "; " comment "\n"
	if (parameters) {
		stream_append_s8(&sb, s8("layout(std140, binding = 0) uniform parameters {\n"
		                         BEAMFORMER_PARAMS_HEAD
		                         BEAMFORMER_UI_PARAMS
		                         BEAMFORMER_PARAMS_TAIL
		                         "};\n\n"));
	}
	#undef X

	switch (shader) {
	case CS_DAS: {
		#define X(type, id, pretty, fixed_tx) "#define DAS_ID_" #type " " #id "\n"
		stream_append_s8(&sb, s8(""
		"layout(local_size_x = " str(DAS_LOCAL_SIZE_X) ", "
		       "local_size_y = " str(DAS_LOCAL_SIZE_Y) ", "
		       "local_size_z = " str(DAS_LOCAL_SIZE_Z) ") in;\n\n"
		"layout(location = " str(DAS_VOXEL_OFFSET_UNIFORM_LOC) ") uniform ivec3 u_voxel_offset;\n"
		"layout(location = " str(DAS_CYCLE_T_UNIFORM_LOC)      ") uniform uint  u_cycle_t;\n\n"
		DAS_TYPES
		));
		#undef X
	} break;
	case CS_DECODE_FLOAT:
	case CS_DECODE_FLOAT_COMPLEX: {
		if (shader == CS_DECODE_FLOAT) stream_append_s8(&sb, s8("#define INPUT_DATA_TYPE_FLOAT\n\n"));
		else                           stream_append_s8(&sb, s8("#define INPUT_DATA_TYPE_FLOAT_COMPLEX\n\n"));
	} /* FALLTHROUGH */
	case CS_DECODE: {
		#define X(type, id, pretty) stream_append_s8(&sb, s8("#define DECODE_MODE_" #type " " #id "\n"));
		DECODE_TYPES
		#undef X
	} break;
	case CS_MIN_MAX: {
		stream_append_s8(&sb, s8("layout(location = " str(CS_MIN_MAX_MIPS_LEVEL_UNIFORM_LOC)
		                         ") uniform int u_mip_map;\n\n"));
	} break;
	case CS_SUM: {
		stream_append_s8(&sb, s8("layout(location = " str(CS_SUM_PRESCALE_UNIFORM_LOC)
		                         ") uniform float u_sum_prescale = 1.0;\n\n"));
	} break;
	default: break;
	}
	stream_append_s8(&sb, s8("\n#line 1\n"));
	return arena_stream_commit(a, &sb);
}

function b32
reload_compute_shader(BeamformerCtx *ctx, s8 path, s8 extra, ComputeShaderReloadContext *csr, Arena tmp)
{
	ComputeShaderCtx *cs = &ctx->csctx;
	b32 result = 0;

	/* NOTE: arena works as stack (since everything here is 1 byte aligned) */
	s8 header      = push_compute_shader_header(&tmp, csr->needs_header, csr->shader);
	s8 shader_text = ctx->os.read_whole_file(&tmp, (c8 *)path.data);
	shader_text.data -= header.len;
	shader_text.len  += header.len;

	if (shader_text.data == header.data) {
		Stream sb = arena_stream(tmp);
		stream_append_s8s(&sb, path, extra);
		s8 info = arena_stream_commit(&tmp, &sb);
		u32 new_program = load_shader(&ctx->os, tmp, 1, s8(""), s8(""), shader_text,
		                              info, csr->label);
		if (new_program) {
			glDeleteProgram(cs->programs[csr->shader]);
			cs->programs[csr->shader] = new_program;
			glUseProgram(cs->programs[csr->shader]);
			glBindBufferBase(GL_UNIFORM_BUFFER, 0, cs->shared_ubo);
		}
	} else {
		Stream sb = arena_stream(tmp);
		stream_append_s8s(&sb, s8("failed to load: "), path, extra, s8("\n"));
		ctx->os.write_file(ctx->os.error_handle, stream_to_s8(&sb));
	}

	return result;
}

function void
complete_queue(BeamformerCtx *ctx, BeamformWorkQueue *q, Arena arena, iptr gl_context, iz barrier_offset)
{
	ComputeShaderCtx       *cs = &ctx->csctx;
	BeamformerParameters   *bp = &ctx->shared_memory->parameters;
	BeamformerSharedMemory *sm = ctx->shared_memory;

	BeamformWork *work = beamform_work_queue_pop(q);
	while (work) {
		b32 can_commit = 1;
		switch (work->type) {
		case BW_RELOAD_SHADER: {
			ComputeShaderReloadContext *csr = work->reload_shader_ctx;
			b32 success = reload_compute_shader(ctx, csr->path, s8(""), csr, arena);
			if (csr->shader == CS_DECODE) {
				/* TODO(rnp): think of a better way of doing this */
				csr->shader = CS_DECODE_FLOAT_COMPLEX;
				success &= reload_compute_shader(ctx, csr->path, s8(" (F32C)"), csr, arena);
				csr->shader = CS_DECODE_FLOAT;
				success &= reload_compute_shader(ctx, csr->path, s8(" (F32)"),  csr, arena);
				csr->shader = CS_DECODE;
			}

			if (success) {
				/* TODO(rnp): this check seems off */
				if (ctx->csctx.raw_data_ssbo) {
					can_commit = 0;
					ImagePlaneTag plane = ctx->beamform_frames[ctx->display_frame_index].image_plane_tag;
					fill_frame_compute_work(ctx, work, plane);
				}
			}
		} break;
		case BW_UPLOAD_BUFFER: {
			try_lock((i32 *)(barrier_offset + work->completion_barrier), -1,
			         ctx->os.wait_on_value);
			BeamformerUploadContext *uc = &work->upload_context;
			u32 tex_type, tex_format, tex_element_count, tex_1d = 0, buffer = 0;
			switch (uc->kind) {
			case BU_KIND_CHANNEL_MAPPING: {
				tex_1d            = cs->channel_mapping_texture;
				tex_type          = GL_SHORT;
				tex_format        = GL_RED_INTEGER;
				tex_element_count = ARRAY_COUNT(sm->channel_mapping);
				ctx->cuda_lib.set_channel_mapping(sm->channel_mapping);
			} break;
			case BU_KIND_FOCAL_VECTORS: {
				tex_1d            = cs->focal_vectors_texture;
				tex_type          = GL_FLOAT;
				tex_format        = GL_RG;
				tex_element_count = ARRAY_COUNT(sm->focal_vectors);
			} break;
			case BU_KIND_SPARSE_ELEMENTS: {
				tex_1d            = cs->sparse_elements_texture;
				tex_type          = GL_SHORT;
				tex_format        = GL_RED_INTEGER;
				tex_element_count = ARRAY_COUNT(sm->sparse_elements);
			} break;
			case BU_KIND_PARAMETERS: {
				ctx->ui_read_params = barrier_offset != 0;
				buffer = cs->shared_ubo;
			} break;
			case BU_KIND_RF_DATA: {
				if (cs->rf_raw_size != uc->size ||
				    !uv4_equal(cs->dec_data_dim, uv4_from_u32_array(bp->dec_data_dim)))
				{
					alloc_shader_storage(ctx, uc->size, arena);
				}
				buffer = cs->raw_data_ssbo;
			} break;
			default: INVALID_CODE_PATH; break;
			}

			if (tex_1d) {
				glTextureSubImage1D(tex_1d, 0, 0, tex_element_count, tex_format,
				                    tex_type, (u8 *)sm + uc->shared_memory_offset);
			}

			if (buffer) {
				glNamedBufferSubData(buffer, 0, uc->size,
				                     (u8 *)sm + uc->shared_memory_offset);
			}
		} break;
		case BW_COMPUTE: {
			atomic_store(&cs->processing_compute, 1);
			start_renderdoc_capture(gl_context);
			if (ctx->starting_compute_from_library) {
				release_lock(&sm->dispatch_compute_sync, ctx->os.wake_waiters);
				atomic_store(&ctx->starting_compute_from_library, 0);
			}

			BeamformComputeFrame *frame = work->frame;
			uv3 try_dim = make_valid_test_dim(bp->output_points);
			if (!uv3_equal(try_dim, frame->frame.dim))
				alloc_beamform_frame(&ctx->gl, &frame->frame, &frame->stats, try_dim,
				                     s8("Beamformed_Data"), arena);

			if (bp->output_points[3] > 1) {
				if (!uv3_equal(try_dim, ctx->averaged_frames[0].frame.dim)) {
					alloc_beamform_frame(&ctx->gl, &ctx->averaged_frames[0].frame,
					                     &ctx->averaged_frames[0].stats,
					                     try_dim, s8("Averaged Frame"), arena);
					alloc_beamform_frame(&ctx->gl, &ctx->averaged_frames[1].frame,
					                     &ctx->averaged_frames[1].stats,
					                     try_dim, s8("Averaged Frame"), arena);
				}
			}

			frame->in_flight = 1;
			frame->frame.min_coordinate = v4_from_f32_array(bp->output_min_coordinate);
			frame->frame.max_coordinate = v4_from_f32_array(bp->output_max_coordinate);
			frame->frame.das_shader_id  = bp->das_shader_id;
			frame->frame.compound_count = bp->dec_data_dim[2];

			b32 did_sum_shader = 0;
			u32 stage_count = sm->compute_stages_count;
			ComputeShaderID *stages = sm->compute_stages;
			for (u32 i = 0; i < stage_count; i++) {
				did_sum_shader |= stages[i] == CS_SUM;
				frame->stats.timer_active[stages[i]] = 1;
				glBeginQuery(GL_TIME_ELAPSED, frame->stats.timer_ids[stages[i]]);
				do_compute_shader(ctx, arena, frame, stages[i]);
				glEndQuery(GL_TIME_ELAPSED);
			}
			/* NOTE(rnp): block until work completes so that we can record timings */
			glFinish();
			cs->processing_progress = 1;

			for (u32 i = 0; i < ARRAY_COUNT(frame->stats.timer_ids); i++) {
				u64 ns = 0;
				if (frame->stats.timer_active[i]) {
					glGetQueryObjectui64v(frame->stats.timer_ids[i],
					                      GL_QUERY_RESULT, &ns);
					frame->stats.timer_active[i] = 0;
				}
				frame->stats.times[i] = (f32)ns / 1e9;
			}

			if (did_sum_shader) {
				u32 aframe_index = (ctx->averaged_frame_index %
				                    ARRAY_COUNT(ctx->averaged_frames));
				ctx->averaged_frames[aframe_index].image_plane_tag  = frame->image_plane_tag;
				ctx->averaged_frames[aframe_index].ready_to_present = 1;
				/* TODO(rnp): not really sure what to do here */
				mem_copy(&ctx->averaged_frames[aframe_index].stats.times,
				         &frame->stats.times, sizeof(frame->stats.times));
				atomic_inc(&ctx->averaged_frame_index, 1);
			}
			frame->ready_to_present = 1;
			cs->processing_compute  = 0;

			end_renderdoc_capture(gl_context);
		} break;
		case BW_SAVE_FRAME: {
			BeamformComputeFrame *frame = work->output_frame_ctx.frame;
			if (frame->ready_to_present) {
				export_frame(ctx, work->output_frame_ctx.file_handle, &frame->frame);
			} else {
				/* TODO(rnp): should we handle this? */
				INVALID_CODE_PATH;
			}
		} break;
		InvalidDefaultCase;
		}

		if (can_commit) {
			if (work->completion_barrier)
				release_lock((i32 *)(barrier_offset + work->completion_barrier),
				             ctx->os.wake_waiters);
			beamform_work_queue_pop_commit(q);
			work = beamform_work_queue_pop(q);
		}
	}
}

DEBUG_EXPORT BEAMFORMER_COMPUTE_SETUP_FN(beamformer_compute_setup)
{
	BeamformerCtx          *ctx = (BeamformerCtx *)user_context;
	BeamformerSharedMemory *sm  = ctx->shared_memory;
	ComputeShaderCtx       *cs  = &ctx->csctx;

	glCreateBuffers(1, &cs->shared_ubo);
	glNamedBufferStorage(cs->shared_ubo, sizeof(sm->parameters), 0, GL_DYNAMIC_STORAGE_BIT);

	glCreateTextures(GL_TEXTURE_1D, 1, &cs->channel_mapping_texture);
	glCreateTextures(GL_TEXTURE_1D, 1, &cs->sparse_elements_texture);
	glCreateTextures(GL_TEXTURE_1D, 1, &cs->focal_vectors_texture);
	glTextureStorage1D(cs->channel_mapping_texture, 1, GL_R16I,  ARRAY_COUNT(sm->channel_mapping));
	glTextureStorage1D(cs->sparse_elements_texture, 1, GL_R16I,  ARRAY_COUNT(sm->sparse_elements));
	glTextureStorage1D(cs->focal_vectors_texture,   1, GL_RG32F, ARRAY_COUNT(sm->focal_vectors));

	LABEL_GL_OBJECT(GL_TEXTURE, cs->channel_mapping_texture, s8("Channel_Mapping"));
	LABEL_GL_OBJECT(GL_TEXTURE, cs->focal_vectors_texture,   s8("Focal_Vectors"));
	LABEL_GL_OBJECT(GL_TEXTURE, cs->sparse_elements_texture, s8("Sparse_Elements"));
	LABEL_GL_OBJECT(GL_BUFFER,  cs->shared_ubo,              s8("Beamformer_Parameters"));
}

DEBUG_EXPORT BEAMFORMER_COMPLETE_COMPUTE_FN(beamformer_complete_compute)
{
	BeamformerCtx *ctx = (BeamformerCtx *)user_context;
	complete_queue(ctx, &ctx->shared_memory->external_work_queue, arena, gl_context, (iz)ctx->shared_memory);
	complete_queue(ctx, ctx->beamform_work_queue, arena, gl_context, 0);
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
		DEBUG_DECL(start_frame_capture = ctx->os.start_frame_capture);
		DEBUG_DECL(end_frame_capture   = ctx->os.end_frame_capture);
	}

	BeamformerParameters *bp = &ctx->shared_memory->parameters;
	if (ctx->shared_memory->dispatch_compute_sync && !ctx->starting_compute_from_library) {
		ImagePlaneTag current_plane = ctx->shared_memory->current_image_plane;
		BeamformWork *work = beamform_work_queue_push(ctx->beamform_work_queue);
		if (work) {
			if (fill_frame_compute_work(ctx, work, current_plane))
				beamform_work_queue_push_commit(ctx->beamform_work_queue);

			if (ctx->shared_memory->export_next_frame) {
				BeamformWork *export = beamform_work_queue_push(ctx->beamform_work_queue);
				if (export) {
					/* TODO: we don't really want the beamformer opening/closing files */
					iptr f = ctx->os.open_for_write(ctx->os.export_pipe_name);
					export->type = BW_SAVE_FRAME;
					export->output_frame_ctx.file_handle = f;
					if (bp->output_points[3] > 1) {
						u32 a_index = !(ctx->averaged_frame_index %
						                ARRAY_COUNT(ctx->averaged_frames));
						BeamformComputeFrame *aframe = ctx->averaged_frames + a_index;
						export->output_frame_ctx.frame = aframe;
					} else {
						export->output_frame_ctx.frame = work->frame;
					}
					beamform_work_queue_push_commit(ctx->beamform_work_queue);
				}
				ctx->shared_memory->export_next_frame = 0;
			}

			atomic_store(&ctx->starting_compute_from_library, 1);
			ctx->os.wake_waiters(&ctx->os.compute_worker.sync_variable);
		}
	}

	if (ctx->start_compute) {
		if (ctx->beamform_frames[ctx->display_frame_index].ready_to_present) {
			BeamformWork *work  = beamform_work_queue_push(ctx->beamform_work_queue);
			ImagePlaneTag plane = ctx->beamform_frames[ctx->display_frame_index].image_plane_tag;
			if (fill_frame_compute_work(ctx, work, plane)) {
				beamform_work_queue_push_commit(ctx->beamform_work_queue);
				ctx->os.wake_waiters(&ctx->os.compute_worker.sync_variable);
				ctx->start_compute = 0;
			}
		}
	}

	ComputeFrameIterator cfi = compute_frame_iterator(ctx, ctx->display_frame_index,
	                                                  ctx->next_render_frame_index - ctx->display_frame_index);
	for (BeamformComputeFrame *frame = frame_next(&cfi); frame; frame = frame_next(&cfi)) {
		if (frame->in_flight && frame->ready_to_present) {
			frame->in_flight         = 0;
			ctx->display_frame_index = frame - cfi.frames;
		}
	}

	if (ctx->start_compute) {
		ctx->start_compute = 0;
		ctx->os.wake_waiters(&ctx->os.compute_worker.sync_variable);
	}

	BeamformComputeFrame *frame_to_draw;
	if (bp->output_points[3] > 1) {
		u32 a_index = !(ctx->averaged_frame_index % ARRAY_COUNT(ctx->averaged_frames));
		frame_to_draw = ctx->averaged_frames + a_index;
	} else {
		frame_to_draw = ctx->beamform_frames + ctx->display_frame_index;
	}

	draw_ui(ctx, input, frame_to_draw->ready_to_present? &frame_to_draw->frame : 0,
	        frame_to_draw->image_plane_tag, &frame_to_draw->stats);

	ctx->frame_view_render_context.updated = 0;

	if (WindowShouldClose())
		ctx->should_exit = 1;
}
