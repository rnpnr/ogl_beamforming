/* See LICENSE for license details. */
/* TODO(rnp):
 * [ ]: filter shader specializations need to be generated per sample mode
 *      - performance was measured with a switch on sampling mode and the perfomance gained
 *        is 80% worse than just having a baked in sampling mode
 *      - should also include channel mapping just in case
 * [ ]: make decode output real values for real inputs and complex values for complex inputs
 *      - this means that das should have a RF version and an IQ version
 *      - this will also flip the current hack to support demodulate after decode to
 *        being a hack to support CudaHilbert after decode
 * [ ]: measure performance of doing channel mapping in a separate shader
 * [ ]: BeamformWorkQueue -> BeamformerWorkQueue
 * [ ]: need to keep track of gpu memory in some way
 *      - want to be able to store more than 16 2D frames but limit 3D frames
 *      - maybe keep track of how much gpu memory is committed for beamformed images
 *        and use that to determine when to loop back over existing textures
 *      - to do this maybe use a circular linked list instead of a flat array
 *      - then have a way of querying how many frames are available for a specific point count
 * [ ]: bug: reinit cuda on hot-reload
 */

#include "beamformer.h"

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
	BeamformerFrame *frames;
	u32 capacity;
	u32 offset;
	u32 cursor;
	u32 needed_frames;
} ComputeFrameIterator;

function void
beamformer_compute_plan_release(BeamformerComputeContext *cc, u32 block)
{
	assert(block < countof(cc->compute_plans));
	BeamformerComputePlan *cp = cc->compute_plans[block];
	if (cp) {
		glDeleteBuffers(countof(cp->ubos), cp->ubos);
		glDeleteTextures(countof(cp->textures), cp->textures);
		for (u32 i = 0; i < countof(cp->filters); i++)
			glDeleteTextures(1, &cp->filters[i].texture);
		cc->compute_plans[block] = 0;
		SLLPushFreelist(cp, cc->compute_plan_freelist);
	}
}

function BeamformerComputePlan *
beamformer_compute_plan_for_block(BeamformerComputeContext *cc, u32 block, Arena *arena)
{
	assert(block < countof(cc->compute_plans));
	BeamformerComputePlan *result = cc->compute_plans[block];
	if (!result) {
		result = SLLPopFreelist(cc->compute_plan_freelist);
		if (result) zero_struct(result);
		else        result = push_struct(arena, BeamformerComputePlan);
		cc->compute_plans[block] = result;

		glCreateBuffers(countof(result->ubos), result->ubos);

		Stream label = arena_stream(*arena);
		#define X(k, t, ...) \
			glNamedBufferStorage(result->ubos[BeamformerComputeUBOKind_##k], sizeof(t), \
			                     0, GL_DYNAMIC_STORAGE_BIT); \
			stream_append_s8(&label, s8(#t "[")); \
			stream_append_u64(&label, block);     \
			stream_append_byte(&label, ']');      \
			glObjectLabel(GL_BUFFER, result->ubos[BeamformerComputeUBOKind_##k], \
			              label.widx, (c8 *)label.data); \
			label.widx = 0;
		BEAMFORMER_COMPUTE_UBO_LIST
		#undef X

		#define X(_k, t, ...) t,
		GLenum gl_kind[] = {BEAMFORMER_COMPUTE_TEXTURE_LIST};
		#undef X
		read_only local_persist s8 tex_prefix[] = {
			#define X(k, ...) s8(#k "["),
			BEAMFORMER_COMPUTE_TEXTURE_LIST
			#undef X
		};
		glCreateTextures(GL_TEXTURE_1D, BeamformerComputeTextureKind_Count - 1, result->textures);
		for (u32 i = 0; i < BeamformerComputeTextureKind_Count - 1; i++) {
			/* TODO(rnp): this could be predicated on channel count for this compute plan */
			glTextureStorage1D(result->textures[i], 1, gl_kind[i], BeamformerMaxChannelCount);
			stream_append_s8(&label, tex_prefix[i]);
			stream_append_u64(&label, block);
			stream_append_byte(&label, ']');
			glObjectLabel(GL_TEXTURE, result->textures[i], label.widx, (c8 *)label.data);
			label.widx = 0;
		}
	}
	return result;
}

function void
beamformer_filter_update(BeamformerFilter *f, BeamformerFilterKind kind,
                         BeamformerFilterParameters fp, u32 block, u32 slot, Arena arena)
{
	#define X(k, ...) s8(#k "Filter"),
	read_only local_persist s8 filter_kinds[] = {BEAMFORMER_FILTER_KIND_LIST(,)};
	#undef X

	Stream sb = arena_stream(arena);
	stream_append_s8s(&sb, filter_kinds[kind % countof(filter_kinds)], s8("["));
	stream_append_u64(&sb, block);
	stream_append_s8(&sb, s8("]["));
	stream_append_u64(&sb, slot);
	stream_append_byte(&sb, ']');
	s8 label = arena_stream_commit(&arena, &sb);

	void *filter = 0;
	switch (kind) {
	case BeamformerFilterKind_Kaiser:{
		/* TODO(rnp): this should also support complex */
		/* TODO(rnp): implement this as an IFIR filter instead to reduce computation */
		filter = kaiser_low_pass_filter(&arena, fp.Kaiser.cutoff_frequency, fp.sampling_frequency,
		                                fp.Kaiser.beta, (i32)fp.Kaiser.length);
		f->length     = (i32)fp.Kaiser.length;
		f->time_delay = (f32)f->length / 2.0f / fp.sampling_frequency;
	}break;
	case BeamformerFilterKind_MatchedChirp:{
		typeof(fp.MatchedChirp) *mc = &fp.MatchedChirp;
		f32 fs    = fp.sampling_frequency;
		f->length = (i32)(mc->duration * fs);
		if (fp.complex) {
			filter = baseband_chirp(&arena, mc->min_frequency, mc->max_frequency, fs, f->length, 1, 0.5f);
			f->time_delay = complex_filter_first_moment(filter, f->length, fs);
		} else {
			filter = rf_chirp(&arena, mc->min_frequency, mc->max_frequency, fs, f->length, 1);
			f->time_delay = real_filter_first_moment(filter, f->length, fs);
		}
	}break;
	InvalidDefaultCase;
	}

	f->kind       = kind;
	f->parameters = fp;

	glDeleteTextures(1, &f->texture);
	glCreateTextures(GL_TEXTURE_1D, 1, &f->texture);
	glTextureStorage1D(f->texture, 1, fp.complex? GL_RG32F : GL_R32F, f->length);
	glTextureSubImage1D(f->texture, 0, 0, f->length, fp.complex? GL_RG : GL_RED, GL_FLOAT, filter);
	glObjectLabel(GL_TEXTURE, f->texture, (i32)label.len, (c8 *)label.data);
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

function BeamformerFrame *
frame_next(ComputeFrameIterator *bfi)
{
	BeamformerFrame *result = 0;
	if (bfi->cursor != bfi->needed_frames) {
		u32 index = (bfi->offset + bfi->cursor++) % bfi->capacity;
		result    = bfi->frames + index;
	}
	return result;
}

function void
alloc_beamform_frame(GLParams *gp, BeamformerFrame *out, iv3 out_dim, s8 name, Arena arena)
{
	out->dim.x = MAX(1, out_dim.x);
	out->dim.y = MAX(1, out_dim.y);
	out->dim.z = MAX(1, out_dim.z);

	if (gp) {
		out->dim.x = MIN(out->dim.x, gp->max_3d_texture_dim);
		out->dim.y = MIN(out->dim.y, gp->max_3d_texture_dim);
		out->dim.z = MIN(out->dim.z, gp->max_3d_texture_dim);
	}

	/* NOTE: allocate storage for beamformed output data;
	 * this is shared between compute and fragment shaders */
	u32 max_dim = (u32)MAX(out->dim.x, MAX(out->dim.y, out->dim.z));
	out->mips   = (i32)ctz_u32(round_up_power_of_2(max_dim)) + 1;

	Stream label = arena_stream(arena);
	stream_append_s8(&label, name);
	stream_append_byte(&label, '[');
	stream_append_hex_u64(&label, out->id);
	stream_append_byte(&label, ']');

	glDeleteTextures(1, &out->texture);
	glCreateTextures(GL_TEXTURE_3D, 1, &out->texture);
	glTextureStorage3D(out->texture, out->mips, GL_RG32F, out->dim.x, out->dim.y, out->dim.z);

	glTextureParameteri(out->texture, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTextureParameteri(out->texture, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

	LABEL_GL_OBJECT(GL_TEXTURE, out->texture, stream_to_s8(&label));
}

function void
update_hadamard_texture(BeamformerComputePlan *cp, i32 order, Arena arena)
{
	i32 *hadamard = make_hadamard_transpose(&arena, order);
	if (hadamard) {
		cp->hadamard_order = order;
		u32 *texture = cp->textures + BeamformerComputeTextureKind_Hadamard;
		glDeleteTextures(1, texture);
		glCreateTextures(GL_TEXTURE_2D, 1, texture);
		glTextureStorage2D(*texture, 1, GL_R8I, order, order);
		glTextureSubImage2D(*texture, 0, 0, 0,  order, order, GL_RED_INTEGER, GL_INT, hadamard);

		Stream label = arena_stream(arena);
		stream_append_s8(&label, s8("Hadamard"));
		stream_append_i64(&label, order);
		LABEL_GL_OBJECT(GL_TEXTURE, *texture, stream_to_s8(&label));
	}
}

function void
alloc_shader_storage(BeamformerCtx *ctx, u32 decoded_data_size, Arena arena)
{
	BeamformerComputeContext *cc = &ctx->compute_context;
	glDeleteBuffers(countof(cc->ping_pong_ssbos), cc->ping_pong_ssbos);
	glCreateBuffers(countof(cc->ping_pong_ssbos), cc->ping_pong_ssbos);

	cc->ping_pong_ssbo_size = decoded_data_size;

	Stream label = arena_stream(arena);
	stream_append_s8(&label, s8("PingPongSSBO["));
	i32 s_widx = label.widx;
	for (i32 i = 0; i < countof(cc->ping_pong_ssbos); i++) {
		glNamedBufferStorage(cc->ping_pong_ssbos[i], (iz)decoded_data_size, 0, 0);
		stream_append_i64(&label, i);
		stream_append_byte(&label, ']');
		LABEL_GL_OBJECT(GL_BUFFER, cc->ping_pong_ssbos[i], stream_to_s8(&label));
		stream_reset(&label, s_widx);
	}

	/* TODO(rnp): (25.08.04) cuda lib is heavily broken atm. First there are multiple RF
	 * buffers and cuda decode shouldn't assume that the data is coming from the rf_buffer
	 * ssbo. Second each parameter block may need a different hadamard matrix so ideally
	 * decode should just take the texture as a parameter. Third, none of these dimensions
	 * need to be pre-known by the library unless its allocating GPU memory which it shouldn't
	 * need to do. For now grab out of parameter block 0 but it is not correct */
	BeamformerParameterBlock *pb = beamformer_parameter_block(ctx->shared_memory.region, 0);
	/* NOTE(rnp): these are stubs when CUDA isn't supported */
	cuda_register_buffers(cc->ping_pong_ssbos, countof(cc->ping_pong_ssbos), cc->rf_buffer.ssbo);
	u32 decoded_data_dimension[3] = {pb->parameters.sample_count, pb->parameters.channel_count, pb->parameters.acquisition_count};
	cuda_init(pb->parameters.raw_data_dimensions, decoded_data_dimension);
}

function void
push_compute_timing_info(ComputeTimingTable *t, ComputeTimingInfo info)
{
	u32 index = atomic_add_u32(&t->write_index, 1) % countof(t->buffer);
	t->buffer[index] = info;
}

function b32
fill_frame_compute_work(BeamformerCtx *ctx, BeamformWork *work, BeamformerViewPlaneTag plane,
                        u32 parameter_block, b32 indirect)
{
	b32 result = 0;
	if (work) {
		result = 1;
		u32 frame_id    = atomic_add_u32(&ctx->next_render_frame_index, 1);
		u32 frame_index = frame_id % countof(ctx->beamform_frames);
		work->kind      = indirect? BeamformerWorkKind_ComputeIndirect : BeamformerWorkKind_Compute;
		work->lock      = BeamformerSharedMemoryLockKind_DispatchCompute;
		work->compute_context.parameter_block = parameter_block;
		work->compute_context.frame = ctx->beamform_frames + frame_index;
		work->compute_context.frame->ready_to_present = 0;
		work->compute_context.frame->view_plane_tag   = plane;
		work->compute_context.frame->id               = frame_id;
	}
	return result;
}

function void
do_sum_shader(BeamformerComputeContext *cc, u32 *in_textures, u32 in_texture_count, f32 in_scale,
              u32 out_texture, iv3 out_data_dim)
{
	/* NOTE: zero output before summing */
	glClearTexImage(out_texture, 0, GL_RED, GL_FLOAT, 0);
	glMemoryBarrier(GL_TEXTURE_UPDATE_BARRIER_BIT);

	glBindImageTexture(0, out_texture, 0, GL_TRUE, 0, GL_READ_WRITE, GL_RG32F);
	glProgramUniform1f(cc->programs[BeamformerShaderKind_Sum], SUM_PRESCALE_UNIFORM_LOC, in_scale);
	for (u32 i = 0; i < in_texture_count; i++) {
		glBindImageTexture(1, in_textures[i], 0, GL_TRUE, 0, GL_READ_ONLY, GL_RG32F);
		glDispatchCompute(ORONE((u32)out_data_dim.x / 32u),
		                  ORONE((u32)out_data_dim.y),
		                  ORONE((u32)out_data_dim.z / 32u));
		glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
	}
}

struct compute_cursor {
	iv3 cursor;
	uv3 dispatch;
	iv3 target;
	u32 points_per_dispatch;
	u32 completed_points;
	u32 total_points;
};

function struct compute_cursor
start_compute_cursor(iv3 dim, u32 max_points)
{
	struct compute_cursor result = {0};
	u32 invocations_per_dispatch = DAS_LOCAL_SIZE_X * DAS_LOCAL_SIZE_Y * DAS_LOCAL_SIZE_Z;

	result.dispatch.y = MIN(max_points / invocations_per_dispatch, (u32)ceil_f32((f32)dim.y / DAS_LOCAL_SIZE_Y));

	u32 remaining     = max_points / result.dispatch.y;
	result.dispatch.x = MIN(remaining / invocations_per_dispatch, (u32)ceil_f32((f32)dim.x / DAS_LOCAL_SIZE_X));
	result.dispatch.z = MIN(remaining / (invocations_per_dispatch * result.dispatch.x),
	                        (u32)ceil_f32((f32)dim.z / DAS_LOCAL_SIZE_Z));

	result.target.x = MAX(dim.x / (i32)result.dispatch.x / DAS_LOCAL_SIZE_X, 1);
	result.target.y = MAX(dim.y / (i32)result.dispatch.y / DAS_LOCAL_SIZE_Y, 1);
	result.target.z = MAX(dim.z / (i32)result.dispatch.z / DAS_LOCAL_SIZE_Z, 1);

	result.points_per_dispatch = 1;
	result.points_per_dispatch *= result.dispatch.x * DAS_LOCAL_SIZE_X;
	result.points_per_dispatch *= result.dispatch.y * DAS_LOCAL_SIZE_Y;
	result.points_per_dispatch *= result.dispatch.z * DAS_LOCAL_SIZE_Z;

	result.total_points = (u32)(dim.x * dim.y * dim.z);

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
	result.x *= (i32)cursor->dispatch.x * DAS_LOCAL_SIZE_X;
	result.y *= (i32)cursor->dispatch.y * DAS_LOCAL_SIZE_Y;
	result.z *= (i32)cursor->dispatch.z * DAS_LOCAL_SIZE_Z;

	return result;
}

function b32
compute_cursor_finished(struct compute_cursor *cursor)
{
	b32 result = cursor->completed_points >= cursor->total_points;
	return result;
}

function m4
das_voxel_transform_matrix(BeamformerParameters *bp)
{
	v3 min = v3_from_f32_array(bp->output_min_coordinate);
	v3 max = v3_from_f32_array(bp->output_max_coordinate);
	v3 extent = v3_abs(v3_sub(max, min));
	v3 points = {{(f32)bp->output_points[0], (f32)bp->output_points[1], (f32)bp->output_points[2]}};

	m4 T1 = m4_translation(v3_scale(v3_sub(points, (v3){{1.0f, 1.0f, 1.0f}}), -0.5f));
	m4 T2 = m4_translation(v3_add(min, v3_scale(extent, 0.5f)));
	m4 S  = m4_scale(v3_div(extent, points));

	m4 R;
	switch (bp->das_shader_id) {
	case DASShaderKind_FORCES:
	case DASShaderKind_UFORCES:
	case DASShaderKind_Flash:
	{
		R = m4_identity();
		S.c[1].E[1]  = 0;
		T2.c[3].E[1] = 0;
	}break;
	case DASShaderKind_HERCULES:
	case DASShaderKind_UHERCULES:
	case DASShaderKind_RCA_TPW:
	case DASShaderKind_RCA_VLS:
	{
		R = m4_rotation_about_z(bp->beamform_plane ? 0.0f : 0.25f);
		if (!(points.x > 1 && points.y > 1 && points.z > 1))
			T2.c[3].E[1] = bp->off_axis_pos;
	}break;
	default:{ R = m4_identity(); }break;
	}
	m4 result = m4_mul(R, m4_mul(T2, m4_mul(S, T1)));
	return result;
}

function void
das_ubo_from_beamformer_parameters(BeamformerDASUBO *du, BeamformerParameters *bp)
{
	du->voxel_transform = das_voxel_transform_matrix(bp);
	mem_copy(du->xdc_transform.E,     bp->xdc_transform,     sizeof(du->xdc_transform));
	mem_copy(du->xdc_element_pitch.E, bp->xdc_element_pitch, sizeof(du->xdc_element_pitch));
	du->sampling_frequency     = bp->sampling_frequency;
	du->demodulation_frequency = bp->demodulation_frequency;
	du->speed_of_sound         = bp->speed_of_sound;
	du->time_offset            = bp->time_offset;
	du->f_number               = bp->f_number;
	du->shader_kind            = bp->das_shader_id;
	du->sample_count           = bp->sample_count;
	du->channel_count          = bp->channel_count;
	du->acquisition_count      = bp->acquisition_count;

	du->shader_flags = 0;
	if (bp->interpolate)         du->shader_flags |= DASShaderFlags_Interpolate;
	if (bp->coherency_weighting) du->shader_flags |= DASShaderFlags_CoherencyWeighting;
	if (bp->transmit_mode == BeamformerTransmitMode_Columns)
		du->shader_flags |= DASShaderFlags_TxColumns;
	if (bp->receive_mode == BeamformerReceiveMode_Columns)
		du->shader_flags |= DASShaderFlags_RxColumns;
}

function void
plan_compute_pipeline(BeamformerComputePlan *cp, BeamformerParameterBlock *pb)
{
	BeamformerDASUBO *bp = &cp->das_ubo_data;

	das_ubo_from_beamformer_parameters(bp, &pb->parameters);

	b32 decode_first = pb->pipeline.shaders[0] == BeamformerShaderKind_Decode;
	b32 run_cuda_hilbert = 0;
	b32 demodulate       = 0;

	for (u32 i = 0; i < pb->pipeline.shader_count; i++) {
		switch (pb->pipeline.shaders[i]) {
		case BeamformerShaderKind_CudaHilbert:{ run_cuda_hilbert = 1; }break;
		case BeamformerShaderKind_Demodulate:{  demodulate = 1;       }break;
		default:{}break;
		}
	}

	if (demodulate) run_cuda_hilbert = 0;

	BeamformerDataKind data_kind = pb->pipeline.data_kind;
	cp->pipeline.shader_count = 0;
	for (u32 i = 0; i < pb->pipeline.shader_count; i++) {
		BeamformerShaderParameters *sp = pb->pipeline.parameters + i;
		u32 shader = pb->pipeline.shaders[i];
		b32 commit = 0;

		switch (shader) {
		case BeamformerShaderKind_CudaHilbert:{ commit = run_cuda_hilbert; }break;
		case BeamformerShaderKind_Decode:{
			BeamformerShaderKind decode_table[] = {
				[BeamformerDataKind_Int16]          = BeamformerShaderKind_Decode,
				[BeamformerDataKind_Int16Complex]   = BeamformerShaderKind_DecodeInt16Complex,
				[BeamformerDataKind_Float32]        = BeamformerShaderKind_DecodeFloat,
				[BeamformerDataKind_Float32Complex] = BeamformerShaderKind_DecodeFloatComplex,
			};
			if (decode_first && demodulate) {
				/* TODO(rnp): for now we assume that if we are demodulating the data is int16 */
				shader = BeamformerShaderKind_DecodeInt16ToFloat;
			} else if (decode_first) {
				shader = decode_table[CLAMP(data_kind, 0, countof(decode_table) - 1)];
			} else {
				if (data_kind == BeamformerDataKind_Int16)
					shader = BeamformerShaderKind_DecodeInt16Complex;
				else
					shader = BeamformerShaderKind_DecodeFloatComplex;
			}
			commit = 1;
		}break;
		case BeamformerShaderKind_Demodulate:{
			BeamformerFilter *f = cp->filters + sp->filter_slot;
			if (decode_first || (!decode_first && data_kind == BeamformerDataKind_Float32)) {
				if (f->parameters.complex) shader = BeamformerShaderKind_DemodulateFloatCF;
				else                       shader = BeamformerShaderKind_DemodulateFloat;
			} else if (f->parameters.complex) {
				shader = BeamformerShaderKind_DemodulateCF;
			}
			bp->time_offset += f->time_delay;
			commit = 1;
		}break;
		case BeamformerShaderKind_Filter:{
			BeamformerFilter *f = cp->filters + sp->filter_slot;
			if (f->parameters.complex) shader = BeamformerShaderKind_FilterCF;
			bp->time_offset += f->time_delay;
			commit = 1;
		}break;
		case BeamformerShaderKind_DAS:{
			if ((bp->shader_flags & DASShaderFlags_CoherencyWeighting) == 0)
				shader = BeamformerShaderKind_DASFast;
			commit = 1;
		}break;
		default:{ commit = 1; }break;
		}

		if (commit) {
			u32 index = cp->pipeline.shader_count++;
			cp->pipeline.shaders[index]    = shader;
			cp->pipeline.parameters[index] = *sp;
		}
	}
	cp->pipeline.data_kind = data_kind;

	u32 das_sample_stride   = 1;
	u32 das_transmit_stride = bp->sample_count;
	u32 das_channel_stride  = bp->acquisition_count * bp->sample_count;

	u32 decimation_rate = MAX(pb->parameters.decimation_rate, 1);
	if (demodulate) {
		das_channel_stride  /= (2 * decimation_rate);
		das_transmit_stride /= (2 * decimation_rate);
	}

	u32 input_sample_stride   = 1;
	u32 input_transmit_stride = bp->sample_count;
	u32 input_channel_stride  = pb->parameters.raw_data_dimensions[0];

	BeamformerDecodeUBO *dp = &cp->decode_ubo_data;
	dp->decode_mode    = pb->parameters.decode;
	dp->transmit_count = bp->acquisition_count;

	dp->input_sample_stride    = decode_first? input_sample_stride   : bp->acquisition_count;
	dp->input_channel_stride   = decode_first? input_channel_stride  : das_channel_stride;
	dp->input_transmit_stride  = decode_first? input_transmit_stride : 1;
	dp->output_sample_stride   = das_sample_stride;
	dp->output_channel_stride  = das_channel_stride;
	dp->output_transmit_stride = das_transmit_stride;
	if (decode_first) {
		dp->output_channel_stride  *= decimation_rate;
		dp->output_transmit_stride *= decimation_rate;
	}

	if (!demodulate) bp->demodulation_frequency = 0;

	cp->decode_dispatch.x = (u32)ceil_f32((f32)bp->sample_count      / DECODE_LOCAL_SIZE_X);
	cp->decode_dispatch.y = (u32)ceil_f32((f32)bp->channel_count     / DECODE_LOCAL_SIZE_Y);
	cp->decode_dispatch.z = (u32)ceil_f32((f32)bp->acquisition_count / DECODE_LOCAL_SIZE_Z);

	/* NOTE(rnp): decode 2 samples per dispatch when data is i16 */
	if (decode_first && data_kind == BeamformerDataKind_Int16)
		cp->decode_dispatch.x = (u32)ceil_f32((f32)cp->decode_dispatch.x / 2);

	/* NOTE(rnp): when we are demodulating we pretend that the sampler was alternating
	 * between sampling the I portion and the Q portion of an IQ signal. Therefore there
	 * is an implicit decimation factor of 2 which must always be included. All code here
	 * assumes that the signal was sampled in such a way that supports this operation.
	 * To recover IQ[n] from the sampled data (RF[n]) we do the following:
	 *   I[n]  = RF[n]
	 *   Q[n]  = RF[n + 1]
	 *   IQ[n] = I[n] - j*Q[n]
	 */
	if (demodulate) {
		BeamformerFilterUBO *mp    = &cp->demod_ubo_data;
		mp->demodulation_frequency = bp->demodulation_frequency;
		mp->sampling_frequency     = bp->sampling_frequency / 2;
		mp->decimation_rate        = decimation_rate;

		if (!decode_first) mp->shader_flags |= FilterShaderFlags_MapChannels;

		bp->sampling_frequency /= 2 * (f32)mp->decimation_rate;
		bp->sample_count       /= 2 * mp->decimation_rate;

		if (decode_first) {
			mp->input_channel_stride  = dp->output_channel_stride;
			mp->input_sample_stride   = dp->output_sample_stride;
			mp->input_transmit_stride = dp->output_transmit_stride;

			mp->output_channel_stride  = das_channel_stride;
			mp->output_sample_stride   = das_sample_stride;
			mp->output_transmit_stride = das_transmit_stride;
		} else {
			mp->input_channel_stride  = input_channel_stride  / 2;
			mp->input_sample_stride   = input_sample_stride;
			mp->input_transmit_stride = input_transmit_stride / 2;

			/* NOTE(rnp): output optimized layout for decoding */
			mp->output_channel_stride  = dp->input_channel_stride;
			mp->output_sample_stride   = dp->input_sample_stride;
			mp->output_transmit_stride = dp->input_transmit_stride;

			cp->decode_dispatch.x = (u32)ceil_f32((f32)bp->sample_count / DECODE_LOCAL_SIZE_X);
		}
	}

	/* TODO(rnp): filter may need a different dispatch layout */
	cp->demod_dispatch.x = (u32)ceil_f32((f32)bp->sample_count      / FILTER_LOCAL_SIZE_X);
	cp->demod_dispatch.y = (u32)ceil_f32((f32)bp->channel_count     / FILTER_LOCAL_SIZE_Y);
	cp->demod_dispatch.z = (u32)ceil_f32((f32)bp->acquisition_count / FILTER_LOCAL_SIZE_Z);

	/* TODO(rnp): if IQ (* 8) else (* 4) */
	cp->rf_size = bp->sample_count * bp->channel_count * bp->acquisition_count * 8;

	/* TODO(rnp): UBO per filter stage */
	BeamformerFilterUBO *flt = &cp->filter_ubo_data;
	flt->demodulation_frequency = bp->demodulation_frequency;
	flt->sampling_frequency     = bp->sampling_frequency;
	flt->decimation_rate        = 1;
	flt->shader_flags           = pb->parameters.sampling_mode & FilterShaderFlags_SamplingModeMask;
	flt->output_channel_stride  = bp->sample_count * bp->acquisition_count;
	flt->output_sample_stride   = 1;
	flt->output_transmit_stride = bp->sample_count;
	flt->input_channel_stride   = bp->sample_count * bp->acquisition_count;
	flt->input_sample_stride    = 1;
	flt->input_transmit_stride  = bp->sample_count;
}

function void
beamformer_commit_parameter_block(BeamformerCtx *ctx, BeamformerComputePlan *cp, u32 block, Arena arena)
{
	BeamformerParameterBlock *pb = beamformer_parameter_block_lock(&ctx->shared_memory, block, -1);
	for (u32 region = ctz_u32(pb->dirty_regions);
	     region != 32;
	     region = ctz_u32(pb->dirty_regions))
	{
		mark_parameter_block_region_clean(ctx->shared_memory.region, block, region);
		switch (region) {
		case BeamformerParameterBlockRegion_ComputePipeline:
		case BeamformerParameterBlockRegion_Parameters:
		{
			plan_compute_pipeline(cp, pb);

			/* NOTE(rnp): these are both handled by plan_compute_pipeline() */
			u32 mask = 1 << BeamformerParameterBlockRegion_ComputePipeline |
			           1 << BeamformerParameterBlockRegion_Parameters;
			pb->dirty_regions &= ~mask;

			#define X(k, t, v) glNamedBufferSubData(cp->ubos[BeamformerComputeUBOKind_##k], \
			                                        0, sizeof(t), &cp->v ## _ubo_data);
			BEAMFORMER_COMPUTE_UBO_LIST
			#undef X

			u32 samples      = pb->parameters.sample_count;
			u32 channels     = pb->parameters.channel_count;
			u32 acquisitions = pb->parameters.acquisition_count;
			u32 decoded_data_size = (u32)(2 * sizeof(f32) * samples * channels * acquisitions);
			if (ctx->compute_context.ping_pong_ssbo_size < decoded_data_size)
				alloc_shader_storage(ctx, decoded_data_size, arena);

			if (cp->hadamard_order != (i32)cp->das_ubo_data.acquisition_count)
				update_hadamard_texture(cp, (i32)cp->das_ubo_data.acquisition_count, arena);

			cp->min_coordinate = v3_from_f32_array(pb->parameters.output_min_coordinate);
			cp->max_coordinate = v3_from_f32_array(pb->parameters.output_max_coordinate);

			cp->output_points.E[0] = MAX(pb->parameters.output_points[0], 1);
			cp->output_points.E[1] = MAX(pb->parameters.output_points[1], 1);
			cp->output_points.E[2] = MAX(pb->parameters.output_points[2], 1);
			cp->average_frames     = pb->parameters.output_points[3];

			if (cp->average_frames > 1 && !iv3_equal(cp->output_points, ctx->averaged_frames[0].dim)) {
				alloc_beamform_frame(&ctx->gl, ctx->averaged_frames + 0, cp->output_points, s8("Averaged Frame"), arena);
				alloc_beamform_frame(&ctx->gl, ctx->averaged_frames + 1, cp->output_points, s8("Averaged Frame"), arena);
			}
		}break;
		case BeamformerParameterBlockRegion_ChannelMapping:
		case BeamformerParameterBlockRegion_FocalVectors:
		case BeamformerParameterBlockRegion_SparseElements:
		{
			BeamformerComputeTextureKind texture_kind = 0;
			u32 texture_type = 0, texture_format = 0;
			/* TODO(rnp): this whole thing could be a table */
			switch (region) {
			case BeamformerParameterBlockRegion_ChannelMapping:{
				texture_kind   = BeamformerComputeTextureKind_ChannelMapping;
				texture_type   = GL_SHORT;
				texture_format = GL_RED_INTEGER;
				/* TODO(rnp): cuda lib */
				cuda_set_channel_mapping(pb->channel_mapping);
			}break;
			case BeamformerParameterBlockRegion_FocalVectors:{
				texture_kind   = BeamformerComputeTextureKind_FocalVectors;
				texture_type   = GL_FLOAT;
				texture_format = GL_RG;
			}break;
			case BeamformerParameterBlockRegion_SparseElements:{
				texture_kind   = BeamformerComputeTextureKind_SparseElements;
				texture_type   = GL_SHORT;
				texture_format = GL_RED_INTEGER;
			}break;
			InvalidDefaultCase;
			}
			glTextureSubImage1D(cp->textures[texture_kind], 0, 0, BeamformerMaxChannelCount,
			                    texture_format, texture_type,
			                    (u8 *)pb + BeamformerParameterBlockRegionOffsets[region]);
		}break;
		}
	}
	beamformer_parameter_block_unlock(&ctx->shared_memory, block);
}

function void
do_compute_shader(BeamformerCtx *ctx, BeamformerComputePlan *cp, BeamformerFrame *frame,
                  BeamformerShaderKind shader, BeamformerShaderParameters *sp, Arena arena)
{
	BeamformerComputeContext *cc = &ctx->compute_context;

	u32 program = cc->programs[shader];
	glUseProgram(program);

	u32 output_ssbo_idx = !cc->last_output_ssbo_index;
	u32 input_ssbo_idx  = cc->last_output_ssbo_index;

	switch (shader) {
	case BeamformerShaderKind_Decode:
	case BeamformerShaderKind_DecodeInt16Complex:
	case BeamformerShaderKind_DecodeFloat:
	case BeamformerShaderKind_DecodeFloatComplex:
	case BeamformerShaderKind_DecodeInt16ToFloat:
	{
		glBindBufferBase(GL_UNIFORM_BUFFER, 0, cp->ubos[BeamformerComputeUBOKind_Decode]);
		glBindImageTexture(0, cp->textures[BeamformerComputeTextureKind_Hadamard], 0, 0, 0, GL_READ_ONLY, GL_R8I);

		if (shader == cp->pipeline.shaders[0]) {
			glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, cc->ping_pong_ssbos[input_ssbo_idx]);
			glBindImageTexture(1, cp->textures[BeamformerComputeTextureKind_ChannelMapping], 0, 0, 0, GL_READ_ONLY, GL_R16I);
			glProgramUniform1ui(program, DECODE_FIRST_PASS_UNIFORM_LOC, 1);

			glDispatchCompute(cp->decode_dispatch.x, cp->decode_dispatch.y, cp->decode_dispatch.z);
			glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);
		}

		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, cc->ping_pong_ssbos[input_ssbo_idx]);
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 3, cc->ping_pong_ssbos[output_ssbo_idx]);

		glProgramUniform1ui(program, DECODE_FIRST_PASS_UNIFORM_LOC, 0);

		glDispatchCompute(cp->decode_dispatch.x, cp->decode_dispatch.y, cp->decode_dispatch.z);
		glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);

		cc->last_output_ssbo_index = !cc->last_output_ssbo_index;
	}break;
	case BeamformerShaderKind_CudaDecode:{
		cuda_decode(0, output_ssbo_idx, 0);
		cc->last_output_ssbo_index = !cc->last_output_ssbo_index;
	}break;
	case BeamformerShaderKind_CudaHilbert:{
		cuda_hilbert(input_ssbo_idx, output_ssbo_idx);
		cc->last_output_ssbo_index = !cc->last_output_ssbo_index;
	}break;
	case BeamformerShaderKind_Filter:
	case BeamformerShaderKind_FilterCF:
	case BeamformerShaderKind_Demodulate:
	case BeamformerShaderKind_DemodulateCF:
	case BeamformerShaderKind_DemodulateFloat:
	case BeamformerShaderKind_DemodulateFloatCF:
	{
		BeamformerFilterUBO *ubo = &cp->demod_ubo_data;
		if (shader == BeamformerShaderKind_Filter)
			ubo = &cp->filter_ubo_data;

		u32 index = shader == BeamformerShaderKind_Filter ? BeamformerComputeUBOKind_Filter
		                                                  : BeamformerComputeUBOKind_Demodulate;
		glBindBufferBase(GL_UNIFORM_BUFFER,        0, cp->ubos[index]);
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, cc->ping_pong_ssbos[output_ssbo_idx]);
		if ((ubo->shader_flags & FilterShaderFlags_MapChannels) == 0)
			glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, cc->ping_pong_ssbos[input_ssbo_idx]);

		GLenum kind = cp->filters[sp->filter_slot].parameters.complex? GL_RG32F : GL_R32F;
		glBindImageTexture(0, cp->filters[sp->filter_slot].texture, 0, 0, 0, GL_READ_ONLY, kind);
		if (ubo->shader_flags & FilterShaderFlags_MapChannels)
			glBindImageTexture(1, cp->textures[BeamformerComputeTextureKind_ChannelMapping], 0, 0, 0, GL_READ_ONLY, GL_R16I);

		glDispatchCompute(cp->demod_dispatch.x, cp->demod_dispatch.y, cp->demod_dispatch.z);
		glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);

		cc->last_output_ssbo_index = !cc->last_output_ssbo_index;
	}break;
	case BeamformerShaderKind_MinMax:{
		for (i32 i = 1; i < frame->mips; i++) {
			glBindImageTexture(0, frame->texture, i - 1, GL_TRUE, 0, GL_READ_ONLY,  GL_RG32F);
			glBindImageTexture(1, frame->texture, i - 0, GL_TRUE, 0, GL_WRITE_ONLY, GL_RG32F);
			glProgramUniform1i(cc->programs[shader], MIN_MAX_MIPS_LEVEL_UNIFORM_LOC, i);

			u32 width  = (u32)frame->dim.x >> i;
			u32 height = (u32)frame->dim.y >> i;
			u32 depth  = (u32)frame->dim.z >> i;
			glDispatchCompute(ORONE(width / 32), ORONE(height), ORONE(depth / 32));
			glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
		}
	}break;
	case BeamformerShaderKind_DAS:
	case BeamformerShaderKind_DASFast:
	{
		BeamformerDASUBO *ubo = &cp->das_ubo_data;
		if (shader == BeamformerShaderKind_DASFast) {
			glClearTexImage(frame->texture, 0, GL_RED, GL_FLOAT, 0);
			glMemoryBarrier(GL_TEXTURE_UPDATE_BARRIER_BIT);
			glBindImageTexture(0, frame->texture, 0, GL_TRUE, 0, GL_READ_WRITE, GL_RG32F);
		} else {
			glBindImageTexture(0, frame->texture, 0, GL_TRUE, 0, GL_WRITE_ONLY, GL_RG32F);
		}

		glBindBufferBase(GL_UNIFORM_BUFFER, 0, cp->ubos[BeamformerComputeUBOKind_DAS]);
		glBindBufferRange(GL_SHADER_STORAGE_BUFFER, 1, cc->ping_pong_ssbos[input_ssbo_idx], 0, cp->rf_size);
		glBindImageTexture(1, cp->textures[BeamformerComputeTextureKind_SparseElements], 0, 0, 0, GL_READ_ONLY, GL_R16I);
		glBindImageTexture(2, cp->textures[BeamformerComputeTextureKind_FocalVectors],   0, 0, 0, GL_READ_ONLY, GL_RG32F);

		glProgramUniform1ui(program, DAS_CYCLE_T_UNIFORM_LOC, cycle_t++);

		if (shader == BeamformerShaderKind_DASFast) {
			i32 loop_end;
			if (ubo->shader_kind == DASShaderKind_RCA_VLS ||
			    ubo->shader_kind == DASShaderKind_RCA_TPW)
			{
				/* NOTE(rnp): to avoid repeatedly sampling the whole focal vectors
				 * texture we loop over transmits for VLS/TPW */
				loop_end = (i32)ubo->acquisition_count;
			} else {
				loop_end = (i32)ubo->channel_count;
			}
			f32 percent_per_step = 1.0f / (f32)loop_end;
			cc->processing_progress = -percent_per_step;
			for (i32 index = 0; index < loop_end; index++) {
				cc->processing_progress += percent_per_step;
				/* IMPORTANT(rnp): prevents OS from coalescing and killing our shader */
				glFinish();
				glProgramUniform1i(program, DAS_FAST_CHANNEL_UNIFORM_LOC, index);
				glDispatchCompute((u32)ceil_f32((f32)frame->dim.x / DAS_FAST_LOCAL_SIZE_X),
				                  (u32)ceil_f32((f32)frame->dim.y / DAS_FAST_LOCAL_SIZE_Y),
				                  (u32)ceil_f32((f32)frame->dim.z / DAS_FAST_LOCAL_SIZE_Z));
				glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
			}
		} else {
			#if 1
			/* TODO(rnp): compute max_points_per_dispatch based on something like a
			 * transmit_count * channel_count product */
			u32 max_points_per_dispatch = KB(64);
			struct compute_cursor cursor = start_compute_cursor(frame->dim, max_points_per_dispatch);
			f32 percent_per_step = (f32)cursor.points_per_dispatch / (f32)cursor.total_points;
			cc->processing_progress = -percent_per_step;
			for (iv3 offset = {0};
			     !compute_cursor_finished(&cursor);
			     offset = step_compute_cursor(&cursor))
			{
				cc->processing_progress += percent_per_step;
				/* IMPORTANT(rnp): prevents OS from coalescing and killing our shader */
				glFinish();
				glProgramUniform3iv(program, DAS_VOXEL_OFFSET_UNIFORM_LOC, 1, offset.E);
				glDispatchCompute(cursor.dispatch.x, cursor.dispatch.y, cursor.dispatch.z);
			}
			#else
			/* NOTE(rnp): use this for testing tiling code. The performance of the above path
			 * should be the same as this path if everything is working correctly */
			iv3 compute_dim_offset = {0};
			glProgramUniform3iv(program, DAS_VOXEL_OFFSET_UNIFORM_LOC, 1, compute_dim_offset.E);
			glDispatchCompute((u32)ceil_f32((f32)dim.x / DAS_LOCAL_SIZE_X),
			                  (u32)ceil_f32((f32)dim.y / DAS_LOCAL_SIZE_Y),
			                  (u32)ceil_f32((f32)dim.z / DAS_LOCAL_SIZE_Z));
			#endif
		}
		glMemoryBarrier(GL_TEXTURE_UPDATE_BARRIER_BIT|GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
	}break;
	case BeamformerShaderKind_Sum:{
		u32 aframe_index = ctx->averaged_frame_index % ARRAY_COUNT(ctx->averaged_frames);
		BeamformerFrame *aframe = ctx->averaged_frames + aframe_index;
		aframe->id              = ctx->averaged_frame_index;
		atomic_store_u32(&aframe->ready_to_present, 0);
		/* TODO(rnp): hack we need a better way of specifying which frames to sum;
		 * this is fine for rolling averaging but what if we want to do something else */
		assert(frame >= ctx->beamform_frames);
		assert(frame < ctx->beamform_frames + countof(ctx->beamform_frames));
		u32 base_index   = (u32)(frame - ctx->beamform_frames);
		u32 to_average   = (u32)cp->average_frames;
		u32 frame_count  = 0;
		u32 *in_textures = push_array(&arena, u32, BeamformerMaxSavedFrames);
		ComputeFrameIterator cfi = compute_frame_iterator(ctx, 1 + base_index - to_average, to_average);
		for (BeamformerFrame *it = frame_next(&cfi); it; it = frame_next(&cfi))
			in_textures[frame_count++] = it->texture;

		assert(to_average == frame_count);

		do_sum_shader(cc, in_textures, frame_count, 1 / (f32)frame_count, aframe->texture, aframe->dim);
		aframe->min_coordinate  = frame->min_coordinate;
		aframe->max_coordinate  = frame->max_coordinate;
		aframe->compound_count  = frame->compound_count;
		aframe->das_shader_kind = frame->das_shader_kind;
	}break;
	InvalidDefaultCase;
	}
}

function s8
shader_text_with_header(ShaderReloadContext *ctx, OS *os, Arena *arena)
{
	Stream sb = arena_stream(*arena);
	stream_append_s8s(&sb, s8("#version 460 core\n\n"), ctx->header);

	switch (ctx->kind) {
	case BeamformerShaderKind_Filter:
	case BeamformerShaderKind_FilterCF:
	case BeamformerShaderKind_Demodulate:
	case BeamformerShaderKind_DemodulateCF:
	case BeamformerShaderKind_DemodulateFloat:
	case BeamformerShaderKind_DemodulateFloatCF:
	{
		stream_append_s8(&sb, s8(""
		"layout(local_size_x = " str(FILTER_LOCAL_SIZE_X) ", "
		       "local_size_y = " str(FILTER_LOCAL_SIZE_Y) ", "
		       "local_size_z = " str(FILTER_LOCAL_SIZE_Z) ") in;\n\n"
		));

		switch (ctx->kind) {
		case BeamformerShaderKind_FilterCF:
		case BeamformerShaderKind_DemodulateCF:
		case BeamformerShaderKind_DemodulateFloatCF:
		{
			stream_append_s8(&sb, s8("#define COMPLEX_FILTER 1\n"));
		}break;
		default:{
			stream_append_s8(&sb, s8("#define COMPLEX_FILTER 0\n"));
		}break;
		}

		switch (ctx->kind) {
		case BeamformerShaderKind_Filter:
		case BeamformerShaderKind_FilterCF:
		case BeamformerShaderKind_DemodulateFloat:
		case BeamformerShaderKind_DemodulateFloatCF:
		{
			stream_append_s8(&sb, s8("#define INPUT_DATA_TYPE_FLOAT\n"));
		}break;
		default:{}break;
		}

		switch (ctx->kind) {
		case BeamformerShaderKind_Demodulate:
		case BeamformerShaderKind_DemodulateCF:
		case BeamformerShaderKind_DemodulateFloat:
		case BeamformerShaderKind_DemodulateFloatCF:
		{
			stream_append_s8(&sb, s8("#define DEMODULATE\n"));
		}break;
		default:{}break;
		}

		stream_append_byte(&sb, '\n');
		#define X(k, v, ...) "#define ShaderFlags_" #k " " #v "\n"
		stream_append_s8(&sb, s8(FILTER_SHADER_FLAGS_LIST));
		#undef X

		stream_append_byte(&sb, '\n');
		#define X(k, v, ...) "#define SamplingMode_" #k " " #v "\n"
		stream_append_s8(&sb, s8(SAMPLING_MODES_LIST));
		#undef X
		stream_append_byte(&sb, '\n');
	}break;
	case BeamformerShaderKind_DAS:
	case BeamformerShaderKind_DASFast:
	{
		if (ctx->kind == BeamformerShaderKind_DAS) {
			stream_append_s8(&sb, s8(""
			"layout(local_size_x = " str(DAS_LOCAL_SIZE_X) ", "
			       "local_size_y = " str(DAS_LOCAL_SIZE_Y) ", "
			       "local_size_z = " str(DAS_LOCAL_SIZE_Z) ") in;\n\n"
			"#define DAS_FAST 0\n\n"
			"layout(location = " str(DAS_VOXEL_OFFSET_UNIFORM_LOC) ") uniform ivec3 u_voxel_offset;\n"
			));
		} else {
			stream_append_s8(&sb, s8(""
			"layout(local_size_x = " str(DAS_FAST_LOCAL_SIZE_X) ", "
			       "local_size_y = " str(DAS_FAST_LOCAL_SIZE_Y) ", "
			       "local_size_z = " str(DAS_FAST_LOCAL_SIZE_Z) ") in;\n\n"
			"#define DAS_FAST 1\n\n"
			"layout(location = " str(DAS_FAST_CHANNEL_UNIFORM_LOC) ") uniform int   u_channel;\n"
			));
		}
		stream_append_s8(&sb, s8(""
		"layout(location = " str(DAS_CYCLE_T_UNIFORM_LOC) ") uniform uint  u_cycle_t;\n\n"));

		#define X(k, v, ...) "#define ShaderFlags_" #k " " #v "\n"
		stream_append_s8(&sb, s8(DAS_SHADER_FLAGS_LIST));
		#undef X

		stream_append_byte(&sb, '\n');

		#define X(k, id, ...) "#define ShaderKind_" #k " " #id "\n"
		stream_append_s8(&sb, s8(DAS_SHADER_KIND_LIST));
		#undef X
	}break;
	case BeamformerShaderKind_Decode:
	case BeamformerShaderKind_DecodeFloat:
	case BeamformerShaderKind_DecodeFloatComplex:
	case BeamformerShaderKind_DecodeInt16Complex:
	case BeamformerShaderKind_DecodeInt16ToFloat:
	{
		s8 define_table[] = {
			[BeamformerShaderKind_DecodeFloatComplex] = s8("#define INPUT_DATA_TYPE_FLOAT_COMPLEX\n\n"),
			[BeamformerShaderKind_DecodeFloat]        = s8("#define INPUT_DATA_TYPE_FLOAT\n\n"),
			[BeamformerShaderKind_DecodeInt16Complex] = s8("#define INPUT_DATA_TYPE_INT16_COMPLEX\n\n"),
			[BeamformerShaderKind_DecodeInt16ToFloat] = s8("#define OUTPUT_DATA_TYPE_FLOAT\n\n"),
		};
		#define X(type, id, pretty) "#define DECODE_MODE_" #type " " #id "\n"
		stream_append_s8s(&sb, define_table[ctx->kind], s8(""
		"layout(local_size_x = " str(DECODE_LOCAL_SIZE_X) ", "
		       "local_size_y = " str(DECODE_LOCAL_SIZE_Y) ", "
		       "local_size_z = " str(DECODE_LOCAL_SIZE_Z) ") in;\n\n"
		"layout(location = " str(DECODE_FIRST_PASS_UNIFORM_LOC) ") uniform bool u_first_pass;\n\n"
		DECODE_TYPES
		));
		#undef X
	}break;
	case BeamformerShaderKind_MinMax:{
		stream_append_s8(&sb, s8("layout(location = " str(MIN_MAX_MIPS_LEVEL_UNIFORM_LOC)
		                         ") uniform int u_mip_map;\n\n"));
	}break;
	case BeamformerShaderKind_Sum:{
		stream_append_s8(&sb, s8("layout(location = " str(SUM_PRESCALE_UNIFORM_LOC)
		                         ") uniform float u_sum_prescale = 1.0;\n\n"));
	}break;
	default:{}break;
	}
	stream_append_s8(&sb, s8("\n#line 1\n"));

	s8 result = arena_stream_commit(arena, &sb);
	if (ctx->path.len) {
		s8 file = os_read_whole_file(arena, (c8 *)ctx->path.data);
		assert(file.data == result.data + result.len);
		result.len += file.len;
	}

	return result;
}

DEBUG_EXPORT BEAMFORMER_RELOAD_SHADER_FN(beamformer_reload_shader)
{
	i32 shader_count = 1;
	ShaderReloadContext *link = src->link;
	while (link != src) { shader_count++; link = link->link; }

	s8  *shader_texts = push_array(&arena, s8,  shader_count);
	u32 *shader_types = push_array(&arena, u32, shader_count);

	i32 index = 0;
	do {
		shader_texts[index] = shader_text_with_header(link, os, &arena);
		shader_types[index] = link->gl_type;
		index++;
		link = link->link;
	} while (link != src);

	glDeleteProgram(*src->shader);
	*src->shader = load_shader(&ctx->os, arena, shader_texts, shader_types, shader_count, shader_name);
	if (src->kind == BeamformerShaderKind_Render3D) ctx->frame_view_render_context.updated = 1;

	return 1;
}

function b32
reload_compute_shader(BeamformerCtx *ctx, ShaderReloadContext *src, s8 name_extra, Arena arena)
{
	Stream sb  = arena_stream(arena);
	stream_append_s8s(&sb, src->name, name_extra);
	s8  name   = arena_stream_commit(&arena, &sb);
	b32 result = beamformer_reload_shader(&ctx->os, ctx, src, arena, name);
	return result;
}

function void
complete_queue(BeamformerCtx *ctx, BeamformWorkQueue *q, Arena *arena, iptr gl_context)
{
	BeamformerComputeContext *cs = &ctx->compute_context;
	BeamformerSharedMemory   *sm = ctx->shared_memory.region;

	BeamformWork *work = beamform_work_queue_pop(q);
	while (work) {
		b32 can_commit = 1;
		switch (work->kind) {
		case BeamformerWorkKind_ReloadShader:{
			ShaderReloadContext *src = work->shader_reload_context;
			b32 success = reload_compute_shader(ctx, src, s8(""), *arena);
			/* TODO(rnp): think of a better way of doing this */
			switch (src->kind) {
			case BeamformerShaderKind_DAS:{
				src->kind   = BeamformerShaderKind_DASFast;
				src->shader = cs->programs + src->kind;
				success &= reload_compute_shader(ctx, src, s8(" (Fast)"), *arena);

				src->kind   = BeamformerShaderKind_DAS;
				src->shader = cs->programs + src->kind;
			}break;
			case BeamformerShaderKind_Decode:{
				read_only local_persist struct { BeamformerShaderKind kind; s8 suffix; } derivatives[] = {
				#define X(k, __1, __2, suffix, ...) {BeamformerShaderKind_## k, s8_comp(suffix)},
				DECODE_SHADER_VARIATIONS
				#undef X
				};
				for EachElement(derivatives, it) {
					src->kind   = derivatives[it].kind;
					src->shader = cs->programs + src->kind;
					success &= reload_compute_shader(ctx, src, derivatives[it].suffix, *arena);
				}
				src->kind   = BeamformerShaderKind_Decode;
				src->shader = cs->programs + src->kind;
			}break;
			case BeamformerShaderKind_Filter:{
				read_only local_persist struct { BeamformerShaderKind kind; s8 suffix; } derivatives[] = {
					{BeamformerShaderKind_Demodulate, s8_comp(" (Demodulate)")},
					#define X(k, __1, __2, suffix, ...) {BeamformerShaderKind_## k, s8_comp(suffix)},
					FILTER_SHADER_VARIATIONS
					#undef X
				};
				for EachElement(derivatives, it) {
					src->kind   = derivatives[it].kind;
					src->shader = cs->programs + src->kind;
					success &= reload_compute_shader(ctx, src, derivatives[it].suffix, *arena);
				}
				src->kind   = BeamformerShaderKind_Filter;
				src->shader = cs->programs + src->kind;
			}break;
			default:{}break;
			}

			if (success && ctx->latest_frame && !sm->live_imaging_parameters.active) {
				fill_frame_compute_work(ctx, work, ctx->latest_frame->view_plane_tag, 0, 0);
				can_commit = 0;
			}
		}break;
		case BeamformerWorkKind_ExportBuffer:{
			/* TODO(rnp): better way of handling DispatchCompute barrier */
			post_sync_barrier(&ctx->shared_memory, BeamformerSharedMemoryLockKind_DispatchCompute, sm->locks);
			os_shared_memory_region_lock(&ctx->shared_memory, sm->locks, (i32)work->lock, (u32)-1);
			BeamformerExportContext *ec = &work->export_context;
			switch (ec->kind) {
			case BeamformerExportKind_BeamformedData:{
				BeamformerFrame *frame = ctx->latest_frame;
				if (frame) {
					assert(frame->ready_to_present);
					u32 texture  = frame->texture;
					iv3 dim      = frame->dim;
					u32 out_size = (u32)dim.x * (u32)dim.y * (u32)dim.z * 2 * sizeof(f32);
					if (out_size <= ec->size) {
						glGetTextureImage(texture, 0, GL_RG, GL_FLOAT, (i32)out_size,
						                  beamformer_shared_memory_scratch_arena(sm).beg);
					}
				}
			}break;
			case BeamformerExportKind_Stats:{
				ComputeTimingTable *table = ctx->compute_timing_table;
				/* NOTE(rnp): do a little spin to let this finish updating */
				while (table->write_index != atomic_load_u32(&table->read_index));
				ComputeShaderStats *stats = ctx->compute_shader_stats;
				if (sizeof(stats->table) <= ec->size)
					mem_copy(beamformer_shared_memory_scratch_arena(sm).beg, &stats->table, sizeof(stats->table));
			}break;
			InvalidDefaultCase;
			}
			os_shared_memory_region_unlock(&ctx->shared_memory, sm->locks, (i32)work->lock);
			post_sync_barrier(&ctx->shared_memory, BeamformerSharedMemoryLockKind_ExportSync, sm->locks);
		}break;
		case BeamformerWorkKind_CreateFilter:{
			/* TODO(rnp): this should probably get deleted and moved to lazy loading */
			BeamformerCreateFilterContext *fctx = &work->create_filter_context;
			u32 block = fctx->parameter_block;
			u32 slot  = fctx->filter_slot;
			BeamformerComputePlan *cp = beamformer_compute_plan_for_block(cs, block, arena);
			beamformer_filter_update(cp->filters + slot, fctx->kind, fctx->parameters, block, slot, *arena);
		}break;
		case BeamformerWorkKind_ComputeIndirect:{
			fill_frame_compute_work(ctx, work, work->compute_indirect_context.view_plane,
			                        work->compute_indirect_context.parameter_block, 1);
		} /* FALLTHROUGH */
		case BeamformerWorkKind_Compute:{
			DEBUG_DECL(glClearNamedBufferData(cs->ping_pong_ssbos[0], GL_RG32F, GL_RG, GL_FLOAT, 0);)
			DEBUG_DECL(glClearNamedBufferData(cs->ping_pong_ssbos[1], GL_RG32F, GL_RG, GL_FLOAT, 0);)
			DEBUG_DECL(glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);)

			push_compute_timing_info(ctx->compute_timing_table,
			                         (ComputeTimingInfo){.kind = ComputeTimingInfoKind_ComputeFrameBegin});

			BeamformerComputePlan *cp = beamformer_compute_plan_for_block(cs, work->compute_context.parameter_block, arena);
			if (beamformer_parameter_block_dirty(sm, work->compute_context.parameter_block)) {
				u32 block = work->compute_context.parameter_block;
				beamformer_commit_parameter_block(ctx, cp, block, *arena);
				atomic_store_u32(&ctx->ui_dirty_parameter_blocks, (u32)(ctx->beamform_work_queue != q) << block);
			}

			post_sync_barrier(&ctx->shared_memory, work->lock, sm->locks);

			atomic_store_u32(&cs->processing_compute, 1);
			start_renderdoc_capture(gl_context);

			BeamformerFrame *frame = work->compute_context.frame;
			if (!iv3_equal(cp->output_points, frame->dim))
				alloc_beamform_frame(&ctx->gl, frame, cp->output_points, s8("Beamformed_Data"), *arena);

			frame->min_coordinate  = cp->min_coordinate;
			frame->max_coordinate  = cp->max_coordinate;
			frame->das_shader_kind = cp->das_ubo_data.shader_kind;
			frame->compound_count  = cp->das_ubo_data.acquisition_count;

			BeamformerComputeContext  *cc       = &ctx->compute_context;
			BeamformerComputePipeline *pipeline = &cp->pipeline;
			/* NOTE(rnp): first stage requires access to raw data buffer directly so we break
			 * it out into a separate step. This way data can get released as soon as possible */
			if (pipeline->shader_count > 0) {
				BeamformerRFBuffer *rf = &cs->rf_buffer;
				u32 slot = rf->compute_index % countof(rf->compute_syncs);

				/* NOTE(rnp): compute indirect is used when uploading data. in this case the thread
				 * must wait on an upload fence. if the fence doesn't yet exist the thread must wait */
				if (work->kind == BeamformerWorkKind_ComputeIndirect)
					spin_wait(!atomic_load_u64(rf->upload_syncs + slot));

				if (rf->upload_syncs[slot]) {
					rf->compute_index++;
					glWaitSync(rf->upload_syncs[slot], 0, GL_TIMEOUT_IGNORED);
					glDeleteSync(rf->upload_syncs[slot]);
				} else {
					slot = (rf->compute_index - 1) % countof(rf->compute_syncs);
				}

				glBindBufferRange(GL_SHADER_STORAGE_BUFFER, 1, rf->ssbo, slot * rf->size, rf->size);

				glBeginQuery(GL_TIME_ELAPSED, cc->shader_timer_ids[0]);
				do_compute_shader(ctx, cp, frame, pipeline->shaders[0], pipeline->parameters + 0, *arena);
				glEndQuery(GL_TIME_ELAPSED);

				if (work->kind == BeamformerWorkKind_ComputeIndirect) {
					rf->compute_syncs[slot] = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
					rf->upload_syncs[slot]  = 0;
					memory_write_barrier();
				}
			}

			b32 did_sum_shader = 0;
			for (u32 i = 1; i < pipeline->shader_count; i++) {
				did_sum_shader |= pipeline->shaders[i] == BeamformerShaderKind_Sum;
				glBeginQuery(GL_TIME_ELAPSED, cc->shader_timer_ids[i]);
				do_compute_shader(ctx, cp, frame, pipeline->shaders[i], pipeline->parameters + i, *arena);
				glEndQuery(GL_TIME_ELAPSED);
			}

			/* NOTE(rnp): the first of these blocks until work completes */
			for (u32 i = 0; i < pipeline->shader_count; i++) {
				ComputeTimingInfo info = {0};
				info.kind   = ComputeTimingInfoKind_Shader;
				info.shader = pipeline->shaders[i];
				glGetQueryObjectui64v(cc->shader_timer_ids[i], GL_QUERY_RESULT, &info.timer_count);
				push_compute_timing_info(ctx->compute_timing_table, info);
			}
			cs->processing_progress = 1;

			frame->ready_to_present = 1;
			if (did_sum_shader) {
				u32 aframe_index = (ctx->averaged_frame_index % countof(ctx->averaged_frames));
				ctx->averaged_frames[aframe_index].view_plane_tag  = frame->view_plane_tag;
				ctx->averaged_frames[aframe_index].ready_to_present = 1;
				atomic_add_u32(&ctx->averaged_frame_index, 1);
				atomic_store_u64((u64 *)&ctx->latest_frame, (u64)(ctx->averaged_frames + aframe_index));
			} else {
				atomic_store_u64((u64 *)&ctx->latest_frame, (u64)frame);
			}
			cs->processing_compute  = 0;

			push_compute_timing_info(ctx->compute_timing_table,
			                         (ComputeTimingInfo){.kind = ComputeTimingInfoKind_ComputeFrameEnd});

			end_renderdoc_capture(gl_context);
		}break;
		InvalidDefaultCase;
		}

		if (can_commit) {
			beamform_work_queue_pop_commit(q);
			work = beamform_work_queue_pop(q);
		}
	}
}

function void
coalesce_timing_table(ComputeTimingTable *t, ComputeShaderStats *stats)
{
	/* TODO(rnp): we do not currently do anything to handle the potential for a half written
	 * info item. this could result in garbage entries but they shouldn't really matter */

	u32 target = atomic_load_u32(&t->write_index);
	u32 stats_index = (stats->latest_frame_index + 1) % countof(stats->table.times);

	static_assert(BeamformerShaderKind_Count + 1 <= 32, "timing coalescence bitfield test");
	u32 seen_info_test = 0;

	while (t->read_index != target) {
		ComputeTimingInfo info = t->buffer[t->read_index % countof(t->buffer)];
		switch (info.kind) {
		case ComputeTimingInfoKind_ComputeFrameBegin:{
			assert(t->compute_frame_active == 0);
			t->compute_frame_active = 1;
			/* NOTE(rnp): allow multiple instances of same shader to accumulate */
			mem_clear(stats->table.times[stats_index], 0, sizeof(stats->table.times[stats_index]));
		}break;
		case ComputeTimingInfoKind_ComputeFrameEnd:{
			assert(t->compute_frame_active == 1);
			t->compute_frame_active = 0;
			stats->latest_frame_index = stats_index;
			stats_index = (stats_index + 1) % countof(stats->table.times);
		}break;
		case ComputeTimingInfoKind_Shader:{
			stats->table.times[stats_index][info.shader] += (f32)info.timer_count / 1.0e9f;
			seen_info_test |= (1u << info.shader);
		}break;
		case ComputeTimingInfoKind_RF_Data:{
			stats->latest_rf_index = (stats->latest_rf_index + 1) % countof(stats->table.rf_time_deltas);
			f32 delta = (f32)(info.timer_count - stats->last_rf_timer_count) / 1.0e9f;
			stats->table.rf_time_deltas[stats->latest_rf_index] = delta;
			stats->last_rf_timer_count = info.timer_count;
			seen_info_test |= (1 << BeamformerShaderKind_Count);
		}break;
		}
		/* NOTE(rnp): do this at the end so that stats table is always in a consistent state */
		atomic_add_u32(&t->read_index, 1);
	}

	if (seen_info_test) {
		for EachEnumValue(BeamformerShaderKind, shader) {
			if (seen_info_test & (1 << shader)) {
				f32 sum = 0;
				for EachElement(stats->table.times, i)
					sum += stats->table.times[i][shader];
				stats->average_times[shader] = sum / countof(stats->table.times);
			}
		}

		if (seen_info_test & (1 << BeamformerShaderKind_Count)) {
			f32 sum = 0;
			for EachElement(stats->table.rf_time_deltas, i)
				sum += stats->table.rf_time_deltas[i];
			stats->rf_time_delta_average = sum / countof(stats->table.rf_time_deltas);
		}
	}
}

DEBUG_EXPORT BEAMFORMER_COMPLETE_COMPUTE_FN(beamformer_complete_compute)
{
	BeamformerCtx *ctx         = (BeamformerCtx *)user_context;
	BeamformerSharedMemory *sm = ctx->shared_memory.region;
	complete_queue(ctx, &sm->external_work_queue, arena, gl_context);
	complete_queue(ctx, ctx->beamform_work_queue, arena, gl_context);
}

function void
beamformer_rf_buffer_allocate(BeamformerRFBuffer *rf, u32 rf_size, Arena arena)
{
	glUnmapNamedBuffer(rf->ssbo);
	glDeleteBuffers(1, &rf->ssbo);
	glCreateBuffers(1, &rf->ssbo);

	rf_size = (u32)round_up_to((iz)rf_size, 64);
	glNamedBufferStorage(rf->ssbo, countof(rf->compute_syncs) * rf_size, 0,
	                     GL_DYNAMIC_STORAGE_BIT|GL_MAP_WRITE_BIT);
	LABEL_GL_OBJECT(GL_BUFFER, rf->ssbo, s8("Raw_RF_SSBO"));
	rf->size = rf_size;
}

DEBUG_EXPORT BEAMFORMER_RF_UPLOAD_FN(beamformer_rf_upload)
{
	BeamformerSharedMemory *sm = ctx->shared_memory->region;

	BeamformerSharedMemoryLockKind scratch_lock = BeamformerSharedMemoryLockKind_ScratchSpace;
	BeamformerSharedMemoryLockKind upload_lock  = BeamformerSharedMemoryLockKind_UploadRF;
	if (sm->locks[upload_lock] &&
	    os_shared_memory_region_lock(ctx->shared_memory, sm->locks, (i32)scratch_lock, (u32)-1))
	{
		BeamformerRFBuffer *rf = ctx->rf_buffer;
		if (rf->size < sm->scratch_rf_size)
			beamformer_rf_buffer_allocate(rf, sm->scratch_rf_size, arena);

		u32 slot = rf->insertion_index++ % countof(rf->compute_syncs);

		/* NOTE(rnp): if the rest of the code is functioning then the first
		 * time the compute thread processes an upload it must have gone
		 * through this path. therefore it is safe to spin until it gets processed */
		spin_wait(atomic_load_u64(rf->upload_syncs + slot));

		if (rf->compute_syncs[slot]) {
			GLenum sync_result = glClientWaitSync(rf->compute_syncs[slot], 0, 1000000000);
			if (sync_result == GL_TIMEOUT_EXPIRED || sync_result == GL_WAIT_FAILED) {
				// TODO(rnp): what do?
			}
			glDeleteSync(rf->compute_syncs[slot]);
		}

		/* NOTE(rnp): nVidia's drivers really don't play nice with persistant mapping,
		 * at least when it is a big as this one wants to be. mapping and unmapping the
		 * desired range each time doesn't seem to introduce any performance hit */
		u32 access = GL_MAP_WRITE_BIT|GL_MAP_FLUSH_EXPLICIT_BIT|GL_MAP_UNSYNCHRONIZED_BIT;
		u8 *buffer = glMapNamedBufferRange(rf->ssbo, slot * rf->size, (i32)rf->size, access);

		mem_copy(buffer, beamformer_shared_memory_scratch_arena(sm).beg, rf->size);
		os_shared_memory_region_unlock(ctx->shared_memory, sm->locks, (i32)scratch_lock);
		post_sync_barrier(ctx->shared_memory, upload_lock, sm->locks);

		glFlushMappedNamedBufferRange(rf->ssbo, 0, (i32)rf->size);
		glUnmapNamedBuffer(rf->ssbo);

		rf->upload_syncs[slot]  = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
		rf->compute_syncs[slot] = 0;
		memory_write_barrier();

		os_wake_waiters(ctx->compute_worker_sync);

		ComputeTimingInfo info = {.kind = ComputeTimingInfoKind_RF_Data};
		glGetQueryObjectui64v(rf->data_timestamp_query, GL_QUERY_RESULT, &info.timer_count);
		glQueryCounter(rf->data_timestamp_query, GL_TIMESTAMP);
		push_compute_timing_info(ctx->compute_timing_table, info);
	}
}

#include "ui.c"

DEBUG_EXPORT BEAMFORMER_FRAME_STEP_FN(beamformer_frame_step)
{
	dt_for_frame = input->dt;

	if (IsWindowResized()) {
		ctx->window_size.h = GetScreenHeight();
		ctx->window_size.w = GetScreenWidth();
	}

	coalesce_timing_table(ctx->compute_timing_table, ctx->compute_shader_stats);

	if (input->executable_reloaded) {
		ui_init(ctx, ctx->ui_backing_store);
		DEBUG_DECL(start_frame_capture = ctx->os.start_frame_capture);
		DEBUG_DECL(end_frame_capture   = ctx->os.end_frame_capture);
	}

	BeamformerSharedMemory *sm = ctx->shared_memory.region;
	if (sm->locks[BeamformerSharedMemoryLockKind_UploadRF] != 0)
		os_wake_waiters(&ctx->os.upload_worker.sync_variable);

	BeamformerFrame        *frame = ctx->latest_frame;
	BeamformerViewPlaneTag  tag   = frame? frame->view_plane_tag : 0;
	draw_ui(ctx, input, frame, tag);

	ctx->frame_view_render_context.updated = 0;

	if (WindowShouldClose())
		ctx->should_exit = 1;
}

/* NOTE(rnp): functions defined in these shouldn't be visible to the whole program */
#if _DEBUG
  #if OS_LINUX
    #include "os_linux.c"
  #elif OS_WINDOWS
    #include "os_win32.c"
  #endif
#endif
