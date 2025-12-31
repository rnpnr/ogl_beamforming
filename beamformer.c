/* See LICENSE for license details. */
/* TODO(rnp):
 * [ ]: refactor: DecodeMode_None should use a different mapping and optional conversion shader
 *      for rf only mode with no filter and demod/filter should gain the OutputFloats flag for iq
 *      case and rf mode with filter; this can also be used instead of first pass uniform
 * [ ]: refactor: replace UploadRF with just the scratch_rf_size variable,
 *      use below to spin wait in library
 * [ ]: utilize umonitor/umwait (intel), monitorx/mwaitx (amd), and wfe/sev (aarch64)
 *      for power efficient low latency waiting
 * [ ]: refactor: split decode into reshape and decode
 *      - the check for first pass reshaping is the last non constant check
 *        in the shader
 *      - this will also remove the need for the channel mapping in the decode shader
 * [X]: refactor: ui: reload only shader which is affected by the interaction
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

#define DECODE_FIRST_PASS_UNIFORM_LOC 1

#define DAS_LOCAL_SIZE_X  16
#define DAS_LOCAL_SIZE_Y   1
#define DAS_LOCAL_SIZE_Z  16

#define DAS_VOXEL_OFFSET_UNIFORM_LOC  2
#define DAS_CYCLE_T_UNIFORM_LOC       3
#define DAS_FAST_CHANNEL_UNIFORM_LOC  4

#define MIN_MAX_MIPS_LEVEL_UNIFORM_LOC 1
#define SUM_PRESCALE_UNIFORM_LOC       1

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
			glDeleteBuffers(1, &cp->filters[i].ssbo);
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
		GLenum gl_kind[] = {BEAMFORMER_COMPUTE_TEXTURE_LIST_FULL};
		#undef X
		read_only local_persist s8 tex_prefix[] = {
			#define X(k, ...) s8_comp(#k "["),
			BEAMFORMER_COMPUTE_TEXTURE_LIST_FULL
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
beamformer_filter_update(BeamformerFilter *f, BeamformerFilterParameters fp, u32 block, u32 slot, Arena arena)
{
	Stream sb = arena_stream(arena);
	stream_append_s8s(&sb,
	                  beamformer_filter_kind_strings[fp.kind % countof(beamformer_filter_kind_strings)],
	                  s8("Filter["));
	stream_append_u64(&sb, block);
	stream_append_s8(&sb, s8("]["));
	stream_append_u64(&sb, slot);
	stream_append_byte(&sb, ']');
	s8 label = arena_stream_commit(&arena, &sb);

	void *filter = 0;
	switch (fp.kind) {
	case BeamformerFilterKind_Kaiser:{
		/* TODO(rnp): this should also support complex */
		/* TODO(rnp): implement this as an IFIR filter instead to reduce computation */
		filter = kaiser_low_pass_filter(&arena, fp.kaiser.cutoff_frequency, fp.sampling_frequency,
		                                fp.kaiser.beta, (i32)fp.kaiser.length);
		f->length     = (i32)fp.kaiser.length;
		f->time_delay = (f32)f->length / 2.0f / fp.sampling_frequency;
	}break;
	case BeamformerFilterKind_MatchedChirp:{
		typeof(fp.matched_chirp) *mc = &fp.matched_chirp;
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

	f->parameters = fp;

	glDeleteBuffers(1, &f->ssbo);
	glCreateBuffers(1, &f->ssbo);
	glNamedBufferStorage(f->ssbo, f->length * (i32)sizeof(f32) * (fp.complex? 2 : 1), filter, 0);
	glObjectLabel(GL_BUFFER, f->ssbo, (i32)label.len, (c8 *)label.data);
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

function b32
beamformer_frame_compatible(BeamformerFrame *f, iv3 dim, GLenum gl_kind)
{
	b32 result = gl_kind == f->gl_kind && iv3_equal(dim, f->dim);
	return result;
}

function iv3
make_valid_output_points(i32 points[3])
{
	iv3 result;
	result.E[0] = CLAMP(points[0], 1, gl_parameters.max_3d_texture_dim);
	result.E[1] = CLAMP(points[1], 1, gl_parameters.max_3d_texture_dim);
	result.E[2] = CLAMP(points[2], 1, gl_parameters.max_3d_texture_dim);
	return result;
}

function void
alloc_beamform_frame(BeamformerFrame *out, iv3 out_dim, GLenum gl_kind, s8 name, Arena arena)
{
	out->dim = make_valid_output_points(out_dim.E);

	/* NOTE: allocate storage for beamformed output data;
	 * this is shared between compute and fragment shaders */
	u32 max_dim = (u32)MAX(out->dim.x, MAX(out->dim.y, out->dim.z));
	out->mips   = (i32)ctz_u32(round_up_power_of_2(max_dim)) + 1;

	out->gl_kind = gl_kind;

	Stream label = arena_stream(arena);
	stream_append_s8(&label, name);
	stream_append_byte(&label, '[');
	stream_append_hex_u64(&label, out->id);
	stream_append_byte(&label, ']');

	glDeleteTextures(1, &out->texture);
	glCreateTextures(GL_TEXTURE_3D, 1, &out->texture);
	glTextureStorage3D(out->texture, out->mips, gl_kind, out->dim.x, out->dim.y, out->dim.z);

	glTextureParameteri(out->texture, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTextureParameteri(out->texture, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

	LABEL_GL_OBJECT(GL_TEXTURE, out->texture, stream_to_s8(&label));
}

function void
update_hadamard_texture(BeamformerComputePlan *cp, i32 order, Arena arena)
{
	f32 *hadamard = make_hadamard_transpose(&arena, order);
	if (hadamard) {
		cp->hadamard_order = order;
		u32 *texture = cp->textures + BeamformerComputeTextureKind_Hadamard;
		glDeleteTextures(1, texture);
		glCreateTextures(GL_TEXTURE_2D, 1, texture);
		glTextureStorage2D(*texture, 1, GL_R32F, order, order);
		glTextureSubImage2D(*texture, 0, 0, 0, order, order, GL_RED, GL_FLOAT, hadamard);

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
	cuda_init(pb->parameters.raw_data_dimensions.E, decoded_data_dimension);
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
	b32 result = work != 0;
	if (result) {
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
do_sum_shader(BeamformerComputeContext *cc, u32 *in_textures, u32 in_texture_count,
              u32 out_texture, iv3 out_data_dim)
{
	/* NOTE: zero output before summing */
	glClearTexImage(out_texture, 0, GL_RED, GL_FLOAT, 0);
	glMemoryBarrier(GL_TEXTURE_UPDATE_BARRIER_BIT);

	glBindImageTexture(0, out_texture, 0, GL_TRUE, 0, GL_READ_WRITE, GL_RG32F);
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
	v3 extent = v3_abs(v3_sub(bp->output_max_coordinate, bp->output_min_coordinate));
	v3 points = v3_from_iv3(make_valid_output_points(bp->output_points.E));

	m4 T1 = m4_translation(v3_scale(v3_sub(points, (v3){{1.0f, 1.0f, 1.0f}}), -0.5f));
	m4 T2 = m4_translation(v3_add(bp->output_min_coordinate, v3_scale(extent, 0.5f)));
	m4 S  = m4_scale(v3_div(extent, points));

	m4 R;
	switch (bp->das_shader_id) {
	case BeamformerAcquisitionKind_FORCES:
	case BeamformerAcquisitionKind_UFORCES:
	case BeamformerAcquisitionKind_Flash:
	{
		R = m4_identity();
		S.c[1].E[1]  = 0;
		T2.c[3].E[1] = 0;
	}break;
	case BeamformerAcquisitionKind_HERO_PA:
	case BeamformerAcquisitionKind_HERCULES:
	case BeamformerAcquisitionKind_UHERCULES:
	case BeamformerAcquisitionKind_RCA_TPW:
	case BeamformerAcquisitionKind_RCA_VLS:
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
plan_compute_pipeline(BeamformerComputePlan *cp, BeamformerParameterBlock *pb)
{
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

	cp->iq_pipeline = demodulate || run_cuda_hilbert;

	f32 sampling_frequency = pb->parameters.sampling_frequency;
	u32 decimation_rate = MAX(pb->parameters.decimation_rate, 1);
	u32 sample_count    = pb->parameters.sample_count;
	if (demodulate) {
		sample_count       /= (2 * decimation_rate);
		sampling_frequency /= 2 * (f32)decimation_rate;
	}

	cp->rf_size = sample_count * pb->parameters.channel_count * pb->parameters.acquisition_count;
	if (cp->iq_pipeline) cp->rf_size *= 8;
	else                 cp->rf_size *= 4;

	u32 das_sample_stride   = 1;
	u32 das_transmit_stride = sample_count;
	u32 das_channel_stride  = sample_count * pb->parameters.acquisition_count;

	f32 time_offset = pb->parameters.time_offset;

	BeamformerDataKind data_kind = pb->pipeline.data_kind;
	cp->pipeline.shader_count = 0;
	for (u32 i = 0; i < pb->pipeline.shader_count; i++) {
		BeamformerShaderParameters *sp = pb->pipeline.parameters + i;
		u32 slot   = cp->pipeline.shader_count;
		u32 shader = pb->pipeline.shaders[i];
		b32 commit = 0;

		BeamformerShaderDescriptor *ld = cp->shader_descriptors + slot - 1;
		BeamformerShaderDescriptor *sd = cp->shader_descriptors + slot;
		zero_struct(sd);

		switch (shader) {
		case BeamformerShaderKind_CudaHilbert:{ commit = run_cuda_hilbert; }break;
		case BeamformerShaderKind_Decode:{
			/* TODO(rnp): rework decode first and demodulate after */
			b32 first = slot == 0;

			sd->bake.data_kind = data_kind;
			if (!first) {
				if (data_kind == BeamformerDataKind_Int16) {
					sd->bake.data_kind = BeamformerDataKind_Int16Complex;
				} else {
					sd->bake.data_kind = BeamformerDataKind_Float32Complex;
				}
			}

			BeamformerShaderKind *last_shader = cp->pipeline.shaders + slot - 1;
			assert(first || ((*last_shader == BeamformerShaderKind_Demodulate ||
			                  *last_shader == BeamformerShaderKind_Filter)));

			BeamformerShaderDecodeBakeParameters *db = &sd->bake.Decode;
			db->decode_mode    = pb->parameters.decode_mode;
			db->transmit_count = pb->parameters.acquisition_count;

			u32 channel_stride         = pb->parameters.acquisition_count * pb->parameters.sample_count;
			db->input_sample_stride    = first? 1                           : ld->bake.Filter.output_sample_stride;
			db->input_channel_stride   = first? channel_stride              : ld->bake.Filter.output_channel_stride;
			db->input_transmit_stride  = first? pb->parameters.sample_count : 1;

			db->output_sample_stride   = das_sample_stride;
			db->output_channel_stride  = das_channel_stride;
			db->output_transmit_stride = das_transmit_stride;
			if (first) {
				db->output_channel_stride  *= decimation_rate;
				db->output_transmit_stride *= decimation_rate;
			}

			if (run_cuda_hilbert) sd->bake.flags |= BeamformerShaderDecodeFlags_DilateOutput;

			if (db->decode_mode == BeamformerDecodeMode_None) {
				sd->layout = (uv3){{64, 1, 1}};

				sd->dispatch.x = (u32)ceil_f32((f32)sample_count                     / (f32)sd->layout.x);
				sd->dispatch.y = (u32)ceil_f32((f32)pb->parameters.channel_count     / (f32)sd->layout.y);
				sd->dispatch.z = (u32)ceil_f32((f32)pb->parameters.acquisition_count / (f32)sd->layout.z);
			} else if (db->transmit_count > 40) {
				sd->bake.flags |= BeamformerShaderDecodeFlags_UseSharedMemory;
				db->to_process = 2;

				if (db->transmit_count == 48)
					db->to_process = db->transmit_count / 16;

				b32 use_16z  = db->transmit_count == 48 || db->transmit_count == 80 ||
				               db->transmit_count == 96 || db->transmit_count == 160;
				sd->layout = (uv3){{4, 1, use_16z? 16 : 32}};

				sd->dispatch.x = (u32)ceil_f32((f32)sample_count                     / (f32)sd->layout.x);
				sd->dispatch.y = (u32)ceil_f32((f32)pb->parameters.channel_count     / (f32)sd->layout.y);
				sd->dispatch.z = (u32)ceil_f32((f32)pb->parameters.acquisition_count / (f32)sd->layout.z / (f32)db->to_process);
			} else {
				db->to_process = 1;

				/* NOTE(rnp): register caching. using more threads will cause the compiler to do
				 * contortions to avoid spilling registers. using less gives higher performance */
				/* TODO(rnp): may need to be adjusted to 16 on NVIDIA */
				sd->layout = (uv3){{32, 1, 1}};

				sd->dispatch.x = (u32)ceil_f32((f32)sample_count                 / (f32)sd->layout.x);
				sd->dispatch.y = (u32)ceil_f32((f32)pb->parameters.channel_count / (f32)sd->layout.y);
				sd->dispatch.z = 1;
			}

			if (first) sd->dispatch.x *= decimation_rate;

			/* NOTE(rnp): decode 2 samples per dispatch when data is i16 */
			if (first && data_kind == BeamformerDataKind_Int16)
				sd->dispatch.x = (u32)ceil_f32((f32)sd->dispatch.x / 2);

			commit = first || db->decode_mode != BeamformerDecodeMode_None;
		}break;
		case BeamformerShaderKind_Demodulate:
		case BeamformerShaderKind_Filter:
		{
			b32 first = slot == 0;
			b32 demod = shader == BeamformerShaderKind_Demodulate;
			BeamformerFilter *f = cp->filters + sp->filter_slot;

			time_offset += f->time_delay;

			BeamformerShaderFilterBakeParameters *fb = &sd->bake.Filter;
			fb->filter_length = (u32)f->length;
			if (demod)                 sd->bake.flags |= BeamformerShaderFilterFlags_Demodulate;
			if (f->parameters.complex) sd->bake.flags |= BeamformerShaderFilterFlags_ComplexFilter;

			sd->bake.data_kind = data_kind;
			if (!first) sd->bake.data_kind = BeamformerDataKind_Float32;

			/* NOTE(rnp): when we are demodulating we pretend that the sampler was alternating
			 * between sampling the I portion and the Q portion of an IQ signal. Therefore there
			 * is an implicit decimation factor of 2 which must always be included. All code here
			 * assumes that the signal was sampled in such a way that supports this operation.
			 * To recover IQ[n] from the sampled data (RF[n]) we do the following:
			 *   I[n]  = RF[n]
			 *   Q[n]  = RF[n + 1]
			 *   IQ[n] = I[n] - j*Q[n]
			 */
			if (demod) {
				fb->demodulation_frequency = pb->parameters.demodulation_frequency;
				fb->sampling_frequency     = pb->parameters.sampling_frequency / 2;
				fb->decimation_rate        = decimation_rate;
				fb->sample_count           = pb->parameters.sample_count;

				fb->output_channel_stride  = das_channel_stride;
				fb->output_sample_stride   = das_sample_stride;
				fb->output_transmit_stride = das_transmit_stride;

				if (first) {
					fb->input_channel_stride  = pb->parameters.sample_count * pb->parameters.acquisition_count / 2;
					fb->input_sample_stride   = 1;
					fb->input_transmit_stride = pb->parameters.sample_count / 2;

					if (pb->parameters.decode_mode == BeamformerDecodeMode_None) {
						sd->bake.flags |= BeamformerShaderFilterFlags_OutputFloats;
					} else {
						/* NOTE(rnp): output optimized layout for decoding */
						fb->output_channel_stride  = das_channel_stride;
						fb->output_sample_stride   = pb->parameters.acquisition_count;
						fb->output_transmit_stride = 1;
					}
				} else {
					assert(cp->pipeline.shaders[slot - 1] == BeamformerShaderKind_Decode);
					fb->input_channel_stride  = ld->bake.Decode.output_channel_stride;
					fb->input_sample_stride   = ld->bake.Decode.output_sample_stride;
					fb->input_transmit_stride = ld->bake.Decode.output_transmit_stride;
				}
			} else {
				fb->decimation_rate        = 1;
				fb->output_channel_stride  = sample_count * pb->parameters.acquisition_count;
				fb->output_sample_stride   = 1;
				fb->output_transmit_stride = sample_count;
				fb->input_channel_stride   = sample_count * pb->parameters.acquisition_count;
				fb->input_sample_stride    = 1;
				fb->input_transmit_stride  = sample_count;
				fb->sample_count           = sample_count;
			}

			/* TODO(rnp): filter may need a different dispatch layout */
			sd->layout     = (uv3){{128, 1, 1}};
			sd->dispatch.x = (u32)ceil_f32((f32)sample_count                     / (f32)sd->layout.x);
			sd->dispatch.y = (u32)ceil_f32((f32)pb->parameters.channel_count     / (f32)sd->layout.y);
			sd->dispatch.z = (u32)ceil_f32((f32)pb->parameters.acquisition_count / (f32)sd->layout.z);

			commit = 1;
		}break;
		case BeamformerShaderKind_DAS:{
			sd->bake.data_kind = BeamformerDataKind_Float32;
			if (cp->iq_pipeline)
				sd->bake.data_kind = BeamformerDataKind_Float32Complex;

			BeamformerShaderDASBakeParameters *db = &sd->bake.DAS;
			BeamformerDASUBO *du = &cp->das_ubo_data;
			du->voxel_transform        = das_voxel_transform_matrix(&pb->parameters);
			du->xdc_transform          = pb->parameters.xdc_transform;
			du->xdc_element_pitch      = pb->parameters.xdc_element_pitch;
			db->sampling_frequency     = sampling_frequency;
			db->demodulation_frequency = pb->parameters.demodulation_frequency;
			db->speed_of_sound         = pb->parameters.speed_of_sound;
			db->time_offset            = time_offset;
			db->f_number               = pb->parameters.f_number;
			db->acquisition_kind       = pb->parameters.das_shader_id;
			db->sample_count           = sample_count;
			db->channel_count          = pb->parameters.channel_count;
			db->acquisition_count      = pb->parameters.acquisition_count;
			db->interpolation_mode     = pb->parameters.interpolation_mode;
			db->transmit_angle         = pb->parameters.focal_vector.E[0];
			db->focus_depth            = pb->parameters.focal_vector.E[1];
			db->transmit_receive_orientation = pb->parameters.transmit_receive_orientation;

			if (pb->parameters.single_focus)        sd->bake.flags |= BeamformerShaderDASFlags_SingleFocus;
			if (pb->parameters.single_orientation)  sd->bake.flags |= BeamformerShaderDASFlags_SingleOrientation;
			if (pb->parameters.coherency_weighting) sd->bake.flags |= BeamformerShaderDASFlags_CoherencyWeighting;
			else                                    sd->bake.flags |= BeamformerShaderDASFlags_Fast;

			u32 id = pb->parameters.das_shader_id;
			if (id == BeamformerAcquisitionKind_UFORCES || id == BeamformerAcquisitionKind_UHERCULES)
				sd->bake.flags |= BeamformerShaderDASFlags_Sparse;

			sd->layout.x = DAS_LOCAL_SIZE_X;
			sd->layout.y = DAS_LOCAL_SIZE_Y;
			sd->layout.z = DAS_LOCAL_SIZE_Z;

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
}

function void
stream_push_shader_header(Stream *s, BeamformerShaderKind shader_kind, s8 header)
{
	stream_append_s8s(s, s8("#version 460 core\n\n"), header);

	switch (shader_kind) {
	case BeamformerShaderKind_DAS:{
		stream_append_s8(s, s8(""
		"layout(location = " str(DAS_VOXEL_OFFSET_UNIFORM_LOC) ") uniform ivec3 u_voxel_offset;\n"
		"layout(location = " str(DAS_CYCLE_T_UNIFORM_LOC)      ") uniform uint  u_cycle_t;\n"
		"layout(location = " str(DAS_FAST_CHANNEL_UNIFORM_LOC) ") uniform int   u_channel;\n\n"
		));
	}break;
	case BeamformerShaderKind_Decode:{
		stream_append_s8s(s, s8(""
		"layout(location = " str(DECODE_FIRST_PASS_UNIFORM_LOC) ") uniform bool u_first_pass;\n\n"
		));
	}break;
	case BeamformerShaderKind_MinMax:{
		stream_append_s8(s, s8("layout(location = " str(MIN_MAX_MIPS_LEVEL_UNIFORM_LOC)
		                       ") uniform int u_mip_map;\n\n"));
	}break;
	case BeamformerShaderKind_Sum:{
		stream_append_s8(s, s8("layout(location = " str(SUM_PRESCALE_UNIFORM_LOC)
		                       ") uniform float u_sum_prescale = 1.0;\n\n"));
	}break;
	default:{}break;
	}
}

function void
load_compute_shader(BeamformerCtx *ctx, BeamformerComputePlan *cp, u32 shader_slot, Arena arena)
{
	read_only local_persist s8 compute_headers[BeamformerShaderKind_ComputeCount] = {
		/* X(name, type, gltype) */
		#define X(name, t, gltype) "\t" #gltype " " #name ";\n"
		[BeamformerShaderKind_DAS] = s8_comp("layout(std140, binding = 0) uniform parameters {\n"
			BEAMFORMER_DAS_UBO_PARAM_LIST
			"};\n\n"
		),
		#undef X
	};

	BeamformerShaderKind shader = cp->pipeline.shaders[shader_slot];

	u32 program          = 0;
	i32 reloadable_index = beamformer_shader_reloadable_index_by_shader[shader];
	if (reloadable_index != -1) {
		BeamformerShaderKind base_shader = beamformer_reloadable_shader_kinds[reloadable_index];
		s8 path;
		if (!BakeShaders)
			path = push_s8_from_parts(&arena, os_path_separator(), s8("shaders"),
		                            beamformer_reloadable_shader_files[reloadable_index]);

		Stream shader_stream = arena_stream(arena);
		stream_push_shader_header(&shader_stream, base_shader, compute_headers[base_shader]);

		i32  header_vector_length = beamformer_shader_header_vector_lengths[reloadable_index];
		i32 *header_vector        = beamformer_shader_header_vectors[reloadable_index];
		for (i32 index = 0; index < header_vector_length; index++)
			stream_append_s8(&shader_stream, beamformer_shader_global_header_strings[header_vector[index]]);

		BeamformerShaderDescriptor *sd = cp->shader_descriptors + shader_slot;

		if (sd->layout.x != 0) {
			stream_append_s8(&shader_stream,  s8("layout(local_size_x = "));
			stream_append_u64(&shader_stream, sd->layout.x);
			stream_append_s8(&shader_stream,  s8(", local_size_y = "));
			stream_append_u64(&shader_stream, sd->layout.y);
			stream_append_s8(&shader_stream,  s8(", local_size_z = "));
			stream_append_u64(&shader_stream, sd->layout.z);
			stream_append_s8(&shader_stream,  s8(") in;\n\n"));
		}

		u32 *parameters = (u32 *)&sd->bake;
		s8  *names      = beamformer_shader_bake_parameter_names[reloadable_index];
		u32  float_bits = beamformer_shader_bake_parameter_float_bits[reloadable_index];
		i32  count      = beamformer_shader_bake_parameter_counts[reloadable_index];

		for (i32 index = 0; index < count; index++) {
			stream_append_s8s(&shader_stream, s8("#define "), names[index],
			                  (float_bits & (1 << index))? s8(" uintBitsToFloat") : s8(" "), s8("(0x"));
			stream_append_hex_u64(&shader_stream, parameters[index]);
			stream_append_s8(&shader_stream, s8(")\n"));
		}

		stream_append_s8(&shader_stream, s8("#define DataKind (0x"));
		stream_append_hex_u64(&shader_stream, sd->bake.data_kind);
		stream_append_s8(&shader_stream, s8(")\n\n"));

		s8  *flag_names = beamformer_shader_flag_strings[reloadable_index];
		u32  flag_count = beamformer_shader_flag_strings_count[reloadable_index];
		u32  flags      = sd->bake.flags;
		for (u32 bit = 0; bit < flag_count; bit++) {
			stream_append_s8s(&shader_stream, s8("#define "), flag_names[bit],
			                  (flags & (1 << bit))? s8(" 1") : s8(" 0"), s8("\n"));
		}

		stream_append_s8(&shader_stream, s8("\n#line 1\n"));

		s8 shader_text;
		if (BakeShaders) {
			stream_append_s8(&shader_stream, beamformer_shader_data[reloadable_index]);
			shader_text = arena_stream_commit(&arena, &shader_stream);
		} else {
			shader_text  = arena_stream_commit(&arena, &shader_stream);
			s8 file_text = os_read_whole_file(&arena, (c8 *)path.data);

			assert(shader_text.data + shader_text.len == file_text.data);
			shader_text.len += file_text.len;
		}

		/* TODO(rnp): instance name */
		s8 shader_name = beamformer_shader_names[shader];
		program = load_shader(arena, &shader_text, (u32 []){GL_COMPUTE_SHADER}, 1, shader_name);
	}

	glDeleteProgram(cp->programs[shader_slot]);
	cp->programs[shader_slot] = program;
}

function void
beamformer_commit_parameter_block(BeamformerCtx *ctx, BeamformerComputePlan *cp, u32 block, Arena arena)
{
	BeamformerParameterBlock *pb = beamformer_parameter_block_lock(&ctx->shared_memory, block, -1);
	for EachBit(pb->dirty_regions, region) {
		switch (region) {
		case BeamformerParameterBlockRegion_ComputePipeline:
		case BeamformerParameterBlockRegion_Parameters:
		{
			plan_compute_pipeline(cp, pb);

			/* NOTE(rnp): these are both handled by plan_compute_pipeline() */
			u32 mask = 1 << BeamformerParameterBlockRegion_ComputePipeline |
			           1 << BeamformerParameterBlockRegion_Parameters;
			pb->dirty_regions &= ~mask;

			for (u32 shader_slot = 0; shader_slot < cp->pipeline.shader_count; shader_slot++) {
				u128 hash = u128_hash_from_data(cp->shader_descriptors + shader_slot, sizeof(BeamformerShaderDescriptor));
				if (!u128_equal(hash, cp->shader_hashes[shader_slot]))
					cp->dirty_programs |= 1 << shader_slot;
				cp->shader_hashes[shader_slot] = hash;
			}

			#define X(k, t, v) glNamedBufferSubData(cp->ubos[BeamformerComputeUBOKind_##k], \
			                                        0, sizeof(t), &cp->v ## _ubo_data);
			BEAMFORMER_COMPUTE_UBO_LIST
			#undef X

			cp->acquisition_count = pb->parameters.acquisition_count;
			cp->acquisition_kind  = pb->parameters.das_shader_id;

			u32 decoded_data_size = cp->rf_size;
			if (ctx->compute_context.ping_pong_ssbo_size < decoded_data_size)
				alloc_shader_storage(ctx, decoded_data_size, arena);

			if (cp->hadamard_order != (i32)cp->acquisition_count)
				update_hadamard_texture(cp, (i32)cp->acquisition_count, arena);

			cp->min_coordinate = pb->parameters.output_min_coordinate;
			cp->max_coordinate = pb->parameters.output_max_coordinate;

			cp->output_points  = make_valid_output_points(pb->parameters.output_points.E);
			cp->average_frames = pb->parameters.output_points.E[3];

			GLenum gl_kind = cp->iq_pipeline ? GL_RG32F : GL_R32F;
			if (cp->average_frames > 1 && !beamformer_frame_compatible(ctx->averaged_frames + 0, cp->output_points, gl_kind)) {
				alloc_beamform_frame(ctx->averaged_frames + 0, cp->output_points, gl_kind, s8("Averaged Frame"), arena);
				alloc_beamform_frame(ctx->averaged_frames + 1, cp->output_points, gl_kind, s8("Averaged Frame"), arena);
			}
		}break;
		case BeamformerParameterBlockRegion_ChannelMapping:{
			cuda_set_channel_mapping(pb->channel_mapping);
		}break;
		case BeamformerParameterBlockRegion_FocalVectors:
		case BeamformerParameterBlockRegion_SparseElements:
		case BeamformerParameterBlockRegion_TransmitReceiveOrientations:
		{
			BeamformerComputeTextureKind texture_kind = 0;
			u32 pixel_type = 0, texture_format = 0;
			switch (region) {
			#define X(kind, _gl, tf, pt, ...) \
			case BeamformerParameterBlockRegion_## kind:{            \
				texture_kind   = BeamformerComputeTextureKind_## kind; \
				texture_format = tf;                                   \
				pixel_type     = pt;                                   \
			}break;
			BEAMFORMER_COMPUTE_TEXTURE_LIST
			#undef X
			InvalidDefaultCase;
			}
			glTextureSubImage1D(cp->textures[texture_kind], 0, 0, BeamformerMaxChannelCount,
			                    texture_format, pixel_type,
			                    (u8 *)pb + BeamformerParameterBlockRegionOffsets[region]);
		}break;
		}
	}
	beamformer_parameter_block_unlock(&ctx->shared_memory, block);
}

function void
do_compute_shader(BeamformerCtx *ctx, BeamformerComputePlan *cp, BeamformerFrame *frame,
                  BeamformerShaderKind shader, u32 shader_slot, BeamformerShaderParameters *sp, Arena arena)
{
	BeamformerComputeContext *cc = &ctx->compute_context;

	u32 program = cp->programs[shader_slot];
	glUseProgram(program);

	u32 output_ssbo_idx = !cc->last_output_ssbo_index;
	u32 input_ssbo_idx  = cc->last_output_ssbo_index;

	uv3 dispatch = cp->shader_descriptors[shader_slot].dispatch;
	switch (shader) {
	case BeamformerShaderKind_Decode:{
		glBindImageTexture(0, cp->textures[BeamformerComputeTextureKind_Hadamard], 0, 0, 0, GL_READ_ONLY, GL_R32F);

		BeamformerDecodeMode mode = cp->shader_descriptors[shader_slot].bake.Decode.decode_mode;
		if (shader_slot == 0) {
			if (mode != BeamformerDecodeMode_None) {
				glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, cc->ping_pong_ssbos[input_ssbo_idx]);
				glProgramUniform1ui(program, DECODE_FIRST_PASS_UNIFORM_LOC, 1);

				glDispatchCompute(dispatch.x, dispatch.y, dispatch.z);
				glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);
			}
		}

		if (mode != BeamformerDecodeMode_None)
			glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, cc->ping_pong_ssbos[input_ssbo_idx]);

		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 3, cc->ping_pong_ssbos[output_ssbo_idx]);

		glProgramUniform1ui(program, DECODE_FIRST_PASS_UNIFORM_LOC, 0);

		glDispatchCompute(dispatch.x, dispatch.y, dispatch.z);
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
	case BeamformerShaderKind_Demodulate:
	{
		if (shader_slot != 0)
			glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, cc->ping_pong_ssbos[input_ssbo_idx]);
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, cc->ping_pong_ssbos[output_ssbo_idx]);
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 3, cp->filters[sp->filter_slot].ssbo);

		glDispatchCompute(dispatch.x, dispatch.y, dispatch.z);
		glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);

		cc->last_output_ssbo_index = !cc->last_output_ssbo_index;
	}break;
	case BeamformerShaderKind_MinMax:{
		for (i32 i = 1; i < frame->mips; i++) {
			glBindImageTexture(0, frame->texture, i - 1, GL_TRUE, 0, GL_READ_ONLY,  GL_RG32F);
			glBindImageTexture(1, frame->texture, i - 0, GL_TRUE, 0, GL_WRITE_ONLY, GL_RG32F);
			glProgramUniform1i(program, MIN_MAX_MIPS_LEVEL_UNIFORM_LOC, i);

			u32 width  = (u32)frame->dim.x >> i;
			u32 height = (u32)frame->dim.y >> i;
			u32 depth  = (u32)frame->dim.z >> i;
			glDispatchCompute(ORONE(width / 32), ORONE(height), ORONE(depth / 32));
			glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
		}
	}break;
	case BeamformerShaderKind_DAS:{
		local_persist u32 das_cycle_t = 0;

		BeamformerShaderBakeParameters *bp = &cp->shader_descriptors[shader_slot].bake;
		b32 fast   = (bp->flags & BeamformerShaderDASFlags_Fast)   != 0;
		b32 sparse = (bp->flags & BeamformerShaderDASFlags_Sparse) != 0;

		if (fast) {
			glClearTexImage(frame->texture, 0, GL_RED, GL_FLOAT, 0);
			glMemoryBarrier(GL_TEXTURE_UPDATE_BARRIER_BIT);
			glBindImageTexture(0, frame->texture, 0, GL_TRUE, 0, GL_READ_WRITE, cp->iq_pipeline ? GL_RG32F : GL_R32F);
		} else {
			glBindImageTexture(0, frame->texture, 0, GL_TRUE, 0, GL_WRITE_ONLY, cp->iq_pipeline ? GL_RG32F : GL_R32F);
		}

		u32 sparse_texture = cp->textures[BeamformerComputeTextureKind_SparseElements];
		if (!sparse) sparse_texture = 0;

		glBindBufferBase(GL_UNIFORM_BUFFER, 0, cp->ubos[BeamformerComputeUBOKind_DAS]);
		glBindBufferRange(GL_SHADER_STORAGE_BUFFER, 1, cc->ping_pong_ssbos[input_ssbo_idx], 0, cp->rf_size);
		glBindImageTexture(1, sparse_texture, 0, 0, 0, GL_READ_ONLY, GL_R16I);
		glBindImageTexture(2, cp->textures[BeamformerComputeTextureKind_FocalVectors], 0, 0, 0, GL_READ_ONLY, GL_RG32F);
		glBindImageTexture(3, cp->textures[BeamformerComputeTextureKind_TransmitReceiveOrientations], 0, 0, 0, GL_READ_ONLY, GL_R8I);

		glProgramUniform1ui(program, DAS_CYCLE_T_UNIFORM_LOC, das_cycle_t++);

		if (fast) {
			i32 loop_end;
			if (bp->DAS.acquisition_kind == BeamformerAcquisitionKind_RCA_VLS ||
			    bp->DAS.acquisition_kind == BeamformerAcquisitionKind_RCA_TPW)
			{
				/* NOTE(rnp): to avoid repeatedly sampling the whole focal vectors
				 * texture we loop over transmits for VLS/TPW */
				loop_end = (i32)bp->DAS.acquisition_count;
			} else {
				loop_end = (i32)bp->DAS.channel_count;
			}
			f32 percent_per_step = 1.0f / (f32)loop_end;
			cc->processing_progress = -percent_per_step;
			for (i32 index = 0; index < loop_end; index++) {
				cc->processing_progress += percent_per_step;
				/* IMPORTANT(rnp): prevents OS from coalescing and killing our shader */
				glFinish();
				glProgramUniform1i(program, DAS_FAST_CHANNEL_UNIFORM_LOC, index);
				glDispatchCompute((u32)ceil_f32((f32)frame->dim.x / DAS_LOCAL_SIZE_X),
				                  (u32)ceil_f32((f32)frame->dim.y / DAS_LOCAL_SIZE_Y),
				                  (u32)ceil_f32((f32)frame->dim.z / DAS_LOCAL_SIZE_Z));
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

		glProgramUniform1f(program, SUM_PRESCALE_UNIFORM_LOC, 1 / (f32)frame_count);
		do_sum_shader(cc, in_textures, frame_count, aframe->texture, aframe->dim);
		aframe->min_coordinate   = frame->min_coordinate;
		aframe->max_coordinate   = frame->max_coordinate;
		aframe->compound_count   = frame->compound_count;
		aframe->acquisition_kind = frame->acquisition_kind;
	}break;
	InvalidDefaultCase;
	}
}

function s8
shader_text_with_header(s8 header, s8 filepath, b32 has_file, BeamformerShaderKind shader_kind, Arena *arena)
{
	Stream sb = arena_stream(*arena);
	stream_push_shader_header(&sb, shader_kind, header);
	stream_append_s8(&sb, s8("\n#line 1\n"));

	s8 result;
	if (BakeShaders) {
		/* TODO(rnp): better handling of shaders with no backing file */
		if (has_file) {
			i32 reloadable_index = beamformer_shader_reloadable_index_by_shader[shader_kind];
			stream_append_s8(&sb, beamformer_shader_data[reloadable_index]);
		}
		result = arena_stream_commit(arena, &sb);
	} else {
		result = arena_stream_commit(arena, &sb);
		if (has_file) {
			s8 file = os_read_whole_file(arena, (c8 *)filepath.data);
			assert(file.data == result.data + result.len);
			result.len += file.len;
		}
	}

	return result;
}

/* NOTE(rnp): currently this function is only handling rendering shaders.
 * look at load_compute_shader for compute shaders */
DEBUG_EXPORT BEAMFORMER_RELOAD_SHADER_FN(beamformer_reload_shader)
{
	BeamformerCtx        *ctx  = src->beamformer_context;
	BeamformerShaderKind  kind = beamformer_reloadable_shader_kinds[src->reloadable_info_index];
	assert(kind == BeamformerShaderKind_Render3D);

	i32 shader_count = 1;
	ShaderReloadContext *link = src->link;
	while (link != src) { shader_count++; link = link->link; }

	s8  *shader_texts = push_array(&arena, s8,  shader_count);
	u32 *shader_types = push_array(&arena, u32, shader_count);

	i32 index = 0;
	do {
		b32 has_file = link->reloadable_info_index >= 0;
		shader_texts[index] = shader_text_with_header(link->header, path, has_file, kind, &arena);
		shader_types[index] = link->gl_type;
		index++;
		link = link->link;
	} while (link != src);

	u32 *shader = &ctx->frame_view_render_context.shader;
	glDeleteProgram(*shader);
	*shader = load_shader(arena, shader_texts, shader_types, shader_count, shader_name);
	ctx->frame_view_render_context.updated = 1;

	return 1;
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
			u32 reserved_blocks = sm->reserved_parameter_blocks;
			for (u32 block = 0; block < reserved_blocks; block++) {
				BeamformerComputePlan *cp = beamformer_compute_plan_for_block(cs, block, arena);
				for (u32 slot = 0; slot < cp->pipeline.shader_count; slot++) {
					i32 shader_index = beamformer_shader_reloadable_index_by_shader[cp->pipeline.shaders[slot]];
					if (beamformer_reloadable_shader_kinds[shader_index] == work->reload_shader)
						cp->dirty_programs |= 1 << slot;
				}
			}

			if (ctx->latest_frame && !sm->live_imaging_parameters.active) {
				fill_frame_compute_work(ctx, work, ctx->latest_frame->view_plane_tag,
				                        ctx->latest_frame->parameter_block, 0);
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
				spin_wait(table->write_index != atomic_load_u32(&table->read_index));
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
			beamformer_filter_update(cp->filters + slot, fctx->parameters, block, slot, *arena);
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

			u32 dirty_programs = atomic_swap_u32(&cp->dirty_programs, 0);
			static_assert(ISPOWEROF2(BeamformerMaxComputeShaderStages),
			              "max compute shader stages must be power of 2");
			assert((dirty_programs & ~((u32)BeamformerMaxComputeShaderStages - 1)) == 0);
			for EachBit(dirty_programs, slot)
				load_compute_shader(ctx, cp, (u32)slot, *arena);

			atomic_store_u32(&cs->processing_compute, 1);
			start_renderdoc_capture(gl_context);

			BeamformerFrame *frame = work->compute_context.frame;

			GLenum gl_kind = cp->iq_pipeline ? GL_RG32F : GL_R32F;
			if (!beamformer_frame_compatible(frame, cp->output_points, gl_kind))
				alloc_beamform_frame(frame, cp->output_points, gl_kind, s8("Beamformed_Data"), *arena);

			frame->min_coordinate   = cp->min_coordinate;
			frame->max_coordinate   = cp->max_coordinate;
			frame->acquisition_kind = cp->acquisition_kind;
			frame->compound_count   = cp->acquisition_count;

			BeamformerComputeContext  *cc       = &ctx->compute_context;
			BeamformerComputePipeline *pipeline = &cp->pipeline;
			/* NOTE(rnp): first stage requires access to raw data buffer directly so we break
			 * it out into a separate step. This way data can get released as soon as possible */
			if (pipeline->shader_count > 0) {
				BeamformerRFBuffer *rf = &cs->rf_buffer;
				u32 slot = rf->compute_index % countof(rf->compute_syncs);

				if (work->kind == BeamformerWorkKind_ComputeIndirect) {
					/* NOTE(rnp): compute indirect is used when uploading data. if compute thread
					 * preempts upload it must wait for the fence to exist. then it must tell the
					 * GPU to wait for upload to complete before it can start compute */
					spin_wait(!atomic_load_u64(rf->upload_syncs + slot));

					glWaitSync(rf->upload_syncs[slot], 0, GL_TIMEOUT_IGNORED);
					glDeleteSync(rf->upload_syncs[slot]);
					rf->compute_index++;
				} else {
					slot = (rf->compute_index - 1) % countof(rf->compute_syncs);
				}

				glBindBufferRange(GL_SHADER_STORAGE_BUFFER, 1, rf->ssbo, slot * rf->active_rf_size, rf->active_rf_size);

				glBeginQuery(GL_TIME_ELAPSED, cc->shader_timer_ids[0]);
				do_compute_shader(ctx, cp, frame, pipeline->shaders[0], 0, pipeline->parameters + 0, *arena);
				glEndQuery(GL_TIME_ELAPSED);

				if (work->kind == BeamformerWorkKind_ComputeIndirect) {
					atomic_store_u64(rf->compute_syncs + slot, glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0));
					atomic_store_u64(rf->upload_syncs + slot,  0);
				}
			}

			b32 did_sum_shader = 0;
			for (u32 i = 1; i < pipeline->shader_count; i++) {
				did_sum_shader |= pipeline->shaders[i] == BeamformerShaderKind_Sum;
				glBeginQuery(GL_TIME_ELAPSED, cc->shader_timer_ids[i]);
				do_compute_shader(ctx, cp, frame, pipeline->shaders[i], i, pipeline->parameters + i, *arena);
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
				u32 aframe_index = ((ctx->averaged_frame_index++) % countof(ctx->averaged_frames));
				ctx->averaged_frames[aframe_index].view_plane_tag  = frame->view_plane_tag;
				ctx->averaged_frames[aframe_index].ready_to_present = 1;
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
beamformer_rf_buffer_allocate(BeamformerRFBuffer *rf, u32 rf_size, b32 nvidia)
{
	assert((rf_size % 64) == 0);
	if (!nvidia) glUnmapNamedBuffer(rf->ssbo);
	glDeleteBuffers(1, &rf->ssbo);
	glCreateBuffers(1, &rf->ssbo);

	u32 buffer_flags = GL_DYNAMIC_STORAGE_BIT;
	if (!nvidia) buffer_flags |= GL_MAP_PERSISTENT_BIT|GL_MAP_WRITE_BIT;

	glNamedBufferStorage(rf->ssbo, countof(rf->compute_syncs) * rf_size, 0, buffer_flags);

	if (!nvidia) {
		u32 access = GL_MAP_PERSISTENT_BIT|GL_MAP_WRITE_BIT|GL_MAP_FLUSH_EXPLICIT_BIT|GL_MAP_UNSYNCHRONIZED_BIT;
		rf->buffer = glMapNamedBufferRange(rf->ssbo, 0, (GLsizei)(countof(rf->compute_syncs) * rf_size), access);
	}

	LABEL_GL_OBJECT(GL_BUFFER, rf->ssbo, s8("Raw_RF_SSBO"));
	rf->size = rf_size;
}

DEBUG_EXPORT BEAMFORMER_RF_UPLOAD_FN(beamformer_rf_upload)
{
	BeamformerSharedMemory *sm                  = ctx->shared_memory->region;
	BeamformerSharedMemoryLockKind scratch_lock = BeamformerSharedMemoryLockKind_ScratchSpace;
	BeamformerSharedMemoryLockKind upload_lock  = BeamformerSharedMemoryLockKind_UploadRF;

	u64 rf_block_rf_size;
	if (atomic_load_u32(sm->locks + upload_lock) &&
	    (rf_block_rf_size = atomic_swap_u64(&sm->rf_block_rf_size, 0)))
	{
		os_shared_memory_region_lock(ctx->shared_memory, sm->locks, (i32)scratch_lock, (u32)-1);

		BeamformerRFBuffer       *rf = ctx->rf_buffer;
		BeamformerParameterBlock *b  = beamformer_parameter_block(sm, (u32)(rf_block_rf_size >> 32ULL));
		BeamformerParameters     *bp = &b->parameters;
		BeamformerDataKind data_kind = b->pipeline.data_kind;

		b32 nvidia = gl_parameters.vendor_id == GLVendor_NVIDIA;

		rf->active_rf_size = (u32)round_up_to(rf_block_rf_size & 0xFFFFFFFFULL, 64);
		if (rf->size < rf->active_rf_size)
			beamformer_rf_buffer_allocate(rf, rf->active_rf_size, nvidia);

		u32 slot = rf->insertion_index++ % countof(rf->compute_syncs);

		/* NOTE(rnp): if the rest of the code is functioning then the first
		 * time the compute thread processes an upload it must have gone
		 * through this path. therefore it is safe to spin until it gets processed */
		spin_wait(atomic_load_u64(rf->upload_syncs + slot));

		if (atomic_load_u64(rf->compute_syncs + slot)) {
			GLenum sync_result = glClientWaitSync(rf->compute_syncs[slot], 0, 1000000000);
			if (sync_result == GL_TIMEOUT_EXPIRED || sync_result == GL_WAIT_FAILED) {
				// TODO(rnp): what do?
			}
			glDeleteSync(rf->compute_syncs[slot]);
		}

		u32 size = bp->channel_count * bp->acquisition_count * bp->sample_count * beamformer_data_kind_byte_size[data_kind];
		u8 *data = beamformer_shared_memory_scratch_arena(sm).beg;

		if (nvidia) glNamedBufferSubData(rf->ssbo, slot * rf->active_rf_size, (i32)size, data);
		else        mem_copy(rf->buffer + slot * rf->active_rf_size, data, size);

		os_shared_memory_region_unlock(ctx->shared_memory, sm->locks, (i32)scratch_lock);
		post_sync_barrier(ctx->shared_memory, upload_lock, sm->locks);

		if (!nvidia)
			glFlushMappedNamedBufferRange(rf->ssbo, slot * rf->active_rf_size, (i32)rf->active_rf_size);

		atomic_store_u64(rf->upload_syncs  + slot, glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0));
		atomic_store_u64(rf->compute_syncs + slot, 0);

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
	BeamformerCtx *ctx = BeamformerContextMemory(input->memory);
	dt_for_frame = input->dt;

	if (IsWindowResized()) {
		ctx->window_size.h = GetScreenHeight();
		ctx->window_size.w = GetScreenWidth();
	}

	coalesce_timing_table(ctx->compute_timing_table, ctx->compute_shader_stats);

	if (input->executable_reloaded) {
		ui_init(ctx, ctx->ui_backing_store);
		DEBUG_DECL(start_frame_capture = ctx->start_frame_capture);
		DEBUG_DECL(end_frame_capture   = ctx->end_frame_capture);
	}

	BeamformerSharedMemory *sm = ctx->shared_memory.region;
	if (atomic_load_u32(sm->locks + BeamformerSharedMemoryLockKind_UploadRF))
		os_wake_waiters(&ctx->upload_worker.sync_variable);
	if (atomic_load_u32(sm->locks + BeamformerSharedMemoryLockKind_DispatchCompute))
		os_wake_waiters(&ctx->compute_worker.sync_variable);

	BeamformerFrame        *frame = ctx->latest_frame;
	BeamformerViewPlaneTag  tag   = frame? frame->view_plane_tag : 0;
	draw_ui(ctx, input, frame, tag);

	ctx->frame_view_render_context.updated = 0;
}

/* NOTE(rnp): functions defined in these shouldn't be visible to the whole program */
#if _DEBUG
  #if OS_LINUX
    #include "os_linux.c"
  #elif OS_WINDOWS
    #include "os_win32.c"
  #endif
#endif
