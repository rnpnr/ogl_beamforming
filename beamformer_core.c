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
 * [ ]: BeamformWorkQueue -> BeamformerWorkQueue
 * [ ]: refactor: work queue needs a cleanup, we should only have a single one
 *      - that queue isn't really considered hot so a lock is probably fine
 * [ ]: bug: reinit cuda on hot-reload
 */

#include "compiler.h"

#if defined(BEAMFORMER_DEBUG) && !defined(BEAMFORMER_EXPORT) && OS_WINDOWS
  #define BEAMFORMER_EXPORT __declspec(dllexport)
#endif

#include "beamformer_internal.h"

global f32 dt_for_frame;

#if !BEAMFORMER_RENDERDOC_HOOKS
#define start_renderdoc_capture(...)
#define end_renderdoc_capture(...)
#else
global renderdoc_start_frame_capture_fn       *start_frame_capture;
global renderdoc_set_capture_path_template_fn *set_capture_path_template;
global renderdoc_end_frame_capture_fn         *end_frame_capture;
#define start_renderdoc_capture()  do { \
	if (set_capture_path_template) set_capture_path_template("captures/ogl.rdc"); \
	if (start_frame_capture)       start_frame_capture(vk_renderdoc_instance_handle(), 0); \
} while(0)
#define end_renderdoc_capture()   if (end_frame_capture)   end_frame_capture(vk_renderdoc_instance_handle(), 0)
#endif

read_only global u32 beamformer_compute_array_parameter_sizes[] = {
	#define X(k, type, elements) sizeof(type) * elements,
	BEAMFORMER_COMPUTE_ARRAY_PARAMETERS_LIST
	#undef X
};

read_only global u32 beamformer_compute_array_parameter_offsets[] = {
	#define X(k, ...) offsetof(BeamformerComputeArrayParameters, k),
	BEAMFORMER_COMPUTE_ARRAY_PARAMETERS_LIST
	#undef X
};

function void
beamformer_compute_plan_release(BeamformerComputeContext *cc, u32 block)
{
	assert(block < countof(cc->compute_plans));
	BeamformerComputePlan *cp = cc->compute_plans[block];
	if (cp) {
		vk_buffer_release(&cp->array_parameters);
		for (u32 i = 0; i < countof(cp->filters); i++)
			vk_buffer_release(&cp->filters[i].buffer);
		cc->compute_plans[block] = 0;
		SLLPushFreelist(cp, cc->compute_plan_freelist);
	}
}

function BeamformerComputePlan *
beamformer_compute_plan_for_block(BeamformerComputeContext *cc, u32 block, Arena *arena)
{
	assert(block < countof(cc->compute_plans));
	BeamformerComputePlan *result = cc->compute_plans[block];

	assert(result || arena);

	if (!result) {
		result = SLLPopFreelist(cc->compute_plan_freelist);
		if (!result) result = push_struct_no_zero(arena, BeamformerComputePlan);
		zero_struct(result);
		cc->compute_plans[block] = result;

		Stream label = arena_stream(*arena);
		stream_append_s8(&label, s8("ComputeParameterArray["));
		stream_append_u64(&label, block);
		stream_append_s8(&label, s8("]"));
		stream_append_byte(&label, 0);

		GPUBufferAllocateInfo allocate_info = {
			.size  = sizeof(BeamformerComputeArrayParameters),
			.flags = VulkanUsageFlag_HostReadWrite,
			.label = stream_to_s8(&label),
		};
		vk_buffer_allocate(&result->array_parameters, &allocate_info);
		assert((result->array_parameters.gpu_pointer & 63) == 0);
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

	u32 byte_size = f->length * (i32)sizeof(f32) * (fp.complex? 2 : 1);
	if (f->buffer.size < byte_size) {
		GPUBufferAllocateInfo allocate_info = {
			.size  = byte_size,
			.flags = VulkanUsageFlag_HostReadWrite,
			.label = label,
		};
		vk_buffer_allocate(&f->buffer, &allocate_info);
	}
	vk_buffer_range_upload(&f->buffer, filter, 0, byte_size, 0);
}

function iv3
make_valid_output_points(i32 points[3])
{
	iv3 result;
	i32 max_dimension_3D = vk_gpu_info()->max_image_dimension_3D;
	result.E[0] = Clamp(points[0], 1, max_dimension_3D);
	result.E[1] = Clamp(points[1], 1, max_dimension_3D);
	result.E[2] = Clamp(points[2], 1, max_dimension_3D);
	return result;
}

function void
update_hadamard(BeamformerComputePlan *cp, i32 order, Arena arena)
{
	f32 *hadamard = make_hadamard_transpose(&arena, order);
	if (hadamard) {
		u64 offset = offsetof(BeamformerComputeArrayParameters, Hadamard);
		u64 size   = sizeof(*((BeamformerComputeArrayParameters *)0)->Hadamard) * order * order;
		vk_buffer_range_upload(&cp->array_parameters, hadamard, offset, size, 0);
		cp->hadamard_order = order;
	}
}

function u64
beamformer_frame_byte_size(iv3 points, BeamformerDataKind kind)
{
	u64 result = points.x * points.y * points.z * beamformer_data_kind_byte_size[kind];
	result = round_up_to(result, 64);
	return result;
}

function BeamformerFrame *
beamformer_frame_next(BeamformerComputeContext *cc, iv3 output_points, b32 complex, u64 reserved_size)
{
	BeamformerFrameBacklog *bl = &cc->backlog;

	BeamformerDataKind kind = complex ? BeamformerDataKind_Float32Complex : BeamformerDataKind_Float32;
	u64 frame_size = beamformer_frame_byte_size(output_points, kind);

	// TODO(rnp): handle this somewhat gracefully (even it produces garbled output)
	assert(frame_size + reserved_size <= (u64)bl->buffer->size);

	if (bl->next_offset > (u64)bl->buffer->size - frame_size - reserved_size)
		bl->next_offset = 0;

	u64 id = bl->counter++;

	BeamformerFrame *result = bl->frames + (id % countof(bl->frames));
	atomic_store_u64(&result->timeline_valid_value, -1ULL);
	result->id            = id & U32_MAX;
	result->buffer_offset = bl->next_offset;
	result->points        = output_points;
	result->data_kind     = kind;

	bl->next_offset += frame_size;

	return result;
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
		work->kind = indirect? BeamformerWorkKind_ComputeIndirect : BeamformerWorkKind_Compute;
		work->lock = BeamformerSharedMemoryLockKind_DispatchCompute;
		work->compute_context.parameter_block = parameter_block;
	}
	return result;
}

function uv3
layout_for_output(iv3 points)
{
	// TODO(rnp): this can be calculated based on dimensionality of points
	uv3 result;
	result.x = 16;
	result.y = 1;
	result.z = 16;
	return result;
}

function uv3
dispatch_for_output(uv3 layout, iv3 points)
{
	uv3 result;
	result.x = (u32)ceil_f32((f32)points.x / layout.x);
	result.y = (u32)ceil_f32((f32)points.y / layout.y);
	result.z = (u32)ceil_f32((f32)points.z / layout.z);
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
	switch (bp->acquisition_kind) {
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

function b32
compute_plan_push_shader(BeamformerComputePlan *p, BeamformerShaderKind shader, BeamformerShaderParameters *sp)
{
	b32 result = 0;
	if (p->pipeline.shader_count < countof(p->pipeline.shaders)) {
		u32 index = p->pipeline.shader_count++;
		p->pipeline.shaders[index]    = shader;
		p->pipeline.parameters[index] = *sp;
		zero_struct(p->shader_descriptors + index);
		result = 1;
	}
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

	u32 subgroup_size = vk_gpu_info()->subgroup_size;

	f32 time_offset = pb->parameters.time_offset;

	BeamformerDataKind data_kind = pb->pipeline.data_kind;
	cp->pipeline.shader_count = 0;
	for (u32 i = 0; i < pb->pipeline.shader_count; i++) {
		BeamformerShaderParameters *sp = pb->pipeline.parameters + i;
		u32 slot   = cp->pipeline.shader_count;
		u32 shader = pb->pipeline.shaders[i];

		BeamformerShaderDescriptor *ld = cp->shader_descriptors + slot - 1;
		BeamformerShaderDescriptor *sd = cp->shader_descriptors + slot;

		switch (shader) {

		case BeamformerShaderKind_CudaHilbert:{
			if (run_cuda_hilbert)
				compute_plan_push_shader(cp, shader, sp);
		}break;

		case BeamformerShaderKind_Decode:{
			/* TODO(rnp): rework decode first and demodulate after */
			b32 first = slot == 0;

			BeamformerShaderKind *last_shader = cp->pipeline.shaders + slot - 1;
			assert(first || ((*last_shader == BeamformerShaderKind_Demodulate ||
			                  *last_shader == BeamformerShaderKind_Filter)));

			if ((first || pb->parameters.decode_mode != BeamformerDecodeMode_None) &&
			    compute_plan_push_shader(cp, shader, sp))
			{
				sd->bake.data_kind = data_kind;
				if (!first) {
					if (data_kind == BeamformerDataKind_Int16) {
						sd->bake.data_kind = BeamformerDataKind_Int16Complex;
					} else {
						sd->bake.data_kind = BeamformerDataKind_Float32Complex;
					}
				}

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
					sd->layout = (uv3){{subgroup_size, 1, 1}};

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
					sd->layout = (uv3){{subgroup_size / 2, 1, 1}};

					sd->dispatch.x = (u32)ceil_f32((f32)sample_count                 / (f32)sd->layout.x);
					sd->dispatch.y = (u32)ceil_f32((f32)pb->parameters.channel_count / (f32)sd->layout.y);
					sd->dispatch.z = 1;
				}

				if (first) sd->dispatch.x *= decimation_rate;

				/* NOTE(rnp): decode 2 samples per dispatch when data is i16 */
				if (first && data_kind == BeamformerDataKind_Int16)
					sd->dispatch.x = (u32)ceil_f32((f32)sd->dispatch.x / 2);
			}
		}break;

		case BeamformerShaderKind_Demodulate:
		case BeamformerShaderKind_Filter:
		{
			if (compute_plan_push_shader(cp, shader, sp)) {
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
			}
		}break;

		case BeamformerShaderKind_DAS:{
			if (compute_plan_push_shader(cp, shader, sp)) {
				sd->bake.data_kind = BeamformerDataKind_Float32;
				if (cp->iq_pipeline)
					sd->bake.data_kind = BeamformerDataKind_Float32Complex;

				BeamformerShaderDASBakeParameters *db = &sd->bake.DAS;
				cp->das_voxel_transform    = das_voxel_transform_matrix(&pb->parameters);
				cp->xdc_element_pitch      = pb->parameters.xdc_element_pitch;
				// NOTE(rnp): old gcc will miscompile an assignment
				mem_copy(cp->xdc_transform.E, pb->parameters.xdc_transform.E, sizeof(cp->xdc_transform));

				db->sampling_frequency     = sampling_frequency;
				db->demodulation_frequency = pb->parameters.demodulation_frequency;
				db->speed_of_sound         = pb->parameters.speed_of_sound;
				db->time_offset            = time_offset;
				db->f_number               = pb->parameters.f_number;
				db->acquisition_kind       = pb->parameters.acquisition_kind;
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

				u32 id = pb->parameters.acquisition_kind;
				if (id == BeamformerAcquisitionKind_UFORCES || id == BeamformerAcquisitionKind_UHERCULES)
					sd->bake.flags |= BeamformerShaderDASFlags_Sparse;

				sd->layout   = layout_for_output(cp->output_points);
				sd->dispatch = dispatch_for_output(sd->layout, cp->output_points);


				if (pb->parameters.coherency_weighting &&
				    compute_plan_push_shader(cp, BeamformerShaderKind_CoherencyWeighting, sp))
				{
					BeamformerShaderDescriptor *shader_descriptor = cp->shader_descriptors + cp->pipeline.shader_count - 1;
					shader_descriptor->layout   = sd->layout;
					shader_descriptor->dispatch = sd->dispatch;
					shader_descriptor->bake.data_kind = sd->bake.data_kind;
				}
			}
		}break;

		#if 0
		case BeamformerShaderKind_Sum:{
			sd->bake.data_kind = BeamformerDataKind_Float32;
			if (cp->iq_pipeline)
				sd->bake.data_kind = BeamformerDataKind_Float32Complex;

			sd->layout   = layout_for_output(cp->output_points);
			sd->dispatch = dispatch_for_output(sd->layout, cp->output_points);

			commit = 1;
		}break;
		#endif

		default:{}break;
		}
	}
	cp->pipeline.data_kind = data_kind;
}

function void
stream_append_shader_header(Stream *s, i32 reloadable_index, BeamformerShaderDescriptor *sd, uv3 layout)
{
	stream_append_s8s(s, s8("#version 460 core\n\n"
	"#extension GL_EXT_buffer_reference : require\n"
	"#extension GL_EXT_shader_16bit_storage : require\n"
	"#extension GL_EXT_shader_explicit_arithmetic_types : require\n\n"));

	i32  header_vector_length = beamformer_shader_header_vector_lengths[reloadable_index];
	i32 *header_vector        = beamformer_shader_header_vectors[reloadable_index];
	for (i32 index = 0; index < header_vector_length; index++)
		stream_append_s8(s, beamformer_shader_global_header_strings[header_vector[index]]);

	if (layout.x != 0) {
		stream_append_s8(s,  s8("layout(local_size_x = "));
		stream_append_u64(s, layout.x);
		stream_append_s8(s,  s8(", local_size_y = "));
		stream_append_u64(s, layout.y);
		stream_append_s8(s,  s8(", local_size_z = "));
		stream_append_u64(s, layout.z);
		stream_append_s8(s,  s8(") in;\n\n"));
	}

	if (sd) {
		u32 *parameters = (u32 *)&sd->bake;
		s8  *names      = beamformer_shader_bake_parameter_names[reloadable_index];
		u32  float_bits = beamformer_shader_bake_parameter_float_bits[reloadable_index];
		i32  count      = beamformer_shader_bake_parameter_counts[reloadable_index];

		for (i32 index = 0; index < count; index++) {
			stream_append_s8s(s, s8("#define "), names[index],
			                  (float_bits & (1 << index))? s8(" uintBitsToFloat") : s8(" "), s8("(0x"));
			stream_append_hex_u64(s, parameters[index]);
			stream_append_s8(s, s8(")\n"));
		}

		stream_append_s8(s, s8("#define DataKind (0x"));
		stream_append_hex_u64(s, sd->bake.data_kind);
		stream_append_s8(s, s8(")\n\n"));

		s8  *flag_names = beamformer_shader_flag_strings[reloadable_index];
		u32  flag_count = beamformer_shader_flag_strings_count[reloadable_index];
		u32  flags      = sd->bake.flags;
		for (u32 bit = 0; bit < flag_count; bit++) {
			stream_append_s8s(s, s8("#define "), flag_names[bit],
			                  (flags & (1 << bit))? s8(" 1") : s8(" 0"), s8("\n"));
		}
	}

	u32  pc_count    = beamformer_shader_push_constant_counts[reloadable_index];
	s8  *pc_names    = beamformer_shader_push_constant_names[reloadable_index];
	s8  *pc_types    = beamformer_shader_push_constant_vk_types[reloadable_index];
	if (pc_count) {
		stream_append_s8s(s, s8("\n\nlayout(push_constant, std430) uniform PushConstants {\n"));
		for (u32 it = 0; it < pc_count; it++)
			stream_append_s8s(s, s8("\t"), pc_types[it], s8(" "), pc_names[it],s8(";\n"));
		stream_append_s8s(s, s8("};"));
	}

	stream_append_s8(s, s8("\n\n#line 1\n"));
}

function void
beamformer_reload_pipeline(VulkanHandle *pipeline, BeamformerShaderReloadInfo *sris, u32 count, Arena arena)
{
	assume(count <= 2);
	s8 paths[2];
	VulkanPipelineCreateInfo infos[2];

	if (!BakeShaders) {
		for (u32 i = 0; i < count; i++)
			paths[i] = push_s8_from_parts(&arena, os_path_separator(), s8("shaders"), sris[i].filename_or_data);
	}

	u32 push_constants_size = 0;
	for (u32 i = 0; i < count; i++) {
		Stream shader_stream = arena_stream(arena);
		i32 reloadable_index = beamformer_shader_reloadable_index_by_shader[sris[i].shader];
		if (i == 0) push_constants_size = beamformer_shader_push_constant_sizes[reloadable_index];
		else        assert(push_constants_size == beamformer_shader_push_constant_sizes[reloadable_index]);

		stream_append_shader_header(&shader_stream, reloadable_index, sris[i].shader_descriptor, sris[i].layout);

		if (BakeShaders) {
			stream_append_s8(&shader_stream, sris[i].filename_or_data);
		} else {
			shader_stream.widx += os_read_entire_file((c8 *)paths[i].data,
			                                          shader_stream.data + shader_stream.widx,
			                                          shader_stream.cap  - shader_stream.widx);
		}

		infos[i].kind = sris[i].shader_kind;
		infos[i].text = arena_stream_commit_zero(&arena, &shader_stream);
		infos[i].name = beamformer_shader_names[sris[i].shader];

		//s8 line = s8("---------------\n");
		//s8 nl   = s8("\n");
		//os_console_log(line.data, line.len);
		//os_console_log(infos[i].name.data, infos[i].name.len);
		//os_console_log(nl.data, nl.len);
		//os_console_log(line.data, line.len);
		//os_console_log(infos[i].text.data, infos[i].text.len);
		//os_console_log(line.data, line.len);
	}

	vk_pipeline_release(*pipeline);
	*pipeline = vk_pipeline(infos, count, push_constants_size);
}

function void
beamformer_reload_render_pipeline(VulkanHandle *pipeline, BeamformerShaderKind shader, Arena arena)
{
	i32 index = beamformer_shader_reloadable_index_by_shader[shader];
	BeamformerShaderReloadInfo infos[2] = {
		{
			.shader      = shader,
			.shader_kind = beamformer_shader_primitive_is_vertex[index] ? VulkanShaderKind_Vertex : VulkanShaderKind_Mesh,
			.filename_or_data = BakeShaders ? beamformer_shader_data[index][0]
			                                : beamformer_reloadable_shader_files[index][0],
		},
		{
			.shader           = shader,
			.shader_kind      = VulkanShaderKind_Fragment,
			.filename_or_data = BakeShaders ? beamformer_shader_data[index][1]
			                                : beamformer_reloadable_shader_files[index][1],
		},
	};
	beamformer_reload_pipeline(pipeline, infos, countof(infos), arena);
}

function void
beamformer_reload_compute_pipeline(VulkanHandle *pipeline, BeamformerShaderKind shader,
                                   BeamformerShaderDescriptor *shader_descriptor, Arena arena)
{
	i32 index  = beamformer_shader_reloadable_index_by_shader[shader];
	uv3 layout = shader_descriptor ? shader_descriptor->layout : (uv3){{vk_gpu_info()->subgroup_size, 1, 1}};
	BeamformerShaderReloadInfo info = {
		.shader            = shader,
		.shader_kind       = VulkanShaderKind_Compute,
		.shader_descriptor = shader_descriptor,
		.filename_or_data  = BakeShaders ? beamformer_shader_data[index][0]
		                                 : beamformer_reloadable_shader_files[index][0],
		.layout            = layout,
	};
	beamformer_reload_pipeline(pipeline, &info, 1, arena);
}

function void
beamformer_commit_parameter_block(BeamformerCtx *ctx, BeamformerComputePlan *cp, u32 block, Arena arena)
{
	BeamformerParameterBlock *pb = beamformer_parameter_block_lock(ctx->shared_memory, block, -1);
	for EachBit(pb->dirty_regions, region) {
		switch (region) {
		case BeamformerParameterBlockRegion_ComputePipeline:
		case BeamformerParameterBlockRegion_Parameters:
		{
			cp->output_points = make_valid_output_points(pb->parameters.output_points.E);

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

			cp->acquisition_count = pb->parameters.acquisition_count;
			cp->acquisition_kind  = pb->parameters.acquisition_kind;

			// NOTE(rnp): buffer size / 2 should be mutiple of 64
			i64 buffer_size = round_up_to(2 * cp->rf_size, 128);
			if (ctx->compute_context.ping_pong_buffer.size < buffer_size) {
				GPUBufferAllocateInfo allocate_info = {.size = buffer_size, .label = s8("PingPongBuffer")};
				vk_buffer_allocate(&ctx->compute_context.ping_pong_buffer, &allocate_info);
				// TODO(rnp): figure out how to share with CUDA
			}

			if (cp->hadamard_order != (i32)cp->acquisition_count)
				update_hadamard(cp, (i32)cp->acquisition_count, arena);

			cp->min_coordinate = pb->parameters.output_min_coordinate;
			cp->max_coordinate = pb->parameters.output_max_coordinate;

			cp->output_points  = make_valid_output_points(pb->parameters.output_points.E);
			cp->average_frames = pb->parameters.output_points.E[3];
		}break;

		case BeamformerParameterBlockRegion_ChannelMapping:{
			cuda_set_channel_mapping(pb->channel_mapping);
		}break;
		case BeamformerParameterBlockRegion_TransmitReceiveOrientations:{
			GPUBuffer *b = &cp->array_parameters;
			u32 kind   = BeamformerComputeArrayParameterKind_TransmitReceiveOrientations;
			u64 offset = beamformer_compute_array_parameter_offsets[kind];
			u64 size   = beamformer_compute_array_parameter_sizes[kind];
			{
				Arena scratch = arena;
				u16 *u16s = push_array(&scratch, u16, countof(pb->transmit_receive_orientations));
				for (u32 i = 0; i < countof(pb->transmit_receive_orientations); i++)
					u16s[i] = pb->transmit_receive_orientations[i];

				vk_buffer_range_upload(b, u16s, offset, size, 0);
			}
		}break;
		case BeamformerParameterBlockRegion_FocalVectors:
		case BeamformerParameterBlockRegion_SparseElements:
		{
			u32 kind = BeamformerComputeArrayParameterKind_Count;
			switch (region) {
			case BeamformerParameterBlockRegion_FocalVectors:{
				kind = BeamformerComputeArrayParameterKind_FocalVectors;
			}break;
			case BeamformerParameterBlockRegion_SparseElements:{
				kind = BeamformerComputeArrayParameterKind_SparseElements;
			}break;
			InvalidDefaultCase;
			}

			if (kind != BeamformerComputeArrayParameterKind_Count) {
				GPUBuffer *b = &cp->array_parameters;
				u64 offset = beamformer_compute_array_parameter_offsets[kind];
				u64 size   = beamformer_compute_array_parameter_sizes[kind];
				vk_buffer_range_upload(b, (u8 *)pb + BeamformerParameterBlockRegionOffsets[region], offset, size, 0);
			}
		}break;
		}
	}
	beamformer_parameter_block_unlock(ctx->shared_memory, block);
}

function void
do_compute_shader(BeamformerCtx *ctx, VulkanHandle cmd, BeamformerComputePlan *cp, BeamformerFrame *frame,
                  u32 shader_slot, Arena arena, u64 rf_pointer)
{
	BeamformerComputeContext *cc = &ctx->compute_context;

	u32 output_index = !cc->ping_pong_input_index;
	u32 input_index  =  cc->ping_pong_input_index;

	u64 pp_size = cc->ping_pong_buffer.size / 2;
	u64 pp_input_pointer  = cc->ping_pong_buffer.gpu_pointer + input_index  * pp_size;
	u64 pp_output_pointer = cc->ping_pong_buffer.gpu_pointer + output_index * pp_size;

	uv3 dispatch = cp->shader_descriptors[shader_slot].dispatch;

	vk_command_bind_pipeline(cmd, cp->vulkan_pipelines[shader_slot]);

	switch (cp->pipeline.shaders[shader_slot]) {

	case BeamformerShaderKind_Decode:{
		BeamformerDecodeMode mode = cp->shader_descriptors[shader_slot].bake.Decode.decode_mode;
		BeamformerShaderDecodePushConstants pc = {
			.hadamard_buffer = cp->array_parameters.gpu_pointer + offsetof(BeamformerComputeArrayParameters, Hadamard),
			.output_buffer   = pp_output_pointer,
		};

		if (shader_slot == 0 && mode != BeamformerDecodeMode_None) {
			pc.output_rf_buffer = pp_input_pointer;
			pc.rf_buffer        = rf_pointer;
			pc.first_pass       = 1;

			GPUMemoryBarrierInfo barrier = {
				.gpu_buffer = &cc->ping_pong_buffer,
				.offset     = pp_input_pointer - cc->ping_pong_buffer.gpu_pointer,
				.size       = pp_size,
			};

			vk_command_push_constants(cmd, 0, sizeof(pc), &pc);
			vk_command_dispatch_compute(cmd, dispatch);
			vk_command_buffer_memory_barriers(cmd, &barrier, 1);

			pc.output_rf_buffer = 0;
		}

		pc.rf_buffer  = pp_input_pointer;
		pc.first_pass = 0;

		GPUMemoryBarrierInfo barrier = {
			.gpu_buffer = &cc->ping_pong_buffer,
			.offset     = pp_output_pointer - cc->ping_pong_buffer.gpu_pointer,
			.size       = pp_size,
		};

		vk_command_push_constants(cmd, 0, sizeof(pc), &pc);
		vk_command_dispatch_compute(cmd, dispatch);
		vk_command_buffer_memory_barriers(cmd, &barrier, 1);

		cc->ping_pong_input_index = !cc->ping_pong_input_index;
	}break;

	case BeamformerShaderKind_CudaDecode:{
		cuda_decode(0, output_index, 0);
		cc->ping_pong_input_index = !cc->ping_pong_input_index;
	}break;
	case BeamformerShaderKind_CudaHilbert:{
		cuda_hilbert(input_index, output_index);
		cc->ping_pong_input_index = !cc->ping_pong_input_index;
	}break;

	case BeamformerShaderKind_Filter:
	case BeamformerShaderKind_Demodulate:
	{
		u32 filter_slot = cp->pipeline.parameters[shader_slot].filter_slot;
		BeamformerShaderFilterPushConstants pc = {
			.filter_coefficients = cp->filters[filter_slot].buffer.gpu_pointer,
			.output_data         = pp_output_pointer,
			.input_data          = shader_slot == 0 ? rf_pointer : pp_input_pointer,
		};

		GPUMemoryBarrierInfo barrier = {
			.gpu_buffer = &cc->ping_pong_buffer,
			.offset     = pp_output_pointer - cc->ping_pong_buffer.gpu_pointer,
			.size       = pp_size,
		};

		vk_command_push_constants(cmd, 0, sizeof(pc), &pc);
		vk_command_dispatch_compute(cmd, dispatch);
		vk_command_buffer_memory_barriers(cmd, &barrier, 1);

		cc->ping_pong_input_index = !cc->ping_pong_input_index;
	}break;

	case BeamformerShaderKind_DAS:{
		local_persist u32 das_cycle_t = 0;

		GPUBuffer *b = cc->backlog.buffer;

		u64 frame_size      = beamformer_frame_byte_size(frame->points, frame->data_kind);
		u64 incoherent_size = frame_size / beamformer_data_kind_element_count[frame->data_kind];

		BeamformerShaderDASPushConstants pc = {
			.voxel_transform               = cp->das_voxel_transform,
			.xdc_transform                 = cp->xdc_transform,
			.xdc_element_pitch             = cp->xdc_element_pitch,
			.rf_data                       = pp_input_pointer,
			.output_data                   = b->gpu_pointer + frame->buffer_offset,
			.incoherent_output             = b->gpu_pointer + b->size - incoherent_size,

			// TODO(rnp): move structure definition to shader, store one pointer
			.focal_vectors                 = cp->array_parameters.gpu_pointer + offsetof(BeamformerComputeArrayParameters, FocalVectors),
			.sparse_elements               = cp->array_parameters.gpu_pointer + offsetof(BeamformerComputeArrayParameters, SparseElements),
			.transmit_receive_orientations = cp->array_parameters.gpu_pointer + offsetof(BeamformerComputeArrayParameters, TransmitReceiveOrientations),

			.output_size_x                 = cp->output_points.x,
			.output_size_y                 = cp->output_points.y,
			.output_size_z                 = cp->output_points.z,
			.cycle_t                       = das_cycle_t++,
		};

		BeamformerShaderBakeParameters *bp = &cp->shader_descriptors[shader_slot].bake;
		b32 coherent = (bp->flags & BeamformerShaderDASFlags_CoherencyWeighting) != 0;

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

		GPUMemoryBarrierInfo memory_barriers[2] = {
			{
				.gpu_buffer = b,
				.offset     = frame->buffer_offset,
				.size       = frame_size,
			},
			{
				.gpu_buffer = b,
				.offset     = pc.incoherent_output - b->gpu_pointer,
				.size       = incoherent_size,
			},
		};

		// NOTE(rnp): barrier to wait for clear pipeline to complete
		vk_command_buffer_memory_barriers(cmd, memory_barriers, 1 + coherent);

		vk_command_push_constants(cmd, 0, sizeof(pc), &pc);
		for (i32 index = 0; index < loop_end; index++) {
			if (index != 0) {
				pc.channel_t = index;
				vk_command_push_constants(cmd, offsetof(BeamformerShaderDASPushConstants, channel_t),
				                          sizeof(pc.channel_t), &pc.channel_t);
			}
			vk_command_dispatch_compute(cmd, dispatch);
			vk_command_buffer_memory_barriers(cmd, memory_barriers, 1 + coherent);
		}
	}break;

	case BeamformerShaderKind_CoherencyWeighting:{
		GPUBuffer *b = cc->backlog.buffer;

		u64 frame_size      = beamformer_frame_byte_size(frame->points, frame->data_kind);
		u64 incoherent_size = frame_size / beamformer_data_kind_element_count[frame->data_kind];

		GPUMemoryBarrierInfo memory_barrier = {
			.gpu_buffer = b,
			.offset     = frame->buffer_offset,
			.size       = frame_size,
		};

		BeamformerShaderCoherencyWeightingPushConstants cwpc = {
			.left_side_buffer  = b->gpu_pointer + frame->buffer_offset,
			.right_side_buffer = b->gpu_pointer + b->size - incoherent_size,
			.elements          = incoherent_size / beamformer_data_kind_element_size[frame->data_kind],
			.scale             = 1.0f,
			.output_size_x     = cp->output_points.x,
			.output_size_y     = cp->output_points.y,
			.output_size_z     = cp->output_points.z,
		};

		vk_command_push_constants(cmd, 0, sizeof(cwpc), &cwpc);
		vk_command_dispatch_compute(cmd, dispatch);
		vk_command_buffer_memory_barriers(cmd, &memory_barrier, 1);
	}break;

	// NOTE(rnp): invalid stages should be filtered in planning phase
	InvalidDefaultCase;
	}

	#if 0
	switch (shader) {
	case BeamformerShaderKind_MinMax:{
		for (u32 i = 1; i < frame->image.mip_map_levels; i++) {
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

		/* NOTE: zero output before summing */
		glClearTexImage(aframe->texture, 0, GL_RED, GL_FLOAT, 0);
		glMemoryBarrier(GL_TEXTURE_UPDATE_BARRIER_BIT);

		glBindImageTexture(0, out_texture, 0, GL_TRUE, 0, GL_READ_WRITE, GL_RG32F);
		for (u32 i = 0; i < in_texture_count; i++) {
			glBindImageTexture(1, in_textures[i], 0, GL_TRUE, 0, GL_READ_ONLY, GL_RG32F);
			glDispatchCompute(dispatch.x, dispatch.y, dispatch.z);
			glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
		}

		aframe->min_coordinate   = frame->min_coordinate;
		aframe->max_coordinate   = frame->max_coordinate;
		aframe->compound_count   = frame->compound_count;
		aframe->acquisition_kind = frame->acquisition_kind;
	}break;
	}
	#endif
}

function void
complete_queue(BeamformerCtx *ctx, BeamformWorkQueue *q, Arena *arena)
{
	BeamformerComputeContext * cs = &ctx->compute_context;
	BeamformerSharedMemory *   sm = ctx->shared_memory;

	BeamformWork *work = beamform_work_queue_pop(q);
	while (work) {
		b32 can_commit = 1;
		switch (work->kind) {

		case BeamformerWorkKind_ExportBuffer:{
			/* TODO(rnp): better way of handling DispatchCompute barrier */
			post_sync_barrier(ctx->shared_memory, BeamformerSharedMemoryLockKind_DispatchCompute);
			beamformer_shared_memory_take_lock(ctx->shared_memory, (i32)work->lock, (u32)-1);
			BeamformerExportContext *ec = &work->export_context;
			switch (ec->kind) {
			case BeamformerExportKind_BeamformedData:{
				BeamformerFrame *f = ctx->latest_frame;
				if (f) {
					u64 frame_size = beamformer_frame_byte_size(f->points, f->data_kind);
					assert((frame_size & 63) == 0);
					if (frame_size <= ec->size) {
						vk_host_wait_timeline(VulkanTimeline_Compute, f->timeline_valid_value, -1ULL);
						vk_buffer_range_download(beamformer_shared_memory_scratch_arena(sm).beg,
						                         ctx->compute_context.backlog.buffer, f->buffer_offset,
						                         frame_size, 1);
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
			beamformer_shared_memory_release_lock(ctx->shared_memory, work->lock);
			post_sync_barrier(ctx->shared_memory, BeamformerSharedMemoryLockKind_ExportSync);
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
			push_compute_timing_info(ctx->compute_timing_table,
			                         (ComputeTimingInfo){.kind = ComputeTimingInfoKind_ComputeFrameBegin});

			BeamformerComputePlan *cp = beamformer_compute_plan_for_block(cs, work->compute_context.parameter_block, arena);
			if unlikely(beamformer_parameter_block_dirty(sm, work->compute_context.parameter_block)) {
				u32 block = work->compute_context.parameter_block;
				beamformer_commit_parameter_block(ctx, cp, block, *arena);
				atomic_store_u32(&ctx->ui_dirty_parameter_blocks, (u32)(ctx->beamform_work_queue != q) << block);
			}

			post_sync_barrier(ctx->shared_memory, work->lock);

			u32 dirty_programs = atomic_swap_u32(&cp->dirty_programs, 0);
			static_assert(ISPOWEROF2(BeamformerMaxComputeShaderStages),
			              "max compute shader stages must be power of 2");
			assert((dirty_programs & ~((u32)BeamformerMaxComputeShaderStages - 1)) == 0);
			for EachBit(dirty_programs, slot) {
				beamformer_reload_compute_pipeline(cp->vulkan_pipelines + slot, cp->pipeline.shaders[slot],
				                                   cp->shader_descriptors + slot, *arena);
			}

			atomic_store_u32(&cs->processing_compute, 1);

			start_renderdoc_capture();

			i32 das_index = -1;
			b32 has_sum   = 0;
			for (u32 i = 0; i < cp->pipeline.shader_count; i++) {
				has_sum |= cp->pipeline.shaders[i] == BeamformerShaderKind_Sum;
				if (cp->pipeline.shaders[i] == BeamformerShaderKind_DAS)
					das_index = (i32)i;
			}
			b32 das_coherent = das_index >= 0 && (cp->shader_descriptors[das_index].bake.flags &
			                                      BeamformerShaderDASFlags_CoherencyWeighting);

			u64 reserved_frame_size = 0;

			if (has_sum)
				reserved_frame_size += beamformer_frame_byte_size(cp->output_points, cp->iq_pipeline ?
				                                                  BeamformerDataKind_Float32Complex :
				                                                  BeamformerDataKind_Float32);

			// TODO(rnp): incoherent sum for different data kinds
			if (das_coherent)
				reserved_frame_size += beamformer_frame_byte_size(cp->output_points, BeamformerDataKind_Float32);

			BeamformerFrame *frame  = beamformer_frame_next(cs, cp->output_points, cp->iq_pipeline, reserved_frame_size);
			frame->min_coordinate   = cp->min_coordinate;
			frame->max_coordinate   = cp->max_coordinate;
			frame->acquisition_kind = cp->acquisition_kind;
			frame->compound_count   = cp->acquisition_count;

			VulkanHandle cmd = vk_command_begin(VulkanTimeline_Compute);
			vk_command_timestamp(cmd);

			if (das_index >= 0) {
				GPUBuffer *backlog = cs->backlog.buffer;
				u32 subgroup_size = vk_gpu_info()->subgroup_size;
				BeamformerShaderBufferClearPushConstants pc = {
					.data       = backlog->gpu_pointer + frame->buffer_offset,
					.clear_word = 0,
					.words      = beamformer_frame_byte_size(frame->points, frame->data_kind) / sizeof(u32),
				};

				u32 index = BeamformerShaderKind_BufferClear - BeamformerShaderKind_ComputeInternalFirst;
				vk_command_bind_pipeline(cmd, cs->compute_internal_pipelines[index]);
				vk_command_push_constants(cmd, 0, sizeof(pc), &pc);
				vk_command_dispatch_compute(cmd, (uv3){{(u32)ceil_f32((f32)pc.words / subgroup_size), 1, 1}});

				if (das_coherent) {
					pc.words = pc.words / beamformer_data_kind_element_count[frame->data_kind];
					pc.data  = backlog->gpu_pointer + backlog->size - sizeof(u32) * pc.words;
					vk_command_push_constants(cmd, 0, sizeof(pc), &pc);
					vk_command_dispatch_compute(cmd, (uv3){{(u32)ceil_f32((f32)pc.words / subgroup_size), 1, 1}});
				}
			}

			BeamformerRFBuffer *rf = &cs->rf_buffer;
			u32 compute_index = rf->compute_index;
			u32 slot = compute_index % countof(rf->upload_complete_values);

			if (work->kind == BeamformerWorkKind_ComputeIndirect) {
				// TODO(rnp): this shouldn't be necessary, there should be a way of communicating
				// what the value will be so that the only the command wait is needed.
				spin_wait(atomic_load_u64(rf->upload_complete_values + slot) <= compute_index);

				/* NOTE(rnp): if the GPU supports BAR there may be no need to synchronize
				 * other than the above spin */
				if (vk_buffer_needs_sync(&rf->buffer))
					vk_command_wait_timeline(cmd, VulkanTimeline_Transfer, rf->upload_complete_values[slot]);
			} else {
				slot = (rf->compute_index - 1) % countof(rf->upload_complete_values);
			}

			for (u32 i = 0; i < cp->pipeline.shader_count; i++) {
				do_compute_shader(ctx, cmd, cp, frame, i, *arena,
				                  rf->buffer.gpu_pointer + slot * rf->active_rf_size);
				vk_command_timestamp(cmd);
			}

			u64 end_timeline_value = vk_command_end(cmd, (VulkanHandle){0}, (VulkanHandle){0});
			if (work->kind == BeamformerWorkKind_ComputeIndirect) {
				atomic_store_u64(rf->compute_complete_values + slot, end_timeline_value);
				atomic_add_u64(&rf->compute_index, 1);
			}

			atomic_store_u64(&frame->timeline_valid_value, end_timeline_value);

			{
				Arena scratch    = *arena;
				/* NOTE(rnp): this blocks until work completes */
				u64 * timestamps = vk_command_read_timestamps(VulkanTimeline_Compute, &scratch);

				u64 last_time    = timestamps[0] > 0 ? timestamps[1] : 0;
				u32 shader_index = 0;
				for (u64 i = 2; i < timestamps[0] + 1; i++) {
					push_compute_timing_info(ctx->compute_timing_table, (ComputeTimingInfo){
						.kind        = ComputeTimingInfoKind_Shader,
						.shader      = cp->pipeline.shaders[shader_index],
						.shader_slot = shader_index,
						.timer_count = timestamps[i] - last_time,
					});
					last_time = timestamps[i];
					shader_index++;
				}
			}

			cs->processing_progress = 1;

			if (has_sum) {
				#if 0
				u32 aframe_index = ((ctx->averaged_frame_index++) % countof(ctx->averaged_frames));
				ctx->averaged_frames[aframe_index].view_plane_tag  = frame->view_plane_tag;
				ctx->averaged_frames[aframe_index].ready_to_present = 1;
				atomic_store_u64((u64 *)&ctx->latest_frame, (u64)(ctx->averaged_frames + aframe_index));
				#endif
			} else {
				atomic_store_u64((u64 *)&ctx->latest_frame, (u64)frame);
			}

			atomic_store_u32(&cs->processing_compute, 0);

			push_compute_timing_info(ctx->compute_timing_table,
			                         (ComputeTimingInfo){.kind = ComputeTimingInfoKind_ComputeFrameEnd});

			end_renderdoc_capture();
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

	b32 has_rf = 0;
	f32 gpu_clocks_to_nano = 1.0e-9f * vk_gpu_info()->timestamp_period_ns;

	// NOTE(rnp): not equal (the index may wrap)
	while (t->read_index != target) {
		ComputeTimingInfo info = t->buffer[t->read_index % countof(t->buffer)];
		switch (info.kind) {

		case ComputeTimingInfoKind_ComputeFrameBegin:{
			assert(t->compute_frame_active == 0);
			t->compute_frame_active = 1;
			/* NOTE(rnp): allow multiple instances of same shader to accumulate */
			t->in_flight_shader_count = 0;
			mem_clear(t->in_flight_shader_ids, 0, sizeof(t->in_flight_shader_ids));
			mem_clear(stats->table.times[stats_index], 0, sizeof(stats->table.times[stats_index]));
		}break;

		case ComputeTimingInfoKind_ComputeFrameEnd:{
			assert(t->compute_frame_active == 1);
			t->compute_frame_active = 0;
			stats->latest_frame_index = stats_index;
			stats_index = (stats_index + 1) % countof(stats->table.times);
			stats->table.shader_count = t->in_flight_shader_count;
			mem_copy(stats->table.shader_ids, t->in_flight_shader_ids, sizeof(t->in_flight_shader_ids));
		}break;

		case ComputeTimingInfoKind_Shader:{
			t->in_flight_shader_count = Max(t->in_flight_shader_count, info.shader_slot + 1u);
			t->in_flight_shader_ids[info.shader_slot] = info.shader;
			stats->table.times[stats_index][info.shader_slot] += info.timer_count * gpu_clocks_to_nano;
		}break;

		case ComputeTimingInfoKind_RF_Data:{
			stats->latest_rf_index = (stats->latest_rf_index + 1) % countof(stats->table.rf_time_deltas);
			f32 delta = info.timer_count / (f32)os_system_info()->timer_frequency;
			stats->table.rf_time_deltas[stats->latest_rf_index] = delta;
			has_rf = 1;
		}break;
		}
		/* NOTE(rnp): do this at the end so that stats table is always in a consistent state */
		t->read_index++;
	}

	for (u32 i = 0; i < stats->table.shader_count; i++) {
		f32 sum = 0;
		for EachElement(stats->table.times, it)
			sum += stats->table.times[it][i];
		stats->average_times[i] = sum / countof(stats->table.times);
	}

	if (has_rf) {
		f32 sum = 0;
		for EachElement(stats->table.rf_time_deltas, i)
			sum += stats->table.rf_time_deltas[i];
		stats->rf_time_delta_average = sum / countof(stats->table.rf_time_deltas);
	}
}

DEBUG_EXPORT BEAMFORMER_COMPLETE_COMPUTE_FN(beamformer_complete_compute)
{
	BeamformerSharedMemory *sm = ctx->shared_memory;
	complete_queue(ctx, &sm->external_work_queue, arena);
	complete_queue(ctx, ctx->beamform_work_queue, arena);
}

DEBUG_EXPORT BEAMFORMER_RF_UPLOAD_FN(beamformer_rf_upload)
{
	BeamformerSharedMemory *sm                  = ctx->shared_memory;
	BeamformerSharedMemoryLockKind scratch_lock = BeamformerSharedMemoryLockKind_ScratchSpace;
	BeamformerSharedMemoryLockKind upload_lock  = BeamformerSharedMemoryLockKind_UploadRF;

	u64 rf_block_rf_size;
	if (atomic_load_u32(sm->locks + upload_lock) &&
	    (rf_block_rf_size = atomic_swap_u64(&sm->rf_block_rf_size, 0)))
	{
		beamformer_shared_memory_take_lock(ctx->shared_memory, (i32)scratch_lock, (u32)-1);

		BeamformerRFBuffer *rf = ctx->rf_buffer;

		rf->active_rf_size = vk_round_up_to_sync_size(rf_block_rf_size & 0xFFFFFFFFULL, 64);
		if unlikely(rf->buffer.size < countof(rf->upload_complete_values) * rf->active_rf_size) {
			GPUBufferAllocateInfo allocate_info = {
				.size  = countof(rf->upload_complete_values) * rf->active_rf_size,
				.flags = VulkanUsageFlag_HostReadWrite,
				.label = s8("RawRFBuffer"),
			};
			vk_buffer_allocate(&rf->buffer, &allocate_info);
		}

		u32 slot = rf->insertion_index % countof(rf->upload_complete_values);

		/* NOTE(rnp): don't overwrite slot if the compute thread hasn't processed it */
		spin_wait(atomic_load_u64(&rf->compute_index) < rf->upload_complete_values[slot]);
		vk_host_wait_timeline(VulkanTimeline_Compute, rf->compute_complete_values[slot], -1ULL);

		vk_buffer_range_upload(&rf->buffer, beamformer_shared_memory_scratch_arena(sm).beg,
		                       slot * rf->active_rf_size, rf->active_rf_size, 1);
		store_fence();

		beamformer_shared_memory_release_lock(ctx->shared_memory, (i32)scratch_lock);
		post_sync_barrier(ctx->shared_memory, upload_lock);

		rf->insertion_index++;
		atomic_store_u64(rf->upload_complete_values + slot, vk_host_signal_timeline(VulkanTimeline_Transfer));

		os_wake_all_waiters(ctx->compute_worker_sync);

		u64 current_time = os_timer_count();
		push_compute_timing_info(ctx->compute_timing_table, (ComputeTimingInfo){
			.kind        = ComputeTimingInfoKind_RF_Data,
			.timer_count = current_time - rf->timestamp,
		});
		rf->timestamp = current_time;
	}
}

function void
beamformer_queue_compute(BeamformerCtx *ctx, BeamformerFrame *frame, u32 parameter_block)
{
	BeamformerSharedMemory *sm = ctx->shared_memory;
	BeamformerSharedMemoryLockKind dispatch_lock = BeamformerSharedMemoryLockKind_DispatchCompute;
	if (!sm->live_imaging_parameters.active && beamformer_shared_memory_take_lock(sm, (i32)dispatch_lock, 0))
	{
		BeamformWork *work = beamform_work_queue_push(ctx->beamform_work_queue);
		BeamformerViewPlaneTag tag = frame ? frame->view_plane_tag : 0;
		if (fill_frame_compute_work(ctx, work, tag, parameter_block, 0))
			beamform_work_queue_push_commit(ctx->beamform_work_queue);
	}
	os_wake_all_waiters(&ctx->compute_worker.sync_variable);
}

#include "ui.c"

function void
beamformer_process_input_events(BeamformerCtx *ctx, BeamformerInput *input,
                                BeamformerInputEvent *events, u32 event_count)
{
	for (u32 index = 0; index < event_count; index++) {
		BeamformerInputEvent *event = events + index;
		switch (event->kind) {

		case BeamformerInputEventKind_ExecutableReload:{
			ui_init(ctx, ctx->ui_backing_store);

			if (!vk_pipeline_valid(ctx->compute_context.compute_internal_pipelines[0])) {
				for EachElement(ctx->compute_context.compute_internal_pipelines, it) {
					beamformer_reload_compute_pipeline(ctx->compute_context.compute_internal_pipelines + it,
					                                   BeamformerShaderKind_ComputeInternalFirst + it, 0,
					                                   ctx->arena);
				}
			}

			#if BEAMFORMER_RENDERDOC_HOOKS
			start_frame_capture       = input->renderdoc_start_frame_capture;
			end_frame_capture         = input->renderdoc_end_frame_capture;
			set_capture_path_template = input->renderdoc_set_capture_file_path_template;
			#endif
		}break;

		case BeamformerInputEventKind_FileEvent:{
			BeamformerFileReloadContext *frc = event->file_watch_user_context;
			switch (frc->kind) {
			case BeamformerFileReloadKind_ComputeInternalShader:{
				// TODO(rnp): this could stall, better to push it onto compute once queue is better
				beamformer_reload_compute_pipeline(frc->shader_reload.pipeline, frc->shader_reload.shader, 0, ctx->arena);
			}break;

			case BeamformerFileReloadKind_ComputeShader:{
				BeamformerSharedMemory *sm = ctx->shared_memory;
				u32 reserved_blocks = sm->reserved_parameter_blocks;

				for (u32 block = 0; block < reserved_blocks; block++) {
					BeamformerComputePlan *cp = beamformer_compute_plan_for_block(&ctx->compute_context, block, 0);
					for (u32 slot = 0; slot < cp->pipeline.shader_count; slot++) {
						i32 shader_index = beamformer_shader_reloadable_index_by_shader[cp->pipeline.shaders[slot]];
						if (beamformer_reloadable_shader_kinds[shader_index] == frc->shader_reload.shader)
							atomic_or_u32(&cp->dirty_programs, 1 << slot);
					}
				}

				// TODO(rnp): track latest parameter block
				if (ctx->latest_frame)
					beamformer_queue_compute(ctx, ctx->latest_frame, 0);
			}break;

			case BeamformerFileReloadKind_RenderShader:{
				beamformer_reload_render_pipeline(frc->shader_reload.pipeline, frc->shader_reload.shader, ctx->arena);
				ctx->render_shader_updated = 1;
			}break;

			InvalidDefaultCase;
			}
		}break;

		InvalidDefaultCase;
		}
	}
}

BEAMFORMER_EXPORT void
beamformer_frame_step(BeamformerInput *input)
{
	BeamformerCtx *ctx = BeamformerContextMemory(input->memory);

	u64 current_time = os_timer_count();
	dt_for_frame = (f64)(current_time - ctx->frame_timestamp) / os_system_info()->timer_frequency;
	ctx->frame_timestamp = current_time;

	if (IsWindowResized()) {
		ctx->window_size.h = GetScreenHeight();
		ctx->window_size.w = GetScreenWidth();
	}

	coalesce_timing_table(ctx->compute_timing_table, ctx->compute_shader_stats);

	beamformer_process_input_events(ctx, input, input->event_queue, input->event_count);

	BeamformerSharedMemory *sm = ctx->shared_memory;
	if (atomic_load_u32(sm->locks + BeamformerSharedMemoryLockKind_UploadRF))
		os_wake_all_waiters(&ctx->upload_worker.sync_variable);
	if (atomic_load_u32(sm->locks + BeamformerSharedMemoryLockKind_DispatchCompute))
		os_wake_all_waiters(&ctx->compute_worker.sync_variable);

	BeamformerFrame        *frame = ctx->latest_frame;
	BeamformerViewPlaneTag  tag   = frame? frame->view_plane_tag : 0;
	draw_ui(ctx, input, frame, tag);

	ctx->render_shader_updated = 0;
}
