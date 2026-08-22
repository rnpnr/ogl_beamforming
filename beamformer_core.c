/* See LICENSE for license details. */
/* TODO(rnp):
 * [ ]: backtrace dumping on SIGSEGV
 * [ ]: cooperative shared memory loading in decode shader
 * [ ]: refactor: save filter parameters with rest of parameters, whole slot thing is dumb
 * [ ]: upload previously exported data for display. maybe this is a UI thing but doing it
 *      programatically would be nice.
 * [ ]: Add interface for multi frame upload. RF upload already uses an offset into SM so
 *      that part works fine. We just need a way of specify a multi frame upload. (Data must
 *      be organized for simple offset access per frame).
 * [ ]: refactor: do_compute should build its own "command graph" which tracks
 *      dependencies better. It is very important that unnecessary barriers are
 *      not placed between compute stages which requires knowledge of the entire
 *      graph.
 * [ ]: refactor: replace UploadRF with just the scratch_rf_size variable,
 *      use below to spin wait in library
 * [ ]: utilize umonitor/umwait (intel), monitorx/mwaitx (amd), and wfe/sev (aarch64)
 *      for power efficient low latency waiting
 * [ ]: BeamformWorkQueue -> BeamformerWorkQueue
 * [ ]: refactor: work queue needs a cleanup, we should only have a single one
 *      - that queue isn't really considered hot so a lock is probably fine
 * [ ]: bug: reinit cuda on hot-reload
 *
 * [ ]: export for special frames/data
 *    - Color Map Data
 *    - Recursive Imaging Result
 *    - Incoherent sum
 *
 * [ ]: Tiled Array Handling
 *    [ ]: add tile count uv2
 *    [ ]: make xdc_transform an array
 *    [ ]: modify CPU DAS dispatch code to lookup xdc_transform based on tile index
 *    [ ]: modify CPU DAS dispatch code to set rf_element_offset based on tile index
 *       - need to check if this works for HERCULES or if the shader just needs to
 *         know about the extra tiles
 *    [ ]: write simple backpropagation code to optimize 2D tile position and 2D tilt
 *         (4 variables per tile).
 *       - two tests:
 *         1. Maximize value of wire target
 *         2. Maximize value of single cyst contrast
 *
 * [ ]: Need somewhere to store display image temporaries
 *    - Coherency Weighting needs somewhere to put its incoherent sum
 *    - Image Averaging needs to put its output somewhere
 *    - Recursive imaging needs to be able to sum/subtract to update on each recursion step
 *    - Power doppler needs somewhere to store current power map
 *    - We don't need a backlog of these but it may be useful to have a front and back buffer
 *      so that the UI always sees consistent state
 *    - Really what we want is a separate temp (GPU) arena that gets recreated on
 *      pipeline creation (in plan_compute_pipeline()).
 *    - NOTE: image offsets/etc needed below don't have a predefined sizes so they would
 *      also be a candidates for storage in GPU temp arena.
 *
 * [ ]: Recursive Imaging Handling
 *    [ ]: add array of image offsets to Compute Array Parameters
 *    [ ]: add array of weighting coeffiecents to Compute Array Parameters
 *    [ ]: Update 2D sum shader to use these
 *
 * [ ]: Power Doppler
 *    [ ]: also needs array of image offsets
 *    [ ]: make modified filter that grabs each sample from a different image offset
 *       - NOTE: this can't use existing striding mechanism because one image may
 *               be at the start of the ring buffer and another at the end of the
 *               ring buffer and the image size may not cleanly divide the ring buffer size.
 *       - Probably want this shader to just output the estimated power map directly.
 *    [ ]: make a modified render shader that takes two images: one structural and one that
 *         is used to index into a color map. (or second render pass that applies color overlay)
 */

#include "base_platform.h"

#if defined(BEAMFORMER_DEBUG) && !defined(BEAMFORMER_EXPORT) && OS_WINDOWS
  #define BEAMFORMER_EXPORT __declspec(dllexport)
#endif

#include "beamformer_internal.h"

typedef struct BeamformerComputeGraphNode BeamformerComputeGraphNode;
struct BeamformerComputeGraphNode {
	// NOTE(rnp): will be BeamformerShaderKind_Count for root node
	BeamformerShaderKind kind;

	// NOTE(rnp): when any of input or output stride is assigned it is assumed that
	// the shader requires a fixed layout for input, output, or both. When two adjacent
	// nodes require incompatible layouts the second pass over the graph will insert
	// Reshape shaders in between.
	BeamformerDataKind input_data_kind;
	iv3                input_stride;

	BeamformerDataKind output_data_kind;
	iv3                output_stride;

	i32                user_pipeline_index;

	BeamformerComputeGraphNode *prev;
	BeamformerComputeGraphNode *next;
};

typedef struct {
	BeamformerComputeGraphNode *first;
	BeamformerComputeGraphNode *last;
	u64                         count;
} BeamformerComputeGraph;

#define GPU_RESOURCE_HASH_TABLE_COUNT 256
typedef struct U64ReferenceNode U64ReferenceNode;
struct U64ReferenceNode {u64 *v; U64ReferenceNode *next;};

typedef struct GPUResource GPUResource;
struct GPUResource {
	str8 name;
	u64  size;
	u64  offset;
	u64  alignment;

	void *data;

	u64  hash;

	U64ReferenceNode *pointer_store_list;

	GPUResource *next;
	GPUResource *hash_next, *hash_prev;
};
typedef struct {GPUResource *first, *last;} GPUResourceHashBucket;

typedef struct {
	Arena *arena;
	u64    position;

	GPUResource *resource_list;
	GPUResourceHashBucket hash_table[GPU_RESOURCE_HASH_TABLE_COUNT];
} GPUResourceBuilder;

read_only global BeamformerFrame       beamformer_nil_frame;
read_only global BeamformerComputePlan beamformer_nil_compute_plan;

global BeamformerCtx   *beamformer_context;
global BeamformerInput *beamformer_input;
global f32 dt_for_frame;

#define beamformer_frame_arena() (beamformer_context->frame_arenas[beamformer_context->frame_index % countof(beamformer_context->frame_arenas)])
#define beamformer_registers() (&beamformer_context->registers->v)
#define beamformer_push_registers(...) beamformer_push_registers_(&(BeamformerRegisters){beamformer_registers_init_literal __VA_ARGS__})
#define BeamformerRegistersScope(...) DeferLoop(beamformer_push_registers(__VA_ARGS__), beamformer_pop_registers())
#define beamformer_command(name, ...) beamformer_push_command(name, &(BeamformerRegisters){beamformer_registers_init_literal __VA_ARGS__})

function BeamformerRegisters *
beamformer_pop_registers(void)
{
	BeamformerRegisters *result = &beamformer_context->registers->v;
	SLLStackPop(beamformer_context->registers, next);
	if (beamformer_context->registers == 0)
		beamformer_context->registers = &beamformer_context->base_registers;
	return result;
}

function BeamformerRegisters *
beamformer_push_registers_(BeamformerRegisters *registers)
{
	BeamformerRegistersNode *node   = push_struct(beamformer_frame_arena(), BeamformerRegistersNode);
	BeamformerRegisters     *result = &node->v;
	memory_copy(result, registers, sizeof(node->v));
	SLLStackPush(beamformer_context->registers, node, next);
	return result;
}

function void
beamformer_command_list_push_new(Arena *arena, BeamformerCommandList *commands, str8 name, BeamformerRegisters *registers)
{
	BeamformerCommandNode *node = push_struct(arena, BeamformerCommandNode);
	node->command.registers = push_struct_no_zero(arena, BeamformerRegisters);
	node->command.name      = push_str8(arena, name);
	memory_copy(node->command.registers, registers, sizeof(*registers));
	DLLInsertLast(0, commands->first, commands->last, node, next, prev);
	commands->count += 1;
}

function void
beamformer_push_command(str8 name, BeamformerRegisters *registers)
{
	beamformer_command_list_push_new(beamformer_frame_arena(), beamformer_context->command_queues + 0,
	                                 name, registers);
}

function BeamformerCommandKind
beamformer_command_kind_from_string(str8 s)
{
	BeamformerCommandKind result = BeamformerCommandKind_Nil;
	for EachElement(beamformer_command_infos, it) {
		if (str8_equal(beamformer_command_infos[it].string, s)) {
			result = (BeamformerCommandKind)it;
			break;
		}
	}
	return result;
}

function BeamformerPanelKind
beamformer_panel_kind_from_string(str8 s)
{
	BeamformerPanelKind result = BeamformerPanelKind_Nil;
	for EachElement(beamformer_panel_infos, it) {
		if (str8_equal(beamformer_panel_infos[it].string, s)) {
			result = (BeamformerPanelKind)it;
			break;
		}
	}
	return result;
}

function BeamformerFrame *
beamformer_frame_from_index(u64 index)
{
	BeamformerFrame *result = &beamformer_nil_frame;
	if (index < countof(beamformer_context->compute_context.backlog.frames)) {
		BeamformerFrame *frame = beamformer_context->compute_context.backlog.frames + index;
		if (frame->timeline_valid_value != 0)
			result = frame;
	}
	return result;
}

function b32
beamformer_frame_valid(u64 index)
{
	b32 result = beamformer_frame_from_index(index) != &beamformer_nil_frame;
	return result;
}

function void
beamformer_compute_plan_release(BeamformerComputeContext *cc, u32 block)
{
	assert(block < countof(cc->compute_plans));
	BeamformerComputePlan *cp = cc->compute_plans[block];
	if (cp) {
		gpu_buffer_release(&cp->array_parameters);
		gpu_buffer_release(&cp->gpu_temp_arena);
		cc->compute_plans[block] = 0;
		SLLPushFreelist(cp, cc->compute_plan_freelist);
	}
}

function GPUResource *
gpu_resource_from_hash(GPUResourceBuilder *rb, u64 hash)
{
	GPUResource *result = 0;

	GPUResourceHashBucket *hb = rb->hash_table + (hash % GPU_RESOURCE_HASH_TABLE_COUNT);
	for (GPUResource *r = hb->first; r; r = r->hash_next) {
		if (hash == r->hash) {
			result = r;
			break;
		}
	}

	return result;
}

typedef struct {
	str8  name;
	u64   align;
	u64   size;
	u64  *store;
	void *data;
} GPUResourcePushInfo;
#define gpu_resource_push(rb, t, count, ...) gpu_resource_push_(rb, (GPUResourcePushInfo){\
	.align = Max(alignof(t), 16), \
	.size  = sizeof(t) * count, \
	__VA_ARGS__})

function void
gpu_resource_push_(GPUResourceBuilder *rb, GPUResourcePushInfo info)
{
	assert(info.store && info.size > 0 && info.name.length > 0 && IsPowerOfTwo(info.align));

	u64 hash = u64_hash_from_str8(info.name);
	GPUResource *r = gpu_resource_from_hash(rb, hash);
	if (!r) {
		r = push_struct(rb->arena, GPUResource);
		GPUResourceHashBucket *hb = rb->hash_table + (hash % GPU_RESOURCE_HASH_TABLE_COUNT);
		DLLInsert(0, hb->first, hb->last, r, hash_next, hash_prev);
		SLLStackPush(rb->resource_list, r, next);
	}

	r->hash      = hash;
	r->name      = info.name;
	r->alignment = Max(16, info.align);
	r->offset    = AlignUpPowerOfTwo(rb->position, r->alignment);
	r->size      = info.size;

	// NOTE(rnp): if this is a new resource and no data is provided it is likely
	// a temporary GPU side buffer. if this is not a new resource and no data
	// is provided then maybe it is shared and someone else already provided it
	if (info.data) r->data = info.data;

	U64ReferenceNode *output = push_struct(rb->arena, U64ReferenceNode);
	output->v = info.store;
	SLLStackPush(r->pointer_store_list, output, next);

	rb->position = r->offset + r->size;
}

function GPUResourceBuilder *
gpu_resource_build_begin(Arena *arena)
{
	GPUResourceBuilder *result = push_struct(arena, GPUResourceBuilder);
	result->arena = arena;
	return result;
}

function void
gpu_resource_build_end(GPUResourceBuilder *rb, GPUBuffer *buffer)
{
	u64 size = gpu_round_up_to_sync_size(rb->position, 64);
	if (size != (u64)buffer->size) {
		gpu_buffer_allocate(buffer, (GPUBufferAllocateInfo){
			.size  = size,
			.flags = VulkanUsageFlag_HostReadWrite|VulkanUsageFlag_TransferDestination,
			.label = push_str8_f(rb->arena, "GPU Temp Arena [%p]", buffer),
		});
	}

	//////////////////////////////////////
	// NOTE(rnp): fill in pointer outputs
	for (GPUResource *r = rb->resource_list; r; r = r->next)
		for (U64ReferenceNode *op = r->pointer_store_list; op; op = op->next)
			*op->v = buffer->gpu_pointer + r->offset;

	//////////////////////////////////////
	// NOTE(rnp): upload data
	for (GPUResource *r = rb->resource_list; r; r = r->next)
		if (r->data)
			gpu_buffer_range_upload(buffer, r->data, r->offset, r->size, 0);
}

function BeamformerComputePlan *
beamformer_compute_plan_for_block(BeamformerComputeContext *cc, u32 block, Arena *arena)
{
	assert(block < countof(cc->compute_plans));
	BeamformerComputePlan *result = cc->compute_plans[block];
	if (!result) {
		result = SLLPopFreelist(cc->compute_plan_freelist);
		if (!result) result = push_struct_no_zero(arena, BeamformerComputePlan);
		zero_struct(result);
		cc->compute_plans[block] = result;

		result->ui_voxel_transform = m4_identity();

		Stream label = arena_stream(arena);
		stream_append_str8(&label, str8("ComputeParameterArray["));
		stream_append_u64(&label, block);
		stream_append_str8(&label, str8("]"));

		GPUBufferAllocateInfo allocate_info = {
			.size  = sizeof(BeamformerComputeArrayParameters),
			.flags = VulkanUsageFlag_HostReadWrite,
			.label = stream_to_str8(&label),
		};
		gpu_buffer_allocate(&result->array_parameters, allocate_info);
		assert((result->array_parameters.gpu_pointer & 63) == 0);
	}
	return result;
}

function BeamformerFilter *
beamformer_filter_create(Arena *arena, BeamformerFilterParameters fp)
{
	BeamformerFilter *result = push_struct(arena, BeamformerFilter);
	switch (fp.kind) {
	case BeamformerFilterKind_Kaiser:{
		/* TODO(rnp): this should also support complex */
		/* TODO(rnp): implement this as an IFIR filter instead to reduce computation */
		result->data = kaiser_low_pass_filter(arena, fp.kaiser.cutoff_frequency, fp.sampling_frequency,
		                                      fp.kaiser.beta, (i32)fp.kaiser.length);
		result->length     = (i32)fp.kaiser.length;
		result->time_delay = (f32)result->length / 2.0f / fp.sampling_frequency;
	}break;

	case BeamformerFilterKind_MatchedChirp:{
		typeof(fp.matched_chirp) *mc = &fp.matched_chirp;
		f32 fs = fp.sampling_frequency;
		result->length = (i32)(mc->duration * fs);
		if (fp.complex) {
			result->data = baseband_chirp(arena, mc->min_frequency, mc->max_frequency, fs, result->length, 1, 0.5f);
			result->time_delay = complex_filter_first_moment(result->data, result->length, fs);
		} else {
			result->data = rf_chirp(arena, mc->min_frequency, mc->max_frequency, fs, result->length, 1);
			result->time_delay = real_filter_first_moment(result->data, result->length, fs);
		}
	}break;

	InvalidDefaultCase;
	}

	result->parameters = fp;
	return result;
}

function iv3
das_valid_points(iv3 points)
{
	iv3 result;
	result.x = Max(points.x, 1);
	result.y = Max(points.y, 1);
	result.z = Max(points.z, 1);
	return result;
}

function GPUBuffer *
beamformer_gpu_buffer_from_frame(BeamformerFrame *frame)
{
	GPUBuffer *result = beamformer_context->compute_context.backlog.buffer;
	if (!Between(frame->gpu_pointer, result->gpu_pointer, result->gpu_pointer + result->size)) {
		result = 0;
		BeamformerComputePlan *cp = beamformer_context->compute_context.compute_plans[frame->parameter_block];
		if (cp) {
			result = &cp->gpu_temp_arena;
			assert(Between(frame->gpu_pointer, result->gpu_pointer, result->gpu_pointer + result->size));
		}
	}
	return result;
}

function u64
beamformer_frame_byte_size(iv3 points, BeamformerDataKind kind)
{
	u64 result = points.x * points.y * points.z * beamformer_data_kind_byte_size[kind];
	result = round_up_to(result, 64);
	return result;
}

function u64
beamformer_incoherent_frame_byte_size(iv3 points, BeamformerDataKind kind)
{
	u64 result = beamformer_frame_byte_size(points, kind) / beamformer_data_kind_element_count[kind];
	return result;
}

function BeamformerFrame *
beamformer_frame_next(BeamformerComputeContext *cc, iv3 output_points, b32 complex)
{
	BeamformerFrameBacklog *bl = &cc->backlog;

	BeamformerDataKind kind = complex ? BeamformerDataKind_Float32Complex : BeamformerDataKind_Float32;
	u64 frame_size = beamformer_frame_byte_size(output_points, kind);

	// TODO(rnp): handle this somewhat gracefully (even it produces garbled output)
	assert(frame_size <= (u64)bl->buffer->size);

	if (bl->next_offset > (u64)bl->buffer->size - frame_size)
		bl->next_offset = 0;

	u64 id = bl->counter++;

	BeamformerFrame *result = bl->frames + (id % countof(bl->frames));
	atomic_store_u64(&result->timeline_valid_value, -1ULL);
	result->id            = id & U32_MAX;
	result->gpu_pointer   = bl->buffer->gpu_pointer + bl->next_offset;
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

function uv3
layout_for_output(iv3 points)
{
	uv3 result = {{1, 1, 1}};

	b32 has_x = points.x > 1;
	b32 has_y = points.y > 1;
	b32 has_z = points.z > 1;

	u32 subgroup_size  = gpu_info()->subgroup_size;
	u32 grid_3d_z_size = Max(1, subgroup_size / (4 * 4));
	u32 grid_2d_y_size = Max(1, subgroup_size / 8);

	switch (iv3_dimension(points)) {
	case 1:{
		if (has_x) result.x = subgroup_size;
		if (has_y) result.y = subgroup_size;
		if (has_z) result.z = subgroup_size;
	}break;

	case 2:{
		if (has_x && has_y) {result.x = 8; result.y = grid_2d_y_size;}
		if (has_x && has_z) {result.x = 8; result.z = grid_2d_y_size;}
		if (has_y && has_z) {result.y = 8; result.z = grid_2d_y_size;}
	}break;

	case 3:{result = (uv3){{4, 4, grid_3d_z_size}};}break;

	InvalidDefaultCase;
	}

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

function b32
compute_plan_push_shader(BeamformerComputePlan *p, BeamformerComputeGraphNode *node, BeamformerShaderParameters *sp)
{
	b32 result = 0;
	if (p->pipeline.shader_count < countof(p->pipeline.shaders)) {
		u32 index = p->pipeline.shader_count++;
		p->pipeline.shaders[index]    = node->kind;
		zero_struct(p->shader_descriptors + index);
		p->pipeline.parameters[index] = sp ? *sp : (BeamformerShaderParameters){0};

		p->shader_descriptors[index].input_data_kind  = node->input_data_kind;
		p->shader_descriptors[index].output_data_kind = node->output_data_kind;

		result = 1;
	}
	return result;
}

function BeamformerComputeGraphNode *
push_compute_graph_node(BeamformerComputeGraph *graph, BeamformerShaderKind kind, Arena *arena)
{
	BeamformerComputeGraphNode *result = push_struct(arena, BeamformerComputeGraphNode);
	if (graph) {
		DLLInsertLast(0, graph->first, graph->last, result, next, prev);
		graph->count++;
	}
	result->kind = kind;
	result->user_pipeline_index = -1;
	// NOTE(rnp): initially don't care data kind
	result->input_data_kind  = BeamformerDataKind_Count;
	result->output_data_kind = BeamformerDataKind_Count;
	return result;
}

function void
plan_compute_pipeline(BeamformerComputePlan *cp, BeamformerParameterBlock *pb, Arena *scratch)
{
	b32 run_hilbert = 0;
	b32 demodulate  = 0;

	for (u32 i = 0; i < pb->pipeline.shader_count; i++) {
		switch (pb->pipeline.shaders[i]) {
		case BeamformerShaderKind_Hilbert:{run_hilbert = 1;}break;
		case BeamformerShaderKind_Demodulate:{demodulate = 1;}break;
		default:{}break;
		}
	}

	if (demodulate) run_hilbert = 0;

	f32 sampling_frequency = pb->parameters.sampling_frequency;
	u32 input_sample_count = pb->parameters.sample_count;
	u32 acquisition_count  = pb->parameters.acquisition_count;
	u32 decimation_rate    = Max(pb->parameters.decimation_rate, 1);

	cp->raw_channel_byte_stride = pb->parameters.sample_count * pb->parameters.acquisition_count
	                              * beamformer_data_kind_byte_size[pb->pipeline.data_kind];

	BeamformerDataKind input_data_kind = pb->pipeline.data_kind;
	if (demodulate) {
		switch (input_data_kind) {
		case BeamformerDataKind_Int16:{  input_data_kind = BeamformerDataKind_Int16Complex;  }break;
		case BeamformerDataKind_Float16:{input_data_kind = BeamformerDataKind_Float16Complex;}break;
		case BeamformerDataKind_Float32:{input_data_kind = BeamformerDataKind_Float32Complex;}break;
		default:{}break;
		}
		input_sample_count /= (2 * decimation_rate);
		sampling_frequency /= (2 * decimation_rate);
	}

	cp->iq_pipeline = beamformer_data_kind_complex[input_data_kind] || run_hilbert;

	BeamformerDataKind das_data_kind = cp->iq_pipeline ? BeamformerDataKind_Float32Complex
	                                                   : BeamformerDataKind_Float32;

	cp->channel_count = pb->parameters.channel_count;
	u32 chunk_channel_count = Min(cp->channel_count, BeamformerChunkChannelCount);

	cp->rf_size = input_sample_count * pb->parameters.acquisition_count * chunk_channel_count
	              * beamformer_data_kind_byte_size[das_data_kind];

	read_only local_persist BeamformerDataKind data_kind_to_element_kind[] = {
		[BeamformerDataKind_Int16]          = BeamformerDataKind_Float16,
		[BeamformerDataKind_Float16]        = BeamformerDataKind_Float16,
		[BeamformerDataKind_Float32]        = BeamformerDataKind_Float32,
		[BeamformerDataKind_Int16Complex]   = BeamformerDataKind_Float16,
		[BeamformerDataKind_Float16Complex] = BeamformerDataKind_Float16,
		[BeamformerDataKind_Float32Complex] = BeamformerDataKind_Float32,
	};

	//////////////////////////////////////
	// NOTE(rnp): First Pass: build initial graph and insert hard layout constraints
	BeamformerComputeGraph graph = {0};
	BeamformerComputeGraphNode *root_node = push_compute_graph_node(&graph, BeamformerShaderKind_Count, scratch);
	root_node->input_data_kind  = input_data_kind;
	root_node->input_stride.x   = 1;                                               // Sample Stride
	root_node->input_stride.y   = pb->parameters.sample_count * acquisition_count; // Channel Stride
	root_node->input_stride.z   = pb->parameters.sample_count;                     // Receive Event Stride
	root_node->output_data_kind = input_data_kind;
	root_node->output_stride.x  = 1;                                               // Sample Stride
	root_node->output_stride.y  = pb->parameters.sample_count * acquisition_count; // Channel Stride
	root_node->output_stride.z  = pb->parameters.sample_count;                     // Receive Event Stride

	for EachIndex(pb->pipeline.shader_count, it) {
		// NOTE(rnp): skip unnecessary shaders
		switch (pb->pipeline.shaders[it]) {
		case BeamformerShaderKind_Hilbert:{if (!run_hilbert) continue;}break;

		case BeamformerShaderKind_Decode:{
			if (pb->parameters.decode_mode == BeamformerDecodeMode_None)
				continue;
		}break;

		case BeamformerShaderKind_Sum:
		case BeamformerShaderKind_MinMax:
		{
			// NOTE(rnp): currently unsupported
			continue;
		}break;

		default:{}break;
		}

		BeamformerComputeGraphNode *node = push_compute_graph_node(&graph, pb->pipeline.shaders[it], scratch);
		node->user_pipeline_index = (i32)it;
		switch (pb->pipeline.shaders[it]) {
		case BeamformerShaderKind_Decode:{
			b32 low_precision   = beamformer_data_kind_element_size[input_data_kind] < 4;
			b32 use_coop_matrix = gpu_info()->cooperative_matrix &&
			                      low_precision &&
			                      (acquisition_count   % 16 == 0) &&
			                      (chunk_channel_count % 16 == 0);

			// NOTE(rnp): fixed input layout required for reasonable performance
			if (low_precision && beamformer_data_kind_complex[input_data_kind])
				node->input_data_kind = BeamformerDataKind_Float16Complex;
			node->input_stride.x = chunk_channel_count * acquisition_count;
			node->input_stride.y = acquisition_count;
			node->input_stride.z = 1;

			if (use_coop_matrix) {
				node->input_data_kind  = BeamformerDataKind_Float16;
				node->output_data_kind = data_kind_to_element_kind[das_data_kind];
				node->output_stride    = node->input_stride;
			}
		}break;

		case BeamformerShaderKind_DAS:{
			node->input_data_kind  = das_data_kind;
			node->input_stride.x   = 1;                                      // Sample Stride
			node->input_stride.y   = input_sample_count * acquisition_count; // Channel Stride
			node->input_stride.z   = input_sample_count;                     // Receive Event Stride
			node->output_stride.x  = 1;
			node->output_stride.y  = cp->output_points.x;
			node->output_stride.z  = cp->output_points.x * cp->output_points.y;
			node->output_data_kind = das_data_kind;

			// NOTE(rnp): insert implicit CoherencyWeighting node
			if (pb->parameters.coherency_weighting)
				node = push_compute_graph_node(&graph, BeamformerShaderKind_CoherencyWeighting, scratch);
		}break;

		default:{}break;
		}
	}

	//////////////////////////////////////
	// NOTE(rnp): Second Pass: resolve layout constraints
	for (BeamformerComputeGraphNode *node = root_node->next; node; node = node->next) {
		b32 needs_reshape = 0;

		// NOTE(rnp): data strides
		{
			b32 input_dont_care       = bv3_any(iv3_equal(node->input_stride, (iv3){0}));
			b32 prev_output_dont_care = bv3_any(iv3_equal(node->prev->output_stride, (iv3){0}));

			if (prev_output_dont_care && !input_dont_care)
				node->prev->output_stride = node->input_stride;

			if (!prev_output_dont_care && input_dont_care)
				node->input_stride = node->prev->output_stride;

			if (prev_output_dont_care && input_dont_care)
				node->input_stride = node->prev->output_stride = node->prev->input_stride;

			needs_reshape |= !bv3_all(iv3_equal(node->input_stride, node->prev->output_stride));
		}

		// NOTE(rnp): data kinds
		{
			b32 input_dont_care       = node->input_data_kind        == BeamformerDataKind_Count;
			b32 prev_output_dont_care = node->prev->output_data_kind == BeamformerDataKind_Count;

			if (prev_output_dont_care && !input_dont_care)
				node->prev->output_data_kind = node->input_data_kind;

			if (!prev_output_dont_care && input_dont_care)
				node->input_data_kind = node->prev->output_data_kind;

			if (prev_output_dont_care && input_dont_care)
				node->input_data_kind = node->prev->output_data_kind = node->prev->input_data_kind;

			needs_reshape |= node->input_data_kind != node->prev->output_data_kind;
		}

		// NOTE(rnp): insert reshape if needed
		if (needs_reshape) {
			BeamformerComputeGraphNode *new = push_compute_graph_node(0, BeamformerShaderKind_Reshape, scratch);
			BeamformerComputeGraphNode *last  = node->prev;
			DLLInsertLast(0, node, last, new, next, prev);
			graph.count++;
			new->input_data_kind  = new->prev->output_data_kind;
			new->input_stride     = new->prev->output_stride;
			new->output_data_kind = new->next->input_data_kind;
			new->output_stride    = new->next->input_stride;
		}
	}

	// NOTE(rnp): ensure last node descriptor gets proper values for output data kind
	if (graph.last->output_data_kind == BeamformerDataKind_Count)
		graph.last->output_data_kind = graph.last->input_data_kind;

	f32 time_offset   = pb->parameters.time_offset;
	u32 subgroup_size = gpu_info()->subgroup_size;

	cp->first_image_shader_index = 0;
	cp->pipeline.shader_count = 0;

	GPUResourceBuilder *resource_builder = gpu_resource_build_begin(scratch);
	for (BeamformerComputeGraphNode *node = root_node->next; node; node = node->next) {
		assert(node->prev->output_data_kind == node->input_data_kind);
		assert(bv3_all(iv3_equal(node->prev->output_stride, node->input_stride)));

		BeamformerShaderParameters *sp = 0;
		if (node->user_pipeline_index >= 0)
			sp = pb->pipeline.parameters + node->user_pipeline_index;

		if (compute_plan_push_shader(cp, node, sp)) {
			BeamformerShaderDescriptor *sd = cp->shader_descriptors + cp->pipeline.shader_count - 1;

			switch (node->kind) {
			case BeamformerShaderKind_Decode:{
				BeamformerDecodeBakeParameters *db = &sd->bake.Decode;

				u32 decode_sample_count = input_sample_count;
				db->DecodeMode    = pb->parameters.decode_mode;
				db->TransmitCount = pb->parameters.acquisition_count;
				db->ChunkChannelCount = chunk_channel_count;

				// NOTE(rnp): ignored when using coop matrices
				db->OutputSampleStride   = node->output_stride.x;
				db->OutputChannelStride  = node->output_stride.y;
				db->OutputTransmitStride = node->output_stride.z;

				db->ToProcess = 1;

				b32 use_coop_matrix = gpu_info()->cooperative_matrix &&
				                      node->input_data_kind == BeamformerDataKind_Float16 &&
				                      (db->TransmitCount % 16 == 0) &&
				                      (chunk_channel_count % 16 == 0);
				if (use_coop_matrix) {
					// TODO(rnp): shared memory for larger sizes
					sd->layout = (uv3){{subgroup_size, 1, 1}};

					if (demodulate)
						decode_sample_count *= 2;

					sd->compile_flags |= BeamformerDecodeCompileFlags_CooperativeMatrix;
					db->CooperativeMatrixM = 16;
					db->CooperativeMatrixN = 16;
					db->CooperativeMatrixK = 16;

					sd->dispatch.x = db->TransmitCount   / db->CooperativeMatrixN;
					sd->dispatch.y = chunk_channel_count / db->CooperativeMatrixM;
					sd->dispatch.z = decode_sample_count;
				} else if (db->TransmitCount > 40) {
					sd->compile_flags |= BeamformerDecodeCompileFlags_UseSharedMemory;

					if (db->TransmitCount == 48)
						db->ToProcess = db->TransmitCount / 16;

					b32 use_16x  = db->TransmitCount == 48 || db->TransmitCount == 80 ||
					               db->TransmitCount == 96 || db->TransmitCount == 160;
					sd->layout.x = use_16x ? 16 : 32;
					sd->layout.y = 4;
					sd->layout.z = 1;

					sd->dispatch.x = (u32)ceil_f32((f32)pb->parameters.acquisition_count / (f32)sd->layout.x / (f32)db->ToProcess);
					sd->dispatch.y = (u32)ceil_f32((f32)chunk_channel_count              / (f32)sd->layout.y);
					sd->dispatch.z = (u32)ceil_f32((f32)decode_sample_count              / (f32)sd->layout.z);
				} else {
					/* NOTE(rnp): register caching. using more threads will cause the compiler to do
					 * contortions to avoid spilling registers. using less gives higher performance */
					sd->layout = (uv3){{subgroup_size / 2, 1, 1}};

					sd->dispatch.x = (u32)ceil_f32((f32)decode_sample_count / (f32)sd->layout.x);
					sd->dispatch.y = (u32)ceil_f32((f32)chunk_channel_count / (f32)sd->layout.y);
					sd->dispatch.z = 1;
				}

				u32 order = pb->parameters.acquisition_count;
				gpu_resource_push(resource_builder, f16, order * order,
				                  .data  = make_hadamard_transpose(scratch, order, use_coop_matrix),
				                  .name  = str8("hadamard"),
				                  .store = &db->Hadamard);
			}break;

			case BeamformerShaderKind_Demodulate:
			case BeamformerShaderKind_Filter:
			{
				b32 demod = node->kind == BeamformerShaderKind_Demodulate;
				BeamformerFilter *f = beamformer_filter_create(scratch, cp->filter_parameters[sp->filter_slot]);

				sd->compile_flags |= BeamformerFilterCompileFlags_Demodulate * demod;
				sd->compile_flags |= BeamformerFilterCompileFlags_ComplexFilter * f->parameters.complex;

				time_offset += f->time_delay;

				BeamformerFilterBakeParameters *fb = &sd->bake.Filter;

				fb->FilterLength = (u32)f->length;
				gpu_resource_push(resource_builder, f32, f->length * (f->parameters.complex ? 2 : 1),
				                  .data  = f->data,
				                  .name  = push_str8_f(scratch, "filter_%u", sp->filter_slot),
				                  .store = &fb->FilterCoefficients);

				fb->SampleCount    = input_sample_count;
				fb->DecimationRate = demod ? decimation_rate : 1;

				b32 deinterleave =  beamformer_data_kind_complex[node->input_data_kind] &&
				                   !beamformer_data_kind_complex[node->output_data_kind];
				if (deinterleave)
					fb->BatchSampleCount = chunk_channel_count * input_sample_count * pb->parameters.acquisition_count;

				fb->OutputSampleStride   = node->output_stride.x;
				fb->OutputChannelStride  = node->output_stride.y;
				fb->OutputTransmitStride = node->output_stride.z;

				fb->InputSampleStride    = node->input_stride.x;
				fb->InputChannelStride   = node->input_stride.y;
				fb->InputTransmitStride  = node->input_stride.z;

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
					fb->DemodulationFrequency = pb->parameters.demodulation_frequency;
					fb->SamplingFrequency     = pb->parameters.sampling_frequency / 2;
				}

				sd->layout     = (uv3){{subgroup_size, 1, 1}};
				sd->dispatch.x = (u32)ceil_f32((f32)input_sample_count               / (f32)sd->layout.x);
				sd->dispatch.y = (u32)ceil_f32((f32)chunk_channel_count              / (f32)sd->layout.y);
				sd->dispatch.z = (u32)ceil_f32((f32)pb->parameters.acquisition_count / (f32)sd->layout.z);
			}break;

			case BeamformerShaderKind_DAS:{
				cp->first_image_shader_index = cp->pipeline.shader_count;

				BeamformerDASBakeParameters *db = &sd->bake.DAS;
				db->SamplingFrequency     = sampling_frequency;
				db->DemodulationFrequency = pb->parameters.demodulation_frequency;
				db->SpeedOfSound          = pb->parameters.speed_of_sound;
				db->TimeOffset            = time_offset;
				db->FNumber               = pb->parameters.f_number;
				db->AcquisitionKind       = pb->parameters.acquisition_kind;
				db->SampleCount           = input_sample_count;
				db->ChannelCount          = pb->parameters.channel_count;
				db->AcquisitionCount      = pb->parameters.acquisition_count;
				db->ChunkChannelCount     = chunk_channel_count;
				db->InterpolationMode     = pb->parameters.interpolation_mode;
				db->TransmitAngle         = pb->parameters.focal_vector.E[0];
				db->FocusDepth            = pb->parameters.focal_vector.E[1];
				db->ReadiGroupCount       = pb->parameters.readi_group_count;
				db->ArrayParameters       = cp->array_parameters.gpu_pointer;
				db->OutputSizeX           = cp->output_points.x;
				db->OutputSizeY           = cp->output_points.y;
				db->OutputSizeZ           = cp->output_points.z;
				db->TransmitReceiveOrientation = pb->parameters.transmit_receive_orientation;

				// NOTE(rnp): old gcc will miscompile an assignment
				memory_copy(cp->xdc_transform.E, pb->parameters.xdc_transform.E, sizeof(cp->xdc_transform));

				cp->voxel_transform   = m4_mul(cp->ui_voxel_transform, pb->parameters.das_voxel_transform);
				cp->xdc_element_pitch = pb->parameters.xdc_element_pitch;

				memory_copy(cp->das_voxel_transform.E, cp->voxel_transform.E, sizeof(cp->voxel_transform));

				u32 id = pb->parameters.acquisition_kind;
				if (id == BeamformerAcquisitionKind_UFORCES || id == BeamformerAcquisitionKind_FORCES)
					cp->das_voxel_transform = m4_mul(cp->xdc_transform, cp->das_voxel_transform);

				db->Sparse = id == BeamformerAcquisitionKind_UFORCES || id == BeamformerAcquisitionKind_UHERCULES;
				db->SingleFocus        = pb->parameters.single_focus;
				db->SingleOrientation  = pb->parameters.single_orientation;

				sd->compile_flags |= BeamformerDASCompileFlags_CoherencyWeighting * pb->parameters.coherency_weighting;
				sd->layout   = layout_for_output(cp->output_points);
				sd->dispatch = dispatch_for_output(sd->layout, cp->output_points);

				if (pb->parameters.coherency_weighting) {
					gpu_resource_push(resource_builder, u32, 0,
					                  .store = &db->IncoherentFrame,
					                  .size  = beamformer_incoherent_frame_byte_size(cp->output_points, das_data_kind),
					                  .name  = str8("incoherent_buffer"));
				}

				cp->readi_group = pb->parameters.readi_group;
				if (db->ReadiGroupCount > 1) {
					u32 order = db->ReadiGroupCount;
					gpu_resource_push(resource_builder, f16, order * order,
					                  .store = &db->Hadamard,
					                  .data  = make_hadamard_transpose(scratch, order, 0),
					                  .name  = str8("readi_hadamard"));
				}
			}break;

			case BeamformerShaderKind_CoherencyWeighting:{
				// NOTE(rnp): beamformed data is stored in linear order; making the layout 2D or 3D
				// here is just slower
				sd->layout   = (uv3){{subgroup_size, 1, 1}};
				sd->dispatch = dispatch_for_output(sd->layout, cp->output_points);

				BeamformerCoherencyWeightingBakeParameters *cw = &sd->bake.CoherencyWeighting;
				cw->Scale        = 1.f;
				cw->OutputVoxels = cp->output_points.x * cp->output_points.y * cp->output_points.z;
				gpu_resource_push(resource_builder, u32, 0,
				                  .store = &cw->IncoherentSum,
				                  .size  = beamformer_incoherent_frame_byte_size(cp->output_points, das_data_kind),
				                  .name  = str8("incoherent_buffer"));
			}break;

			case BeamformerShaderKind_Reshape:{
				BeamformerReshapeBakeParameters *rb = &sd->bake.Reshape;
				b32 deinterleave =  beamformer_data_kind_complex[node->input_data_kind] &&
				                   !beamformer_data_kind_complex[node->output_data_kind];
				b32 interleave   = !beamformer_data_kind_complex[node->input_data_kind] &&
				                    beamformer_data_kind_complex[node->output_data_kind];
				assert(interleave == 0 || (interleave != deinterleave));
				sd->compile_flags |= BeamformerReshapeCompileFlags_Deinterleave * deinterleave;
				sd->compile_flags |= BeamformerReshapeCompileFlags_Interleave   * interleave;

				rb->InputStrideX   = node->input_stride.x;
				rb->InputStrideY   = node->input_stride.y;
				rb->InputStrideZ   = node->input_stride.z;
				rb->OutputStrideX  = node->output_stride.x;
				rb->OutputStrideY  = node->output_stride.y;
				rb->OutputStrideZ  = node->output_stride.z;

				// NOTE(rnp): order doesn't really matter here but it must match the dispatch layout
				rb->SizeX          = input_sample_count;
				rb->SizeY          = chunk_channel_count;
				rb->SizeZ          = acquisition_count;

				sd->layout.x = 1;
				sd->layout.z = Min(subgroup_size, rb->SizeZ);
				sd->layout.y = subgroup_size / sd->layout.z;

				sd->dispatch.x = (u32)(ceil_f32((f32)rb->SizeX / sd->layout.x));
				sd->dispatch.y = (u32)(ceil_f32((f32)rb->SizeY / sd->layout.y));
				sd->dispatch.z = (u32)(ceil_f32((f32)rb->SizeZ / sd->layout.z));
			}break;

			default:{}break;

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

			}
		}
	}

	cp->pipeline.data_kind = input_data_kind;

	if (cp->first_image_shader_index == 0)
		cp->first_image_shader_index = cp->pipeline.shader_count;

	gpu_resource_build_end(resource_builder, &cp->gpu_temp_arena);
}

function void
stream_append_shader_header(Stream *s, i32 reloadable_index, BeamformerShaderDescriptor *sd, uv3 layout)
{
	stream_append_str8(s, str8("#version 460 core\n\n"
	"#extension GL_EXT_buffer_reference : require\n"
	"#extension GL_EXT_shader_16bit_storage : require\n"
	"#extension GL_EXT_shader_explicit_arithmetic_types : require\n\n"
	"#define f32     float32_t\n"
	"#define f16     float16_t\n"
	"#define s32     int32_t\n"
	"#define u64     uint64_t\n"
	"#define u32     uint32_t\n"
	"#define s16     int16_t\n"
	"#define u16     uint16_t\n"
	"#define u8      uint8_t\n"
	"#define s32vec2 i32vec2\n"
	"#define s16vec2 i16vec2\n"
	"\n"));

	i32  header_vector_length = beamformer_shader_header_vector_lengths[reloadable_index];
	i32 *header_vector        = beamformer_shader_header_vectors[reloadable_index];
	for (i32 index = 0; index < header_vector_length; index++)
		stream_append_str8(s, beamformer_shader_global_header_strings[header_vector[index]]);

	if (layout.x != 0) {
		stream_append_str8(s, str8("layout(local_size_x = "));
		stream_append_u64(s,  layout.x);
		stream_append_str8(s, str8(", local_size_y = "));
		stream_append_u64(s,  layout.y);
		stream_append_str8(s, str8(", local_size_z = "));
		stream_append_u64(s,  layout.z);
		stream_append_str8(s, str8(") in;\n\n"));
	}

	{
		u32 max_length = 0;
		for EachElement(beamformer_data_kind_str8, it)
			max_length = Max(max_length, (u32)beamformer_data_kind_str8[it].length);

		for EachElement(beamformer_data_kind_str8, it) {
			stream_append_str8s(s, str8("#define DataKind_"), beamformer_data_kind_str8[it]);
			stream_pad(s, ' ', max_length - beamformer_data_kind_str8[it].length + 1);
			stream_append_u64(s, it);
			stream_append_byte(s, '\n');
		}
		stream_append_byte(s, '\n');
	}

	if (sd) {
		BeamformerDataKind data_kinds[] = {sd->input_data_kind, sd->output_data_kind};
		str8 line_prefixes[] = {str8_comp("Input"), str8_comp("Output")};
		for EachElement(data_kinds, it) {
			if (data_kinds[it] != BeamformerDataKind_Count) {
				stream_append_str8s(s, str8("#define "), line_prefixes[it], str8("DataType "),
				                    beamformer_data_kind_glsl_type[data_kinds[it]],
				                    str8("\n#define "), line_prefixes[it], str8("DataKind DataKind_"),
				                    beamformer_data_kind_str8[data_kinds[it]],
				                    str8("\n#define "), line_prefixes[it], str8("DataKindByteSize "));
				stream_append_u64(s, beamformer_data_kind_byte_size[data_kinds[it]]);
				stream_append_byte(s, '\n');
			}
		}
		stream_append_byte(s, '\n');

		stream_append_str8(s, str8("#define CompileFlags (0x"));
		stream_append_hex_u64_width(s, sd->compile_flags, 8);
		stream_append_str8(s, str8(")\n"));

		i32 struct_id = beamformer_base_shader_to_bake_struct_id[reloadable_index];
		if (struct_id != -1) {
			str8             *names = meta_struct_member_names_by_id[struct_id];
			MetaStructInfo   *si    = meta_struct_info_by_id + struct_id;
			MetaStructMember *sm    = meta_struct_members_by_id[struct_id];
			for (u32 index = 0; index < si->member_count; index++) {
				str8 type = meta_kind_glsl_types[sm[index].type_id];
				stream_append_str8(s, str8("layout(constant_id = "));
				stream_append_u64(s, index);
				stream_append_str8s(s, str8(") const "), type, str8(" "), names[index], str8(" = "), type, str8("(1);\n"));
			}
		}
	}

	if (!renderdoc_attached())
		stream_append_str8(s, str8("\n\n#line 1\n"));
}

function void
beamformer_reload_pipeline(VulkanHandle *pipeline, BeamformerShaderReloadInfo *sris, u32 count, Arena *scratch)
{
	assume(count <= 2);
	str8 paths[2];
	VulkanPipelineCreateInfo infos[2];

	if (!BakeShaders) {
		for (u32 i = 0; i < count; i++)
			paths[i] = push_str8_from_parts(scratch, os_path_separator(), str8("shaders"), sris[i].filename_or_data);
	}

	u32 push_constants_size = 0;
	for (u32 i = 0; i < count; i++) {
		Stream shader_stream = arena_stream(scratch);
		i32 reloadable_index = beamformer_shader_reloadable_index_by_shader[sris[i].shader];
		if (i == 0) push_constants_size = beamformer_shader_push_constant_sizes[reloadable_index];
		else        assert(push_constants_size == beamformer_shader_push_constant_sizes[reloadable_index]);

		stream_append_shader_header(&shader_stream, reloadable_index, sris[i].shader_descriptor, sris[i].layout);

		str8 shader_text;
		if (BakeShaders) {
			stream_append_str8(&shader_stream, sris[i].filename_or_data);
			shader_text = arena_stream_commit_zero(scratch, &shader_stream);
		} else {
			str8 stream_data = arena_stream_commit(scratch, &shader_stream);
			str8 shader_data = os_read_entire_file(scratch, (c8 *)paths[i].data);
			// NOTE(rnp): kinda sucky but need to make sure these are a contiguous string
			shader_text = push_str8_from_parts(scratch, str8(""), stream_data, shader_data);
		}

		infos[i].kind = sris[i].shader_kind;
		infos[i].text = shader_text;
		infos[i].name = beamformer_shader_names[sris[i].shader];
		infos[i].specialization_data      = sris[i].shader_descriptor ? &sris[i].shader_descriptor->bake : 0;
		infos[i].specialization_struct_id = beamformer_base_shader_to_bake_struct_id[reloadable_index];

		//str8 line = str8("---------------\n");
		//str8 nl   = str8("\n");
		//os_console_log(line.data, line.length);
		//os_console_log(infos[i].name.data, infos[i].name.length);
		//os_console_log(nl.data, nl.length);
		//os_console_log(line.data, line.length);
		//os_console_log(infos[i].text.data, infos[i].text.length);
		//os_console_log(line.data, line.length);
	}

	vk_pipeline_release(*pipeline);
	*pipeline = vk_pipeline(infos, count, push_constants_size);
}

function void
beamformer_reload_render_pipeline(VulkanHandle *pipeline, BeamformerShaderKind shader, Arena *scratch)
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
	beamformer_reload_pipeline(pipeline, infos, countof(infos), scratch);
}

function void
beamformer_reload_compute_pipeline(VulkanHandle *pipeline, BeamformerShaderKind shader,
                                   BeamformerShaderDescriptor *shader_descriptor, Arena *scratch)
{
	i32 index  = beamformer_shader_reloadable_index_by_shader[shader];
	uv3 layout = shader_descriptor ? shader_descriptor->layout : (uv3){{gpu_info()->subgroup_size, 1, 1}};
	BeamformerShaderReloadInfo info = {
		.shader            = shader,
		.shader_kind       = VulkanShaderKind_Compute,
		.shader_descriptor = shader_descriptor,
		.filename_or_data  = BakeShaders ? beamformer_shader_data[index][0]
		                                 : beamformer_reloadable_shader_files[index][0],
		.layout            = layout,
	};
	beamformer_reload_pipeline(pipeline, &info, 1, scratch);
}

function void
beamformer_commit_parameter_block(BeamformerCtx *ctx, BeamformerComputePlan *cp, u32 block, Arena *scratch)
{
	BeamformerParameterBlock *pb;
	DeferLoop(pb = beamformer_parameter_block_lock(ctx->shared_memory, block, -1),
	          beamformer_parameter_block_unlock(ctx->shared_memory, block))
	for EachBit(pb->region_update_flags, region)
	{
		pb->region_update_flags &= ~(1ul << region);
		switch (region) {
		case BeamformerParameterRegionFlag_NotifyUI:{
			atomic_store_u32(&ctx->ui_dirty_parameter_blocks, 1u << block);
		}break;

		case BeamformerParameterRegionFlag_ComputePipeline:
		case BeamformerParameterRegionFlag_Parameters:
		{
			cp->output_points  = das_valid_points(pb->parameters.output_points.xyz);
			cp->average_frames = pb->parameters.output_points.E[3];

			plan_compute_pipeline(cp, pb, scratch);

			/* NOTE(rnp): these are both handled by plan_compute_pipeline() */
			u32 mask = 1 << BeamformerParameterBlockRegion_ComputePipeline |
			           1 << BeamformerParameterBlockRegion_Parameters;
			pb->region_update_flags &= ~mask;

			for (u32 shader_slot = 0; shader_slot < cp->pipeline.shader_count; shader_slot++) {
				u128 hash = u128_hash_from_data(cp->shader_descriptors + shader_slot, sizeof(BeamformerShaderDescriptor));
				if (!u128_equal(hash, cp->shader_hashes[shader_slot]))
					cp->dirty_programs |= 1 << shader_slot;
				cp->shader_hashes[shader_slot] = hash;
			}

			cp->acquisition_count = pb->parameters.acquisition_count;
			cp->acquisition_kind  = pb->parameters.acquisition_kind;
			cp->contrast_mode     = pb->parameters.contrast_mode;

			i64 buffer_size = PING_PONG_BUFFER_SLOTS * round_up_to(cp->rf_size, 64);
			if (ctx->compute_context.ping_pong_buffer.size < buffer_size) {
				b32 cuda = cuda_supported();
				GPUBufferAllocateInfo allocate_info = {
					.size   = buffer_size,
					.export = cuda ? &ctx->compute_context.ping_pong_export_handle : 0,
					.label  = str8("PingPongBuffer"),
				};
				gpu_buffer_allocate(&ctx->compute_context.ping_pong_buffer, allocate_info);

				BeamformerShaderResourceInfo shader_resource_infos[] = {
					{
						.kind   = BeamformerShaderResourceKind_Buffer,
						.handle = ctx->compute_context.ping_pong_buffer.handle,
						.slot   = BeamformerShaderBufferSlot_PingPong,
					},
				};
				vk_bind_shader_resources(shader_resource_infos, countof(shader_resource_infos));

				// TODO(rnp): figure out how to share with CUDA
				// IMPORTANT: on linux the handle is returned to os and should be cleared after import
				// see usage of glImportMemoryFdEXT and surrounding code in ui.c for examples
				if (cuda) {
				}
			}
		}break;

		case BeamformerParameterBlockRegion_ChannelMapping:{
			cuda_set_channel_mapping(pb->channel_mapping);
		}break;

		case BeamformerParameterRegionFlag_FocalVectors:
		case BeamformerParameterRegionFlag_SparseElements:
		case BeamformerParameterRegionFlag_TransmitReceiveOrientations:
		{
			u32 kind = BeamformerComputeArrayParametersField_Count;
			switch (region) {
			case BeamformerParameterRegionFlag_TransmitReceiveOrientations:{
				kind = BeamformerComputeArrayParametersField_TransmitReceiveOrientations;
			}break;
			case BeamformerParameterBlockRegion_FocalVectors:{
				kind = BeamformerComputeArrayParametersField_FocalVectors;
			}break;
			case BeamformerParameterBlockRegion_SparseElements:{
				kind = BeamformerComputeArrayParametersField_SparseElements;
			}break;
			InvalidDefaultCase;
			}

			if (kind != BeamformerComputeArrayParametersField_Count) {
				GPUBuffer *b = &cp->array_parameters;
				u64 offset = beamformer_compute_array_parameter_offsets[kind];
				u64 size   = beamformer_compute_array_parameter_sizes[kind];
				gpu_buffer_range_upload(b, (u8 *)pb + BeamformerParameterBlockRegionOffsets[region], offset, size, 0);
			}
		}break;
		}
	}
}

function void
do_compute_shader(BeamformerCtx *ctx, GPUCommandList cmd, BeamformerComputePlan *cp,
                  BeamformerFrame *frame, u32 shader_slot, u32 channel_offset, u64 rf_pointer)
{
	BeamformerComputeContext *cc = &ctx->compute_context;

	u32 output_index     = !cc->ping_pong_input_index;
	u32 input_index      =  cc->ping_pong_input_index;
	u32 das_output_index =  PING_PONG_BUFFER_SLOTS - 1;

	u64 pp_size           = cc->ping_pong_buffer.size / PING_PONG_BUFFER_SLOTS;
	u64 pp_input_pointer  = cc->ping_pong_buffer.gpu_pointer + input_index      * pp_size;
	u64 pp_output_pointer = cc->ping_pong_buffer.gpu_pointer + output_index     * pp_size;
	u64 pp_das_pointer    = cc->ping_pong_buffer.gpu_pointer + das_output_index * pp_size;

	u32 das_index = cp->first_image_shader_index - 1;

	uv3 dispatch = cp->shader_descriptors[shader_slot].dispatch;

	gpu_command_bind_pipeline(cmd, cp->vulkan_pipelines[shader_slot]);

	switch (cp->pipeline.shaders[shader_slot]) {

	case BeamformerShaderKind_Decode:{
		BeamformerDecodePushConstants pc = {.rf_buffer = pp_input_pointer};

		if ((shader_slot + 1) == das_index) pc.output_buffer = pp_das_pointer;
		else                                pc.output_buffer = pp_output_pointer;

		gpu_command_pipeline_barrier(cmd);
		gpu_command_push_constants(cmd, 0, sizeof(pc), &pc);
		gpu_command_dispatch_compute(cmd, dispatch);

		cc->ping_pong_input_index = !cc->ping_pong_input_index;
	}break;

	case BeamformerShaderKind_Hilbert:{
		cuda_hilbert(input_index, output_index);
		cc->ping_pong_input_index = !cc->ping_pong_input_index;
	}break;

	case BeamformerShaderKind_Filter:
	case BeamformerShaderKind_Demodulate:
	{
		BeamformerDataKind output_data_kind = cp->shader_descriptors[shader_slot].output_data_kind;

		u64 element_size = beamformer_data_kind_byte_size[output_data_kind];
		BeamformerFilterPushConstants pc = {
			.input_data            = shader_slot == 0 ? rf_pointer : pp_input_pointer,
			.output_element_offset = output_index * pp_size / element_size,
		};

		if ((shader_slot + 1) == das_index)
			pc.output_element_offset = das_output_index * pp_size / element_size;

		if (shader_slot != 0 || (shader_slot + 1) == das_index)
			gpu_command_pipeline_barrier(cmd);

		gpu_command_push_constants(cmd, 0, sizeof(pc), &pc);
		gpu_command_dispatch_compute(cmd, dispatch);

		cc->ping_pong_input_index = !cc->ping_pong_input_index;
	}break;

	case BeamformerShaderKind_DAS:{
		u64 element_size = beamformer_data_kind_byte_size[cp->shader_descriptors[shader_slot].input_data_kind];

		BeamformerDASPushConstants pc = {
			.xdc_element_pitch = cp->xdc_element_pitch,
			.rf_element_offset = das_output_index * pp_size / element_size,
			.output_frame      = frame->gpu_pointer,
			.channel_offset    = channel_offset,
			.readi_group       = cp->readi_group,
		};
		memory_copy(pc.voxel_transform.E, cp->das_voxel_transform.E, sizeof(pc.voxel_transform));
		memory_copy(pc.xdc_transform.E,   cp->xdc_transform.E,       sizeof(pc.xdc_transform));

		gpu_command_pipeline_barrier(cmd);
		gpu_command_push_constants(cmd, 0, sizeof(pc), &pc);
		gpu_command_dispatch_compute(cmd, dispatch);
	}break;

	case BeamformerShaderKind_CoherencyWeighting:{
		BeamformerCoherencyWeightingPushConstants pc = {.coherent_sum = frame->gpu_pointer};
		gpu_command_pipeline_barrier(cmd);
		gpu_command_push_constants(cmd, 0, sizeof(pc), &pc);
		gpu_command_dispatch_compute(cmd, dispatch);
	}break;

	case BeamformerShaderKind_Reshape:{
		BeamformerDataKind input_data_kind = cp->shader_descriptors[shader_slot].input_data_kind;
		BeamformerReshapeBakeParameters *rb = &cp->shader_descriptors[shader_slot].bake.Reshape;
		u64 input_pointer = shader_slot == 0 ? rf_pointer : pp_input_pointer;
		BeamformerReshapePushConstants pc = {
			.left_input_buffer  = input_pointer,
			.right_input_buffer = input_pointer + rb->SizeX * rb->SizeY * rb->SizeZ
			                                      * beamformer_data_kind_byte_size[input_data_kind],
		};

		if ((shader_slot + 1) == das_index) pc.output_buffer = pp_das_pointer;
		else                                pc.output_buffer = pp_output_pointer;

		gpu_command_pipeline_barrier(cmd);
		gpu_command_push_constants(cmd, 0, sizeof(pc), &pc);
		gpu_command_dispatch_compute(cmd, dispatch);

		cc->ping_pong_input_index = !cc->ping_pong_input_index;
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
		u32 aframe_index = ctx->averaged_frame_index % countof(ctx->averaged_frames);
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
		u32 *in_textures = push_array(&arena, u32, BeamformerMaxBacklogFrames);
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

		memory_copy(aframe->voxel_transform.E,  frame->voxel_transform.E, sizeof(frame->voxel_transform));
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

	for (BeamformWork *work = beamform_work_queue_pop(q);
	     work;
	     beamform_work_queue_pop_commit(q), work = beamform_work_queue_pop(q))
	{
		switch (work->kind) {

		case BeamformerWorkKind_ExportBuffer:{
			/* TODO(rnp): better way of handling DispatchCompute barrier */
			post_sync_barrier(ctx->shared_memory, BeamformerSharedMemoryLockKind_DispatchCompute);
			beamformer_shared_memory_take_lock(ctx->shared_memory, (i32)work->lock, (u32)-1);
			BeamformerExportContext *ec = &work->export_context;
			switch (ec->kind) {
			case BeamformerExportKind_BeamformedData:{
				BeamformerFrameBacklog *bl = &ctx->compute_context.backlog;
				u32 req_count = Clamp(ec->count, 1, bl->counter);
				u32 frame_idx = bl->counter - req_count;
				u8 *sm_output = beamformer_shared_memory_data_pointer(sm, ctx->shared_memory_size);
				u64 exported_size = 0;
				for (u32 export_count = 0; export_count < req_count; export_count++, frame_idx++) {
					BeamformerFrame *f = bl->frames + frame_idx % countof(bl->frames);
					u64 frame_size = beamformer_frame_byte_size(f->points, f->data_kind);
					assert((frame_size & 63) == 0);
					// NOTE(tkh) we don't want to assume that all req_count frames are the same size,
					// so we either need to count the total size of all requested frames first or
					// just fill up as much as possible.
					if (exported_size + frame_size <= ec->size) {
						u64 offset = f->gpu_pointer - bl->buffer->gpu_pointer;
						gpu_host_wait_timeline(GPUTimeline_Compute, f->timeline_valid_value, -1ULL);
						gpu_buffer_range_download(sm_output + exported_size, bl->buffer, offset, frame_size, 1);
						exported_size += frame_size;
					}
				}
			}break;

			case BeamformerExportKind_Stats:{
				ComputeTimingTable *table = ctx->compute_timing_table;
				/* NOTE(rnp): do a little spin to let this finish updating */
				spin_wait(table->write_index != atomic_load_u32(&table->read_index));
				ComputeShaderStats *stats = ctx->compute_shader_stats;
				if (sizeof(stats->table) <= ec->size)
					memory_copy(beamformer_shared_memory_data_pointer(sm, ctx->shared_memory_size),
					         &stats->table, sizeof(stats->table));
			}break;
			InvalidDefaultCase;
			}
			beamformer_shared_memory_release_lock(ctx->shared_memory, work->lock);
			post_sync_barrier(ctx->shared_memory, BeamformerSharedMemoryLockKind_ExportSync);
		}break;

		case BeamformerWorkKind_CreateFilter:{
			BeamformerCreateFilterContext *fctx = &work->create_filter_context;
			u32 block = fctx->parameter_block;
			u32 slot  = fctx->filter_slot;
			BeamformerComputePlan *cp = beamformer_compute_plan_for_block(cs, block, arena);
			cp->filter_parameters[slot] = fctx->parameters;
		}break;

		case BeamformerWorkKind_ComputeIndirect:
		case BeamformerWorkKind_Compute:
		{
			push_compute_timing_info(ctx->compute_timing_table,
			                         (ComputeTimingInfo){.kind = ComputeTimingInfoKind_ComputeFrameBegin});

			BeamformerComputePlan *cp = beamformer_compute_plan_for_block(cs, work->compute_context.parameter_block, arena);
			if unlikely(beamformer_parameter_block_dirty(sm, work->compute_context.parameter_block)) {
				u32 block = work->compute_context.parameter_block;
				Temp scratch = temp_begin(arena);
				beamformer_commit_parameter_block(ctx, cp, block, arena);
				temp_end(scratch);
			}

			post_sync_barrier(ctx->shared_memory, BeamformerSharedMemoryLockKind_DispatchCompute);

			u32 dirty_programs = atomic_swap_u32(&cp->dirty_programs, 0);
			static_assert(BeamformerMaxComputeShaderStages <= 32, "");
			if unlikely(dirty_programs) {
				for EachBit(dirty_programs, slot) {
					assert(slot < BeamformerMaxComputeShaderStages);
					Temp scratch = temp_begin(arena);
					beamformer_reload_compute_pipeline(cp->vulkan_pipelines + slot,
					                                   cp->pipeline.shaders[slot],
					                                   cp->shader_descriptors + slot, arena);
					temp_end(scratch);
				}
			}

			atomic_store_u32(&cs->processing_compute, 1);

			start_renderdoc_capture();

			i32 das_index = -1;
			i32 coherency_weighting = -1;
			for (u32 i = 0; i < cp->pipeline.shader_count; i++) {
				if (cp->pipeline.shaders[i] == BeamformerShaderKind_CoherencyWeighting)
					coherency_weighting = (i32)i;

				if (cp->pipeline.shaders[i] == BeamformerShaderKind_DAS)
					das_index = (i32)i;
			}

			BeamformerFrame *frame  = beamformer_frame_next(cs, cp->output_points, cp->iq_pipeline);
			frame->acquisition_kind = cp->acquisition_kind;
			frame->contrast_mode    = cp->contrast_mode;
			frame->compound_count   = cp->acquisition_count;
			frame->parameter_block  = work->compute_context.parameter_block;
			frame->view_plane_tag   = work->compute_context.view_plane;
			memory_copy(frame->voxel_transform.E, cp->voxel_transform.E, sizeof(cp->voxel_transform));

			GPUCommandList cmd = gpu_command_list_begin(GPUTimeline_Compute);
			gpu_command_timestamp(cmd);

			if (das_index >= 0) {
				GPUBuffer *backlog = cs->backlog.buffer;
				u64 frame_size = beamformer_frame_byte_size(frame->points, frame->data_kind);
				u64 offset     = frame->gpu_pointer - backlog->gpu_pointer;
				gpu_command_clear_buffer(cmd, backlog, offset, frame_size, 0);
			}

			if (coherency_weighting >= 0) {
				BeamformerCoherencyWeightingBakeParameters *cw = &cp->shader_descriptors[coherency_weighting].bake.CoherencyWeighting;
				GPUBuffer *gpu_arena = &cp->gpu_temp_arena;
				u64 coherent_size = beamformer_incoherent_frame_byte_size(frame->points, frame->data_kind);
				gpu_command_clear_buffer(cmd, gpu_arena, cw->IncoherentSum - gpu_arena->gpu_pointer, coherent_size, 0);
			}

			BeamformerRFBuffer *rf = &cs->rf_buffer;
			u32 compute_index = rf->compute_index;
			u32 slot = compute_index % countof(rf->upload_complete_values);

			if (work->kind == BeamformerWorkKind_ComputeIndirect) {
				// TODO(rnp): this shouldn't be necessary, there should be a way of communicating
				// what the value will be so that the only the command wait is needed.
				spin_wait(atomic_load_u64(&rf->insertion_index) <= compute_index);

				/* NOTE(rnp): if the GPU supports BAR there may be no need to synchronize
				 * other than the above spin */
				if (vk_buffer_needs_sync(&rf->buffer))
					gpu_command_wait_timeline(cmd, GPUTimeline_Transfer, rf->upload_complete_values[slot]);
			} else {
				slot = (rf->compute_index - 1) % countof(rf->upload_complete_values);
			}

			for (u32 channel_offset = 0;
			     channel_offset < cp->channel_count;
			     channel_offset += BeamformerChunkChannelCount)
			{
				u64 rf_pointer = rf->buffer.gpu_pointer + slot * rf->active_rf_size;
				rf_pointer += cp->raw_channel_byte_stride * channel_offset;
				for (u32 i = 0; i < cp->first_image_shader_index; i++) {
					do_compute_shader(ctx, cmd, cp, frame, i, channel_offset, rf_pointer);
					gpu_command_timestamp(cmd);
				}
			}

			for (u32 i = cp->first_image_shader_index; i < cp->pipeline.shader_count; i++) {
				do_compute_shader(ctx, cmd, cp, frame, i, 0, 0);
				gpu_command_timestamp(cmd);
			}
			u64 end_timeline_value = gpu_command_list_end(cmd, (VulkanHandle){0}, (VulkanHandle){0});
			if (work->kind == BeamformerWorkKind_ComputeIndirect) {
				atomic_store_u64(rf->compute_complete_values + slot, end_timeline_value);
				atomic_add_u64(&rf->compute_index, 1);
			}

			atomic_store_u64(&frame->timeline_valid_value, end_timeline_value);

			Temp scratch;
			DeferLoop(scratch = temp_begin(arena), temp_end(scratch))
			{
				/* NOTE(rnp): this blocks until work completes */
				u64  count       = 0;
				u64 *timestamps  = gpu_read_timestamps(GPUTimeline_Compute, &count, arena);

				i32 steps        = ((i32)cp->channel_count / BeamformerChunkChannelCount) - 1;
				i32 step         = 0;
				u32 shader_index = 0;
				u64 last_time    = count > 0 ? timestamps[0] : 0;

				for (u64 i = 1; i < count; i++) {
					push_compute_timing_info(ctx->compute_timing_table, (ComputeTimingInfo){
						.kind        = ComputeTimingInfoKind_Shader,
						.shader      = cp->pipeline.shaders[shader_index],
						.shader_slot = shader_index,
						.timer_count = timestamps[i] - last_time,
					});
					last_time = timestamps[i];

					shader_index++;
					if (shader_index == cp->first_image_shader_index && step < steps) {
						shader_index = 0;
						step++;
					}
				}
			}

			cs->processing_progress = 1;

			//if (has_sum) {
			if (0) {
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
	}
}

function void
coalesce_timing_table(ComputeTimingTable *t, ComputeShaderStats *stats)
{
	/* TODO(rnp): we do not currently do anything to handle the potential for a half written
	 * info item. this could result in garbage entries but they shouldn't really matter */

	u32 target = atomic_load_u32(&t->write_index);
	u32 stats_index = stats->latest_frame_index;

	b32 has_rf = 0;
	f32 gpu_clocks_to_nano = 1.0e-9f * gpu_info()->timestamp_period_ns;

	// NOTE(rnp): not equal (the index may wrap)
	while (t->read_index != target) {
		ComputeTimingInfo info = t->buffer[t->read_index % countof(t->buffer)];
		switch (info.kind) {

		case ComputeTimingInfoKind_ComputeFrameBegin:{
			assert(t->compute_frame_active == 0);
			t->compute_frame_active = 1;
			/* NOTE(rnp): allow multiple instances of same shader to accumulate */
			t->in_flight_shader_count = 0;
			memory_clear(t->in_flight_shader_ids, 0, sizeof(t->in_flight_shader_ids));
			memory_clear(stats->table.times[stats_index], 0, sizeof(stats->table.times[stats_index]));
		}break;

		case ComputeTimingInfoKind_ComputeFrameEnd:{
			assert(t->compute_frame_active == 1);
			t->compute_frame_active = 0;
			stats_index = stats->latest_frame_index = (stats_index + 1) % countof(stats->table.times);
			stats->table.shader_count = t->in_flight_shader_count;
			memory_copy(stats->table.shader_ids, t->in_flight_shader_ids, sizeof(t->in_flight_shader_ids));
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

		rf->active_rf_size = gpu_round_up_to_sync_size(rf_block_rf_size & 0xFFFFFFFFULL, 64);
		if unlikely(rf->buffer.size < countof(rf->upload_complete_values) * rf->active_rf_size) {
			GPUBufferAllocateInfo allocate_info = {
				.size  = countof(rf->upload_complete_values) * rf->active_rf_size,
				.flags = VulkanUsageFlag_HostReadWrite,
				.label = str8("RawRFBuffer"),
			};
			gpu_buffer_allocate(&rf->buffer, allocate_info);
		}

		u64 slot = rf->insertion_index % countof(rf->upload_complete_values);

		/* NOTE(rnp): don't overwrite slot if the compute thread hasn't processed it */
		spin_wait(atomic_load_u64(&rf->compute_index) < rf->insertion_index);
		gpu_host_wait_timeline(GPUTimeline_Compute, rf->compute_complete_values[slot], -1ULL);

		gpu_buffer_range_upload(&rf->buffer, beamformer_shared_memory_data_pointer(sm, ctx->shared_memory_size),
		                        slot * rf->active_rf_size, rf->active_rf_size, 1);
		store_fence();

		beamformer_shared_memory_release_lock(ctx->shared_memory, (i32)scratch_lock);
		post_sync_barrier(ctx->shared_memory, upload_lock);

		atomic_store_u64(rf->upload_complete_values + slot, gpu_host_signal_timeline(GPUTimeline_Transfer));
		atomic_add_u64(&rf->insertion_index, 1);

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
		if (work) {
			work->kind = BeamformerWorkKind_Compute;
			work->compute_context.view_plane      = frame ? frame->view_plane_tag : 0;
			work->compute_context.parameter_block = parameter_block;
			beamform_work_queue_push_commit(ctx->beamform_work_queue);
		}
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

		// NOTE(rnp): ui will handle these
		case BeamformerInputEventKind_ButtonPress:
		case BeamformerInputEventKind_ButtonRelease:
		case BeamformerInputEventKind_MouseScroll:
		case BeamformerInputEventKind_WindowResize:
		{}break;

		case BeamformerInputEventKind_ExecutableReload:{
			ui_init(ctx, ctx->ui_arena);
		}break;

		case BeamformerInputEventKind_FileEvent:{
			BeamformerFileReloadContext *frc = event->file_watch_user_context;
			switch (frc->kind) {
			case BeamformerFileReloadKind_ComputeInternalShader:{
				// TODO(rnp): this could stall, better to push it onto compute once queue is better
				beamformer_reload_compute_pipeline(frc->shader_reload.pipeline, frc->shader_reload.shader, 0, ctx->arena);
			}break;

			case BeamformerFileReloadKind_ComputeShader:{
				for EachElement(ctx->compute_context.compute_plans, block) {
					BeamformerComputePlan *cp = ctx->compute_context.compute_plans[block];
					for (u32 slot = 0; cp && slot < cp->pipeline.shader_count; slot++) {
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

function void
beamformer_panel_group_insert_at(BeamformerUIPanel *group, BeamformerUIPanel *tab, u64 new_child_index)
{
	if (tab->parent) beamformer_ui_panel_unlink(tab);
	new_child_index = Min(new_child_index, group->child_count);

	tab->parent = group;
	group->child_count++;
	if (group->kind == BeamformerPanelKind_TabGroup) group->u.tab_focus = tab;

	BeamformerUIPanel *previous_sibling = new_child_index == 0 ? 0 : group->first_child;
	for (u64 child_index = 1; child_index < new_child_index; child_index++)
		previous_sibling = previous_sibling->next_sibling;

	if (previous_sibling) {
		tab->previous_sibling = previous_sibling;
		tab->next_sibling     = previous_sibling->next_sibling;
		if (tab->next_sibling) tab->next_sibling->previous_sibling = tab;
		previous_sibling->next_sibling = tab;
		if (previous_sibling == group->last_child) group->last_child = tab;
	} else {
		DLLInsertFirst(0, group->first_child, group->last_child, tab, next_sibling, previous_sibling);
	}
}

BEAMFORMER_EXPORT void
beamformer_frame_step(void *memory, BeamformerInput *input)
{
	BeamformerCtx *ctx = beamformer_context = memory;
	beamformer_input = input;

	u64 current_time = os_timer_count();
	dt_for_frame = (f64)(current_time - ctx->frame_timestamp) / os_system_info()->timer_frequency;
	ctx->frame_timestamp = current_time;
	ctx->frame_index++;

	coalesce_timing_table(ctx->compute_timing_table, ctx->compute_shader_stats);

	// NOTE(rnp): reset frame state
	{
		ctx->registers = &ctx->base_registers;
		swap(ctx->command_queues[0], ctx->command_queues[1]);
		zero_struct(ctx->command_queues + 0);
		//zero_struct(ctx->registers);
		arena_clear(beamformer_frame_arena());
	}

	beamformer_process_input_events(ctx, input, input->event_queue, input->event_count);

	BeamformerSharedMemory *sm = ctx->shared_memory;
	u32 live_imaging_active = atomic_load_u32(&sm->live_imaging_parameters.active);
	if (live_imaging_active != ctx->live_imaging_active) {
		if (ctx->live_imaging_active) {
			if (ctx->auto_live_control_panel) {
				BeamformerUIPanel *parent = ctx->auto_live_control_panel->parent;
				beamformer_command(beamformer_command_infos[BeamformerCommandKind_CloseTab].string, .tree_node = (u64)ctx->auto_live_control_panel);
				if (parent->child_count == 1)
					beamformer_command(beamformer_command_infos[BeamformerCommandKind_CloseTab].string, .tree_node = (u64)parent);
			}
		} else {
			if (beamformer_registers()->live_controls) {
				beamformer_command(beamformer_command_infos[BeamformerCommandKind_FocusTab].string,
				                   .tree_node = beamformer_registers()->live_controls);
			} else {
				ctx->auto_live_control_panel = beamformer_ui_push_panel(0, BeamformerPanelKind_LiveImagingControls);
				beamformer_command(beamformer_command_infos[BeamformerCommandKind_SplitTree].string,
				                   .tree_node        = (u64)ctx->auto_live_control_panel,
				                   .split_axis       = Axis2_X,
				                   .split_left_tree  = (u64)ui_context->tree,
				                   .split_right_tree = 0,
				                   .drop_target_tree = (u64)ui_context->tree);
			}
			ctx->live_imaging_active_frame = ctx->frame_index;
		}
		ctx->live_imaging_active = live_imaging_active;
	}

	if (atomic_load_u32(sm->locks + BeamformerSharedMemoryLockKind_UploadRF))
		os_wake_all_waiters(&ctx->upload_worker.sync_variable);
	if (atomic_load_u32(sm->locks + BeamformerSharedMemoryLockKind_DispatchCompute))
		os_wake_all_waiters(&ctx->compute_worker.sync_variable);

	beamformer_registers()->frame = (u64)(ctx->latest_frame - ctx->compute_context.backlog.frames);

	beamformer_ui_frame();

	// NOTE(rnp): execute commands
	for (BeamformerCommandNode *node = ctx->command_queues[0].first;
	     node;
	     node = node == node->next ? 0 : node->next)
	{
		BeamformerRegistersScope()
		{
			memory_copy(beamformer_registers(), node->command.registers, sizeof(*node->command.registers));
			BeamformerCommandKind kind = beamformer_command_kind_from_string(node->command.name);
			switch (kind) {
			InvalidDefaultCase;
			case BeamformerCommandKind_CloseTab:{
				BeamformerUIPanel *tab = (BeamformerUIPanel *)beamformer_registers()->tree_node;
				ui_kill_panel(tab);
			}break;

			case BeamformerCommandKind_FocusTab:{
				BeamformerUIPanel *tab = (BeamformerUIPanel *)beamformer_registers()->tree_node;
				assert(tab->parent->kind == BeamformerPanelKind_TabGroup);
				tab->parent->u.tab_focus = tab;
			}break;

			case BeamformerCommandKind_MoveTab:{
				BeamformerUIPanel *move   = (BeamformerUIPanel *)beamformer_registers()->tree_node;
				BeamformerUIPanel *group  = (BeamformerUIPanel *)beamformer_registers()->drop_target_tree;
				BeamformerUIPanel *parent = move->parent;
				u64 new_child_index = beamformer_registers()->drop_child_index;
				beamformer_panel_group_insert_at(group, move, new_child_index);

				if (move->kind == BeamformerPanelKind_LiveImagingControls) {
					beamformer_context->base_registers.v.live_controls = (u64)move;
					if (move == ctx->auto_live_control_panel)
						ctx->auto_live_control_panel = 0;
				}

				if (parent->child_count == 0)
					beamformer_command(beamformer_command_infos[BeamformerCommandKind_CloseTab].string, .tree_node = (u64)parent);
			}break;

			case BeamformerCommandKind_OpenTab:{
				BeamformerUIPanel *panel = (BeamformerUIPanel *)beamformer_registers()->tree_node;
				assert(panel->kind == BeamformerPanelKind_TabGroup);

				BeamformerPanelKind new_panel_kind = beamformer_panel_kind_from_string(beamformer_registers()->string);
				beamformer_ui_push_panel(panel, new_panel_kind);
			}break;

			case BeamformerCommandKind_SplitTree:{
				BeamformerUIPanel *drag  = (BeamformerUIPanel *)beamformer_registers()->tree_node;
				BeamformerUIPanel *left  = (BeamformerUIPanel *)beamformer_registers()->split_left_tree;
				BeamformerUIPanel *right = (BeamformerUIPanel *)beamformer_registers()->split_right_tree;
				Axis2 axis = beamformer_registers()->split_axis;

				BeamformerUIPanel *new_split     = beamformer_ui_push_panel(0, BeamformerPanelKind_Split);
				BeamformerUIPanel *new_tab_group = beamformer_ui_push_panel(0, BeamformerPanelKind_TabGroup);
				beamformer_panel_group_insert_at(new_tab_group, drag, 0);

				BeamformerUIPanel *target = 0;
				u32 target_child_index = 0;
				f32 new_split_pct = 0.5f;

				if (left == 0 || right == 0) {
					// NOTE(rnp): split on edge of window
					target             = left ? left : right;
					target_child_index = left ? 0 : 1;

					if (target->kind == BeamformerPanelKind_TabGroup) {
						new_split->kind        = BeamformerPanelKind_TabGroup;
						new_split->u.tab_focus = target->u.tab_focus;
					}

					for (BeamformerUIPanel *child = target->last_child, *next; child; child = next) {
						next = child->previous_sibling;
						beamformer_panel_group_insert_at(new_split, child, 0);
					}

					beamformer_panel_group_insert_at(target, new_tab_group, 0);
				} else if (((drag == left)  && right->kind == BeamformerPanelKind_Split) ||
				           ((drag == right) && left->kind  == BeamformerPanelKind_Split))
				{
					// NOTE(rnp): split on internal split
					target             = left == drag ? right : left;
					target_child_index = 1;
					new_split_pct      = 1.f / 3.f;
					beamformer_panel_group_insert_at(new_split, new_tab_group, 0);
					beamformer_panel_group_insert_at(new_split, target->last_child, 1);
				} else {
					// NOTE(rnp): TabGroup Split
					target             = left == drag ? right : left;
					target_child_index = left == drag ? 1 : 0;
					assert(target->kind == BeamformerPanelKind_TabGroup);

					BeamformerUIPanel *focus = target->u.tab_focus;
					new_split->kind = BeamformerPanelKind_TabGroup;
					for (BeamformerUIPanel *child = target->last_child, *next; child; child = next) {
						next = child->previous_sibling;
						beamformer_panel_group_insert_at(new_split, child, 0);
					}
					new_split->u.tab_focus = focus;

					beamformer_panel_group_insert_at(target, new_tab_group, 0);
				}

				beamformer_panel_group_insert_at(target, new_split, target_child_index);
				if (target->kind == BeamformerPanelKind_Split) {
					new_split->u.split.axis     = target->u.split.axis;
					new_split->u.split.fraction = target->u.split.fraction;
				}
				target->kind             = BeamformerPanelKind_Split;
				target->u.split.axis     = axis;
				target->u.split.fraction = new_split_pct;
			}break;

			}
		}
	}

	ctx->render_shader_updated = 0;
}
