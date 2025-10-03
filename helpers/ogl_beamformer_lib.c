/* See LICENSE for license details. */
#include "../compiler.h"

#include "../util.h"
#include "../generated/beamformer.meta.c"
#include "../beamformer_parameters.h"
#include "ogl_beamformer_lib_base.h"

#if OS_LINUX
#include "../os_linux.c"
#elif OS_WINDOWS
#include "../os_win32.c"

W32(iptr) OpenFileMappingA(u32, b32, c8 *);

#else
#error Unsupported Platform
#endif

#include "../beamformer_shared_memory.c"

global struct {
	SharedMemoryRegion      shared_memory;
	BeamformerSharedMemory *bp;
	i32                     timeout_ms;
	BeamformerLibErrorKind  last_error;
} g_beamformer_library_context;

#if OS_LINUX

function b32
os_reserve_region_locks(iptr os_context, u32 count)
{
	b32 result = count <= BeamformerMaxParameterBlockSlots;
	return result;
}

function SharedMemoryRegion
os_open_shared_memory_area(char *name)
{
	SharedMemoryRegion result = {0};
	i32 fd = shm_open(name, O_RDWR, S_IRUSR|S_IWUSR);
	if (fd > 0) {
		void *new = mmap(0, BEAMFORMER_SHARED_MEMORY_SIZE, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
		if (new != MAP_FAILED) result.region = new;
		close(fd);
	}
	return result;
}

#elif OS_WINDOWS

function b32
os_reserve_region_locks(iptr os_context, u32 count)
{
	local_persist iptr semaphores[(u32)BeamformerSharedMemoryLockKind_Count + (u32)BeamformerMaxParameterBlockSlots];
	w32_shared_memory_context *ctx = (typeof(ctx))os_context;

	b32 result = count <= BeamformerMaxParameterBlockSlots;
	if (result) {
		count += BeamformerSharedMemoryLockKind_Count;
		if (count > ctx->reserved_count) {
			u8 buffer[1024];
			Stream sb = {.data = buffer, .cap = countof(buffer)};
			stream_append_s8(&sb, s8(OS_SHARED_MEMORY_NAME "_lock_"));

			u32 new_reserved_count;
			for (new_reserved_count = ctx->reserved_count;
			     new_reserved_count < count && result;
			     new_reserved_count++)
			{
				Stream lb = sb;
				stream_append_u64(&lb, new_reserved_count);
				stream_append_byte(&lb, 0);
				semaphores[new_reserved_count] = CreateSemaphoreA(0, 1, 1, (c8 *)lb.data);
				result &= semaphores[new_reserved_count] != INVALID_FILE;
			}

			if (result) {
				ctx->semaphores     = semaphores;
				ctx->reserved_count = count;
			} else {
				for (u32 j = ctx->reserved_count; j < new_reserved_count; j++)
					CloseHandle(semaphores[j]);
			}
		} else if (count < ctx->reserved_count) {
			for (u32 i = ctx->reserved_count; i > count;)
				CloseHandle(semaphores[--i]);
			ctx->reserved_count = count;
		}
	}
	return result;
}

function SharedMemoryRegion
os_open_shared_memory_area(char *name)
{
	local_persist w32_shared_memory_context ctx = {0};
	SharedMemoryRegion result = {0};
	iptr h = OpenFileMappingA(FILE_MAP_ALL_ACCESS, 0, name);
	if (h != INVALID_FILE) {
		void *new = MapViewOfFile(h, FILE_MAP_ALL_ACCESS, 0, 0, BEAMFORMER_SHARED_MEMORY_SIZE);
		if (new && os_reserve_region_locks((iptr)&ctx, 1)) {
			result.region     = new;
			result.os_context = (iptr)&ctx;
		}
		CloseHandle(h);
	}
	return result;
}

#endif

function b32
lib_error_check(b32 condition, BeamformerLibErrorKind error_kind)
{
	b32 result = condition;
	if (!result) g_beamformer_library_context.last_error = error_kind;
	return result;
}

function b32
check_shared_memory(void)
{
	if (!g_beamformer_library_context.shared_memory.region) {
		g_beamformer_library_context.shared_memory = os_open_shared_memory_area(OS_SHARED_MEMORY_NAME);
		if (lib_error_check(g_beamformer_library_context.shared_memory.region != 0, BF_LIB_ERR_KIND_SHARED_MEMORY)) {
			u32 version = ((BeamformerSharedMemory *)g_beamformer_library_context.shared_memory.region)->version;
			if (lib_error_check(version == BEAMFORMER_SHARED_MEMORY_VERSION, BF_LIB_ERR_KIND_VERSION_MISMATCH))
				g_beamformer_library_context.bp = g_beamformer_library_context.shared_memory.region;
		}
	}

	b32 result = 0;
	if (g_beamformer_library_context.bp)
		result = lib_error_check(!g_beamformer_library_context.bp->invalid, BF_LIB_ERR_KIND_INVALID_ACCESS);
	return result;
}

function b32
valid_parameter_block(u32 block)
{
	b32 result = check_shared_memory();
	if (result) {
		result = lib_error_check(block < g_beamformer_library_context.bp->reserved_parameter_blocks,
		                         BF_LIB_ERR_KIND_PARAMETER_BLOCK_UNALLOCATED);
	}
	return result;
}

function BeamformWork *
try_push_work_queue(void)
{
	BeamformWork *result = beamform_work_queue_push(&g_beamformer_library_context.bp->external_work_queue);
	lib_error_check(result != 0, BF_LIB_ERR_KIND_WORK_QUEUE_FULL);
	return result;
}

function b32
lib_try_lock(i32 lock, i32 timeout_ms)
{
	b32 result = os_shared_memory_region_lock(&g_beamformer_library_context.shared_memory,
	                                          g_beamformer_library_context.bp->locks,
	                                          lock, (u32)timeout_ms);
	lib_error_check(result, BF_LIB_ERR_KIND_SYNC_VARIABLE);
	return result;
}

function void
lib_release_lock(i32 lock)
{
	os_shared_memory_region_unlock(&g_beamformer_library_context.shared_memory,
	                               g_beamformer_library_context.bp->locks, (i32)lock);
}

u32
beamformer_get_api_version(void)
{
	return BEAMFORMER_SHARED_MEMORY_VERSION;
}

const char *
beamformer_error_string(BeamformerLibErrorKind kind)
{
	#define X(type, num, string) string,
	local_persist const char *error_string_table[] = {BEAMFORMER_LIB_ERRORS "invalid error kind"};
	#undef X
	return error_string_table[MIN(kind, countof(error_string_table) - 1)];
}

BeamformerLibErrorKind
beamformer_get_last_error(void)
{
	return g_beamformer_library_context.last_error;
}

const char *
beamformer_get_last_error_string(void)
{
	return beamformer_error_string(beamformer_get_last_error());
}

b32
beamformer_set_global_timeout(i32 timeout_ms)
{
	b32 result = lib_error_check(timeout_ms >= -1, BF_LIB_ERR_KIND_INVALID_TIMEOUT);
	if (result) g_beamformer_library_context.timeout_ms = timeout_ms;
	return result;
}

b32
beamformer_reserve_parameter_blocks(uint32_t count)
{
	b32 result = 0;
	if (check_shared_memory() &&
	    lib_error_check(os_reserve_region_locks(g_beamformer_library_context.shared_memory.os_context, count),
	                    BF_LIB_ERR_KIND_PARAMETER_BLOCK_OVERFLOW))
	{
		u32 old_count = g_beamformer_library_context.bp->reserved_parameter_blocks;
		g_beamformer_library_context.bp->reserved_parameter_blocks = count;
		for (u32 i = old_count; i < count; i++)
			zero_struct(beamformer_parameter_block(g_beamformer_library_context.bp, i));
		result = 1;
	}
	return result;
}

function b32
validate_pipeline(i32 *shaders, u32 shader_count, BeamformerDataKind data_kind)
{
	b32 result = lib_error_check(shader_count <= BeamformerMaxComputeShaderStages, BF_LIB_ERR_KIND_COMPUTE_STAGE_OVERFLOW);
	if (result) {
		for (u32 i = 0; i < shader_count; i++)
			result &= BETWEEN(shaders[i], BeamformerShaderKind_ComputeFirst, BeamformerShaderKind_ComputeLast);
		if (!result) {
			g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_INVALID_COMPUTE_STAGE;
		} else if (shaders[0] != BeamformerShaderKind_Demodulate &&
		           shaders[0] != BeamformerShaderKind_Decode)
		{
			g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_INVALID_START_SHADER;
			result = 0;
		} else if (shaders[0] == BeamformerShaderKind_Demodulate &&
		           !(data_kind == BeamformerDataKind_Int16 || data_kind == BeamformerDataKind_Float32))
		{
			g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_INVALID_DEMOD_DATA_KIND;
			result = 0;
		}
	}
	return result;
}

function b32
validate_simple_parameters(BeamformerSimpleParameters *bp)
{
	b32 result = check_shared_memory();
	if (result) {
		result &= bp->channel_count <= BeamformerMaxChannelCount;
		if (!result)
			g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_INVALID_SIMPLE_PARAMETERS;
	}
	return result;
}

function b32
parameter_block_region_upload(void *data, u32 size, u32 block, BeamformerParameterBlockRegions region_id,
                              u32 block_offset, i32 timeout_ms)
{
	i32 lock   = BeamformerSharedMemoryLockKind_Count + (i32)block;
	b32 result = valid_parameter_block(block) && lib_try_lock(lock, timeout_ms);
	if (result) {
		mem_copy((u8 *)beamformer_parameter_block(g_beamformer_library_context.bp, block) + block_offset,
		         data, size);
		mark_parameter_block_region_dirty(g_beamformer_library_context.bp, block, region_id);
		lib_release_lock(lock);
	}
	return result;
}

b32
beamformer_set_pipeline_stage_parameters_at(u32 stage_index, i32 parameter, u32 block)
{
	u32 offset  = BeamformerParameterBlockRegionOffsets[BeamformerParameterBlockRegion_ComputePipeline];
	offset     += offsetof(BeamformerComputePipeline, parameters);
	offset     += (stage_index % BeamformerMaxComputeShaderStages) * sizeof(BeamformerShaderParameters);
	b32 result  = parameter_block_region_upload(&parameter, sizeof(BeamformerShaderParameters), block,
	                                            BeamformerParameterBlockRegion_ComputePipeline, offset,
	                                            g_beamformer_library_context.timeout_ms);
	return result;
}

b32
beamformer_set_pipeline_stage_parameters(u32 stage_index, i32 parameter)
{
	b32 result = beamformer_set_pipeline_stage_parameters_at(stage_index, parameter, 0);
	return result;
}

b32
beamformer_push_pipeline_at(i32 *shaders, u32 shader_count, BeamformerDataKind data_kind, u32 block)
{
	b32 result = 0;
	if (check_shared_memory() && validate_pipeline(shaders, shader_count, data_kind)) {
		i32 lock = BeamformerSharedMemoryLockKind_Count + (i32)block;
		if (valid_parameter_block(block) && lib_try_lock(lock, g_beamformer_library_context.timeout_ms)) {
			BeamformerParameterBlock *b = beamformer_parameter_block(g_beamformer_library_context.bp, block);
			mem_copy(&b->pipeline.shaders, shaders, shader_count * sizeof(*shaders));
			mark_parameter_block_region_dirty(g_beamformer_library_context.bp, block,
			                                  BeamformerParameterBlockRegion_ComputePipeline);
			b->pipeline.shader_count = shader_count;
			b->pipeline.data_kind    = data_kind;
			lib_release_lock(lock);
			result = 1;
		}
	}
	return result;
}

b32
beamformer_push_pipeline(i32 *shaders, u32 shader_count, BeamformerDataKind data_kind)
{
	b32 result = beamformer_push_pipeline_at(shaders, shader_count, data_kind, 0);
	return result;
}

function b32
beamformer_create_filter_base(BeamformerFilterKind kind, BeamformerFilterParameters params, u8 filter_slot, u8 parameter_block)
{
	b32 result = 0;
	if (check_shared_memory()) {
		BeamformWork *work = try_push_work_queue();
		if (work) {
			BeamformerCreateFilterContext *ctx = &work->create_filter_context;
			work->kind = BeamformerWorkKind_CreateFilter;
			ctx->kind            = kind;
			ctx->parameters      = params;
			ctx->filter_slot     = filter_slot     % BeamformerFilterSlots;
			ctx->parameter_block = parameter_block % BeamformerMaxParameterBlockSlots;
			beamform_work_queue_push_commit(&g_beamformer_library_context.bp->external_work_queue);
			result = 1;
		}
	}
	return result;
}

b32
beamformer_create_filter(BeamformerFilterKind kind, f32 *filter_parameters, u32 filter_parameter_count,
                         f32 sampling_frequency, b32 complex, u8 filter_slot, u8 parameter_block)
{
	b32 result = 0;
	if (lib_error_check(kind >= 0 && kind < BeamformerFilterKind_Count, BF_LIB_ERR_KIND_INVALID_FILTER_KIND)) {
		BeamformerFilterParameters fp = {.sampling_frequency = sampling_frequency, .complex = complex != 0};
		#define X(kind, ...) sizeof(fp.kind),
		read_only local_persist u32 kind_sizes[] = {BEAMFORMER_FILTER_KIND_LIST(,)};
		#undef X
		if (lib_error_check(kind_sizes[kind] == sizeof(f32) * filter_parameter_count,
		                    BF_LIB_ERR_KIND_INVALID_FILTER_PARAM_COUNT))
		{
			/* NOTE(rnp): any filter kind struct works as base offset of union */
			mem_copy(&fp.Kaiser, filter_parameters, kind_sizes[kind]);
			result = beamformer_create_filter_base(kind, fp, filter_slot, parameter_block);
		}
	}
	return result;
}

function b32
beamformer_flush_commands(i32 timeout_ms)
{
	b32 result = lib_try_lock(BeamformerSharedMemoryLockKind_DispatchCompute, timeout_ms);
	return result;
}

function b32
beamformer_compute_indirect(BeamformerViewPlaneTag tag, u32 block)
{
	b32 result = 0;
	if (check_shared_memory() &&
	    lib_error_check(tag   < BeamformerViewPlaneTag_Count, BF_LIB_ERR_KIND_INVALID_IMAGE_PLANE) &&
	    lib_error_check(block < g_beamformer_library_context.bp->reserved_parameter_blocks,
	                    BF_LIB_ERR_KIND_PARAMETER_BLOCK_UNALLOCATED))
	{
		BeamformWork *work = try_push_work_queue();
		if (work) {
			work->kind = BeamformerWorkKind_ComputeIndirect;
			work->compute_indirect_context.view_plane      = tag;
			work->compute_indirect_context.parameter_block = block;
			beamform_work_queue_push_commit(&g_beamformer_library_context.bp->external_work_queue);
			beamformer_flush_commands(0);
			result = 1;
		}
	}
	return result;
}

b32
beamformer_start_compute(void)
{
	b32 result = beamformer_compute_indirect(0, 0);
	return result;
}

b32
beamformer_wait_for_compute_dispatch(i32 timeout_ms)
{
	b32 result = beamformer_flush_commands(timeout_ms);
	/* NOTE(rnp): if you are calling this function you are probably about
	 * to start some other work and it might be better to not do this... */
	if (result) lib_release_lock(BeamformerSharedMemoryLockKind_DispatchCompute);
	return result;
}

#define BEAMFORMER_UPLOAD_FNS \
	X(channel_mapping,               i16, 1, ChannelMapping) \
	X(focal_vectors,                 f32, 2, FocalVectors)   \
	X(sparse_elements,               i16, 1, SparseElements) \
	X(transmit_receive_orientations, u8,  1, TransmitReceiveOrientations)

#define X(name, dtype, elements, region_name) \
b32 beamformer_push_##name ##_at(dtype *data, u32 count, u32 block) { \
	b32 result = 0; \
	if (lib_error_check(count <= countof(((BeamformerParameterBlock *)0)->name), BF_LIB_ERR_KIND_BUFFER_OVERFLOW)) { \
		result = parameter_block_region_upload(data, count * elements * sizeof(dtype), block, \
		                                       BeamformerParameterBlockRegion_##region_name,  \
		                                       offsetof(BeamformerParameterBlock, name),      \
		                                       g_beamformer_library_context.timeout_ms);      \
	} \
	return result; \
}
BEAMFORMER_UPLOAD_FNS
#undef X

#define X(name, dtype, ...) \
b32 beamformer_push_##name (dtype *data, u32 count) { \
	b32 result = beamformer_push_##name ##_at(data, count, 0); \
	return result; \
}
BEAMFORMER_UPLOAD_FNS
#undef X

function b32
beamformer_push_data_base(void *data, u32 data_size, i32 timeout_ms)
{
	b32 result = 0;
	if (check_shared_memory()) {
		Arena scratch = beamformer_shared_memory_scratch_arena(g_beamformer_library_context.bp);
		if (lib_error_check(data_size <= arena_capacity(&scratch, u8), BF_LIB_ERR_KIND_BUFFER_OVERFLOW)) {
			if (lib_try_lock(BeamformerSharedMemoryLockKind_UploadRF, timeout_ms)) {
				if (lib_try_lock(BeamformerSharedMemoryLockKind_ScratchSpace, 0)) {
					mem_copy(scratch.beg, data, data_size);
					/* TODO(rnp): need a better way to communicate this */
					g_beamformer_library_context.bp->scratch_rf_size = data_size;
					lib_release_lock(BeamformerSharedMemoryLockKind_ScratchSpace);
					result = 1;
				}
			}
		}
	}
	return result;
}

b32
beamformer_push_data(void *data, u32 data_size)
{
	return beamformer_push_data_base(data, data_size, g_beamformer_library_context.timeout_ms);
}

b32
beamformer_push_data_with_compute(void *data, u32 data_size, u32 image_plane_tag, u32 parameter_slot)
{
	b32 result = beamformer_push_data_base(data, data_size, g_beamformer_library_context.timeout_ms);
	if (result) result = beamformer_compute_indirect(image_plane_tag, parameter_slot);
	return result;
}

b32
beamformer_push_parameters_at(BeamformerParameters *bp, u32 block)
{
	b32 result = parameter_block_region_upload(bp, sizeof(*bp), block,
	                                           BeamformerParameterBlockRegion_Parameters,
	                                           offsetof(BeamformerParameterBlock, parameters),
	                                           g_beamformer_library_context.timeout_ms);
	return result;
}

b32
beamformer_push_parameters(BeamformerParameters *bp)
{
	b32 result = beamformer_push_parameters_at(bp, 0);
	return result;
}

b32
beamformer_push_simple_parameters_at(BeamformerSimpleParameters *bp, u32 block)
{
	b32 result = validate_simple_parameters(bp);
	if (result) {
		alignas(64) v2 focal_vectors[countof(bp->steering_angles)];
		for (u32 i = 0; i < countof(bp->steering_angles); i++)
			focal_vectors[i] = (v2){{bp->steering_angles[i], bp->focal_depths[i]}};

		result &= beamformer_push_parameters_at((BeamformerParameters *)bp, block);
		result &= beamformer_push_pipeline_at(bp->compute_stages, bp->compute_stages_count, (BeamformerDataKind)bp->data_kind, block);
		result &= beamformer_push_channel_mapping_at(bp->channel_mapping, bp->channel_count, block);
		result &= beamformer_push_focal_vectors_at((f32 *)focal_vectors, countof(focal_vectors), block);
		result &= beamformer_push_transmit_receive_orientations_at(bp->transmit_receive_orientations,
		                                                           bp->acquisition_count, block);

		if (bp->das_shader_id == BeamformerAcquisitionKind_UFORCES || bp->das_shader_id == BeamformerAcquisitionKind_UHERCULES)
			result &= beamformer_push_sparse_elements_at(bp->sparse_elements, bp->acquisition_count, block);

		for (u32 stage = 0; stage < bp->compute_stages_count; stage++)
			result &= beamformer_set_pipeline_stage_parameters_at(stage, bp->compute_stage_parameters[stage], block);
	}
	return result;
}

b32
beamformer_push_simple_parameters(BeamformerSimpleParameters *bp)
{
	b32 result = beamformer_push_simple_parameters_at(bp, 0);
	return result;
}

b32
beamformer_push_parameters_ui(BeamformerUIParameters *bp)
{
	b32 result = parameter_block_region_upload(bp, sizeof(*bp), 0, BeamformerParameterBlockRegion_Parameters,
	                                           offsetof(BeamformerParameterBlock, parameters_ui),
	                                           g_beamformer_library_context.timeout_ms);
	return result;
}

b32
beamformer_push_parameters_head(BeamformerParametersHead *bp)
{
	b32 result = parameter_block_region_upload(bp, sizeof(*bp), 0, BeamformerParameterBlockRegion_Parameters,
	                                           offsetof(BeamformerParameterBlock, parameters_head),
	                                           g_beamformer_library_context.timeout_ms);
	return result;
}

function b32
beamformer_export_buffer(BeamformerExportContext export_context)
{
	BeamformWork *work = try_push_work_queue();
	b32 result = work && lib_try_lock(BeamformerSharedMemoryLockKind_ExportSync, 0);
	if (result) {
		work->export_context = export_context;
		work->kind = BeamformerWorkKind_ExportBuffer;
		work->lock = BeamformerSharedMemoryLockKind_ScratchSpace;
		beamform_work_queue_push_commit(&g_beamformer_library_context.bp->external_work_queue);
	}
	return result;
}

function b32
beamformer_export(BeamformerExportContext export, void *out, i32 timeout_ms)
{
	b32 result = 0;
	if (beamformer_export_buffer(export)) {
		/* NOTE(rnp): if this fails it just means that the work from push_data hasn't
		 * started yet. This is here to catch the other case where the work started
		 * and finished before we finished queuing the export work item */
		beamformer_flush_commands(0);

		if (lib_try_lock(BeamformerSharedMemoryLockKind_ExportSync, timeout_ms)) {
			if (lib_try_lock(BeamformerSharedMemoryLockKind_ScratchSpace, 0)) {
				Arena scratch = beamformer_shared_memory_scratch_arena(g_beamformer_library_context.bp);
				mem_copy(out, scratch.beg, export.size);
				lib_release_lock(BeamformerSharedMemoryLockKind_ScratchSpace);
				result = 1;
			}
			lib_release_lock(BeamformerSharedMemoryLockKind_ExportSync);
		}
	}
	return result;
}

b32
beamformer_beamform_data(BeamformerSimpleParameters *bp, void *data, uint32_t data_size,
                         void *out_data, int32_t timeout_ms)
{
	b32 result = validate_simple_parameters(bp);
	if (result) {
		bp->output_points[0] = MAX(1, bp->output_points[0]);
		bp->output_points[1] = MAX(1, bp->output_points[1]);
		bp->output_points[2] = MAX(1, bp->output_points[2]);

		beamformer_push_simple_parameters(bp);

		b32 complex = 0;
		for (u32 stage = 0; stage < bp->compute_stages_count; stage++) {
			BeamformerShaderKind shader = (BeamformerShaderKind)bp->compute_stages[stage];
			complex |= shader == BeamformerShaderKind_Demodulate || shader == BeamformerShaderKind_CudaHilbert;
		}

		iz output_size = bp->output_points[0] * bp->output_points[1] * bp->output_points[2] * (i32)sizeof(f32);
		if (complex) output_size *= 2;

		Arena scratch = beamformer_shared_memory_scratch_arena(g_beamformer_library_context.bp);
		if (result && lib_error_check(output_size <= arena_capacity(&scratch, u8), BF_LIB_ERR_KIND_EXPORT_SPACE_OVERFLOW)
		    && beamformer_push_data_with_compute(data, data_size, 0, 0))
		{
			BeamformerExportContext export;
			export.kind = BeamformerExportKind_BeamformedData;
			export.size = (u32)output_size;
			result = beamformer_export(export, out_data, timeout_ms);
		}
	}
	return result;
}

b32
beamformer_compute_timings(BeamformerComputeStatsTable *output, i32 timeout_ms)
{
	static_assert(sizeof(*output) <= BEAMFORMER_SHARED_MEMORY_MAX_SCRATCH_SIZE,
	              "timing table size exceeds scratch space");

	b32 result = 0;
	if (check_shared_memory()) {
		Arena scratch = beamformer_shared_memory_scratch_arena(g_beamformer_library_context.bp);
		if (lib_error_check((iz)sizeof(*output) <= arena_capacity(&scratch, u8), BF_LIB_ERR_KIND_EXPORT_SPACE_OVERFLOW)) {
			BeamformerExportContext export;
			export.kind = BeamformerExportKind_Stats;
			export.size = sizeof(*output);
			result = beamformer_export(export, output, timeout_ms);
		}
	}
	return result;
}

i32
beamformer_live_parameters_get_dirty_flag(void)
{
	i32 result = -1;
	if (check_shared_memory()) {
		u32 flag = ctz_u32(g_beamformer_library_context.bp->live_imaging_dirty_flags);
		if (flag != 32) {
			atomic_and_u32(&g_beamformer_library_context.bp->live_imaging_dirty_flags, ~(1u << flag));
			result = (i32)flag;
		}
	}
	return result;
}

BeamformerLiveImagingParameters *
beamformer_get_live_parameters(void)
{
	BeamformerLiveImagingParameters *result = 0;
	if (check_shared_memory()) result = &g_beamformer_library_context.bp->live_imaging_parameters;
	return result;
}

b32
beamformer_set_live_parameters(BeamformerLiveImagingParameters *new)
{
	b32 result = 0;
	if (check_shared_memory()) {
		mem_copy(&g_beamformer_library_context.bp->live_imaging_parameters, new, sizeof(*new));
		memory_write_barrier();
		result = 1;
	}
	return result;
}
