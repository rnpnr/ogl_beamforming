/* See LICENSE for license details. */
#include "../compiler.h"

#define BEAMFORMER_IMPORT static

#include "../beamformer.h"

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

#include "../util_os.c"
#include "../beamformer_shared_memory.c"

global struct {
	BeamformerSharedMemory *bp;
	i32                     timeout_ms;
	BeamformerLibErrorKind  last_error;
	i64                     shared_memory_size;
} g_beamformer_library_context;

#if OS_LINUX

function s8
os_open_shared_memory_area(char *name)
{
	s8 result = {0};
	i32 fd = shm_open(name, O_RDWR, S_IRUSR|S_IWUSR);
	if (fd > 0) {
		struct stat sb;
		if (fstat(fd, &sb) != -1) {
			void *new = mmap(0, sb.st_size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
			if (new != MAP_FAILED) {
				result.data = new;
				result.len  = sb.st_size;
			}
		}
		close(fd);
	}
	return result;
}

function void
os_close_shared_memory_area(void *memory, i64 size)
{
	munmap(memory, size);
}

#elif OS_WINDOWS

W32(u64) VirtualQuery(void *base_address, void *memory_basic_info, u64 memory_basic_info_size);
W32(b32) UnmapViewOfFile(void *);

function b32
os_reserve_region_locks(void)
{
	u8 buffer[1024];
	Stream sb = {.data = buffer, .cap = countof(buffer)};
	stream_append_s8(&sb, s8(OS_SHARED_MEMORY_NAME "_lock_"));

	i32 start_index    = sb.widx;
	u32 reserved_count = 0;
	for EachElement(os_w32_shared_memory_semaphores, it) {
		stream_reset(&sb, start_index);
		stream_append_u64(&sb, it);
		stream_append_byte(&sb, 0);
		os_w32_shared_memory_semaphores[it] = os_w32_create_semaphore((c8 *)sb.data, 1, 1);
		if InvalidHandle(os_w32_shared_memory_semaphores[it])
			break;
		reserved_count++;
	}

	b32 result = reserved_count == countof(os_w32_shared_memory_semaphores);
	if (!result) {
		for (u32 i = 0; i < reserved_count; i++)
			CloseHandle(os_w32_shared_memory_semaphores[i].value[0]);
	}

	return result;
}

function s8
os_open_shared_memory_area(char *name)
{
	struct alignas(16) {
		void *BaseAddress;
		void *AllocationBase;
		u32   AllocationProtect;
		u32   __alignment1;
		u64   RegionSize;
		u32   State;
		u32   Protect;
		u32   Type;
		u32   __alignment2;
	} memory_basic_info;

	s8 result = {0};
	iptr h = OpenFileMappingA(FILE_MAP_ALL_ACCESS, 0, name);
	if (h != INVALID_FILE) {
		// NOTE(rnp): a size of 0 maps the whole region, we can determine its size after
		void *new = MapViewOfFile(h, FILE_MAP_ALL_ACCESS, 0, 0, 0);
		if (new &&
		    VirtualQuery(new, &memory_basic_info, sizeof(memory_basic_info)) == sizeof(memory_basic_info) &&
		    os_reserve_region_locks())
		{
			result.data = new;
			result.len  = (i64)memory_basic_info.RegionSize;
		}

		if (new && !result.data)
			UnmapViewOfFile(new);

		CloseHandle(h);
	}
	return result;
}

function void
os_close_shared_memory_area(void *memory, i64 size)
{
	UnmapViewOfFile(memory);
}

#endif

#define lib_error_check(c, e) lib_error_check_(c, BeamformerLibErrorKind_##e)
function b32
lib_error_check_(b32 condition, BeamformerLibErrorKind error_kind)
{
	b32 result = condition;
	if (!result) g_beamformer_library_context.last_error = error_kind;
	assert(result);
	return result;
}

function b32
check_shared_memory(void)
{
	b32 result = g_beamformer_library_context.bp != 0;
	if unlikely(!g_beamformer_library_context.bp) {
		s8 shared_memory = os_open_shared_memory_area(OS_SHARED_MEMORY_NAME);
		if (lib_error_check(shared_memory.data != 0, SharedMemory)) {
			BeamformerSharedMemory *bp = (BeamformerSharedMemory *)shared_memory.data;
			result = lib_error_check(bp->version == BEAMFORMER_SHARED_MEMORY_VERSION, VersionMismatch);
			if (result) {
				g_beamformer_library_context.bp                 = bp;
				g_beamformer_library_context.shared_memory_size = shared_memory.len;
			} else {
				os_close_shared_memory_area(shared_memory.data, shared_memory.len);
			}
		}
	}

	if likely(g_beamformer_library_context.bp)
		result = lib_error_check(likely(!g_beamformer_library_context.bp->invalid), InvalidAccess);
	return result;
}

function b32
valid_parameter_block(u32 block)
{
	b32 result = check_shared_memory();
	if (result) {
		result = lib_error_check(block < g_beamformer_library_context.bp->reserved_parameter_blocks,
		                         ParameterBlockUnallocated);
	}
	return result;
}

function BeamformWork *
try_push_work_queue(void)
{
	BeamformWork *result = beamform_work_queue_push(&g_beamformer_library_context.bp->external_work_queue);
	lib_error_check(result != 0, WorkQueueFull);
	return result;
}

function b32
lib_try_lock(i32 lock, i32 timeout_ms)
{
	b32 result = beamformer_shared_memory_take_lock(g_beamformer_library_context.bp, lock, (u32)timeout_ms);
	lib_error_check(result, SyncVariable);
	return result;
}

function void
lib_release_lock(i32 lock)
{
	beamformer_shared_memory_release_lock(g_beamformer_library_context.bp, lock);
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

void
beamformer_set_global_timeout(u32 timeout_ms)
{
	g_beamformer_library_context.timeout_ms = timeout_ms;
}

b32
beamformer_reserve_parameter_blocks(uint32_t count)
{
	b32 result = 0;
	if (check_shared_memory() &&
	    lib_error_check(count <= BeamformerMaxParameterBlocks, ParameterBlockOverflow))
	{
		g_beamformer_library_context.bp->reserved_parameter_blocks = count;
		result = 1;
	}
	return result;
}

function b32
validate_parameters(BeamformerParameters *bp)
{
	if (!lib_error_check(Between(bp->contrast_mode, 0, BeamformerContrastMode_Count - 1), InvalidContrastMode))
		return 0;

	u32 contrast_raw_sample_count = bp->acquisition_count * bp->sample_count * beamformer_contrast_mode_samples[bp->contrast_mode];
	if (!lib_error_check(contrast_raw_sample_count <= bp->raw_data_dimensions.x, DataSizeMismatch))
		return 0;

	return 1;
}

function b32
validate_pipeline(i32 *shaders, u32 shader_count, BeamformerDataKind data_kind)
{
	b32 data_kind_test = Between(data_kind, 0, BeamformerDataKind_Count - 1) &&
	                     data_kind != BeamformerDataKind_Float16 &&
	                     data_kind != BeamformerDataKind_Float16Complex;
	if (!lib_error_check(data_kind_test, InvalidDataKind))
		return 0;

	if (!lib_error_check(shader_count <= BeamformerMaxComputeShaderStages, ComputeStageOverflow))
		return 0;

	for (u32 i = 0; i < shader_count; i++) {
		b32 stage_test = Between(shaders[i], BeamformerShaderKind_ComputeFirst, BeamformerShaderKind_ComputeLast);
		if (!lib_error_check(stage_test, InvalidComputeStage))
			return 0;

		if (shaders[i] == BeamformerShaderKind_Demodulate &&
		    !lib_error_check(!beamformer_data_kind_complex[data_kind], InvalidDemodulationDataKind))
		{
			return 0;
		}
	}

	b32 start_stage_test = shaders[0] == BeamformerShaderKind_Demodulate ||
	                       shaders[0] == BeamformerShaderKind_Decode;
	if (!lib_error_check(start_stage_test, InvalidStartShader))
		return 0;

	return 1;
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
beamformer_create_filter_base(BeamformerFilterParameters params, u8 filter_slot, u8 parameter_block)
{
	b32 result = 0;
	if (check_shared_memory()) {
		BeamformWork *work = try_push_work_queue();
		if (work) {
			BeamformerCreateFilterContext *ctx = &work->create_filter_context;
			work->kind = BeamformerWorkKind_CreateFilter;
			ctx->parameters      = params;
			ctx->filter_slot     = filter_slot     % BeamformerFilterSlots;
			ctx->parameter_block = parameter_block % BeamformerMaxParameterBlocks;
			beamform_work_queue_push_commit(&g_beamformer_library_context.bp->external_work_queue);
			result = 1;
		}
	}
	return result;
}

b32
beamformer_create_filter(BeamformerFilterKind kind, void *filter_parameters, u32 filter_size,
                         f32 sampling_frequency, b32 complex, u8 filter_slot, u8 parameter_block)
{
	b32 result = 0;
	if (lib_error_check(kind >= 0 && kind < BeamformerFilterKind_Count, InvalidFilterKind)) {
		BeamformerFilterParameters fp = {0};
		/* NOTE(rnp): any parameter struct works as base offset */
		filter_size = MIN(filter_size, sizeof(fp) - offsetof(BeamformerFilterParameters, kaiser));
		mem_copy(&fp.kaiser, filter_parameters, filter_size);
		fp.kind               = kind;
		fp.complex            = complex != 0;
		fp.sampling_frequency = sampling_frequency;
		result = beamformer_create_filter_base(fp, filter_slot, parameter_block);
	}
	return result;
}

function void
beamformer_flush_commands(void)
{
	i32 lock = BeamformerSharedMemoryLockKind_DispatchCompute;
	beamformer_shared_memory_take_lock(g_beamformer_library_context.bp, lock, 0);
}

#define BEAMFORMER_UPLOAD_FNS \
	X(channel_mapping,               i16, 1, ChannelMapping) \
	X(focal_vectors,                 f32, 2, FocalVectors)   \
	X(sparse_elements,               i16, 1, SparseElements) \
	X(transmit_receive_orientations, u8,  1, TransmitReceiveOrientations)

#define X(name, dtype, elements, region_name) \
b32 beamformer_push_##name ##_at(dtype *data, u32 count, u32 block) { \
	b32 result = 0; \
	if (lib_error_check(count <= countof(((BeamformerParameterBlock *)0)->name), BufferOverflow)) { \
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

#define BEAMFORMER_REDUCE_A1S2_CONTRAST_FN(name) void name(void *restrict output_v, \
                                                           void *restrict input_v, \
                                                           u32 sample_count)
typedef BEAMFORMER_REDUCE_A1S2_CONTRAST_FN(beamformer_reduce_a1s2_contrast_fn);

#define BEAMFORMER_REDUCE_A1S2_CONTRAST_LIST \
	X(i16) \
	X(f32) \
	X(f16) \

static_assert(BeamformerDataKind_Float16Complex == (BeamformerDataKind_Count - 1), "");

#define X(type, ...) \
function BEAMFORMER_REDUCE_A1S2_CONTRAST_FN(beamformer_reduce_a1s2_contrast_##type) \
{ \
	type *input_a = (type *)input_v + 0 * sample_count; \
	type *input_b = (type *)input_v + 1 * sample_count; \
	type *input_c = (type *)input_v + 2 * sample_count; \
	type *output  = (type *)output_v; \
	for (u32 sample = 0; sample < sample_count; sample++) \
		output[sample] = input_a[sample] - input_b[sample] - input_c[sample]; \
}
BEAMFORMER_REDUCE_A1S2_CONTRAST_LIST
#undef X

function b32
beamformer_push_data_base(void *data, u32 data_size, i32 timeout_ms, u32 block)
{
	b32 result = 0;
	Arena scratch = beamformer_shared_memory_scratch_arena(g_beamformer_library_context.bp,
	                                                       g_beamformer_library_context.shared_memory_size);
	BeamformerParameterBlock *b  = beamformer_parameter_block(g_beamformer_library_context.bp, block);
	BeamformerParameters     *bp = &b->parameters;
	BeamformerDataKind     data_kind     = b->pipeline.data_kind;
	BeamformerContrastMode contrast_mode = bp->contrast_mode;

	u32 size     = bp->acquisition_count * bp->sample_count * bp->channel_count * beamformer_data_kind_byte_size[data_kind];
	u32 raw_size = bp->raw_data_dimensions.x * bp->raw_data_dimensions.y * beamformer_data_kind_byte_size[data_kind];

	if (lib_error_check(size <= arena_capacity(&scratch, u8), BufferOverflow) &&
	    lib_error_check(size <= data_size && data_size == raw_size, DataSizeMismatch))
	{
		if (lib_try_lock(BeamformerSharedMemoryLockKind_UploadRF, timeout_ms)) {
			if (lib_try_lock(BeamformerSharedMemoryLockKind_ScratchSpace, 0)) {
				u32 channel_count      = bp->channel_count;
				u32 out_channel_stride = beamformer_data_kind_byte_size[data_kind] * bp->sample_count * bp->acquisition_count;
				u32 in_channel_stride  = beamformer_data_kind_byte_size[data_kind] * bp->raw_data_dimensions.x;

				for (u32 channel = 0; channel < channel_count; channel++) {
					u16 data_channel = (u16)b->channel_mapping[channel];
					u32 out_off = out_channel_stride * channel;
					u32 in_off  = in_channel_stride  * data_channel;
					switch (contrast_mode) {
					default:{
						/* NOTE(rnp): non temporal copy would be better, but we can't ensure
						 * 64 byte boundaries. */
						memory_copy(scratch.beg + out_off, (u8 *)data + in_off, out_channel_stride);
					}break;

					case BeamformerContrastMode_A1S2:{
						read_only local_persist u8 reduce_a1s2_index_map[] = {
							[BeamformerDataKind_Int16]          = 0,
							[BeamformerDataKind_Int16Complex]   = 0,
							[BeamformerDataKind_Float32]        = 1,
							[BeamformerDataKind_Float32Complex] = 1,
							[BeamformerDataKind_Float16]        = 2,
							[BeamformerDataKind_Float16Complex] = 2,
						};
						static_assert(BeamformerDataKind_Float16Complex == (BeamformerDataKind_Count - 1), "");

						read_only local_persist beamformer_reduce_a1s2_contrast_fn *reduce_a1s2_fn_table[] = {
							#define X(type, ...) beamformer_reduce_a1s2_contrast_##type,
							BEAMFORMER_REDUCE_A1S2_CONTRAST_LIST
							#undef X
						};

						// TODO(rnp): HACK: for some unknown reason loading contrast data after loading
						// non-contrast data causes the dataset to not be stored correctly (it looks
						// like mix of the old and new dataset). Putting this here fixes the issue.
						// Counter-intuitively this improves throughput on my zen4 test computer,
						// however it obviously should not be needed.
						memory_clear(scratch.beg + out_off, 0, out_channel_stride);

						u32 sample_count = bp->sample_count * beamformer_data_kind_element_count[data_kind];
						reduce_a1s2_fn_table[reduce_a1s2_index_map[data_kind]](scratch.beg + out_off,
						                                                       (u8 *)data + in_off,
						                                                       sample_count);
					}break;
					}
				}

				lib_release_lock(BeamformerSharedMemoryLockKind_ScratchSpace);
				/* TODO(rnp): need a better way to communicate this */
				u64 rf_block_rf_size = (u64)block << 32ULL | (u64)size;
				atomic_store_u64(&g_beamformer_library_context.bp->rf_block_rf_size, rf_block_rf_size);
				result = 1;
			}
		}
	}
	return result;
}

b32
beamformer_push_data_with_compute(void *data, u32 data_size, u32 image_plane_tag, u32 parameter_slot)
{
	b32 result = 0;
	if (check_shared_memory()) {
		u32 reserved_blocks = g_beamformer_library_context.bp->reserved_parameter_blocks;
		if (lib_error_check(image_plane_tag < BeamformerViewPlaneTag_Count, InvalidImagePlane) &&
		    lib_error_check(parameter_slot < reserved_blocks, ParameterBlockUnallocated) &&
		    beamformer_push_data_base(data, data_size, g_beamformer_library_context.timeout_ms, parameter_slot))
		{
			BeamformWork *work = try_push_work_queue();
			if (work) {
				work->kind = BeamformerWorkKind_ComputeIndirect;
				work->compute_indirect_context.view_plane      = image_plane_tag;
				work->compute_indirect_context.parameter_block = parameter_slot;
				beamform_work_queue_push_commit(&g_beamformer_library_context.bp->external_work_queue);
				beamformer_flush_commands();
				result = 1;
			}
		}
	}
	return result;
}

b32
beamformer_push_parameters_at(BeamformerParameters *bp, u32 block)
{
	b32 result = check_shared_memory() && validate_parameters(bp);
	if (result) {
		result = parameter_block_region_upload(bp, sizeof(*bp), block,
		                                       BeamformerParameterBlockRegion_Parameters,
		                                       offsetof(BeamformerParameterBlock, parameters),
		                                       g_beamformer_library_context.timeout_ms);
		if (result) {
			BeamformerParameterBlock *pb = beamformer_parameter_block(g_beamformer_library_context.bp, block);
			atomic_or_u32(&pb->region_update_flags, 1u << BeamformerParameterRegionFlag_NotifyUI);
		}
	}
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
	b32 result = check_shared_memory();
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

		if (bp->acquisition_kind == BeamformerAcquisitionKind_UFORCES ||
		    bp->acquisition_kind == BeamformerAcquisitionKind_UHERCULES)
		{
			result &= beamformer_push_sparse_elements_at(bp->sparse_elements, bp->acquisition_count, block);
		}

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
		beamformer_flush_commands();

		if (lib_try_lock(BeamformerSharedMemoryLockKind_ExportSync, timeout_ms)) {
			if (lib_try_lock(BeamformerSharedMemoryLockKind_ScratchSpace, 0)) {
				Arena scratch = beamformer_shared_memory_scratch_arena(g_beamformer_library_context.bp,
				                                                       g_beamformer_library_context.shared_memory_size);
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
	b32 result = beamformer_push_simple_parameters(bp);
	if (result) {
		iv3 output_points = bp->output_points.xyz;
		output_points.E[0] = Max(1, output_points.E[0]);
		output_points.E[1] = Max(1, output_points.E[1]);
		output_points.E[2] = Max(1, output_points.E[2]);

		b32 complex = 0;
		for (u32 stage = 0; stage < bp->compute_stages_count; stage++) {
			BeamformerShaderKind shader = (BeamformerShaderKind)bp->compute_stages[stage];
			complex |= shader == BeamformerShaderKind_Demodulate || shader == BeamformerShaderKind_CudaHilbert;
		}

		iz output_size = output_points.x * output_points.y * output_points.z * (i32)sizeof(f32);
		if (complex) output_size *= 2;

		Arena scratch = beamformer_shared_memory_scratch_arena(g_beamformer_library_context.bp,
		                                                       g_beamformer_library_context.shared_memory_size);
		if (out_data) result &= lib_error_check(output_size <= arena_capacity(&scratch, u8), ExportSpaceOverflow);

		if (result) {
			result = beamformer_push_data_with_compute(data, data_size, 0, 0);
			if (result && out_data) {
				BeamformerExportContext export;
				export.kind = BeamformerExportKind_BeamformedData;
				export.size = (u32)output_size;
				result = beamformer_export(export, out_data, timeout_ms);
			}
		}
	}
	return result;
}

b32
beamformer_compute_timings(BeamformerComputeStatsTable *output, i32 timeout_ms)
{
	b32 result = 0;
	if (check_shared_memory()) {
		Arena scratch = beamformer_shared_memory_scratch_arena(g_beamformer_library_context.bp,
		                                                       g_beamformer_library_context.shared_memory_size);
		if (lib_error_check((iz)sizeof(*output) <= arena_capacity(&scratch, u8), ExportSpaceOverflow)) {
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
		u32 flag = ctz_u64(g_beamformer_library_context.bp->live_imaging_dirty_flags);
		if (flag != 64) {
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
		store_fence();
		result = 1;
	}
	return result;
}
