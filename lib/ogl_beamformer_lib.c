/* See LICENSE for license details. */

// TODO(rnp): lib updates and beamformer usage notes
// VLS/TPW:
// - need to sort based on receive orientation on data intern
//   - AcquisitionCount[orientation]
//   - orientation passed in through push constants
//   - need to calculate channel offset push constant:
//     channel_offset - (channel_offset >= ChannelCount[0] ? ChannelCount[0] : 0);
//   - orientation push const set based on (channel_offset < ChannelCount[0])
//   - sum of acquisition count in each orientation is still a fixed count
// RF Upload:
// - multi frame upload allowed
//   - no handling for it with parameter sets
//     - use per frame parameter set if required
//     - data descriptor compared for compatibility between
//       dataset and parameter set
//   - beamforming side only ever considers 1 frame. only parameter
//     related to data frames is the onetime offset added to the rf_pointer
//     - 1D rf_offset needs to validated. we compute single frame size from
//       the data descriptor and verify (total_size - rf_offset) >= single_frame_size
// - want both explicit RF datasets with handles and
//   current implicit last dataset
// Parameter Sets:
// - parameter sets will be immutable
//   - need some internal fields that are not part of the
//     parameter set the library user passes in
//   - channel mapping, tx_rx_orientations (depending on aquisition kind),
//     and data layouts need to be persistent. everything else can be
//     overwritten once interned by beamformer
// - previously fixed channel count, emission count limits will
//   be removed
// [ ]: remove shared memory region semaphores
//   - these are not needed since only library is touching that region of memory

#define BEAMFORMER_IMPORT static

#ifndef BASE_PLATFORM_H
#define BASE_PLATFORM_NO_MAIN 1
#define BASE_EXPORT static
#endif

#include "../util.h"

#include "../generated/beamformer.c"
#include "ogl_beamformer_lib_base.h"

#if OS_LINUX
#include "../base_linux.c"
#elif OS_WINDOWS
#include "../base_win32.c"

W32(iptr) OpenFileMappingA(u32, b32, c8 *);

#else
#error Unsupported Platform
#endif

#define BEAMFORMER_LIB_SHARED_MEMORY_INCLUDE
#include "../beamformer_compute_stats.c"
#include "../beamformer_shared_memory.c"

global struct {
	BeamformerSharedMemory *bp;
	b32                     new_rf;
	i32                     timeout_ms;
	BeamformerLibErrorKind  last_error;
} g_beamformer_library_context;

#if OS_LINUX

function str8
os_open_shared_memory_area(char *name)
{
	str8 result = {0};
	i32 fd = shm_open(name, O_RDWR, S_IRUSR|S_IWUSR);
	if (fd > 0) {
		struct stat sb;
		if (fstat(fd, &sb) != -1) {
			void *new = mmap(0, sb.st_size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
			if (new != MAP_FAILED) {
				result.data   = new;
				result.length = sb.st_size;
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
	stream_append_str8(&sb, str8(OS_SHARED_MEMORY_NAME "_lock_"));

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

function str8
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

	str8 result = {0};
	iptr h = OpenFileMappingA(FILE_MAP_ALL_ACCESS, 0, name);
	if (h != INVALID_FILE) {
		// NOTE(rnp): a size of 0 maps the whole region, we can determine its size after
		void *new = MapViewOfFile(h, FILE_MAP_ALL_ACCESS, 0, 0, 0);
		if (new &&
		    VirtualQuery(new, &memory_basic_info, sizeof(memory_basic_info)) == sizeof(memory_basic_info) &&
		    os_reserve_region_locks())
		{
			result.data   = new;
			result.length = (i64)memory_basic_info.RegionSize;
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
		str8 shared_memory = os_open_shared_memory_area(OS_SHARED_MEMORY_NAME);
		if (lib_error_check(shared_memory.data != 0, SharedMemory)) {
			BeamformerSharedMemory *bp = (BeamformerSharedMemory *)shared_memory.data;
			result = lib_error_check(bp->version == BEAMFORMER_SHARED_MEMORY_VERSION, VersionMismatch);
			if (result) {
				g_beamformer_library_context.bp = bp;
			} else {
				os_close_shared_memory_area(shared_memory.data, shared_memory.length);
			}
		}
	}

	if likely(g_beamformer_library_context.bp)
		result = lib_error_check(likely(!g_beamformer_library_context.bp->invalid), InvalidAccess);
	return result;
}

function b32
beamformer_lib_validate_rf_offset(u64 offset, BeamformerDataDescriptor data_descriptor)
{
	uv4 dim = data_descriptor.dimensions;
	u64 frame_size = dim.E[BeamformerRFDimension_Channels]
	                 * dim.E[BeamformerRFDimension_Events]
	                 * dim.E[BeamformerRFDimension_Samples]
	                 * beamformer_data_kind_byte_size[data_descriptor.kind];
	b32 result = ((offset % frame_size) == 0) &&
	             (dim.E[BeamformerRFDimension_Frames] > offset / frame_size);
	return result;
}

function b32
beamformer_lib_validate_parameters(BeamformerParameters *bp)
{
	if (!lib_error_check(Between(bp->contrast_mode, 0, BeamformerContrastMode_Count - 1), InvalidContrastMode))
		return 0;

	u32 contrast_raw_sample_count = bp->acquisition_count * bp->sample_count * beamformer_contrast_mode_samples[bp->contrast_mode];
	if (!lib_error_check(contrast_raw_sample_count <= bp->raw_data_dimensions.x, DataSizeMismatch))
		return 0;

	// NOTE(rnp): frame size checks
	{
		// TODO(rnp): this check is overly conservative, what if we are exporting something smaller than Float32Complex
		u64 buffer_size     = g_beamformer_library_context.bp->beamformed_frame_buffer_size;
		u64 frame_size      = Max(1, bp->output_points.x) * Max(1, bp->output_points.y) * Max(1, bp->output_points.z)
		                      * beamformer_data_kind_byte_size[BeamformerDataKind_Float32Complex];
		u64 incoherent_size = frame_size / 2;
		if (bp->coherency_weighting)
			buffer_size -= incoherent_size;

		if (!lib_error_check(frame_size <= buffer_size, FrameSizeOverflow))
			return 0;
	}

	return 1;
}

function b32
beamformer_lib_validate_pipeline(i32 *shaders, u32 shader_count, BeamformerDataKind data_kind)
{
	b32 data_kind_test = Between(data_kind, 0, BeamformerDataKind_Count - 1);
	if (!lib_error_check(data_kind_test, InvalidDataKind))
		return 0;

	if (!lib_error_check(shader_count <= BeamformerMaxComputeShaderStages, ComputeStageOverflow))
		return 0;

	for (u32 i = 0; i < shader_count; i++) {
		b32 stage_test = Between(shaders[i], BeamformerShaderKind_ComputeFirst, BeamformerShaderKind_ComputeLast);
		if (!lib_error_check(stage_test, InvalidComputeStage))
			return 0;

		if (shaders[i] == BeamformerShaderKind_Hilbert &&
		    !lib_error_check(g_beamformer_library_context.bp->capabilities.hilbert != 0, InvalidComputeStage))
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

#define beamformer_shared_memory_transient_alloc(sm, ...) beamformer_shared_memory_transient_alloc_(sm, (ArenaAllocateInfo){.align = 8, .count = 1, __VA_ARGS__})
function BeamformerSharedMemoryAllocationHandle
beamformer_shared_memory_transient_alloc_(BeamformerSharedMemory *sm, ArenaAllocateInfo info)
{
	BeamformerMemoryAllocation *ma = beamformer_page_allocate(&sm->transient, sm, BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE, 0, info);
	BeamformerSharedMemoryAllocationHandle result = {0};
	if (ma) {
		u64 page = (u64)((u8 *)ma - ((u8 *)sm + sm->transient.offset)) / BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE;
		result.v = beamformer_shared_memory_make_handle(BeamformerSharedMemoryHandleKind_Memory, page);
	}
	return result;
}

// TODO(rnp): remove
function BeamformerParameterSetHandle
beamformer_parameter_set_handle_for_block(BeamformerSharedMemory *sm, u32 block)
{
	assert(block < BeamformerMaxParameterBlocks);
	BeamformerParameterSetMetadata *set = sm->parameter_sets + block;
	BeamformerParameterSetHandle result;
	result.v = beamformer_shared_memory_make_handle(BeamformerSharedMemoryHandleKind_ParameterSet,
	                                                (u64)block << 32u | (u64)set->generation);
	return result;
}

function void *
beamformer_parameter_set_persistent_data(BeamformerSharedMemory *sm, BeamformerParameterSetMetadata *set)
{
	const u64 page_size = BEAMFORMER_SHARED_MEMORY_PARAMETER_PAGE_SIZE;
	u8   *base   = (u8 *)sm + sm->persistent.offset + (sm->persistent.page_count - 1) * page_size;
	void *result = base - set->first_page * page_size;
	return result;
}

function void
beamformer_shared_memory_release_persistent_pages(BeamformerSharedMemory *sm, u32 page, u32 count)
{
	BeamformerPageAllocatorContext *pa = &sm->persistent;

	// NOTE(rnp): pages in persistent set refer to pages from end but
	// beamformer_shared_memory_release_pages expects pages from start
	// so this must get reversed
	page = pa->page_count - (page + count);
	u32 free = beamformer_shared_memory_release_pages((u8 *)sm + pa->offset, pa->first_free_page,
	                                                  page, count, BEAMFORMER_SHARED_MEMORY_PARAMETER_PAGE_SIZE);

	pa->first_free_page = free;
}

function b32
beamformer_parameter_set_validate(BeamformerParameterSet *ps)
{
	b32 result = Between(ps->acquisition_kind, 0, BeamformerAcquisitionKind_Count);

	result &= ps->channel_mapping       != 0;
	result &= ps->transducer_transforms != 0;

	result &= (ps->tilting_angles == 0 && ps->origin_offsets == 0) ||
	          (ps->tilting_angles != 0 && ps->origin_offsets == 0) ||
	          (ps->tilting_angles == 0 && ps->origin_offsets != 0);

	u64 flags = beamformer_acquisition_kind_flags[ps->acquisition_kind];
	if (flags & BeamformerAcquisitionKindFlags_MultiDepth)
		result &= ps->focus_depths != 0;
	if (flags & BeamformerAcquisitionKindFlags_MultiSteering)
		result &= (ps->tilting_angles != 0 || ps->origin_offsets != 0);
	if (flags & BeamformerAcquisitionKindFlags_MultiOrientation)
		result &= ps->transmit_receive_orientations != 0;
	if (flags & BeamformerAcquisitionKindFlags_Sparse)
		result &= ps->sparse_elements != 0;

	result &= beamformer_lib_validate_pipeline((i32 *)ps->compute_stages, ps->compute_stages_count, ps->data_description.kind);

	return result;
}

function u64
beamformer_shared_memory_first_free_parameter_set(BeamformerSharedMemory *sm)
{
	u64 result = bitset_find_first_clear(sm->parameter_sets_used_masks, countof(sm->parameter_sets_used_masks));
	if (result >= BeamformerMaxParameterSets)
		result = ~0ULL;
	return result;
}

function b32
beamformer_push_parameter_set_at(BeamformerParameterSet *ps, u32 set_index)
{
	b32 result = check_shared_memory();
	if (result) {
		BeamformerSharedMemory *sm = g_beamformer_library_context.bp;

		u64 flags = beamformer_acquisition_kind_flags[ps->acquisition_kind];

		BeamformerDataDescriptor *dd = &ps->data_description;

		BeamformerParameterSetFlat sf[1] = {{beamformer_parameter_set_dynamic_field_init_literal}};

		static_assert(offsetof(BeamformerParameterSetFlat, compute_stage_parameters) + sizeof(sf->compute_stage_parameters)
		              == offsetof(BeamformerParameterSetFlat, transducer_transforms_offset), "Hey Stinky!");
		memory_copy(sf, ps, offsetof(BeamformerParameterSetFlat, transducer_transforms_offset));

		u32 offset = AlignUpPowerOfTwo(sizeof(BeamformerParameterSetFlat), 16);
		sf->transducer_transforms_offset = offset;
		offset += ps->transducer_tile_count[0] * ps->transducer_tile_count[1] * sizeof(*ps->transducer_transforms);

		if (flags & BeamformerAcquisitionKindFlags_Sparse) {
			sf->sparse_elements_offset += AlignUpPowerOfTwo(offset, 16);
			offset += dd->dimensions.E[BeamformerRFDimension_Events] * sizeof(*ps->sparse_elements);
		}

		if (flags & BeamformerAcquisitionKindFlags_MultiDepth) {
			sf->focus_depths_offset = AlignUpPowerOfTwo(offset, 16);
			offset += dd->dimensions.E[BeamformerRFDimension_Events] * sizeof(*ps->focus_depths);
		}

		if (flags & BeamformerAcquisitionKindFlags_MultiSteering) {
			// NOTE(rnp): we only give space for either origin offsets or tilting angles
			static_assert(sizeof(*ps->origin_offsets) == sizeof(*ps->tilting_angles), "");
			b32 vls = ps->acquisition_kind == BeamformerAcquisitionKind_RCA_VLS;
			if (vls) sf->origin_offsets_offset = AlignUpPowerOfTwo(offset, 16);
			else     sf->tilting_angles_offset = AlignUpPowerOfTwo(offset, 16);
			offset += dd->dimensions.E[BeamformerRFDimension_Events] * sizeof(*ps->origin_offsets);
		}

		// TODO(rnp): delete with above change
		if (flags & BeamformerAcquisitionKindFlags_MultiOrientation) {
			sf->transmit_receive_orientations_offset = AlignUpPowerOfTwo(offset, 16);
			offset += dd->dimensions.E[BeamformerRFDimension_Events] * sizeof(u8);
		}

		u64 persistent_bytes = beamformer_parameter_set_persistent_size(dd->dimensions.E[BeamformerRFDimension_Channels],
		                                                                dd->dimensions.E[BeamformerRFDimension_Events]);

		BeamformerParameterSetMetadata *set = sm->parameter_sets + set_index;
		if (set->page_count) {
			beamformer_shared_memory_release_persistent_pages(sm, set->first_page, set->page_count);
			set->first_page = -1u;
			set->page_count = 0;
		}

		u32 persistent_page  = beamformer_shared_memory_persistent_alloc(sm, persistent_bytes);
		u32 persistent_count = DivRoundUp(persistent_bytes, BEAMFORMER_SHARED_MEMORY_PARAMETER_PAGE_SIZE);
		BeamformerSharedMemoryAllocationHandle transient_handle = {0};
		if (persistent_page != -1u)
			transient_handle = beamformer_shared_memory_transient_alloc(sm, .size = offset, .align = alignof(BeamformerParameterSetFlat));

		result = transient_handle.v != 0;
		if (!transient_handle.v) {
			beamformer_shared_memory_release_persistent_pages(sm, persistent_page, persistent_count);
		} else {
			set->generation++;
			set->first_page = persistent_page;
			set->page_count = persistent_count;
			set->release_requested = 0;

			set->data_description = ps->data_description;

			////////////////////////////
			// NOTE(rnp): persistent data
			{
				u8 *persistent_data = beamformer_parameter_set_persistent_data(sm, set);
				memory_copy(persistent_data, ps->channel_mapping, sizeof(*ps->channel_mapping) * dd->dimensions.E[BeamformerRFDimension_Channels]);

				// NOTE(rnp): build acquisition table
				// TODO(rnp): remap table for acquisitions. use below for storing focus and in push_data
			}

			////////////////////////////
			// NOTE(rnp): transient data
			{
				BeamformerParameterSetFlat *nf = beamformer_shared_memory_allocation_pointer(sm, transient_handle);
				memory_copy(nf, sf, sizeof(*nf));

				u32 events = dd->dimensions.E[BeamformerRFDimension_Events];
				u32 transducer_tile_count = ps->transducer_tile_count[0] * ps->transducer_tile_count[1];
				beamformer_parameter_set_flat_append_dynamic_data(nf, ps, events, transducer_tile_count);
			}

			BeamformerSharedMemoryAllocationHandle old_handle;
			old_handle.v = atomic_swap_u64(&set->transient_handle.v, transient_handle.v);

			if (old_handle.v) beamformer_shared_memory_allocation_release(sm, old_handle);
		}
	}
	return result;
}

function b32
beamformer_shared_memory_register_parameters(BeamformerParameterSet *ps, BeamformerParameterSetHandle *out_handle)
{
	b32 result = check_shared_memory();
	if (result) {
		BeamformerSharedMemory *sm = g_beamformer_library_context.bp;

		u64 set_index = beamformer_shared_memory_first_free_parameter_set(sm);
		if (lib_error_check(set_index != ~0UL, ParameterSetHandlesExhausted) &&
		    lib_error_check(beamformer_parameter_set_validate(ps), ParameterSetInvalid))
		{
			if (beamformer_push_parameter_set_at(ps, set_index)) {
				bitset_set(sm->parameter_sets_used_masks, countof(sm->parameter_sets_used_masks), set_index);

				u32 generation = sm->parameter_sets[set_index].generation;
				static_assert(BeamformerMaxParameterSets <= U16_MAX, "");
				out_handle->v = beamformer_shared_memory_make_handle(BeamformerSharedMemoryHandleKind_ParameterSet,
				                                                     (u64)set_index << 32u | (u64)generation);
			}
		}
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
	return error_string_table[Min(kind, countof(error_string_table) - 1)];
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

u64
beamformer_maximum_rf_data_size(void)
{
	u64 result = U64_MAX;
	if (check_shared_memory()) {
		u64 transient_size = g_beamformer_library_context.bp->transient.page_count
		                     * BEAMFORMER_SHARED_MEMORY_TRANSIENT_PAGE_SIZE;
		result = Min(transient_size, g_beamformer_library_context.bp->capabilities.max_rf_data_size);
	}
	return result;
}

u64
beamformer_maximum_frames_for_parameters(BeamformerParameters *bp)
{
	u64 result = U64_MAX;
	if (check_shared_memory() && beamformer_lib_validate_parameters(bp)) {
		// TODO(rnp): overly conservative frame size check
		u64 buffer_size     = g_beamformer_library_context.bp->beamformed_frame_buffer_size;
		u64 frame_size      = Max(1, bp->output_points.x) * Max(1, bp->output_points.y) * Max(1, bp->output_points.z)
		                      * beamformer_data_kind_byte_size[BeamformerDataKind_Float32Complex];
		u64 incoherent_size = frame_size / 2;
		if (bp->coherency_weighting)
			buffer_size -= incoherent_size;
		result = buffer_size / frame_size;
	}
	return result;
}

u64
beamformer_maximum_frames_for_simple_parameters(BeamformerSimpleParameters *bp)
{
	u64 result = beamformer_maximum_frames_for_parameters((BeamformerParameters *)bp);
	return result;
}

b32
beamformer_create_filter(BeamformerFilterParameters *filter, u8 filter_slot, u8 parameter_block)
{
	b32 result = 0;
	if (lib_error_check(filter->kind >= 0 && filter->kind < BeamformerFilterKind_Count, InvalidFilterKind)) {
		if (check_shared_memory()) {
			BeamformWork *work = try_push_work_queue();
			if (work) {
				BeamformerSharedMemory *sm = g_beamformer_library_context.bp;
				BeamformerCreateFilterContext *ctx = &work->create_filter_context;
				work->kind = BeamformerWorkKind_CreateFilter;
				ctx->parameters  = *filter;
				ctx->filter_slot = filter_slot % BeamformerFilterSlots;
				ctx->set_handle  = beamformer_parameter_set_handle_for_block(sm, parameter_block % BeamformerMaxParameterBlocks);
				beamform_work_queue_push_commit(&sm->external_work_queue);
				result = 1;
			}
		}
	}
	return result;
}

function void
beamformer_flush_commands(void)
{
	i32 lock = BeamformerSharedMemoryLockKind_DispatchCompute;
	beamformer_shared_memory_take_lock(g_beamformer_library_context.bp, lock, 0);
}

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

function u64
beamformer_data_descriptor_byte_size(BeamformerDataDescriptor dd)
{
	u32 d[4];
	static_assert(sizeof(d) == sizeof(dd.dimensions), "");
	for EachElement(dd.dimensions.E, it)
		d[it] = Max(1, dd.dimensions.E[it]);

	u64 result = d[0] * d[1] * d[2] * d[3] * beamformer_data_kind_byte_size[dd.kind];
	return result;
}

function b32
beamformer_push_data_base(const void *data, BeamformerSharedMemory *sm, i32 timeout_ms,
                          const BeamformerDataDescriptor *raw_description,
                          const BeamformerDataDescriptor *flat_description,
                          const u16 *channel_mapping)
{
	b32 result = 0;
	BeamformerDataKind data_kind = raw_description->kind;

	u64 max_rf_size = sm->capabilities.max_rf_data_size;
	u64 rf_size     = beamformer_data_descriptor_byte_size(*flat_description);
	u64 raw_size    = beamformer_data_descriptor_byte_size(*raw_description);

	// TODO(rnp): support multi push upload so that max_rf_size is actual limit
	if (lib_try_lock(BeamformerSharedMemoryLockKind_UploadRF, timeout_ms)) {
		BeamformerSharedMemoryAllocationHandle rf_handle = beamformer_shared_memory_transient_alloc(sm, .size = rf_size, .align = BEAMFORMER_RF_ALIGN);
		if (lib_error_check(rf_handle.v != 0, TransientSpaceOverflow) &&
		    lib_error_check(rf_size <= max_rf_size, RFDataSizeOverflow) &&
		    lib_error_check(rf_size <= raw_size, DataSizeMismatch))
		{
			u32 sample_count  = flat_description->dimensions.E[BeamformerRFDimension_Samples];
			u32 event_count   = flat_description->dimensions.E[BeamformerRFDimension_Events];
			u32 channel_count = flat_description->dimensions.E[BeamformerRFDimension_Channels];

			u32 out_channel_stride = sample_count * event_count   * beamformer_data_kind_byte_size[data_kind];
			u32 in_channel_stride  = raw_description->dimensions.x * beamformer_data_kind_byte_size[data_kind];

			u8 *base = beamformer_shared_memory_allocation_pointer(sm, rf_handle);
			for (u32 channel = 0; channel < channel_count; channel++) {
				u32 out_off = out_channel_stride * channel;
				u32 in_off  = in_channel_stride  * channel_mapping[channel];

				u8 *memory = base + out_off;
				switch (raw_description->contrast_mode) {
				default:{
					/* NOTE(rnp): non temporal copy would be better, but we can't ensure
					 * 64 byte boundaries. */
					memory_copy(memory, (u8 *)data + in_off, out_channel_stride);
				}break;

				case BeamformerContrastMode_A1S2:{
					read_only u8 reduce_a1s2_index_map[] = {
						[BeamformerDataKind_Int16]          = 0,
						[BeamformerDataKind_Int16Complex]   = 0,
						[BeamformerDataKind_Float32]        = 1,
						[BeamformerDataKind_Float32Complex] = 1,
						[BeamformerDataKind_Float16]        = 2,
						[BeamformerDataKind_Float16Complex] = 2,
					};
					static_assert(BeamformerDataKind_Float16Complex == (BeamformerDataKind_Count - 1), "");

					local_persist beamformer_reduce_a1s2_contrast_fn *reduce_a1s2_fn_table[] = {
						#define X(type, ...) beamformer_reduce_a1s2_contrast_##type,
						BEAMFORMER_REDUCE_A1S2_CONTRAST_LIST
						#undef X
					};
					beamformer_reduce_a1s2_contrast_fn *reduce = reduce_a1s2_fn_table[reduce_a1s2_index_map[data_kind]];

					u32 flat_sample_count = sample_count * beamformer_data_kind_element_count[data_kind];
					u32 event_stride      = sample_count * beamformer_data_kind_byte_size[data_kind];
					u32 in_stride         = event_stride * beamformer_contrast_mode_samples[raw_description->contrast_mode];
					for (u32 event = 0; event < event_count; event++) {
						void *out = memory + event * event_stride;
						void *in  = (u8 *)data + in_off + in_stride * event;
						reduce(out, in, flat_sample_count);
					}
				}break;
				}
			}

			/* TODO(rnp): need a better way to communicate this */
			atomic_store_u64(&sm->rf_handle.v, rf_handle.v);
			g_beamformer_library_context.new_rf = 1;
			result = 1;
		}

		if (!result && rf_handle.v) {
			beamformer_shared_memory_allocation_release(sm, rf_handle);
			lib_release_lock(BeamformerSharedMemoryLockKind_UploadRF);
		}
	}
	return result;
}

function b32
beamformer_push_data(const void *data, const BeamformerDataDescriptor *raw_descriptor, BeamformerParameterSetHandle set_handle)
{
	b32 result = 0;
	if (check_shared_memory()) {
		BeamformerSharedMemory         *sm   = g_beamformer_library_context.bp;
		BeamformerParameterSetMetadata *meta = beamformer_parameter_set_metadata_from_handle(sm, set_handle);
		if (lib_error_check(meta != 0, ParameterSetInvalid)) {
			result = beamformer_push_data_base(data, sm, 0, raw_descriptor, &meta->data_description,
			                                   beamformer_parameter_set_persistent_data(sm, meta));
		}
	}
	return result;
}

function b32
beamformer_start_compute_base(const BeamformerPushParameters *pp, BeamformerParameterSetHandle set_handle,
                              BeamformerParameterSetMetadata *meta)
{
	b32 result = 0;
	if (lib_error_check(pp->view_plane_tag < BeamformerViewPlaneTag_Count, InvalidImagePlane) &&
	    lib_error_check(beamformer_lib_validate_rf_offset(pp->rf_offset, meta->data_description), RFOffsetInvalid))
	{
		BeamformWork *work = try_push_work_queue();
		if (work) {
			b32 new_rf = g_beamformer_library_context.new_rf;
			g_beamformer_library_context.new_rf = 0;
			work->kind = new_rf ? BeamformerWorkKind_WaitThenCompute : BeamformerWorkKind_Compute;
			memory_copy(&work->compute_context.parameters, pp, sizeof(*pp));
			work->compute_context.set_handle = set_handle;
			beamform_work_queue_push_commit(&g_beamformer_library_context.bp->external_work_queue);
			beamformer_flush_commands();
			result = 1;
		}
	}
	return result;
}

function b32
beamformer_start_compute(const BeamformerPushParameters *pp, BeamformerParameterSetHandle handle)
{
	b32 result = 0;
	if (check_shared_memory()) {
		BeamformerSharedMemory         *sm   = g_beamformer_library_context.bp;
		BeamformerParameterSetMetadata *meta = beamformer_parameter_set_metadata_from_handle(sm, handle);
		if (lib_error_check(meta != 0, ParameterSetInvalid))
			result = beamformer_start_compute_base(pp, handle, meta);
	}
	return result;
}

BEAMFORMER_LIB_EXPORT b32
beamformer_push_data_with_compute(const void *data,
                                  const BeamformerDataDescriptor *raw_descriptor,
                                  const BeamformerPushParameters *pp,
                                  BeamformerParameterSetHandle handle)
{
	b32 result = 0;
	if (check_shared_memory()) {
		BeamformerSharedMemory         *sm   = g_beamformer_library_context.bp;
		BeamformerParameterSetMetadata *meta = beamformer_parameter_set_metadata_from_handle(sm, handle);
		if (lib_error_check(meta != 0, ParameterSetInvalid)) {
			if (beamformer_push_data_base(data, sm, 0, raw_descriptor, &meta->data_description,
			                              beamformer_parameter_set_persistent_data(sm, meta)))
			{
				result = beamformer_start_compute_base(pp, handle, meta);
			}
		}
	}
	return result;
}

function void
beamformer_parameter_set_from_simple(BeamformerParameterSet *set, BeamformerSimpleParameters *bp)
{
	set->data_description.layout = BeamformerDataLayout_ChannelEventSample;
	set->data_description.kind   = bp->data_kind;
	set->data_description.contrast_mode = bp->contrast_mode;
	set->data_description.dimensions.E[BeamformerRFDimension_Samples]  = bp->sample_count;
	set->data_description.dimensions.E[BeamformerRFDimension_Events]   = bp->acquisition_count;
	set->data_description.dimensions.E[BeamformerRFDimension_Channels] = bp->channel_count;
	set->data_description.dimensions.E[BeamformerRFDimension_Frames]   = 1;

	set->transducer_channel_count[0] = bp->channel_count;
	set->transducer_channel_count[1] = bp->channel_count;
	set->transducer_tile_count[0] = 1;
	set->transducer_tile_count[1] = 1;
	set->transducer_element_pitch = bp->xdc_element_pitch;
	set->output_points            = bp->output_points;
	set->acquisition_kind         = bp->acquisition_kind;
	set->decode_mode              = bp->decode_mode;
	set->sampling_mode            = bp->sampling_mode;
	set->sampling_frequency       = bp->sampling_frequency;
	set->demodulation_frequency   = bp->demodulation_frequency;
	set->decimation_rate          = bp->decimation_rate;
	set->speed_of_sound           = bp->speed_of_sound;
	set->time_offset              = bp->time_offset;
	set->f_number                 = bp->f_number;
	set->interpolation_mode       = bp->interpolation_mode;
	set->coherency_weighting      = bp->coherency_weighting;
	set->readi_group_count        = bp->readi_group_count;
	set->emission_parameters      = bp->emission_parameters;
	set->transducer_transforms    = &bp->xdc_transform;
	set->sparse_elements          = (u16 *)bp->sparse_elements;
	set->channel_mapping          = (u16 *)bp->channel_mapping;
	set->transmit_receive_orientations = bp->transmit_receive_orientations;
	set->focus_depths             = bp->focal_depths;

	// TODO(rnp): dynamic
	memory_copy(set->compute_stages, bp->compute_stages, sizeof(bp->compute_stages));
	memory_copy(set->compute_stage_parameters, bp->compute_stage_parameters, sizeof(bp->compute_stage_parameters));
	set->compute_stages_count = bp->compute_stages_count;
}

b32
beamformer_push_simple_parameters_at(BeamformerSimpleParameters *bp, u32 block)
{
	b32 result = check_shared_memory();
	if (result) {
		BeamformerParameterSet set = {0};
		beamformer_parameter_set_from_simple(&set, bp);

		f32 origin_offsets[BeamformerMaxEmissionsCount];
		if (bp->acquisition_kind == BeamformerAcquisitionKind_RCA_VLS) {
			set.origin_offsets = origin_offsets;
			for EachIndex(bp->acquisition_count, it) {
				f32 depth = bp->focal_depths[it];
				f32 angle = bp->steering_angles[it];
				// TODO(rnp): sign?
				origin_offsets[it] = depth * tan_f32(angle * PI / 180.f);
			}
		} else if (bp->acquisition_kind == BeamformerAcquisitionKind_RCA_TPW) {
			set.tilting_angles = bp->steering_angles;
		}

		result = beamformer_push_parameter_set_at(&set, block);
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
			BeamformerSharedMemory *sm = g_beamformer_library_context.bp;
			memory_copy(out, beamformer_shared_memory_allocation_pointer(sm, export.memory),
			            beamformer_shared_memory_allocation_size(sm, export.memory));
			lib_release_lock(BeamformerSharedMemoryLockKind_ExportSync);
			result = 1;
		}
	}
	return result;
}

BEAMFORMER_LIB_EXPORT b32
beamformer_get_last_frames(void *out_data, u64 out_data_size, u32 count)
{
	BeamformerExportContext export = {0};
	export.kind  = BeamformerExportKind_BeamformedData;
	export.count = count;
	if (out_data_size) {
		export.memory = beamformer_shared_memory_transient_alloc(g_beamformer_library_context.bp,
		                                                         .size = out_data_size, .align = 64);
	}

	b32 result = lib_error_check(export.memory.v != 0, TransientSpaceOverflow) && count;
	if (result)
		result = beamformer_export(export, out_data, g_beamformer_library_context.timeout_ms);
	if (export.memory.v)
		beamformer_shared_memory_allocation_release(g_beamformer_library_context.bp, export.memory);

	return result;
}

b32
beamformer_beamform_data(BeamformerSimpleParameters *bp, void *data, uint32_t data_size,
                         void *out_data, int32_t timeout_ms)
{
	b32 result = beamformer_push_simple_parameters(bp);
	if (result) {
		BeamformerSharedMemory *sm = g_beamformer_library_context.bp;
		beamformer_set_global_timeout(timeout_ms);

		iv3 output_points = bp->output_points.xyz;
		output_points.E[0] = Max(1, output_points.E[0]);
		output_points.E[1] = Max(1, output_points.E[1]);
		output_points.E[2] = Max(1, output_points.E[2]);

		b32 complex = 0;
		for (u32 stage = 0; stage < bp->compute_stages_count; stage++) {
			BeamformerShaderKind shader = (BeamformerShaderKind)bp->compute_stages[stage];
			complex |= shader == BeamformerShaderKind_Demodulate || shader == BeamformerShaderKind_Hilbert;
		}

		u64 output_size = output_points.x * output_points.y * output_points.z * sizeof(f32);
		if (complex) output_size *= 2;

		BeamformerParameterSetHandle    set_handle = beamformer_parameter_set_handle_for_block(sm, 0);
		BeamformerParameterSetMetadata *set_meta   = beamformer_parameter_set_metadata_from_handle(sm, set_handle);

		BeamformerDataDescriptor raw_descriptor = {
			.kind          = bp->data_kind,
			.layout        = BeamformerDataLayout_RawBuffer,
			.contrast_mode = bp->contrast_mode,
			.dimensions.x  = bp->raw_data_dimensions.x,
			.dimensions.y  = bp->raw_data_dimensions.y,
			.dimensions.z  = 1,
			.dimensions.w  = 1,
		};

		result = beamformer_push_data_base(data, sm, 0, &raw_descriptor, &set_meta->data_description,
		                                   beamformer_parameter_set_persistent_data(sm, set_meta));
		if (result) {
			BeamformerPushParameters push = {
				.focus_depth                  = bp->single_focus ? bp->focal_vector.y : bp->focal_depths[0],
				.steering_angle               = bp->single_focus ? bp->focal_vector.x : bp->steering_angles[0],
				.transmit_receive_orientation = bp->single_orientation ? bp->transmit_receive_orientation
				                                                       : bp->transmit_receive_orientations[0],
				.view_plane_tag               = 0,
			};
			memory_copy(&push.voxel_transform, &bp->das_voxel_transform, sizeof(push.voxel_transform));
			result = beamformer_start_compute_base(&push, set_handle, set_meta);
		}

		if (result && out_data)
			result = beamformer_get_last_frames(out_data, output_size, 1);
	}
	return result;
}

function b32
beamformer_compute_timings(BeamformerComputeStatsTable *output, i32 timeout_ms)
{
	b32 result = 0;
	if (check_shared_memory()) {
		BeamformerExportContext export = {0};
		export.kind   = BeamformerExportKind_Stats;
		export.memory = beamformer_shared_memory_transient_alloc(g_beamformer_library_context.bp,
		                                                         .size  = sizeof(BeamformerComputeStatsTable),
		                                                         .align = alignof(BeamformerComputeStatsTable));
		if (lib_error_check(export.memory.v != 0, TransientSpaceOverflow)) {
			result = beamformer_export(export, output, timeout_ms);
			beamformer_shared_memory_allocation_release(g_beamformer_library_context.bp, export.memory);
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
		memory_copy(&g_beamformer_library_context.bp->live_imaging_parameters, new, sizeof(*new));
		store_fence();
		result = 1;
	}
	return result;
}
