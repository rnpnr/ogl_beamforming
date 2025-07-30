/* See LICENSE for license details. */
#include "../compiler.h"

#include "../util.h"
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

#include "../beamformer_work_queue.c"

global struct {
	SharedMemoryRegion      shared_memory;
	BeamformerSharedMemory *bp;
	i32                     timeout_ms;
	BeamformerLibErrorKind  last_error;
} g_beamformer_library_context;

#if OS_LINUX

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

function SharedMemoryRegion
os_open_shared_memory_area(char *name)
{
	SharedMemoryRegion result = {0};
	iptr h = OpenFileMappingA(FILE_MAP_ALL_ACCESS, 0, name);
	if (h != INVALID_FILE) {
		void *new = MapViewOfFile(h, FILE_MAP_ALL_ACCESS, 0, 0, BEAMFORMER_SHARED_MEMORY_SIZE);
		if (new) {
			u8 buffer[1024];
			Stream sb = {.data = buffer, .cap = 1024};
			stream_append_s8s(&sb, c_str_to_s8(name), s8("_lock_"));
			local_persist iptr semaphores[BeamformerSharedMemoryLockKind_Count];
			local_persist w32_shared_memory_context ctx = {.semaphores = semaphores};
			b32 all_semaphores = 1;
			for (i32 i = 0; i < countof(semaphores); i++) {
				Stream lb = sb;
				stream_append_i64(&lb, i);
				stream_append_byte(&lb, 0);
				semaphores[i] = CreateSemaphoreA(0, 1, 1, (c8 *)lb.data);
				all_semaphores &= semaphores[i] != INVALID_FILE;
			}
			if (all_semaphores) {
				result.region     = new;
				result.os_context = (iptr)&ctx;
			}
		}
		CloseHandle(h);
	}
	return result;
}

#endif

function b32
check_shared_memory(void)
{
	b32 result = 1;
	if (!g_beamformer_library_context.shared_memory.region) {
		g_beamformer_library_context.shared_memory = os_open_shared_memory_area(OS_SHARED_MEMORY_NAME);
		if (!g_beamformer_library_context.shared_memory.region) {
			g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_SHARED_MEMORY;
			result = 0;
		} else if (((BeamformerSharedMemory *)g_beamformer_library_context.shared_memory.region)->version !=
		           BEAMFORMER_SHARED_MEMORY_VERSION)
		{
			g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_VERSION_MISMATCH;
			result = 0;
		}
	}
	if (result && ((BeamformerSharedMemory *)g_beamformer_library_context.shared_memory.region)->invalid) {
		g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_INVALID_ACCESS;
		result = 0;
	}
	if (result) g_beamformer_library_context.bp = g_beamformer_library_context.shared_memory.region;
	return result;
}

function BeamformWork *
try_push_work_queue(void)
{
	BeamformWork *result = beamform_work_queue_push(&g_beamformer_library_context.bp->external_work_queue);
	if (!result) g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_WORK_QUEUE_FULL;
	return result;
}

function b32
lib_try_lock(BeamformerSharedMemoryLockKind lock, i32 timeout_ms)
{
	b32 result = os_shared_memory_region_lock(&g_beamformer_library_context.shared_memory,
	                                          g_beamformer_library_context.bp->locks,
	                                          (i32)lock, (u32)timeout_ms);
	if (!result) g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_SYNC_VARIABLE;
	return result;
}

function void
lib_release_lock(BeamformerSharedMemoryLockKind lock)
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
	b32 result = timeout_ms >= -1;
	if (result) {
		g_beamformer_library_context.timeout_ms = timeout_ms;
	} else {
		g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_INVALID_TIMEOUT;
	}
	return result;
}

function b32
validate_pipeline(i32 *shaders, i32 shader_count, BeamformerDataKind data_kind)
{
	b32 result = shader_count <= countof(g_beamformer_library_context.bp->shaders);
	if (result) {
		for (i32 i = 0; i < shader_count; i++)
			result &= BETWEEN(shaders[i], 0, BeamformerShaderKind_ComputeCount);
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
	} else {
		g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_COMPUTE_STAGE_OVERFLOW;
	}
	return result;
}

b32
beamformer_set_pipeline_stage_parameters(i32 stage_index, i32 parameter)
{
	b32 result = 0;
	BeamformerSharedMemoryLockKind lock = BeamformerSharedMemoryLockKind_ComputePipeline;
	if (check_shared_memory() && g_beamformer_library_context.bp->shader_count != 0 &&
	    lib_try_lock(lock, g_beamformer_library_context.timeout_ms))
	{
		stage_index %= (i32)g_beamformer_library_context.bp->shader_count;
		g_beamformer_library_context.bp->shader_parameters[stage_index].filter_slot = (u8)parameter;
		atomic_or_u32(&g_beamformer_library_context.bp->dirty_regions, 1 << (lock - 1));
		lib_release_lock(lock);
	}
	return result;
}

b32
beamformer_push_pipeline(i32 *shaders, i32 shader_count, BeamformerDataKind data_kind)
{
	b32 result = 0;
	if (validate_pipeline(shaders, shader_count, data_kind) && check_shared_memory()) {
		BeamformerSharedMemoryLockKind lock = BeamformerSharedMemoryLockKind_ComputePipeline;
		if (lib_try_lock(lock, g_beamformer_library_context.timeout_ms)) {
			g_beamformer_library_context.bp->shader_count = shader_count;
			g_beamformer_library_context.bp->data_kind    = data_kind;
			for (i32 i = 0; i < shader_count; i++)
				g_beamformer_library_context.bp->shaders[i] = (BeamformerShaderKind)shaders[i];
			atomic_or_u32(&g_beamformer_library_context.bp->dirty_regions, 1 << (lock - 1));
			lib_release_lock(lock);
			result = 1;
		}
	}
	return result;
}

b32
beamformer_create_kaiser_low_pass_filter(f32 beta, f32 cutoff_frequency, i16 length, u8 slot)
{
	b32 result = 0;
	if (check_shared_memory()) {
		BeamformWork *work = try_push_work_queue();
		result = work != 0;
		if (result) {
			BeamformerCreateFilterContext *ctx = &work->create_filter_context;
			work->kind            = BeamformerWorkKind_CreateFilter;
			ctx->kind             = BeamformerFilterKind_Kaiser;
			ctx->cutoff_frequency = cutoff_frequency;
			ctx->beta             = beta;
			ctx->length           = length;
			ctx->slot             = slot % BEAMFORMER_FILTER_SLOTS;
			beamform_work_queue_push_commit(&g_beamformer_library_context.bp->external_work_queue);
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
beamformer_compute_indirect(BeamformerViewPlaneTag tag)
{
	b32 result = check_shared_memory();
	if (result) {
		result = tag < BeamformerViewPlaneTag_Count;
		if (result) {
			BeamformWork *work = try_push_work_queue();
			result = work != 0;
			if (result) {
				work->kind = BeamformerWorkKind_ComputeIndirect;
				work->compute_indirect_plane = tag;
				beamform_work_queue_push_commit(&g_beamformer_library_context.bp->external_work_queue);
				beamformer_flush_commands(0);
			}
		} else {
			g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_INVALID_IMAGE_PLANE;
		}
	}
	return result;
}

b32
beamformer_start_compute(void)
{
	b32 result = beamformer_compute_indirect(0);
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

function b32
locked_region_upload(void *region, void *data, u32 size, BeamformerSharedMemoryLockKind lock,
                     b32 *dirty, i32 timeout_ms)
{
	b32 result = lib_try_lock(lock, timeout_ms);
	if (result) {
		if (dirty) *dirty = is_shared_memory_region_dirty(g_beamformer_library_context.bp, (i32)lock);
		mem_copy(region, data, size);
		mark_shared_memory_region_dirty(g_beamformer_library_context.bp, (i32)lock);
		lib_release_lock(lock);
	}
	return result;
}

function b32
beamformer_upload_buffer(void *data, u32 size, i32 store_offset, BeamformerUploadKind kind,
                         BeamformerSharedMemoryLockKind lock, i32 timeout_ms)
{
	b32 result = 0;
	if (check_shared_memory()) {
		BeamformWork *work = try_push_work_queue();
		b32 dirty = 0;
		result = work && locked_region_upload((u8 *)g_beamformer_library_context.bp + store_offset,
		                                      data, size, lock, &dirty, timeout_ms);
		if (result && !dirty) {
			work->upload_context.shared_memory_offset = store_offset;
			work->upload_context.kind = kind;
			work->upload_context.size = size;
			work->kind = BeamformerWorkKind_UploadBuffer;
			work->lock = lock;
			beamform_work_queue_push_commit(&g_beamformer_library_context.bp->external_work_queue);
		}
	}
	return result;
}

#define BEAMFORMER_UPLOAD_FNS \
	X(channel_mapping, i16, 1, ChannelMapping) \
	X(sparse_elements, i16, 1, SparseElements) \
	X(focal_vectors,   f32, 2, FocalVectors)

#define X(name, dtype, elements, lock_name) \
b32 beamformer_push_##name (dtype *data, u32 count) { \
	b32 result = 0; \
	if (count <= countof(g_beamformer_library_context.bp->name)) { \
		result = beamformer_upload_buffer(data, count * elements * sizeof(dtype), \
		                                  offsetof(BeamformerSharedMemory, name), \
		                                  BeamformerUploadKind_##lock_name,       \
		                                  BeamformerSharedMemoryLockKind_##lock_name, \
		                                  g_beamformer_library_context.timeout_ms); \
	} else { \
		g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_BUFFER_OVERFLOW; \
	} \
	return result; \
}
BEAMFORMER_UPLOAD_FNS
#undef X

function b32
beamformer_push_data_base(void *data, u32 data_size, i32 timeout_ms)
{
	b32 result = 0;
	if (data_size <= BEAMFORMER_MAX_RF_DATA_SIZE) {
		if (lib_try_lock(BeamformerSharedMemoryLockKind_UploadRF, timeout_ms)) {
			result = locked_region_upload((u8 *)g_beamformer_library_context.bp + BEAMFORMER_SCRATCH_OFF,
			                              data, data_size, BeamformerSharedMemoryLockKind_ScratchSpace,
			                              0, 0);
			/* TODO(rnp): need a better way to communicate this */
			if (result) g_beamformer_library_context.bp->scratch_rf_size = data_size;
		}
	} else {
		g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_BUFFER_OVERFLOW;
	}
	return result;
}

b32
beamformer_push_data(void *data, u32 data_size)
{
	return beamformer_push_data_base(data, data_size, g_beamformer_library_context.timeout_ms);
}

b32
beamformer_push_data_with_compute(void *data, u32 data_size, u32 image_plane_tag)
{
	b32 result = beamformer_push_data_base(data, data_size, g_beamformer_library_context.timeout_ms);
	if (result) result = beamformer_compute_indirect(image_plane_tag);
	return result;
}

b32
beamformer_push_parameters(BeamformerParameters *bp)
{
	b32 result = 0;
	if (check_shared_memory()) {
		result = locked_region_upload((u8 *)g_beamformer_library_context.bp +
		                              offsetof(BeamformerSharedMemory, parameters),
		                              bp, sizeof(*bp), BeamformerSharedMemoryLockKind_Parameters, 0,
		                              g_beamformer_library_context.timeout_ms);
	}
	return result;
}

b32
beamformer_push_parameters_ui(BeamformerUIParameters *bp)
{
	b32 result = 0;
	if (check_shared_memory()) {
		result = locked_region_upload((u8 *)g_beamformer_library_context.bp +
		                              offsetof(BeamformerSharedMemory, parameters_ui),
		                              bp, sizeof(*bp), BeamformerSharedMemoryLockKind_Parameters, 0,
		                              g_beamformer_library_context.timeout_ms);
	}
	return result;
}

b32
beamformer_push_parameters_head(BeamformerParametersHead *bp)
{
	b32 result = 0;
	if (check_shared_memory()) {
		result = locked_region_upload((u8 *)g_beamformer_library_context.bp +
		                              offsetof(BeamformerSharedMemory, parameters_head),
		                              bp, sizeof(*bp), BeamformerSharedMemoryLockKind_Parameters, 0,
		                              g_beamformer_library_context.timeout_ms);
	}
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
beamformer_read_output(void *out, uz size, i32 timeout_ms)
{
	b32 result = 0;
	if (lib_try_lock(BeamformerSharedMemoryLockKind_ExportSync, timeout_ms)) {
		lib_release_lock(BeamformerSharedMemoryLockKind_ExportSync);
		if (lib_try_lock(BeamformerSharedMemoryLockKind_ScratchSpace, 0)) {
			mem_copy(out, (u8 *)g_beamformer_library_context.bp + BEAMFORMER_SCRATCH_OFF, size);
			lib_release_lock(BeamformerSharedMemoryLockKind_ScratchSpace);
			result = 1;
		}
	}
	return result;
}

b32
beamform_data_synchronized(void *data, u32 data_size, i32 output_points[3], f32 *out_data, i32 timeout_ms)
{
	b32 result = 0;
	if (check_shared_memory()) {
		output_points[0] = MAX(1, output_points[0]);
		output_points[1] = MAX(1, output_points[1]);
		output_points[2] = MAX(1, output_points[2]);

		g_beamformer_library_context.bp->parameters.output_points[0] = output_points[0];
		g_beamformer_library_context.bp->parameters.output_points[1] = output_points[1];
		g_beamformer_library_context.bp->parameters.output_points[2] = output_points[2];

		uz output_size = (u32)output_points[0] * (u32)output_points[1] * (u32)output_points[2] * sizeof(f32) * 2;
		if (output_size <= BEAMFORMER_SCRATCH_SIZE && beamformer_push_data_with_compute(data, data_size, 0)) {
			BeamformerExportContext export;
			export.kind = BeamformerExportKind_BeamformedData;
			export.size = (u32)output_size;
			if (beamformer_export_buffer(export)) {
				/* NOTE(rnp): if this fails it just means that the work from push_data hasn't
				 * started yet. This is here to catch the other case where the work started
				 * and finished before we finished queuing the export work item */
				beamformer_flush_commands(0);

				result = beamformer_read_output(out_data, output_size, timeout_ms);
			}
		} else {
			g_beamformer_library_context.last_error = BF_LIB_ERR_KIND_EXPORT_SPACE_OVERFLOW;
		}
	}
	return result;
}

b32
beamformer_compute_timings(BeamformerComputeStatsTable *output, i32 timeout_ms)
{
	b32 result = 0;
	if (check_shared_memory()) {
		static_assert(sizeof(*output) <= BEAMFORMER_SCRATCH_SIZE, "timing table size exceeds scratch space");
		BeamformerExportContext export;
		export.kind = BeamformerExportKind_Stats;
		export.size = sizeof(*output);
		if (beamformer_export_buffer(export) && beamformer_flush_commands(0))
			result = beamformer_read_output(output, sizeof(*output), timeout_ms);
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
			atomic_and_u32(&g_beamformer_library_context.bp->live_imaging_dirty_flags, ~(1 << flag));
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
