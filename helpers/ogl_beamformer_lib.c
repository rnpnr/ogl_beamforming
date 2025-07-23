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

global SharedMemoryRegion      g_shared_memory;
global BeamformerSharedMemory *g_bp;
global BeamformerLibErrorKind  g_lib_last_error;

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
	if (!g_shared_memory.region) {
		g_shared_memory = os_open_shared_memory_area(OS_SHARED_MEMORY_NAME);
		if (!g_shared_memory.region) {
			g_lib_last_error = BF_LIB_ERR_KIND_SHARED_MEMORY;
			result = 0;
		} else if (((BeamformerSharedMemory *)g_shared_memory.region)->version !=
		           BEAMFORMER_SHARED_MEMORY_VERSION)
		{
			g_lib_last_error = BF_LIB_ERR_KIND_VERSION_MISMATCH;
			result = 0;
		}
	}
	if (result && ((BeamformerSharedMemory *)g_shared_memory.region)->invalid) {
		g_lib_last_error = BF_LIB_ERR_KIND_INVALID_ACCESS;
		result = 0;
	}
	if (result) g_bp = g_shared_memory.region;
	return result;
}

function BeamformWork *
try_push_work_queue(void)
{
	BeamformWork *result = beamform_work_queue_push(&g_bp->external_work_queue);
	if (!result) g_lib_last_error = BF_LIB_ERR_KIND_WORK_QUEUE_FULL;
	return result;
}

function b32
lib_try_lock(BeamformerSharedMemoryLockKind lock, i32 timeout_ms)
{
	b32 result = os_shared_memory_region_lock(&g_shared_memory, g_bp->locks, (i32)lock, (u32)timeout_ms);
	if (!result) g_lib_last_error = BF_LIB_ERR_KIND_SYNC_VARIABLE;
	return result;
}

function void
lib_release_lock(BeamformerSharedMemoryLockKind lock)
{
	os_shared_memory_region_unlock(&g_shared_memory, g_bp->locks, (i32)lock);
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
	return g_lib_last_error;
}

const char *
beamformer_get_last_error_string(void)
{
	return beamformer_error_string(beamformer_get_last_error());
}

b32
set_beamformer_pipeline(i32 *stages, u32 stages_count)
{
	b32 result = 0;
	if (stages_count <= countof(g_bp->compute_stages)) {
		if (check_shared_memory()) {
			g_bp->compute_stages_count = 0;
			for (u32 i = 0; i < stages_count; i++) {
				if (BETWEEN(stages[i], 0, BeamformerShaderKind_ComputeCount)) {
					g_bp->compute_stages[g_bp->compute_stages_count++] = (BeamformerShaderKind)stages[i];
				}
			}
			result = g_bp->compute_stages_count == stages_count;
			if (!result) {
				g_lib_last_error = BF_LIB_ERR_KIND_INVALID_COMPUTE_STAGE;
				g_bp->compute_stages_count = 0;
			}
		}
	} else {
		g_lib_last_error = BF_LIB_ERR_KIND_COMPUTE_STAGE_OVERFLOW;
	}
	return result;
}


b32
beamformer_set_pipeline_stage_parameters(i32 stage_index, i32 parameter)
{
	b32 result = 0;
	if (check_shared_memory() && g_bp->compute_stages_count != 0) {
		stage_index %= (i32)g_bp->compute_stages_count;
		g_bp->compute_shader_parameters[stage_index].filter_slot = (u8)parameter;
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
			beamform_work_queue_push_commit(&g_bp->external_work_queue);
		}
	}
	return result;
}

b32
beamformer_start_compute(i32 timeout_ms)
{
	u32 lock   = BeamformerSharedMemoryLockKind_DispatchCompute;
	b32 result = check_shared_memory() && lib_try_lock(lock, timeout_ms);
	return result;
}

b32
beamformer_wait_for_compute_dispatch(i32 timeout_ms)
{
	u32 lock   = BeamformerSharedMemoryLockKind_DispatchCompute;
	b32 result = check_shared_memory() && lib_try_lock(lock, timeout_ms);
	/* NOTE(rnp): if you are calling this function you are probably about
	 * to start some other work and it might be better to not do this... */
	if (result) lib_release_lock(BeamformerSharedMemoryLockKind_DispatchCompute);
	return result;
}

function b32
beamformer_upload_buffer(void *data, u32 size, i32 store_offset, BeamformerUploadContext upload_context,
                         BeamformerSharedMemoryLockKind lock, i32 timeout_ms)
{
	b32 result = 0;
	if (check_shared_memory()) {
		BeamformWork *work = try_push_work_queue();
		result = work && lib_try_lock(lock, timeout_ms);
		if (result) {
			work->upload_context = upload_context;
			work->kind = BeamformerWorkKind_UploadBuffer;
			work->lock = lock;
			mem_copy((u8 *)g_bp + store_offset, data, size);
			if ((atomic_load_u32(&g_bp->dirty_regions) & (1 << (lock - 1))) == 0) {
				atomic_or_u32(&g_bp->dirty_regions, (1 << (lock - 1)));
				beamform_work_queue_push_commit(&g_bp->external_work_queue);
			}
			lib_release_lock(lock);
		}
	}
	return result;
}

#define BEAMFORMER_UPLOAD_FNS \
	X(channel_mapping, i16, 1, ChannelMapping) \
	X(sparse_elements, i16, 1, SparseElements) \
	X(focal_vectors,   f32, 2, FocalVectors)

#define X(name, dtype, elements, lock_name) \
b32 beamformer_push_##name (dtype *data, u32 count, i32 timeout_ms) { \
	b32 result = 0; \
	if (count <= countof(g_bp->name)) { \
		BeamformerUploadContext uc = {0}; \
		uc.shared_memory_offset = offsetof(BeamformerSharedMemory, name); \
		uc.kind = BeamformerUploadKind_##lock_name; \
		uc.size = count * elements * sizeof(dtype); \
		result = beamformer_upload_buffer(data, uc.size, uc.shared_memory_offset, uc, \
		                                  BeamformerSharedMemoryLockKind_##lock_name, timeout_ms); \
	} else { \
		g_lib_last_error = BF_LIB_ERR_KIND_BUFFER_OVERFLOW; \
	} \
	return result; \
}
BEAMFORMER_UPLOAD_FNS
#undef X

function b32
beamformer_push_data_base(void *data, u32 data_size, i32 timeout_ms, b32 start_from_main)
{
	b32 result = 0;
	if (data_size <= BEAMFORMER_MAX_RF_DATA_SIZE) {
		BeamformerUploadContext uc = {0};
		uc.shared_memory_offset = BEAMFORMER_SCRATCH_OFF;
		uc.size = data_size;
		uc.kind = BeamformerUploadKind_RFData;
		result = beamformer_upload_buffer(data, data_size, uc.shared_memory_offset, uc,
		                                  BeamformerSharedMemoryLockKind_ScratchSpace, timeout_ms);
		if (result && start_from_main) atomic_store_u32(&g_bp->start_compute_from_main, 1);
	} else {
		g_lib_last_error = BF_LIB_ERR_KIND_BUFFER_OVERFLOW;
	}
	return result;
}

b32
beamformer_push_data(void *data, u32 data_size, i32 timeout_ms)
{
	return beamformer_push_data_base(data, data_size, timeout_ms, 1);
}

b32
beamformer_push_data_with_compute(void *data, u32 data_size, u32 image_plane_tag, i32 timeout_ms)
{
	b32 result = beamformer_push_data_base(data, data_size, timeout_ms, 0);
	if (result) {
		result = image_plane_tag < BeamformerViewPlaneTag_Count;
		if (result) {
			BeamformWork *work = try_push_work_queue();
			if (work) {
				work->kind = BeamformerWorkKind_ComputeIndirect;
				work->compute_indirect_plane = image_plane_tag;
				beamform_work_queue_push_commit(&g_bp->external_work_queue);
				result = beamformer_start_compute(0);
			}
		} else {
			g_lib_last_error = BF_LIB_ERR_KIND_INVALID_IMAGE_PLANE;
		}
	}
	return result;
}

b32
beamformer_push_parameters(BeamformerParameters *bp, i32 timeout_ms)
{
	BeamformerUploadContext uc = {0};
	uc.shared_memory_offset = offsetof(BeamformerSharedMemory, parameters);
	uc.size = sizeof(g_bp->parameters);
	uc.kind = BeamformerUploadKind_Parameters;
	b32 result = beamformer_upload_buffer(bp, sizeof(*bp),
	                                      offsetof(BeamformerSharedMemory, parameters), uc,
	                                      BeamformerSharedMemoryLockKind_Parameters, timeout_ms);
	return result;
}

b32
beamformer_push_parameters_ui(BeamformerUIParameters *bp, i32 timeout_ms)
{
	BeamformerUploadContext uc = {0};
	uc.shared_memory_offset = offsetof(BeamformerSharedMemory, parameters);
	uc.size = sizeof(g_bp->parameters);
	uc.kind = BeamformerUploadKind_Parameters;
	b32 result = beamformer_upload_buffer(bp, sizeof(*bp),
	                                      offsetof(BeamformerSharedMemory, parameters_ui), uc,
	                                      BeamformerSharedMemoryLockKind_Parameters, timeout_ms);
	return result;
}

b32
beamformer_push_parameters_head(BeamformerParametersHead *bp, i32 timeout_ms)
{
	BeamformerUploadContext uc = {0};
	uc.shared_memory_offset = offsetof(BeamformerSharedMemory, parameters);
	uc.size = sizeof(g_bp->parameters);
	uc.kind = BeamformerUploadKind_Parameters;
	b32 result = beamformer_upload_buffer(bp, sizeof(*bp),
	                                      offsetof(BeamformerSharedMemory, parameters_head), uc,
	                                      BeamformerSharedMemoryLockKind_Parameters, timeout_ms);
	return result;
}

b32
set_beamformer_parameters(BeamformerParametersV0 *new_bp)
{
	b32 result = 1;
	result &= beamformer_push_channel_mapping((i16 *)new_bp->channel_mapping,
	                                          countof(new_bp->channel_mapping), 0);
	result &= beamformer_push_sparse_elements((i16 *)new_bp->uforces_channels,
	                                          countof(new_bp->uforces_channels), 0);
	v2 focal_vectors[256];
	for (u32 i = 0; i < countof(focal_vectors); i++)
		focal_vectors[i] = (v2){{new_bp->transmit_angles[i], new_bp->focal_depths[i]}};
	result &= beamformer_push_focal_vectors((f32 *)focal_vectors, countof(focal_vectors), 0);
	result &= beamformer_push_parameters((BeamformerParameters *)&new_bp->xdc_transform, 0);
	return result;
}

b32
send_data(void *data, u32 data_size)
{
	b32 result = 0;
	if (beamformer_push_data(data, data_size, 0))
		result = beamformer_start_compute(-1);
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
		beamform_work_queue_push_commit(&g_bp->external_work_queue);
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
			mem_copy(out, (u8 *)g_bp + BEAMFORMER_SCRATCH_OFF, size);
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

		g_bp->parameters.output_points[0] = output_points[0];
		g_bp->parameters.output_points[1] = output_points[1];
		g_bp->parameters.output_points[2] = output_points[2];

		uz output_size = (u32)output_points[0] * (u32)output_points[1] * (u32)output_points[2] * sizeof(f32) * 2;
		if (output_size <= BEAMFORMER_SCRATCH_SIZE &&
		    beamformer_push_data_with_compute(data, data_size, 0, 0))
		{
			BeamformerExportContext export;
			export.kind = BeamformerExportKind_BeamformedData;
			export.size = (u32)output_size;
			if (beamformer_export_buffer(export)) {
				/* NOTE(rnp): if this fails it just means that the work from push_data hasn't
				 * started yet. This is here to catch the other case where the work started
				 * and finished before we finished queuing the export work item */
				beamformer_start_compute(0);

				result = beamformer_read_output(out_data, output_size, timeout_ms);
			}
		} else {
			g_lib_last_error = BF_LIB_ERR_KIND_EXPORT_SPACE_OVERFLOW;
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
		if (beamformer_export_buffer(export) && beamformer_start_compute(0))
			result = beamformer_read_output(output, sizeof(*output), timeout_ms);
	}
	return result;
}

i32
beamformer_live_parameters_get_dirty_flag(void)
{
	i32 result = -1;
	if (check_shared_memory()) {
		u32 flag = ctz_u32(g_bp->live_imaging_dirty_flags);
		if (flag != 32) {
			atomic_and_u32(&g_bp->live_imaging_dirty_flags, ~(1 << flag));
			result = (i32)flag;
		}
	}
	return result;
}

BeamformerLiveImagingParameters *
beamformer_get_live_parameters(void)
{
	BeamformerLiveImagingParameters *result = 0;
	if (check_shared_memory()) result = &g_bp->live_imaging_parameters;
	return result;
}

b32
beamformer_set_live_parameters(BeamformerLiveImagingParameters *new)
{
	b32 result = 0;
	if (check_shared_memory()) {
		mem_copy(&g_bp->live_imaging_parameters, new, sizeof(*new));
		memory_write_barrier();
		result = 1;
	}
	return result;
}
