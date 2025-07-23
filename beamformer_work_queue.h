/* See LICENSE for license details. */
#ifndef _BEAMFORMER_WORK_QUEUE_H_
#define _BEAMFORMER_WORK_QUEUE_H_

#define BEAMFORMER_SHARED_MEMORY_VERSION (10UL)

typedef struct BeamformerComputeFrame BeamformerComputeFrame;
typedef struct ShaderReloadContext    ShaderReloadContext;

typedef enum {
	BeamformerWorkKind_Compute,
	BeamformerWorkKind_ComputeIndirect,
	BeamformerWorkKind_CreateFilter,
	BeamformerWorkKind_ReloadShader,
	BeamformerWorkKind_ExportBuffer,
	BeamformerWorkKind_UploadBuffer,
} BeamformerWorkKind;

typedef enum {
	BeamformerUploadKind_ChannelMapping,
	BeamformerUploadKind_FocalVectors,
	BeamformerUploadKind_RFData,
	BeamformerUploadKind_SparseElements,
} BeamformerUploadKind;

typedef struct {
	BeamformerUploadKind kind;
	u32 size;
	i32 shared_memory_offset;
} BeamformerUploadContext;

typedef struct {
	BeamformerFilterKind kind;
	union {
		struct {
			f32 beta;
			f32 cutoff_frequency;
		};
		f32 xdc_center_frequency;
	};
	i16 length;
	i16 slot;
} BeamformerCreateFilterContext;

typedef enum {
	BeamformerExportKind_BeamformedData,
	BeamformerExportKind_Stats,
} BeamformerExportKind;

typedef struct {
	BeamformerExportKind kind;
	u32 size;
} BeamformerExportContext;

typedef union {
	u8 filter_slot;
} BeamformerShaderParameters;

#define BEAMFORMER_SHARED_MEMORY_LOCKS \
	X(None)            \
	X(ComputePipeline) \
	X(ChannelMapping)  \
	X(FocalVectors)    \
	X(Parameters)      \
	X(ScratchSpace)    \
	X(SparseElements)  \
	X(ExportSync)      \
	X(DispatchCompute)

#define X(name) BeamformerSharedMemoryLockKind_##name,
typedef enum {BEAMFORMER_SHARED_MEMORY_LOCKS BeamformerSharedMemoryLockKind_Count} BeamformerSharedMemoryLockKind;
#undef X

/* NOTE: discriminated union based on type */
typedef struct {
	union {
		BeamformerComputeFrame        *frame;
		BeamformerCreateFilterContext  create_filter_context;
		BeamformerExportContext        export_context;
		BeamformerUploadContext        upload_context;
		BeamformerViewPlaneTag         compute_indirect_plane;
		ShaderReloadContext           *shader_reload_context;
		void                          *generic;
	};
	BeamformerSharedMemoryLockKind lock;
	BeamformerWorkKind kind;
} BeamformWork;

typedef struct {
	union {
		u64 queue;
		struct {u32 widx, ridx;};
	};
	BeamformWork work_items[1 << 6];
} BeamformWorkQueue;

#define BEAMFORM_WORK_QUEUE_PUSH_FN(name) BeamformWork *name(BeamformWorkQueue *q)
typedef BEAMFORM_WORK_QUEUE_PUSH_FN(beamform_work_queue_push_fn);

#define BEAMFORM_WORK_QUEUE_PUSH_COMMIT_FN(name) void name(BeamformWorkQueue *q)
typedef BEAMFORM_WORK_QUEUE_PUSH_COMMIT_FN(beamform_work_queue_push_commit_fn);

#define BEAMFORMER_SHARED_MEMORY_SIZE (GB(2))
#define BEAMFORMER_SCRATCH_OFF        (sizeof(BeamformerSharedMemory) + 4096ULL \
                                       - (uintptr_t)(sizeof(BeamformerSharedMemory) & 4095ULL))
#define BEAMFORMER_SCRATCH_SIZE       (BEAMFORMER_SHARED_MEMORY_SIZE - BEAMFORMER_SCRATCH_OFF)
#define BEAMFORMER_MAX_RF_DATA_SIZE   (BEAMFORMER_SCRATCH_SIZE)

#define X(name, id) BeamformerLiveImagingDirtyFlags_##name = (1 << id),
typedef enum {BEAMFORMER_LIVE_IMAGING_DIRTY_FLAG_LIST} BeamformerLiveImagingDirtyFlags;
#undef X

typedef struct {
	u32 version;

	/* NOTE(rnp): causes future library calls to fail.
	 * see note in beamformer_invalidate_shared_memory() */
	b32 invalid;

	/* NOTE(rnp): not used for locking on w32 but we can use these to peek at the status of
	 * the lock without leaving userspace. also this struct needs a bunch of padding */
	i32 locks[BeamformerSharedMemoryLockKind_Count];

	/* NOTE(rnp): used to coalesce uploads when they are not yet uploaded to the GPU */
	u32 dirty_regions;
	static_assert(BeamformerSharedMemoryLockKind_Count <= 32, "only 32 lock regions supported");

	/* NOTE(rnp): interleaved transmit angle, focal depth pairs */
	align_as(64) v2 focal_vectors[256];

	i16 channel_mapping[256];
	i16 sparse_elements[256];

	union {
		BeamformerParameters parameters;
		struct {
			BeamformerParametersHead parameters_head;
			BeamformerUIParameters   parameters_ui;
			BeamformerParametersTail parameters_tail;
		};
	};

	//////////////////////////
	// Pipeline Configuration
	BeamformerShaderKind       shaders[MAX_COMPUTE_SHADER_STAGES];
	BeamformerShaderParameters shader_parameters[MAX_COMPUTE_SHADER_STAGES];
	i32                        shader_count;
	BeamformerDataKind         data_kind;

	/* TODO(rnp): hack: we need a different way of dispatching work for export */
	b32 start_compute_from_main;

	/* TODO(rnp): this shouldn't be needed */
	b32 export_next_frame;

	BeamformerLiveImagingParameters live_imaging_parameters;
	BeamformerLiveImagingDirtyFlags live_imaging_dirty_flags;

	BeamformWorkQueue external_work_queue;
} BeamformerSharedMemory;

#endif /* _BEAMFORMER_WORK_QUEUE_H_ */
