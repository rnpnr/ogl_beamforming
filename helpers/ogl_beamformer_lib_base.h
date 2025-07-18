/* See LICENSE for license details. */
#ifndef LIB_FN
  #if defined(_WIN32)
    #define LIB_FN __declspec(dllexport)
  #else
    #define LIB_FN
  #endif
#endif

#define BEAMFORMER_LIB_ERRORS \
	X(NONE,                    0, "None") \
	X(VERSION_MISMATCH,        1, "host-library version mismatch")                \
	X(INVALID_ACCESS,          2, "library in invalid state")                     \
	X(COMPUTE_STAGE_OVERFLOW,  3, "compute stage overflow: maximum stages: " str(MAX_COMPUTE_SHADER_STAGES)) \
	X(INVALID_COMPUTE_STAGE,   4, "invalid compute shader stage")                 \
	X(INVALID_IMAGE_PLANE,     5, "invalid image plane")                          \
	X(BUFFER_OVERFLOW,         6, "passed buffer size exceeds available space")   \
	X(WORK_QUEUE_FULL,         7, "work queue full")                              \
	X(EXPORT_SPACE_OVERFLOW,   8, "not enough space for data export")             \
	X(SHARED_MEMORY,           9, "failed to open shared memory region")          \
	X(SYNC_VARIABLE,          10, "failed to acquire lock within timeout period")

#define X(type, num, string) BF_LIB_ERR_KIND_ ##type = num,
typedef enum {BEAMFORMER_LIB_ERRORS} BeamformerLibErrorKind;
#undef X

LIB_FN uint32_t beamformer_get_api_version(void);

LIB_FN BeamformerLibErrorKind beamformer_get_last_error(void);
LIB_FN const char *beamformer_get_last_error_string(void);
LIB_FN const char *beamformer_error_string(BeamformerLibErrorKind kind);

/* IMPORTANT: timeout of -1 will block forever */

LIB_FN uint32_t set_beamformer_parameters(BeamformerParametersV0 *);
LIB_FN uint32_t set_beamformer_pipeline(int32_t *stages, uint32_t stages_count);
LIB_FN uint32_t send_data(void *data, uint32_t data_size);
/* NOTE: sends data and waits for (complex) beamformed data to be returned.
 * out_data: must be allocated by the caller as 2 floats per output point. */
LIB_FN uint32_t beamform_data_synchronized(void *data, uint32_t data_size, int32_t output_points[3],
                                           float *out_data, int32_t timeout_ms);

/* NOTE: downloads the last 32 frames worth of compute timings into output */
LIB_FN uint32_t beamformer_compute_timings(BeamformerComputeStatsTable *output, int32_t timeout_ms);

/* NOTE: tells the beamformer to start beamforming and waits until it starts or for timeout_ms */
LIB_FN uint32_t beamformer_start_compute(int32_t timeout_ms);

/* NOTE: waits for previously queued beamform to start or for timeout_ms */
LIB_FN uint32_t beamformer_wait_for_compute_dispatch(int32_t timeout_ms);

/* NOTE: these functions only queue an upload; you must flush (old data functions or start_compute) */
LIB_FN uint32_t beamformer_push_data(void *data, uint32_t size, int32_t timeout_ms);
LIB_FN uint32_t beamformer_push_data_with_compute(void *data, uint32_t size, uint32_t image_plane_tag, int32_t timeout_ms);
LIB_FN uint32_t beamformer_push_channel_mapping(int16_t *mapping,  uint32_t count, int32_t timeout_ms);
LIB_FN uint32_t beamformer_push_sparse_elements(int16_t *elements, uint32_t count, int32_t timeout_ms);
LIB_FN uint32_t beamformer_push_focal_vectors(float     *vectors,  uint32_t count, int32_t timeout_ms);

LIB_FN uint32_t beamformer_push_parameters(BeamformerParameters *, int32_t timeout_ms);
LIB_FN uint32_t beamformer_push_parameters_ui(BeamformerUIParameters *, int32_t timeout_ms);
LIB_FN uint32_t beamformer_push_parameters_head(BeamformerParametersHead *, int32_t timeout_ms);

//////////////////////////
// Live Imaging Controls
LIB_FN int32_t  beamformer_live_parameters_get_dirty_flag(void);
LIB_FN uint32_t beamformer_set_live_parameters(BeamformerLiveImagingParameters *);
LIB_FN BeamformerLiveImagingParameters *beamformer_get_live_parameters(void);
