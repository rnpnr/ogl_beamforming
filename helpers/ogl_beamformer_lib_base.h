/* See LICENSE for license details. */
#ifndef LIB_FN
  #if defined(_WIN32)
    #define LIB_FN __declspec(dllexport)
  #else
    #define LIB_FN
  #endif
#endif

#define BEAMFORMER_LIB_ERRORS \
	X(NONE,                     0, "None") \
	X(VERSION_MISMATCH,         1, "host-library version mismatch")                 \
	X(INVALID_ACCESS,           2, "library in invalid state")                      \
	X(COMPUTE_STAGE_OVERFLOW,   3, "compute stage overflow: maximum stages: " str(MAX_COMPUTE_SHADER_STAGES)) \
	X(INVALID_COMPUTE_STAGE,    4, "invalid compute shader stage")                  \
	X(INVALID_START_SHADER,     5, "starting shader not Decode or Demodulate")      \
	X(INVALID_DEMOD_DATA_KIND,  6, "data kind for demodulation not Int16 or Float") \
	X(INVALID_IMAGE_PLANE,      7, "invalid image plane")                           \
	X(BUFFER_OVERFLOW,          8, "passed buffer size exceeds available space")    \
	X(WORK_QUEUE_FULL,          9, "work queue full")                               \
	X(EXPORT_SPACE_OVERFLOW,   10, "not enough space for data export")              \
	X(SHARED_MEMORY,           11, "failed to open shared memory region")           \
	X(SYNC_VARIABLE,           12, "failed to acquire lock within timeout period")  \
	X(INVALID_TIMEOUT,         13, "invalid timeout value")

#define X(type, num, string) BF_LIB_ERR_KIND_ ##type = num,
typedef enum {BEAMFORMER_LIB_ERRORS} BeamformerLibErrorKind;
#undef X

LIB_FN uint32_t beamformer_get_api_version(void);

LIB_FN BeamformerLibErrorKind beamformer_get_last_error(void);
LIB_FN const char *beamformer_get_last_error_string(void);
LIB_FN const char *beamformer_error_string(BeamformerLibErrorKind kind);

/* NOTE: sets timeout for all functions which may timeout but don't
 * take a timeout argument. The majority of such functions will not
 * timeout in the normal case and so passing a timeout parameter around
 * every where is cumbersome.
 *
 * timeout_ms: milliseconds in the range [-1, ...) (Default: 0)
 *
 * IMPORTANT: timeout of -1 will block forever */
LIB_FN uint32_t beamformer_set_global_timeout(int32_t timeout_ms);

/* NOTE: sends data and waits for (complex) beamformed data to be returned.
 * out_data: must be allocated by the caller as 2 floats per output point. */
LIB_FN uint32_t beamform_data_synchronized(void *data, uint32_t data_size, int32_t output_points[3],
                                           float *out_data, int32_t timeout_ms);

/* NOTE: downloads the last 32 frames worth of compute timings into output */
LIB_FN uint32_t beamformer_compute_timings(BeamformerComputeStatsTable *output, int32_t timeout_ms);

/* NOTE: tells the beamformer to start beamforming */
LIB_FN uint32_t beamformer_start_compute(void);

/* NOTE: waits for previously queued beamform to start or for timeout_ms */
LIB_FN uint32_t beamformer_wait_for_compute_dispatch(int32_t timeout_ms);

LIB_FN uint32_t beamformer_push_data_with_compute(void *data, uint32_t size, uint32_t image_plane_tag);
/* NOTE: these functions only queue an upload; you must flush (start_compute) */
LIB_FN uint32_t beamformer_push_data(void *data, uint32_t size);
LIB_FN uint32_t beamformer_push_channel_mapping(int16_t *mapping,  uint32_t count);
LIB_FN uint32_t beamformer_push_sparse_elements(int16_t *elements, uint32_t count);
LIB_FN uint32_t beamformer_push_focal_vectors(float     *vectors,  uint32_t count);

LIB_FN uint32_t beamformer_set_pipeline_stage_parameters(int32_t stage_index, int32_t parameter);
LIB_FN uint32_t beamformer_push_pipeline(int32_t *shaders, int32_t shader_count, BeamformerDataKind data_kind);
LIB_FN uint32_t beamformer_push_parameters(BeamformerParameters *);
LIB_FN uint32_t beamformer_push_parameters_ui(BeamformerUIParameters *);
LIB_FN uint32_t beamformer_push_parameters_head(BeamformerParametersHead *);

////////////////////
// Filter Creation

/* Kaiser Low-Pass Parameter Selection
 * see: "Discrete Time Signal Processing" (Oppenheim)
 * δ:   fractional passband ripple
 * ω_p: highest angular frequency of passband
 * ω_s: lowest  angular frequency of stopband
 * ω_c: cutoff angular frequency. midpoint of ω_s and ω_p
 * M:   length of filter
 *
 * Define: A = -20log10(δ)
 * β:
 *   β = 0.1102(A - 8.7)                             if 50 <  A
 *   β = 0.5842 * pow(A - 21, 0.4) + 0.07886(A − 21) if 21 <= A <= 50
 *   β = 0                                           if       A <  21
 * M:
 *   M = (A - 8) / (2.285 (ω_s - ω_p))
 */
LIB_FN uint32_t beamformer_create_kaiser_low_pass_filter(float beta, float cutoff_frequency,
                                                         int16_t length, uint8_t slot);

//////////////////////////
// Live Imaging Controls
LIB_FN int32_t  beamformer_live_parameters_get_dirty_flag(void);
LIB_FN uint32_t beamformer_set_live_parameters(BeamformerLiveImagingParameters *);
LIB_FN BeamformerLiveImagingParameters *beamformer_get_live_parameters(void);
