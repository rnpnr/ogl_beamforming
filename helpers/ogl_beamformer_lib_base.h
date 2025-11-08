/* See LICENSE for license details. */
#ifndef LIB_FN
  #if defined(_WIN32)
    #define LIB_FN __declspec(dllexport)
  #else
    #define LIB_FN
  #endif
#endif

#define BEAMFORMER_LIB_ERRORS \
	X(NONE,                         0, "None") \
	X(VERSION_MISMATCH,             1, "host-library version mismatch")                     \
	X(INVALID_ACCESS,               2, "library in invalid state")                          \
	X(PARAMETER_BLOCK_OVERFLOW,     3, "parameter block count overflow")                    \
	X(PARAMETER_BLOCK_UNALLOCATED,  4, "push to unallocated parameter block")               \
	X(COMPUTE_STAGE_OVERFLOW,       5, "compute stage overflow")                            \
	X(INVALID_COMPUTE_STAGE,        6, "invalid compute shader stage")                      \
	X(INVALID_START_SHADER,         7, "starting shader not Decode or Demodulate")          \
	X(INVALID_DEMOD_DATA_KIND,      8, "data kind for demodulation not Int16 or Float")     \
	X(INVALID_IMAGE_PLANE,          9, "invalid image plane")                               \
	X(BUFFER_OVERFLOW,             10, "passed buffer size exceeds available space")        \
	X(DATA_SIZE_MISMATCH,          11, "data size is less than specified from parameters")  \
	X(WORK_QUEUE_FULL,             12, "work queue full")                                   \
	X(EXPORT_SPACE_OVERFLOW,       13, "not enough space for data export")                  \
	X(SHARED_MEMORY,               14, "failed to open shared memory region")               \
	X(SYNC_VARIABLE,               15, "failed to acquire lock within timeout period")      \
	X(INVALID_TIMEOUT,             16, "invalid timeout value")                             \
	X(INVALID_FILTER_KIND,         17, "invalid filter kind")                               \
	X(INVALID_FILTER_PARAM_COUNT,  18, "invalid parameters count passed for filter")        \
	X(INVALID_SIMPLE_PARAMETERS,   19, "invalid simple parameters struct")

#define X(type, num, string) BF_LIB_ERR_KIND_ ##type = num,
typedef enum {BEAMFORMER_LIB_ERRORS} BeamformerLibErrorKind;
#undef X

LIB_FN uint32_t beamformer_get_api_version(void);

LIB_FN BeamformerLibErrorKind beamformer_get_last_error(void);
LIB_FN const char *beamformer_get_last_error_string(void);
LIB_FN const char *beamformer_error_string(BeamformerLibErrorKind kind);

///////////////////////////
// NOTE: Simple API
/* Usage:
 *   - fill out a BeamformerSimpleParameters
 *     - filters need to be created with beamformer_create_filter, and the slot
 *       needs to be assigned in compute_stage_parameters
 *   - allocate a buffer with enough space for all Float32 or Float32Complex output points
 *   - pass the buffer along with the data and parameters to beamformer_beamform_data()
 *   - if the function was unsuccessful you can check the error with beamformer_get_last_error()
 *     or beamformer_get_last_error_string()
 */
LIB_FN uint32_t beamformer_beamform_data(BeamformerSimpleParameters *bp, void *data, uint32_t data_size,
                                         void *out_data, int32_t timeout_ms);

/* NOTE: sets timeout for all functions which may timeout but don't
 * take a timeout argument. The majority of such functions will not
 * timeout in the normal case and so passing a timeout parameter around
 * every where is cumbersome.
 *
 * timeout_ms: milliseconds in the range [-1, ...) (Default: 0)
 *
 * IMPORTANT: timeout of -1 will block forever */
LIB_FN uint32_t beamformer_set_global_timeout(int32_t timeout_ms);

///////////////////////////
// NOTE: Advanced API

/* NOTE: downloads the last 32 frames worth of compute timings into output */
LIB_FN uint32_t beamformer_compute_timings(BeamformerComputeStatsTable *output, int32_t timeout_ms);

/* NOTE: pushes data and tries to immediately starts a compute */
LIB_FN uint32_t beamformer_push_data_with_compute(void *data, uint32_t size,
                                                  uint32_t image_plane_tag,
                                                  uint32_t parameter_slot);

///////////////////////////
// Parameter Configuration
LIB_FN uint32_t beamformer_reserve_parameter_blocks(uint32_t count);
LIB_FN uint32_t beamformer_set_pipeline_stage_parameters(uint32_t stage_index, int32_t parameter);
LIB_FN uint32_t beamformer_push_pipeline(int32_t *shaders, uint32_t shader_count, BeamformerDataKind data_kind);

LIB_FN uint32_t beamformer_set_pipeline_stage_parameters_at(uint32_t stage_index,
                                                            int32_t  parameter,
                                                            uint32_t parameter_slot);
LIB_FN uint32_t beamformer_push_pipeline_at(int32_t *shaders, uint32_t shader_count,
                                            BeamformerDataKind data_kind, uint32_t parameter_slot);

LIB_FN uint32_t beamformer_push_simple_parameters(BeamformerSimpleParameters *bp);
LIB_FN uint32_t beamformer_push_simple_parameters_at(BeamformerSimpleParameters *bp, uint32_t parameter_slot);

LIB_FN uint32_t beamformer_push_parameters(BeamformerParameters *);
LIB_FN uint32_t beamformer_push_parameters_ui(BeamformerUIParameters *);
LIB_FN uint32_t beamformer_push_parameters_head(BeamformerParametersHead *);

LIB_FN uint32_t beamformer_push_parameters_at(BeamformerParameters *, uint32_t parameter_slot);

LIB_FN uint32_t beamformer_push_channel_mapping(int16_t *mapping, uint32_t count);
LIB_FN uint32_t beamformer_push_channel_mapping_at(int16_t *mapping, uint32_t count, uint32_t parameter_slot);

LIB_FN uint32_t beamformer_push_sparse_elements(int16_t *elements, uint32_t count);
LIB_FN uint32_t beamformer_push_sparse_elements_at(int16_t *elements, uint32_t count, uint32_t parameter_slot);

LIB_FN uint32_t beamformer_push_focal_vectors(float *vectors, uint32_t count);
LIB_FN uint32_t beamformer_push_focal_vectors_at(float *vectors, uint32_t count, uint32_t parameter_slot);

LIB_FN uint32_t beamformer_push_transmit_receive_orientations(uint8_t *values, uint32_t count);
LIB_FN uint32_t beamformer_push_transmit_receive_orientations_at(uint8_t *values, uint32_t count, uint32_t parameter_slot);

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

LIB_FN uint32_t beamformer_create_filter(BeamformerFilterKind kind, float *filter_parameters,
                                         uint32_t filter_parameter_count, float sampling_frequency,
                                         uint32_t complex, uint8_t filter_slot, uint8_t parameter_block);

//////////////////////////
// Live Imaging Controls
LIB_FN int32_t  beamformer_live_parameters_get_dirty_flag(void);
LIB_FN uint32_t beamformer_set_live_parameters(BeamformerLiveImagingParameters *);
LIB_FN BeamformerLiveImagingParameters *beamformer_get_live_parameters(void);
