/* See LICENSE for license details. */
#ifndef BEAMFORMER_LIB_EXPORT
  #if defined(_WIN32)
    #define BEAMFORMER_LIB_EXPORT __declspec(dllexport)
  #else
    #define BEAMFORMER_LIB_EXPORT
  #endif
#endif

#define BEAMFORMER_LIB_ERRORS \
	X(None,                          0, "None") \
	X(VersionMismatch,               1, "host-library version mismatch")                     \
	X(InvalidAccess,                 2, "library in invalid state")                          \
	X(ParameterBlockOverflow,        3, "parameter block count overflow")                    \
	X(ParameterBlockUnallocated,     4, "push to unallocated parameter block")               \
	X(ComputeStageOverflow,          5, "compute stage overflow")                            \
	X(InvalidComputeStage,           6, "invalid compute shader stage")                      \
	X(InvalidStartShader,            7, "starting shader not Decode or Demodulate")          \
	X(InvalidDemodulationDataKind,   8, "data kind for demodulation not Int16 or Float")     \
	X(InvalidImagePlane,             9, "invalid image plane")                               \
	X(BufferOverflow,               10, "passed buffer size exceeds available space")        \
	X(DataSizeMismatch,             11, "data size doesn't match the size specified in parameters")  \
	X(WorkQueueFull,                12, "work queue full")                                   \
	X(ExportSpaceOverflow,          13, "not enough space for data export")                  \
	X(SharedMemory,                 14, "failed to open shared memory region")               \
	X(SyncVariable,                 15, "failed to acquire lock within timeout period")      \
	X(InvalidFilterKind,            16, "invalid filter kind")                               \
	X(InvalidParameters,            17, "invalid parameters struct")                         \
	X(FrameSizeOverflow,            18, "maximum frame size exceeded")                       \

#define X(type, num, string) BeamformerLibErrorKind_##type = num,
typedef enum {BEAMFORMER_LIB_ERRORS} BeamformerLibErrorKind;
#undef X

BEAMFORMER_LIB_EXPORT uint32_t beamformer_get_api_version(void);

BEAMFORMER_LIB_EXPORT BeamformerLibErrorKind beamformer_get_last_error(void);
BEAMFORMER_LIB_EXPORT const char *beamformer_get_last_error_string(void);
BEAMFORMER_LIB_EXPORT const char *beamformer_error_string(BeamformerLibErrorKind kind);

// NOTE: returns U64_MAX if shared memory could not be opened
BEAMFORMER_LIB_EXPORT uint64_t beamformer_maximum_frame_size(void);

///////////////////////////
// NOTE: Simple API
/* Usage:
 *   - fill out a BeamformerSimpleParameters
 *     - filters need to be created with beamformer_create_filter, and the slot
 *       needs to be assigned in compute_stage_parameters
 *   - (Optional) allocate a buffer with enough space for all Float32 or Float32Complex output points
 *   - pass the data and parameters to beamformer_beamform_data()
 *     - pass 0 for out_data if you do not need the beamformed data returned
 *   - if the function was unsuccessful you can check the error with beamformer_get_last_error()
 *     or beamformer_get_last_error_string()
 */
BEAMFORMER_LIB_EXPORT uint32_t beamformer_beamform_data(BeamformerSimpleParameters *bp, void *data,
                                                        uint32_t data_size, void *out_data,
                                                        int32_t timeout_ms);

/* NOTE: sets timeout for all functions which may timeout but don't
 * take a timeout argument. The majority of such functions will not
 * timeout in the normal case and so passing a timeout parameter around
 * every where is cumbersome.
 *
 * timeout_ms: milliseconds (Default: 0)
 *
 * IMPORTANT: timeout of -1 will block forever */
BEAMFORMER_LIB_EXPORT void beamformer_set_global_timeout(uint32_t timeout_ms);

///////////////////////////
// NOTE: Advanced API

/* NOTE: downloads the last 32 frames worth of compute timings into output */
BEAMFORMER_LIB_EXPORT uint32_t beamformer_compute_timings(BeamformerComputeStatsTable *output,
                                                          int32_t timeout_ms);

/* NOTE: pushes data and tries to immediately starts a compute */
BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_data_with_compute(void *data, uint32_t size,
                                                                 uint32_t image_plane_tag,
                                                                 uint32_t parameter_slot);

///////////////////////////
// Parameter Configuration
BEAMFORMER_LIB_EXPORT uint32_t beamformer_reserve_parameter_blocks(uint32_t count);
BEAMFORMER_LIB_EXPORT uint32_t beamformer_set_pipeline_stage_parameters(uint32_t stage_index,
                                                                        int32_t parameter);
BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_pipeline(int32_t *shaders, uint32_t shader_count,
                                                        BeamformerDataKind data_kind);

BEAMFORMER_LIB_EXPORT uint32_t beamformer_set_pipeline_stage_parameters_at(uint32_t stage_index,
                                                                           int32_t  parameter,
                                                                           uint32_t parameter_slot);
BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_pipeline_at(int32_t *shaders, uint32_t shader_count,
                                                           BeamformerDataKind data_kind,
                                                           uint32_t parameter_slot);

BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_simple_parameters(BeamformerSimpleParameters *bp);
BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_simple_parameters_at(BeamformerSimpleParameters *bp,
                                                                    uint32_t parameter_slot);

BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_parameters(BeamformerParameters *);
BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_parameters_ui(BeamformerUIParameters *);
BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_parameters_head(BeamformerParametersHead *);

BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_parameters_at(BeamformerParameters *,
                                                             uint32_t parameter_slot);

BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_channel_mapping(int16_t *mapping, uint32_t count);
BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_channel_mapping_at(int16_t *mapping, uint32_t count,
                                                                  uint32_t parameter_slot);

BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_sparse_elements(int16_t *elements, uint32_t count);
BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_sparse_elements_at(int16_t *elements, uint32_t count,
                                                                  uint32_t parameter_slot);

BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_focal_vectors(float *vectors, uint32_t count);
BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_focal_vectors_at(float *vectors, uint32_t count,
                                                                uint32_t parameter_slot);

BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_transmit_receive_orientations(uint8_t *values,
                                                                             uint32_t count);
BEAMFORMER_LIB_EXPORT uint32_t beamformer_push_transmit_receive_orientations_at(uint8_t *values,
                                                                                uint32_t count,
                                                                                uint32_t parameter_slot);

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

BEAMFORMER_LIB_EXPORT uint32_t beamformer_create_filter(BeamformerFilterKind kind,
                                                        void *filter_parameters, uint32_t filter_size,
                                                        float sampling_frequency, uint32_t complex,
                                                        uint8_t filter_slot, uint8_t parameter_block);

//////////////////////////
// Live Imaging Controls
BEAMFORMER_LIB_EXPORT int32_t  beamformer_live_parameters_get_dirty_flag(void);
BEAMFORMER_LIB_EXPORT uint32_t beamformer_set_live_parameters(BeamformerLiveImagingParameters *);
BEAMFORMER_LIB_EXPORT BeamformerLiveImagingParameters *beamformer_get_live_parameters(void);
