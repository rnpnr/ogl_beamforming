/* See LICENSE for license details. */
#include <stdint.h>

/* TODO(rnp):
 * [ ]: shader kinds have ballooned; shader stats table needs to be compressed
 * [ ]: Upload previously exported data for display. maybe this is a UI thing but doing it
 *      programatically would be nice.
 * [ ]: Add interface for multi frame upload. RF upload already uses an offset into SM so
 *      that part works fine. We just need a way of specify a multi frame upload. (Data must
 *      be organized for simple offset access per frame).
 */

typedef struct {
	/* NOTE(rnp): this wants to be iterated on both dimensions. it depends entirely on which
	 * visualization method you want to use. the coalescing function wants both directions */
	float times[32][BeamformerShaderKind_ComputeCount];
	float rf_time_deltas[32];
} BeamformerComputeStatsTable;

/* X(type, id, pretty name) */
#define BEAMFORMER_VIEW_PLANE_TAG_LIST \
	X(XZ,        0, "XZ")        \
	X(YZ,        1, "YZ")        \
	X(XY,        2, "XY")        \
	X(Arbitrary, 3, "Arbitrary")

typedef enum {
	#define X(type, id, pretty) BeamformerViewPlaneTag_##type = id,
	BEAMFORMER_VIEW_PLANE_TAG_LIST
	#undef X
	BeamformerViewPlaneTag_Count,
} BeamformerViewPlaneTag;

#define BEAMFORMER_CONSTANTS_LIST \
	X(FilterSlots,                4) \
	X(MaxChannelCount,          256) \
	X(MaxComputeShaderStages,    16) \
	X(MaxParameterBlockSlots,    16) \
	X(MaxRawDataFramesInFlight,   3) \
	X(MaxSavedFrames,            16)
#define X(k, v, ...) Beamformer##k = v,
typedef enum {BEAMFORMER_CONSTANTS_LIST} BeamformerConstants;
#undef X

/* X(name, type, size, matlab_type, elements, comment) */
#define BEAMFORMER_PARAMS_HEAD \
	X(xdc_transform,                float,    [16], single, 16, "IMPORTANT: column major order")           \
	X(xdc_element_pitch,            float,     [2], single,  2, "[m] Transducer Element Pitch {row, col}") \
	X(raw_data_dimensions,          uint32_t,  [2], uint32,  2, "Raw Data Dimensions")                     \
	X(focal_vector,                 float,     [2], single,  2, "[degree, m] focal point {angle, depth}")  \
	X(transmit_receive_orientation, uint32_t,     , uint32,  1, "")                                        \
	X(sample_count,                 uint32_t,     , uint32,  1, "")                                        \
	X(channel_count,                uint32_t,     , uint32,  1, "")                                        \
	X(acquisition_count,            uint32_t,     , uint32,  1, "")                                        \
	X(das_shader_id,                uint32_t,     , uint32,  1, "")                                        \
	X(time_offset,                  float,        , single,  1, "pulse length correction time [s]")        \
	X(single_focus,                 uint8_t,      , uint8,   1, "")                                        \
	X(single_orientation,           uint8_t,      , uint8,   1, "")                                        \
	X(decode_mode,                  uint8_t,      , uint8,   1, "")                                        \
	X(sampling_mode,                uint8_t,      , uint8,   1, "")

#define BEAMFORMER_UI_PARAMS \
	X(output_min_coordinate,  float,     [3], single, 3, "[m] Back-Top-Left corner of output region")                     \
	X(output_max_coordinate,  float,     [3], single, 3, "[m] Front-Bottom-Right corner of output region")                \
	X(output_points,          int32_t,   [4], int32,  4, "Width * Height * Depth * (Frame Average Count)")                \
	X(sampling_frequency,     float,        , single, 1, "[Hz]")                                                          \
	X(demodulation_frequency, float,        , single, 1, "[Hz]")                                                          \
	X(speed_of_sound,         float,        , single, 1, "[m/s]")                                                         \
	X(f_number,               float,        , single, 1, "F# (set to 0 to disable)")                                      \
	X(off_axis_pos,           float,        , single, 1, "[m] Position on screen normal to beamform in TPW/VLS/HERCULES") \
	X(interpolation_mode,     uint32_t,     , uint32, 1, "Nearest, Linear, or Cubic Interpolation of RF Samples")         \
	X(coherency_weighting,    uint32_t,     , uint32, 1, "Apply coherency weighting to output data")                      \
	X(beamform_plane,         uint32_t,     , uint32, 1, "Plane to Beamform in TPW/VLS/HERCULES")                         \
	X(decimation_rate,        uint32_t,     , uint32, 1, "Number of times to decimate")

#define BEAMFORMER_SIMPLE_PARAMS \
	X(channel_mapping,               int16_t,  [BeamformerMaxChannelCount],        int16,  BeamformerMaxChannelCount) \
	X(sparse_elements,               int16_t,  [BeamformerMaxChannelCount],        int16,  BeamformerMaxChannelCount) \
	X(transmit_receive_orientations, uint8_t,  [BeamformerMaxChannelCount],        uint8,  BeamformerMaxChannelCount) \
	X(steering_angles,               float,    [BeamformerMaxChannelCount],        single, BeamformerMaxChannelCount) \
	X(focal_depths,                  float,    [BeamformerMaxChannelCount],        single, BeamformerMaxChannelCount) \
	X(compute_stages,                int32_t,  [BeamformerMaxComputeShaderStages], int32,  BeamformerMaxComputeShaderStages) \
	X(compute_stage_parameters,      int16_t,  [BeamformerMaxComputeShaderStages], int16,  BeamformerMaxComputeShaderStages) \
	X(compute_stages_count,          uint32_t, ,                                   uint32, 1) \
	X(data_kind,                     int32_t,  ,                                   int32,  1)

#define X(name, type, size, ...) type name size;
typedef struct {BEAMFORMER_PARAMS_HEAD} BeamformerParametersHead;
typedef struct {BEAMFORMER_UI_PARAMS}   BeamformerUIParameters;

typedef struct {
	BEAMFORMER_PARAMS_HEAD
	BEAMFORMER_UI_PARAMS
} BeamformerParameters;

typedef struct {
	BEAMFORMER_PARAMS_HEAD
	BEAMFORMER_UI_PARAMS
	BEAMFORMER_SIMPLE_PARAMS
} BeamformerSimpleParameters;
#undef X

#define BEAMFORMER_LIVE_IMAGING_DIRTY_FLAG_LIST \
	X(ImagePlaneOffsets, 0) \
	X(TransmitPower,     1) \
	X(TGCControlPoints,  2) \
	X(SaveData,          3) \
	X(SaveNameTag,       4) \
	X(StopImaging,       5)
/* NOTE(rnp): if this exceeds 32 you need to fix the flag handling code */

#define BEAMFORMER_LIVE_IMAGING_PARAMETERS_LIST \
	X(active,               uint32_t, ,                               1)   \
	X(save_enabled,         uint32_t, ,                               1)   \
	X(save_active,          uint32_t, ,                               1)   \
	X(transmit_power,       float,    ,                               1)   \
	X(image_plane_offsets,  float,    [BeamformerViewPlaneTag_Count], BeamformerViewPlaneTag_Count) \
	X(tgc_control_points,   float,    [8],                            8)   \
	X(save_name_tag_length, int32_t,  ,                               1)   \
	X(save_name_tag,        char,     [128],                          128)

#define X(name, type, size, ...) type name size;
typedef struct {BEAMFORMER_LIVE_IMAGING_PARAMETERS_LIST} BeamformerLiveImagingParameters;
#undef X
