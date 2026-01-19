/* See LICENSE for license details. */

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
	X(FilterSlots,                 4) \
	X(MaxChannelCount,           256) \
	X(MaxComputeShaderStages,     16) \
	X(MaxParameterBlockSlots,     16) \
	X(MaxRawDataFramesInFlight,    3) \
	X(MaxBacklogFrames,         4096) \

#define X(k, v, ...) Beamformer##k = v,
typedef enum {BEAMFORMER_CONSTANTS_LIST} BeamformerConstants;
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
