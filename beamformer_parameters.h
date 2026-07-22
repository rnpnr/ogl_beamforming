/* See LICENSE for license details. */

/* TODO(rnp):
 * [ ]: Upload previously exported data for display. maybe this is a UI thing but doing it
 *      programatically would be nice.
 * [ ]: Add interface for multi frame upload. RF upload already uses an offset into SM so
 *      that part works fine. We just need a way of specify a multi frame upload. (Data must
 *      be organized for simple offset access per frame).
 */

typedef struct {
	uint64_t shader_count;
	uint32_t shader_ids[BeamformerMaxComputeShaderStages];
	/* NOTE(rnp): this wants to be iterated on both dimensions. it depends entirely on which
	 * visualization method you want to use. the coalescing function wants both directions */
	float    times[32][BeamformerMaxComputeShaderStages];
	float    rf_time_deltas[32];
} BeamformerComputeStatsTable;
