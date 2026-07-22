/* See LICENSE for license details. */

typedef struct {
	u64 shader_count;
	u32 shader_ids[BeamformerMaxComputeShaderStages];
	/* NOTE(rnp): this wants to be iterated on both dimensions. it depends entirely on which
	 * visualization method you want to use. the coalescing function wants both directions */
	f32 times[32][BeamformerMaxComputeShaderStages];
	f32 rf_time_deltas[32];
} BeamformerComputeStatsTable;
