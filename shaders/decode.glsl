/* See LICENSE for license details. */

#if CooperativeMatrix
#extension GL_KHR_cooperative_matrix : require
#extension GL_KHR_memory_scope_semantics : require
#endif

#if   DataKind == DataKind_Float32
  #define INPUT_DATA_TYPE  f32
#elif DataKind == DataKind_Float16
  #define INPUT_DATA_TYPE  f16
#elif DataKind == DataKind_Int16
  #define INPUT_DATA_TYPE  s16
#else
  #error unsupported data kind for Decode
#endif

#define SAMPLE_DATA_TYPE f32

layout(std430, buffer_reference, buffer_reference_align = 64) restrict readonly buffer RF {
	INPUT_DATA_TYPE values[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict writeonly buffer OutputRF {
	INPUT_DATA_TYPE values[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict writeonly buffer Output {
	SAMPLE_DATA_TYPE values[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict readonly buffer Hadamard {
	f16 values[];
};

SAMPLE_DATA_TYPE sample_rf_data(uint index)
{
	SAMPLE_DATA_TYPE result = SAMPLE_DATA_TYPE(RF(rf_buffer).values[index]);
	return result;
}

#if UseSharedMemory

shared INPUT_DATA_TYPE rf[gl_WorkGroupSize.x * TransmitCount];
void run_decode_large(void)
{
	uint time_sample = gl_GlobalInvocationID.x;
	uint channel     = gl_GlobalInvocationID.y;
	uint transmit    = gl_GlobalInvocationID.z * ToProcess;

	uint thread_count = gl_WorkGroupSize.x * gl_WorkGroupSize.y * gl_WorkGroupSize.z;
	uint thread_index = gl_LocalInvocationIndex;

	uint samples_per_thread  = rf.length() / thread_count;
	uint leftover_samples    = rf.length() % thread_count;
	uint samples_this_thread = samples_per_thread + uint(thread_index < leftover_samples);

	u32 rf_offset = TransmitCount * ChunkChannelCount * gl_WorkGroupID.x * gl_WorkGroupSize.x + TransmitCount * channel;

	for (uint i = 0; i < samples_this_thread; i++) {
		uint index = i * thread_count + thread_index;
		rf[index] = RF(rf_buffer).values[rf_offset + index];
	}

	barrier();

	SAMPLE_DATA_TYPE result[ToProcess];
	if (time_sample < OutputTransmitStride) {
		for (uint i = 0; i < ToProcess; i++)
			result[i] = SAMPLE_DATA_TYPE(0);

		for (int j = 0; j < TransmitCount; j++) {
			SAMPLE_DATA_TYPE s = SAMPLE_DATA_TYPE(rf[gl_LocalInvocationID.x * TransmitCount + j]);
			for (uint i = 0; i < ToProcess; i++)
				result[i] += s * Hadamard(hadamard_buffer).values[TransmitCount * j + (i + transmit)];
		}

		for (uint i = 0; i < ToProcess; i++)
			result[i] /= float(TransmitCount);
	}

	/* NOTE(rnp): DO NOT combine with above; compiler shits the bed on TransmitCount == 80
	 * and it kills performance. reinvestigate when we further optimize */
	if (time_sample < OutputTransmitStride) {
		uint out_off = OutputChannelStride  * channel +
		               OutputTransmitStride * transmit +
		               OutputSampleStride   * time_sample;

		for (uint i = 0; i < ToProcess; i++, out_off += OutputTransmitStride)
			if (TransmitCount % (gl_WorkGroupSize.z * ToProcess) == 0 || transmit + i < TransmitCount)
				Output(output_buffer).values[out_off] = result[i];
	}
}
#endif

#if CooperativeMatrix

void run_decode_coop(void)
{
	#if UseSharedMemory
	#else

	u32vec2 tile_index  = gl_WorkGroupID.xy;
	u32     time_sample = gl_WorkGroupID.z;

	coopmat<f16, gl_ScopeSubgroup, CooperativeMatrixM, CooperativeMatrixK, gl_MatrixUseA>           rf_matrix;
	coopmat<f16, gl_ScopeSubgroup, CooperativeMatrixK, CooperativeMatrixN, gl_MatrixUseB>           hadamard_matrix;
	coopmat<f32, gl_ScopeSubgroup, CooperativeMatrixM, CooperativeMatrixN, gl_MatrixUseAccumulator> result;
	result = coopmat<f32, gl_ScopeSubgroup, CooperativeMatrixM, CooperativeMatrixN, gl_MatrixUseAccumulator>(0.0f);

	u32 result_row = CooperativeMatrixM * tile_index.y;
	u32 result_col = CooperativeMatrixN * tile_index.x;

	u32 offset = ChunkChannelCount * TransmitCount * time_sample;

	for (u32 k = 0; k < TransmitCount; k += CooperativeMatrixK) {
		u32 rf_tile_row = CooperativeMatrixM * tile_index.y;
		u32 rf_tile_col = k;
		coopMatLoad(rf_matrix, RF(rf_buffer).values, offset + TransmitCount * rf_tile_row + rf_tile_col,
		            TransmitCount, gl_CooperativeMatrixLayoutRowMajor);

		u32 hadamard_tile_row = k;
		u32 hadamard_tile_col = CooperativeMatrixN * tile_index.x;
		coopMatLoad(hadamard_matrix, Hadamard(hadamard_buffer).values,
		            TransmitCount * hadamard_tile_row + hadamard_tile_col, TransmitCount,
		            gl_CooperativeMatrixLayoutRowMajor);

		result = coopMatMulAdd(rf_matrix, hadamard_matrix, result);
	}

	for (s32 i = 0; i < result.length(); i++)
		result[i] = result[i] / f32(TransmitCount);

	Output out_buffer = Output(output_buffer);
	coopMatStore(result, out_buffer.values, offset + TransmitCount * result_row + result_col,
	             TransmitCount, gl_CooperativeMatrixLayoutRowMajor);
	#endif
}
#endif

void run_decode_small(void)
{
	u32 time_sample = gl_GlobalInvocationID.x;
	u32 channel     = gl_GlobalInvocationID.y;
	u32 rf_offset   = TransmitCount * ChunkChannelCount * time_sample + TransmitCount * channel;

	if (time_sample < OutputTransmitStride) {
		INPUT_DATA_TYPE rf[TransmitCount];
		for (int j = 0; j < TransmitCount; j++)
			rf[j] = RF(rf_buffer).values[rf_offset + j];

		SAMPLE_DATA_TYPE result[TransmitCount];
		for (int j = 0; j < TransmitCount; j++)
			result[j] = SAMPLE_DATA_TYPE(0);

		for (int i = 0; i < TransmitCount; i++) {
			SAMPLE_DATA_TYPE s = SAMPLE_DATA_TYPE(rf[i]);
			for (int j = 0; j < TransmitCount; j++) {
				result[j] += s * Hadamard(hadamard_buffer).values[TransmitCount * i + j];
			}
		}

		for (int i = 0; i < TransmitCount; i++)
			result[i] /= float(TransmitCount);

		uint out_off = OutputChannelStride  * channel +
		               OutputSampleStride   * time_sample;
		for (int i = 0; i < TransmitCount; i++, out_off += OutputTransmitStride)
			Output(output_buffer).values[out_off] = result[i];
	}
}

void main()
{
	switch (DecodeMode) {
	case DecodeMode_Hadamard:{
		#if CooperativeMatrix
			run_decode_coop();
		#elif UseSharedMemory
			run_decode_large();
		#else
			run_decode_small();
		#endif
	}break;
	}
}
