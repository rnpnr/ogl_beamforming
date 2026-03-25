/* See LICENSE for license details. */

#if CooperativeMatrix
#extension GL_KHR_cooperative_matrix : require
#extension GL_KHR_memory_scope_semantics : require
#endif

layout(std430, buffer_reference, buffer_reference_align = 64) restrict readonly buffer RF {
	InputDataType x[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict writeonly buffer Output {
	OutputDataType x[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict readonly buffer Hadamard {
	f16 x[];
};

OutputDataType sample_rf_data(u32 index)
{
	OutputDataType result = OutputDataType(RF(rf_buffer).x[index]);
	return result;
}

#if UseSharedMemory

shared InputDataType rf[gl_WorkGroupSize.y][TransmitCount];
void run_decode_large(void)
{
	u32 transmit    = gl_GlobalInvocationID.x * ToProcess;
	u32 channel     = gl_GlobalInvocationID.y;
	u32 time_sample = gl_GlobalInvocationID.z;

	const u32 samples_per_thread = TransmitCount / gl_WorkGroupSize.x;
	const u32 leftover_samples   = TransmitCount % gl_WorkGroupSize.x;

	u32 thread_index_x      = gl_LocalInvocationID.x;
	u32 samples_this_thread = samples_per_thread + u32(thread_index_x < leftover_samples);

	u32 rf_offset = TransmitCount * ChunkChannelCount * gl_WorkGroupID.z + TransmitCount * channel;

	for (u32 i = 0; i < samples_this_thread; i++) {
		u32 index = i * gl_WorkGroupSize.x + thread_index_x;
		rf[gl_LocalInvocationID.y][index] = RF(rf_buffer).x[rf_offset + index];
	}

	barrier();

	OutputDataType result[ToProcess];
	if (time_sample < OutputTransmitStride) {
		for (s32 i = 0; i < ToProcess; i++)
			result[i] = OutputDataType(0);

		for (s32 j = 0; j < TransmitCount; j++) {
			OutputDataType s = OutputDataType(rf[gl_LocalInvocationID.y][j]);
			for (s32 i = 0; i < ToProcess; i++)
				result[i] += s * Hadamard(hadamard_buffer).x[TransmitCount * j + (i + transmit)];
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
			if (TransmitCount % (gl_WorkGroupSize.x * ToProcess) == 0 || transmit + i < TransmitCount)
				Output(output_buffer).x[out_off] = result[i];
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
		coopMatLoad(rf_matrix, RF(rf_buffer).x, offset + TransmitCount * rf_tile_row + rf_tile_col,
		            TransmitCount, gl_CooperativeMatrixLayoutRowMajor);

		u32 hadamard_tile_row = k;
		u32 hadamard_tile_col = CooperativeMatrixN * tile_index.x;
		coopMatLoad(hadamard_matrix, Hadamard(hadamard_buffer).x,
		            TransmitCount * hadamard_tile_row + hadamard_tile_col, TransmitCount,
		            gl_CooperativeMatrixLayoutRowMajor);

		result = coopMatMulAdd(rf_matrix, hadamard_matrix, result);
	}

	for (s32 i = 0; i < result.length(); i++)
		result[i] = result[i] / f32(TransmitCount);

	Output out_buffer = Output(output_buffer);
	coopMatStore(result, out_buffer.x, offset + TransmitCount * result_row + result_col,
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
		InputDataType rf[TransmitCount];
		for (s32 j = 0; j < TransmitCount; j++)
			rf[j] = RF(rf_buffer).x[rf_offset + j];

		OutputDataType result[TransmitCount];
		for (s32 j = 0; j < TransmitCount; j++)
			result[j] = OutputDataType(0);

		for (s32 i = 0; i < TransmitCount; i++) {
			OutputDataType s = OutputDataType(rf[i]);
			for (s32 j = 0; j < TransmitCount; j++) {
				result[j] += s * Hadamard(hadamard_buffer).x[TransmitCount * i + j];
			}
		}

		for (int i = 0; i < TransmitCount; i++)
			result[i] /= float(TransmitCount);

		uint out_off = OutputChannelStride  * channel +
		               OutputSampleStride   * time_sample;
		for (int i = 0; i < TransmitCount; i++, out_off += OutputTransmitStride)
			Output(output_buffer).x[out_off] = result[i];
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
