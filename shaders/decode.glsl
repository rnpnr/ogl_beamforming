/* See LICENSE for license details. */

/* NOTE(rnp): invoked with samples x channels x transmits
 * Each instance extracts a single time sample from a single channel for all transmits
 * and does a dot product with the appropriate row of the bound hadamard matrix
 * (unless decode_mode == DECODE_MODE_NONE). The result of this dot product is stored in the
 * output. In bulk this has the effect of computing a matrix multiply of the
 * sample-transmit plane with the bound hadamard matrix.
 */

#if   DataKind == DataKind_Float32
  #define INPUT_DATA_TYPE  float
  #define SAMPLE_DATA_TYPE float
#elif DataKind == DataKind_Float32Complex
  #define INPUT_DATA_TYPE  vec2
  #define SAMPLE_DATA_TYPE vec2
#elif DataKind == DataKind_Int16Complex
  #define INPUT_DATA_TYPE  i16vec2
  #define SAMPLE_DATA_TYPE vec2
#elif DataKind == DataKind_Int16
  #define INPUT_DATA_TYPE  int16_t
  #define SAMPLE_DATA_TYPE float
#else
  #error unsupported data kind for Decode
#endif

// TODO(rnp): fix DilateOutput

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
	float values[];
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

	uint rf_offset = InputChannelStride * channel + TransmitCount * gl_WorkGroupID.x * gl_WorkGroupSize.x;

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

void run_decode_small(void)
{
	uint time_sample = gl_GlobalInvocationID.x;
	uint channel     = gl_GlobalInvocationID.y;
	uint rf_offset   = InputChannelStride * channel + TransmitCount * time_sample;

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
	case DecodeMode_None:{
		uint time_sample = gl_GlobalInvocationID.x;
		uint channel     = gl_GlobalInvocationID.y;
		uint transmit    = gl_GlobalInvocationID.z;

		if (time_sample < OutputTransmitStride) {
			uint in_off = InputChannelStride  * channel +
			              InputTransmitStride * transmit +
			              InputSampleStride   * time_sample;

			uint out_off = OutputChannelStride  * channel +
			               OutputTransmitStride * transmit +
			               OutputSampleStride   * time_sample;

			Output(output_buffer).values[out_off] = sample_rf_data(in_off);
		}
	}break;
	case DecodeMode_Hadamard:{
		if (first_pass) {
			uint time_sample = gl_GlobalInvocationID.x;
			uint channel     = gl_GlobalInvocationID.y;
			uint transmit    = gl_GlobalInvocationID.z * ToProcess;
			if (time_sample < InputTransmitStride) {
				uint out_off = InputChannelStride * channel + TransmitCount     * time_sample;
				uint in_off  = InputChannelStride * channel + InputSampleStride * time_sample;
				#if UseSharedMemory
					in_off  += InputTransmitStride * transmit;
					out_off += transmit;
					for (uint i = 0; i < ToProcess; i++, in_off += InputTransmitStride) {
						if (transmit + i < TransmitCount)
							OutputRF(output_rf_buffer).values[out_off + i] = RF(rf_buffer).values[in_off];
					}
				#else
					for (uint i = 0; i < TransmitCount; i++, in_off += InputTransmitStride)
						OutputRF(output_rf_buffer).values[out_off + i] = RF(rf_buffer).values[in_off];
				#endif
			}
		} else {
			#if UseSharedMemory
				run_decode_large();
			#else
				run_decode_small();
			#endif
		}
	}break;
	}
}
