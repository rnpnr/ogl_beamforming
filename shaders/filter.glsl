/* See LICENSE for license details. */
/* TODO(rnp): bug: this won't filter RF data correctly */
#define SAMPLE_TYPE vec2
#if DataKind == DataKind_Float32
  #define DATA_TYPE           vec2
  #define RESULT_TYPE_CAST(v) (v)
  #define SAMPLE_TYPE_CAST(v) (v)
#else
  #define DATA_TYPE           uint
  #define RESULT_TYPE_CAST(v) packSnorm2x16(v)
  #define SAMPLE_TYPE_CAST(v) unpackSnorm2x16(v)
#endif

#if ComplexFilter
	#define FILTER_TYPE         vec2
	#define apply_filter(iq, h) complex_mul((iq), (h))
#else
	#define FILTER_TYPE         float
	#define apply_filter(iq, h) ((iq) * (h))
#endif

layout(std430, binding = 1) readonly restrict buffer buffer_1 {
	DATA_TYPE in_data[];
};

layout(std430, binding = 2) writeonly restrict buffer buffer_2 {
	DATA_TYPE out_data[];
};

layout(std430, binding = 3) readonly restrict buffer buffer_3 {
	FILTER_TYPE filter_coefficients[FilterLength];
};

layout(r16i, binding = 1) readonly restrict uniform iimage1D channel_mapping;

vec2 complex_mul(vec2 a, vec2 b)
{
	mat2 m = mat2(b.x, b.y, -b.y, b.x);
	vec2 result = m * a;
	return result;
}

#if Demodulate
vec2 rotate_iq(vec2 iq, int index)
{
	vec2 result;
	switch (SamplingMode) {
	case SamplingMode_4X:{
		// fs = 2 * fd
		// arg = PI * index
		// cos -> 1 -1  1 -1
		// sin -> 0  0  0  0
		const float scales[2] = {1, -1};
		result = scales[index & 1] * iq;
	}break;
	case SamplingMode_2X:{
		// fs  = fd
		// arg = 2 * PI * index
		// cos -> 1 1 1 1
		// sin -> 0 0 0 0
		result = iq;
	}break;
	default:{
		float arg    = radians(360) * DemodulationFrequency * index / SamplingFrequency;
		mat2  phasor = mat2(cos(arg), -sin(arg),
		                    sin(arg),  cos(arg));
		result = phasor * iq;
	}break;
	}
	return result;
}
#endif

SAMPLE_TYPE sample_rf(uint index)
{
	SAMPLE_TYPE result = SAMPLE_TYPE_CAST(in_data[index]);
	return result;
}

shared SAMPLE_TYPE local_samples[FilterLength + gl_WorkGroupSize.x];

void main()
{
	uint out_sample = gl_GlobalInvocationID.x;
	uint channel    = gl_GlobalInvocationID.y;
	uint transmit   = gl_GlobalInvocationID.z;

	uint in_channel = bool(MapChannels) ? imageLoad(channel_mapping, int(channel)).x : channel;
	uint in_offset  = InputChannelStride * in_channel + InputTransmitStride * transmit;
	uint out_offset = OutputChannelStride  * channel +
	                  OutputTransmitStride * transmit +
	                  OutputSampleStride   * out_sample;

	int thread_index = int(gl_LocalInvocationIndex);
	int thread_count = int(gl_WorkGroupSize.x * gl_WorkGroupSize.y * gl_WorkGroupSize.z);
	/////////////////////////
	// NOTE: sample caching
	{
		int min_sample = DecimationRate * int((gl_WorkGroupID.x + 0) * gl_WorkGroupSize.x) - FilterLength;
		int max_sample = DecimationRate * int((gl_WorkGroupID.x + 1) * gl_WorkGroupSize.x);

		in_offset += min_sample;
		int total_samples      = max_sample - min_sample;
		int samples_per_thread = total_samples / thread_count;
		int leftover_count     = total_samples % thread_count;
		int thread_first_index = samples_per_thread * thread_index  + min(thread_index, leftover_count);
		int thread_last_index  = thread_first_index + samples_per_thread + int(thread_index < leftover_count);

		const float scale = bool(ComplexFilter) ? 1 : sqrt(2);
		for (int i = thread_first_index; i <= thread_last_index; i++) {
			SAMPLE_TYPE valid = SAMPLE_TYPE(i + min_sample >= 0);
			#if Demodulate
				local_samples[i] = scale * valid * rotate_iq(sample_rf(in_offset + i) * vec2(1, -1), -i);
			#else
				local_samples[i] = valid * sample_rf(in_offset + i);
			#endif
		}
	}
	barrier();

	if (out_sample < SampleCount / DecimationRate) {
		SAMPLE_TYPE result = SAMPLE_TYPE(0);
		int offset = DecimationRate * thread_index;
		for (int j = 0; j < FilterLength; j++) {
			result += apply_filter(local_samples[offset + j],
			                       filter_coefficients[FilterLength - 1 - j]);
		}
		out_data[out_offset] = RESULT_TYPE_CAST(result);
	}
}
