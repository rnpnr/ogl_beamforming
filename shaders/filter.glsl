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
vec2 rotate_iq(vec2 iq, uint index)
{
	vec2 result;
	switch (SamplingMode) {
	case SamplingMode_4X:{
		// fs = 2 * fd
		// arg = PI * index
		// cos -> 1 -1  1 -1
		// sin -> 0  0  0  0
		const float scales[2] = {1, -1};
		result = scales[index & 1u] * iq;
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

shared SAMPLE_TYPE rf[FilterLength + gl_WorkGroupSize.x - 1];

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

	uint thread_index = gl_LocalInvocationIndex;
	uint thread_count = gl_WorkGroupSize.x * gl_WorkGroupSize.y * gl_WorkGroupSize.z;
	/////////////////////////
	// NOTE: sample caching
	{
		in_offset += DecimationRate * gl_WorkGroupID.x * gl_WorkGroupSize.x - (FilterLength - 1);

		uint total_samples       = rf.length();
		uint samples_per_thread  = total_samples / thread_count;
		uint leftover_count      = total_samples % thread_count;
		uint samples_this_thread = samples_per_thread + uint(thread_index < leftover_count);

		const float scale = bool(ComplexFilter) ? 1 : sqrt(2);
		for (uint i = 0; i < samples_this_thread; i++) {
			uint index = thread_count * i + thread_index;
			if (gl_WorkGroupID.x == 0 && index < FilterLength) {
				rf[index] = SAMPLE_TYPE(0);
			} else {
				#if Demodulate
					rf[index] = scale * rotate_iq(sample_rf(in_offset + index) * vec2(1, -1), -index);
				#else
					rf[index] = sample_rf(in_offset + index);
				#endif
			}
		}
	}
	barrier();

	if (out_sample < SampleCount / DecimationRate) {
		SAMPLE_TYPE result = SAMPLE_TYPE(0);
		uint offset = DecimationRate * thread_index;
		for (uint j = 0; j < FilterLength; j++)
			result += apply_filter(rf[offset + j], filter_coefficients[j]);
		out_data[out_offset] = RESULT_TYPE_CAST(result);
	}
}
