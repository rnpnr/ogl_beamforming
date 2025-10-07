/* See LICENSE for license details. */
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
	FILTER_TYPE filter_coefficients[];
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

vec2 sample_rf(uint index)
{
	vec2 result = SAMPLE_TYPE_CAST(in_data[index]);
	return result;
}

void main()
{
	uint in_sample  = gl_GlobalInvocationID.x * DecimationRate;
	uint out_sample = gl_GlobalInvocationID.x;
	uint channel    = gl_GlobalInvocationID.y;
	uint transmit   = gl_GlobalInvocationID.z;

	uint in_channel = bool(MapChannels) ? imageLoad(channel_mapping, int(channel)).x : channel;
	uint in_offset  = InputChannelStride * in_channel + InputTransmitStride * transmit;
	uint out_offset = OutputChannelStride  * channel +
	                  OutputTransmitStride * transmit +
	                  OutputSampleStride   * out_sample;

	int target;
	if (bool(MapChannels)) {
		target = OutputChannelStride / OutputSampleStride;
	} else {
		target = OutputTransmitStride;
	}

	if (out_sample < target) {
		target *= DecimationRate;

		vec2 result  = vec2(0);
		int a_length = target;
		int index    = int(in_sample);

		const float scale = bool(ComplexFilter) ? 1 : sqrt(2);

		for (int j = max(0, index - FilterLength); j < min(index, a_length); j++) {
			vec2        iq = sample_rf(in_offset + j);
			FILTER_TYPE h  = filter_coefficients[index - j];
		#if Demodulate
			result  += scale * apply_filter(rotate_iq(iq * vec2(1, -1), -j), h);
		#else
			result  += apply_filter(iq, h);
		#endif
		}

		out_data[out_offset] = RESULT_TYPE_CAST(result);
	}
}
