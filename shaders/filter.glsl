/* See LICENSE for license details. */
#if   defined(INPUT_DATA_TYPE_FLOAT)
  #define DATA_TYPE           vec2
  #define RESULT_TYPE_CAST(v) (v)
  #define SAMPLE_TYPE_CAST(v) (v)
#else
  #define DATA_TYPE           uint
  #define RESULT_TYPE_CAST(v) packSnorm2x16(v)
  #define SAMPLE_TYPE_CAST(v) unpackSnorm2x16(v)
#endif

layout(std430, binding = 1) readonly restrict buffer buffer_1 {
	DATA_TYPE in_data[];
};

layout(std430, binding = 2) writeonly restrict buffer buffer_2 {
	DATA_TYPE out_data[];
};

layout(r16i, binding = 1) readonly restrict uniform iimage1D channel_mapping;

#if COMPLEX_FILTER
	layout(rg32f, binding = 0) readonly restrict uniform  image1D filter_coefficients;
	#define apply_filter(iq, h) complex_mul((iq), (h).xy)
#else
	layout(r32f, binding = 0) readonly restrict uniform  image1D filter_coefficients;
	#define apply_filter(iq, h) ((iq) * (h).x)
#endif

vec2 complex_mul(vec2 a, vec2 b)
{
	mat2 m = mat2(b.x, b.y, -b.y, b.x);
	vec2 result = m * a;
	return result;
}

vec2 rotate_iq(vec2 iq, int index)
{
	float arg    = radians(360) * demodulation_frequency * index / sampling_frequency;
	/* TODO(rnp): this can be optimized based on the sampling mode. for  4x sampling
	 * (NS200BW) coefficients cycle through (cos) {1, 0, -1, 0} (sin) {0, -1, 0, 1}
	 * so we don't actually need to use the special function unit.  There should be an
	 * equivalent for BS100BW and BS50BW as well */
	mat2  phasor = mat2(cos(arg), -sin(arg),
	                    sin(arg),  cos(arg));
	vec2  result = phasor * iq;
	return result;
}

vec2 sample_rf(uint index)
{
	vec2 result = SAMPLE_TYPE_CAST(in_data[index]);
	return result;
}

void main()
{
	uint in_sample  = gl_GlobalInvocationID.x * decimation_rate;
	uint out_sample = gl_GlobalInvocationID.x;
	uint channel    = gl_GlobalInvocationID.y;
	uint transmit   = gl_GlobalInvocationID.z;

	uint in_channel = map_channels ? imageLoad(channel_mapping, int(channel)).x : channel;
	uint in_offset  = input_channel_stride * in_channel + input_transmit_stride * transmit;
	uint out_offset = output_channel_stride  * channel +
	                  output_transmit_stride * transmit +
	                  output_sample_stride   * out_sample;

	int target;
	if (map_channels) {
		target = int(output_channel_stride / output_sample_stride);
	} else {
		target = int(output_transmit_stride);
	}

	if (out_sample < target) {
		target *= int(decimation_rate);

		vec2 result  = vec2(0);
		int a_length = target;
		int b_length = imageSize(filter_coefficients).x;
		int index    = int(in_sample);

		const float scale = bool(COMPLEX_FILTER) ? 1 : sqrt(2);

		for (int j = max(0, index - b_length); j < min(index, a_length); j++) {
			vec2 iq  = sample_rf(in_offset + j);
			vec4 h   = imageLoad(filter_coefficients, index - j);
		#if defined(DEMODULATE)
			result  += scale * apply_filter(rotate_iq(iq * vec2(1, -1), -j), h);
		#else
			result  += apply_filter(iq, h);
		#endif
		}

		out_data[out_offset] = RESULT_TYPE_CAST(result);
	}
}
