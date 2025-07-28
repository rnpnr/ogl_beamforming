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

layout(r32f, binding = 0) readonly restrict uniform  image1D filter_coefficients;
layout(r16i, binding = 1) readonly restrict uniform iimage1D channel_mapping;

#if defined(DEMODULATE)
  #define run_filter demodulate
#else
  #define run_filter filter_base
#endif

vec2 rotate_iq(vec2 iq, int index)
{
	float arg    = radians(360) * demodulation_frequency * index / sampling_frequency;
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

vec2 filter_base(uint out_sample, uint in_offset, int index, int start, int target)
{
	vec2 result = vec2(0);
	for (int i   = start; i < imageSize(filter_coefficients).x && index < target; i++, index++) {
		vec2 iq  = rotate_iq(sample_rf(in_offset + index), int(out_sample));
		result  += iq * imageLoad(filter_coefficients, imageSize(filter_coefficients).x - i - 1).x;
	}
	result = rotate_iq(result, -int(out_sample));
	return result;
}

vec2 demodulate(uint out_sample, uint in_offset, int index, int start, int target)
{
	vec2 result = vec2(0);
	for (int i = start; i < imageSize(filter_coefficients).x && index < target; i++, index++) {
		vec2 iq  = sqrt(2.0f) * rotate_iq(sample_rf(in_offset + index) * vec2(1, -1), -index);
		result  += iq * imageLoad(filter_coefficients, imageSize(filter_coefficients).x - i - 1).x;
	}
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
		int  index   = int(in_sample) - imageSize(filter_coefficients).x;
		int  start   = index < 0 ? -index : 0;
		index       += start;
		vec2 result  = run_filter(out_sample, in_offset, index, start, target * int(decimation_rate));
		out_data[out_offset] = RESULT_TYPE_CAST(result);
	}
}
