/* See LICENSE for license details. */
#if   defined(INPUT_DATA_TYPE_FLOAT)
	#define INPUT_DATA_TYPE        vec2
	#define RF_SAMPLES_PER_INDEX   1
	#define RESULT_TYPE_CAST(x)    (x)
	#define SAMPLE_TYPE_CAST(v, i) (v).x
#else
	#define INPUT_DATA_TYPE          uint
	#define RF_SAMPLES_PER_INDEX     2
	#define RESULT_TYPE_CAST(v)      packSnorm2x16(v)
	#define SAMPLE_TYPE_CAST(v, i)   unpackSnorm2x16(v)[(~(i)) & 1u]
	/* NOTE(rnp): for outputting complex floats */
	//#define RESULT_TYPE_CAST(v)      (v)
	//#define SAMPLE_TYPE_CAST(v, i)   unpackSnorm2x16(v)[(~(i)) & 1u] * 32767.0f
#endif

layout(std430, binding = 1) readonly restrict buffer buffer_1 {
	INPUT_DATA_TYPE in_data[];
};

layout(std430, binding = 2) writeonly restrict buffer buffer_2 {
	INPUT_DATA_TYPE out_data[];
};

layout(r32f, binding = 0) readonly restrict uniform  image1D filter_coefficients;
layout(r16i, binding = 1) readonly restrict uniform iimage1D channel_mapping;

float sample_rf(uint index)
{
	float result = SAMPLE_TYPE_CAST(in_data[index], index);
	return result;
}

void main()
{
	uint in_sample  = gl_GlobalInvocationID.x * decimation_rate;
	uint channel    = gl_GlobalInvocationID.y;
	uint transmit   = gl_GlobalInvocationID.z;

	uint in_channel = map_channels ? imageLoad(channel_mapping, int(channel)).x : channel;
	uint in_offset  = input_channel_stride * in_channel + input_transmit_stride * transmit;
	uint out_offset = output_channel_stride  * channel +
	                  output_transmit_stride * transmit +
	                  output_sample_stride   * gl_GlobalInvocationID.x;

	float arg    = radians(360) * demodulation_frequency / sampling_frequency;
	vec2  result = vec2(0);
	for (int i = 0; i < imageSize(filter_coefficients).x; i++) {
		uint index = in_sample + i;
		if (index < input_transmit_stride) {
			float data = sample_rf((in_offset + index) / RF_SAMPLES_PER_INDEX);
			vec2 iq = sqrt(2.0f) * data * vec2(cos(arg * index), -sin(arg * index));
			result += imageLoad(filter_coefficients, imageSize(filter_coefficients).x - i - 1).x * iq;
		}
	}
	out_data[out_offset] = RESULT_TYPE_CAST(result);
}
