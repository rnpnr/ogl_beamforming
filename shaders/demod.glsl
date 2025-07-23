/* See LICENSE for license details. */
layout(std430, binding = 1) readonly restrict buffer buffer_1 {
	vec2 in_data[];
};

layout(std430, binding = 2) writeonly restrict buffer buffer_2 {
	vec2 out_data[];
};

layout(r32f, binding = 0) readonly  restrict uniform image1D filter_coefficients;

void main()
{
	uint in_sample  = gl_GlobalInvocationID.x * decimation_rate;
	uint out_sample = gl_GlobalInvocationID.x;
	uint channel    = gl_GlobalInvocationID.y;
	uint transmit   = gl_GlobalInvocationID.z;

	uint in_offset  = (dec_data_dim.x * dec_data_dim.z * channel + dec_data_dim.x * transmit);
	uint out_offset = (dec_data_dim.x * dec_data_dim.z * channel + dec_data_dim.x * transmit) + out_sample;

	float arg    = radians(360) * center_frequency / (sampling_frequency * decimation_rate);
	vec2  result = vec2(0);
	for (int i = 0; i < imageSize(filter_coefficients).x; i++) {
		int index = int(in_sample + i);
		if (index < dec_data_dim.x) {
			float data = in_data[in_offset + index].x;
			vec2 iq = sqrt(2.0f) * data * vec2(cos(arg * index), -sin(arg * index));
			result += imageLoad(filter_coefficients, imageSize(filter_coefficients).x - i - 1).x * iq;
		}
	}
	out_data[out_offset] = result;
}
