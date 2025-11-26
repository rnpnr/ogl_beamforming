/* See LICENSE for license details. */
layout(std430, binding = 0) restrict buffer output_buffer {
	float output_data[Elements][Components];
};

layout(std430, binding = 1) readonly restrict buffer input_buffer {
	float input_data[Elements][Components];
};

void main(void)
{
	uint element = gl_GlobalInvocationID.x;
	if (element < Elements) {
		for (uint c = 0; c < Components; c++)
			output_data[element][c] += Scale * input_data[element][c];
	}
}
