/* See LICENSE for license details. */

layout(std430, buffer_reference) readonly buffer Input  { InputDataType  x[]; };
layout(std430, buffer_reference)          buffer Output { OutputDataType x[]; };

void main(void)
{
	if (all(lessThan(gl_GlobalInvocationID, uvec3(SizeX, SizeY, SizeZ)))) {
		u32 x = gl_GlobalInvocationID.x;
		u32 y = gl_GlobalInvocationID.y;
		u32 z = gl_GlobalInvocationID.z;

		u32 input_index  = InputStrideX  * x + InputStrideY  * y + InputStrideZ  * z;
		u32 output_index = OutputStrideX * x + OutputStrideY * y + OutputStrideZ * z;

		OutputDataType out_value = OutputDataType(0);

		#if Interleave
		out_value[0] = Input(left_input_buffer).x[input_index];
		out_value[1] = Input(right_input_buffer).x[input_index];
		#else
		out_value = Input(left_input_buffer).x[input_index];
		#endif

		Output(output_buffer).x[output_index] = out_value;
	}
}
