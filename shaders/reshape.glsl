/* See LICENSE for license details. */

#if   InputDataKind == DataKind_Float32Complex
  #define Input Float32Complex
#elif InputDataKind == DataKind_Float32
  #define Input Float32
#elif InputDataKind == DataKind_Float16Complex || InputDataKind == DataKind_Int16Complex
  #define Input Int16Complex
#elif InputDataKind == DataKind_Float16 || InputDataKind == DataKind_Int16
  #define Input Int16
#else
  #error unsupported data kind for Reshape
#endif

#if   OutputDataKind == DataKind_Float32Complex
  #define Output     Float32Complex
  #define OutputKind f32vec2
#elif OutputDataKind == DataKind_Float32
  #define Output     Float32
  #define OutputKind f32
#elif OutputDataKind == DataKind_Float16Complex || OutputDataKind == DataKind_Int16Complex
  #define Output     Int16Complex
  #define OutputKind s16vec2
#elif OutputDataKind == DataKind_Float16 || OutputDataKind == DataKind_Int16
  #define Output     Int16
  #define OutputKind s16
#else
  #error unsupported data kind for Reshape
#endif

layout(std430, buffer_reference, buffer_reference_align = 8) restrict buffer Int16 {
	s16 x[];
};

layout(std430, buffer_reference, buffer_reference_align = 8) restrict buffer Int16Complex {
	s16vec2 x[];
};

layout(std430, buffer_reference, buffer_reference_align = 8) restrict buffer Float32 {
	f32 x[];
};

layout(std430, buffer_reference, buffer_reference_align = 8) restrict buffer Float32Complex {
	f32vec2 x[];
};

void main(void)
{
	if (all(lessThan(gl_GlobalInvocationID, uvec3(SizeX, SizeY, SizeZ)))) {
		u32 x = gl_GlobalInvocationID.x;
		u32 y = gl_GlobalInvocationID.y;
		u32 z = gl_GlobalInvocationID.z;

		u32 input_index  = InputStrideX  * x + InputStrideY  * y + InputStrideZ  * z;
		u32 output_index = OutputStrideX * x + OutputStrideY * y + OutputStrideZ * z;

		OutputKind out_value = OutputKind(0);

		#if Interleave
		out_value[0] = Input(left_input_buffer).x[input_index];
		out_value[1] = Input(right_input_buffer).x[input_index];
		#else
		out_value = Input(left_input_buffer).x[input_index];
		#endif

		Output(output_buffer).x[output_index] = out_value;
	}
}
