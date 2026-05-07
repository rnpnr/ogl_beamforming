/* See LICENSE for license details. */
layout(std430, buffer_reference, buffer_reference_align = 8) restrict buffer Int16 {
	int16_t values[];
};

layout(std430, buffer_reference, buffer_reference_align = 8) restrict buffer Int16Complex {
	i16vec2 values[];
};

layout(std430, buffer_reference, buffer_reference_align = 8) restrict buffer Float32 {
	float values[];
};

layout(std430, buffer_reference, buffer_reference_align = 8) restrict buffer Float32Complex {
	vec2 values[];
};

#if   DataKind == DataKind_Float32
  #define COHERENT_SAMPLE(index)    Float32(left_side_buffer).values[index]
  #define INCOHERENT_SAMPLE(index)  Float32(right_side_buffer).values[index]
#elif DataKind == DataKind_Float32Complex
  #define COHERENT_SAMPLE(index)    Float32Complex(left_side_buffer).values[index]
  #define INCOHERENT_SAMPLE(index)  Float32(right_side_buffer).values[index]
#else
  #error DataKind unsupported for CoherencyWeighting
#endif

uint32_t output_index(uint32_t x, uint32_t y, uint32_t z)
{
	uint32_t result = output_size_x * output_size_y * z + output_size_x * y + x;
	return result;
}

void main()
{
	uvec3 out_voxel = gl_GlobalInvocationID;
	if (!all(lessThan(out_voxel, uvec3(output_size_x, output_size_y, output_size_z))))
		return;
	uint32_t index = output_index(out_voxel.x, out_voxel.y, out_voxel.z);
  COHERENT_SAMPLE(index) *= COHERENT_SAMPLE(index) / INCOHERENT_SAMPLE(index);
}
