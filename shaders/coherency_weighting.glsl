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

#if   InputDataKind == DataKind_Float32
  #define COHERENT_SAMPLE(index)    Float32(coherent_sum).values[index]
  #define INCOHERENT_SAMPLE(index)  Float32(IncoherentSum).values[index]
#elif InputDataKind == DataKind_Float32Complex
  #define COHERENT_SAMPLE(index)    Float32Complex(coherent_sum).values[index]
  #define INCOHERENT_SAMPLE(index)  Float32(IncoherentSum).values[index]
#else
  #error DataKind unsupported for CoherencyWeighting
#endif

u32 output_index(const u32 x, const u32 y, const u32 z)
{
	u32 result = OutputSizeX * OutputSizeY * z + OutputSizeX * y + x;
	return result;
}

void main()
{
	uvec3 out_voxel = gl_GlobalInvocationID;
	if (!all(lessThan(out_voxel, uvec3(OutputSizeX, OutputSizeY, OutputSizeZ))))
		return;
	u32 index = output_index(out_voxel.x, out_voxel.y, out_voxel.z);
	COHERENT_SAMPLE(index) *= Scale * COHERENT_SAMPLE(index) / INCOHERENT_SAMPLE(index);
}
