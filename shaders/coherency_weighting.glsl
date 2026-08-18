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

void main()
{
	const uvec3 total_threads = gl_WorkGroupSize * gl_NumWorkGroups;
	u32 index = gl_GlobalInvocationID.z * total_threads.x * total_threads.y +
	            gl_GlobalInvocationID.y * total_threads.x +
	            gl_GlobalInvocationID.x;

	if (index < OutputVoxels)
		COHERENT_SAMPLE(index) *= Scale * COHERENT_SAMPLE(index) / INCOHERENT_SAMPLE(index);
}
