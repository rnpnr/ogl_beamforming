/* See LICENSE for license details. */
layout(std430, buffer_reference) restrict buffer F16 {
	f16 x[];
};

layout(std430, buffer_reference) restrict buffer F32 {
	f32 x[];
};

layout(std430, buffer_reference) restrict buffer F32C {
	f32vec2 x[];
};

layout(std430, buffer_reference) restrict buffer U32 {
	u32 x[];
};

layout(std430, buffer_reference) buffer Input {
	InputDataType x[];
};

shared InputDataType data[Order][gl_WorkGroupSize.x];

f32vec2 complex_mul(f32vec2 a, f32vec2 b)
{
	mat2 m = mat2(b.x, b.y, -b.y, b.x);
	f32vec2 result = m * a;
	return result;
}

void main()
{
	const uvec3 total_threads = gl_WorkGroupSize * gl_NumWorkGroups;
	u32 index = gl_GlobalInvocationID.z * total_threads.x * total_threads.y +
	            gl_GlobalInvocationID.y * total_threads.x +
	            gl_GlobalInvocationID.x;

	////////////////////////////
	// NOTE(rnp): gather voxels
	const u32 thread_count = gl_WorkGroupSize.x * gl_WorkGroupSize.y * gl_WorkGroupSize.z;
	const u32 tid          = gl_LocalInvocationIndex;
	const u32 n_per_thread = Order;

	for (u32 i = 0; i < n_per_thread; i++) {
		Input image = Input(ImageBuffer + U32(ImageOffsets).x[i]);
		data[i][tid] = index < OutputVoxels ? image.x[index] : InputDataType(0);
	}

	////////////////////////////
	// NOTE(rnp): filter
	vec2 last = vec2(0.f);
	vec2 r1   = vec2(0.f);
	f32  r0   = 0.f;
	F16  f    = F16(FilterMatrix);
	for (u32 j = 0; j < Order; j++) {
		vec2 s = vec2(0.f);
		for (u32 i = 0; i < Order; i++)
			s += f.x[j * Order + i] * data[i][tid];

		r0 += dot(s, s);
		if (j != 0)
			r1 += complex_mul(s, last);

		last.x = s.x;
		last.y = -s.y;
	}

	////////////////////////////
	// NOTE(rnp): scatter results
	if (index < OutputVoxels) {
		F32 outbuf   = F32(OutputBuffer);
		F32 feedback = F32(FeedbackBuffer);
		// NOTE(rnp): apply some persistence with the previous output
		f32 a = 0.3f;
		outbuf.x[3 * index + 0] = a * r0    + (1.f - a) * feedback.x[3 * index + 0];
		outbuf.x[3 * index + 1] = a * r1[0] + (1.f - a) * feedback.x[3 * index + 1];
		outbuf.x[3 * index + 2] = a * r1[1] + (1.f - a) * feedback.x[3 * index + 2];
	}
}
