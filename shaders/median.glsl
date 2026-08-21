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

u32 output_index(const u32 x, const u32 y, const u32 z)
{
	u32 result = OutputSizeX * OutputSizeY * z + OutputSizeX * y + x;
	return result;
}

u32 output_dimension()
{
	const u32 result = u32(OutputSizeX > 1) + u32(OutputSizeY > 1) + u32(OutputSizeZ > 1);
	return result;
}

#define swap2(a, b) { vec3 t = a; a = min(a, b); b = max(t, b); }
void median_2d_3x3()
{
	uvec3 out_voxel = gl_GlobalInvocationID;
	if (!all(lessThan(out_voxel, uvec3(OutputSizeX, OutputSizeY, OutputSizeZ))))
		return;

	vec3 v[9];
	s32 index = 0;
	for (s32 y = -1; y <= 1; ++y) {
		for (s32 x = -1; x <= 1; ++x) {
			ivec3 voxel = ivec3(out_voxel) + ivec3(x, y, 0);
			u32 voxel_index = output_index(u32(voxel.x), u32(voxel.y), u32(voxel.z));
			if (all(lessThan(voxel, ivec3(OutputSizeX, OutputSizeY, OutputSizeZ))) &&
			    all(greaterThanEqual(voxel, ivec3(0))))
			{
				v[index][0] = F32(InputBuffer).x[3 * voxel_index + 0];
				v[index][1] = F32(InputBuffer).x[3 * voxel_index + 1];
				v[index][2] = F32(InputBuffer).x[3 * voxel_index + 2];
				index++;
			} else {
				v[index++] = vec3(0.f);
			}
		}
	}

	// NOTE(rnp): branchless sorting network
	swap2(v[1], v[2]); swap2(v[4], v[5]); swap2(v[7], v[8]);
	swap2(v[0], v[1]); swap2(v[3], v[4]); swap2(v[6], v[7]);
	swap2(v[1], v[2]); swap2(v[4], v[5]); swap2(v[7], v[8]);
	swap2(v[0], v[3]); swap2(v[5], v[8]); swap2(v[4], v[7]);
	swap2(v[3], v[6]); swap2(v[1], v[4]); swap2(v[2], v[5]);
	swap2(v[4], v[7]); swap2(v[4], v[2]); swap2(v[6], v[4]);
	swap2(v[4], v[2]);

	u32 out_index = output_index(out_voxel.x, out_voxel.y, out_voxel.z);
	F32(OutputBuffer).x[3 * out_index + 0] = v[4][0];
	F32(OutputBuffer).x[3 * out_index + 1] = v[4][1];
	F32(OutputBuffer).x[3 * out_index + 2] = v[4][2];
}

void main()
{
	switch (output_dimension()) {
	case 2:{
		median_2d_3x3();
	}break;
	}
}
