/* See LICENSE for license details. */
layout(std430, buffer_reference, buffer_reference_align = 32) restrict writeonly buffer Buffer {
	u32vec4 x[];
};

void main()
{
	u32 index = gl_GlobalInvocationID.x;
	if (index < bins)
		Buffer(data).x[index] = clear_v4;
}
