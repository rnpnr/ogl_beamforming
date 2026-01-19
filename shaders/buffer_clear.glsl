/* See LICENSE for license details. */
layout(std430, buffer_reference, buffer_reference_align = 8) restrict writeonly buffer Buffer {
	uint32_t values[];
};

void main()
{
	uint32_t word = gl_GlobalInvocationID.x;
	if (word < words)
		Buffer(data).values[word] = clear_word;
}
