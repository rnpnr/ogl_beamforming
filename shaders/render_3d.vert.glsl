layout(location = 0) out vec3 f_normal;
layout(location = 1) out vec3 f_texture_coordinate;

layout(std430, buffer_reference, buffer_reference_align = 16) readonly buffer Vector4 {
	vec4 values[];
};

void main()
{
	vec3 position = Vector4(positions).values[gl_VertexIndex].xyz;
	vec3 normal   = Vector4(normals).values[gl_VertexIndex].xyz;
	vec3 texture_coordinate = (2 * position + 1) / 2;

	f_texture_coordinate = texture_coordinate.xzy;
	f_normal             = normal;
	//f_normal             = normalize(mat3(mvp_matrix) * normal);

	gl_Position = mvp_matrix * vec4(position, 1);
}
