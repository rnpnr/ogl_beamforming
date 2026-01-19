/* See LICENSE for license details. */
layout(location = 0) in  vec3 normal;
layout(location = 1) in  vec3 texture_coordinate;
layout(location = 0) out vec4 out_colour;

layout(std430, buffer_reference, buffer_reference_align = 64) readonly buffer InputVec2 {
	vec2 values[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) readonly buffer InputFloat {
	float values[];
};

/* input:  h [0,360] | s,v [0, 1] *
 * output: rgb [0,1]              */
vec3 hsv2rgb(vec3 hsv)
{
	vec3 k = mod(vec3(5, 3, 1) + hsv.x / 60, 6);
	k = max(min(min(k, 4 - k), 1), 0);
	return hsv.z - hsv.z * hsv.y * k;
}

/* NOTE(rnp): adapted from: https://iquilezles.org/articles/distfunctions */
float sdf_wire_box_outside(vec3 p, vec3 b, float e)
{
	p = abs(p) - b;
	vec3 q = abs(p + e) - e;
	float result = min(min(length(max(vec3(p.x, q.y, q.z), 0.0)),
	                       length(max(vec3(q.x, p.y, q.z), 0.0))),
	                       length(max(vec3(q.x, q.y, p.z), 0.0)));
	return result;
}

uint32_t input_index(vec3 uv)
{
	uv *= vec3(input_size_x - 1, input_size_y - 1, input_size_z - 1);
	uint32_t result = input_size_y * input_size_x * uint32_t(uv.z) +
	                                 input_size_x * uint32_t(uv.y) +
	                                                uint32_t(uv.x);
	result = min(result, input_size_z * input_size_y * input_size_x - 1);
	return result;
}

void main()
{
	float data = 0;
	// NOTE(rnp): for the x-plane view we sometimes want one plane to render without data
	if (input_data != 0) {
		uint32_t index = input_index(texture_coordinate);
		switch (data_kind) {
		case DataKind_Float32:{        data = length(InputFloat(input_data).values[index]); }break;
		case DataKind_Float32Complex:{ data = length(InputVec2(input_data).values[index]);  }break;
		}
	}

	float threshold_value = pow(10.0f, threshold / 20.0f);
	data = clamp(data, 0.0f, threshold_value);
	data = data / threshold_value;
	data = pow(data, gamma);

	//float t = test_texture_coordinate.y;
	//smp = smp * smoothstep(-0.4, 1.1, t) * u_gain;

	if (db_cutoff > 0) {
		data = 20 * log(data) / log(10);
		data = clamp(data, -db_cutoff, 0) / -db_cutoff;
		data = 1 - data;
	}

	vec3  p = 2.0f * texture_coordinate - 1.0f;
	float t = clamp(sdf_wire_box_outside(p, vec3(1.0f), bounding_box_fraction) /  bounding_box_fraction, 0, 1);

	out_colour = vec4(t * vec3(data) + (1 - t) * bounding_box_colour.xyz, 1);
	//if (u_solid_bb) out_colour = u_bb_colour;

	//out_colour = vec4(textureQueryLod(u_texture, texture_coordinate).y, 0, 0, 1);
	//out_colour = vec4(abs(normal), 1);
	//out_colour = vec4(1, 1, 1, smp);
	//out_colour = vec4(smp * abs(normal), 1);
}
