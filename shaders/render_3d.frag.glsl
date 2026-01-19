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

uint32_t texture_dimension(uvec3 points)
{
	points = uvec3(greaterThan(points, uvec3(1)));
	return points.x + points.y + points.z;
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

float sample_value(vec3 p)
{
	float result;
	if (input_data != 0) {
		uint32_t index = input_index(texture_coordinate);
		switch (data_kind) {
		case DataKind_Float32:{        result = length(InputFloat(input_data).values[index]); }break;
		case DataKind_Float32Complex:{ result = length(InputVec2(input_data).values[index]);  }break;
		}
	}

	float threshold_val = pow(10.0f, threshold / 20.0f);
	result = clamp(result, 0.0f, threshold_val);
	result = result / threshold_val;
	result = pow(result, gamma);

	if (db_cutoff > 0) {
		result = 20 * log(result) / log(10);
		result = clamp(result, -db_cutoff, 0) / -db_cutoff;
		result = 1 - result;
	}

	return result;
}

float grad(float x)
{
	float h  = length(fwidth(texture_coordinate.xy));
	float s1 = sample_value(vec3(x + h, 0, 0));
	float s2 = sample_value(vec3(x - h, 0, 0));
	return (s1 - s2) / (2.0f * h);
}

void main(void)
{
	uint32_t dimension = texture_dimension(uvec3(input_size_x, input_size_y, input_size_z));

	if (dimension == 3) {
		// TODO(rnp): add slice offset passed in as a uniform
	}

	float data = sample_value(texture_coordinate);
	//float t = test_texture_coordinate.y;
	//smp = smp * smoothstep(-0.4, 1.1, t) * u_gain;

	vec3  p = 2.0f * texture_coordinate - 1.0f;

	switch (dimension) {
	case 1:{

		float df = mix(grad(texture_coordinate.x), dFdx(data),
		               smoothstep(0.0f, 0.55f, abs(texture_coordinate.x - 0.5f)));
		float de = abs(data - texture_coordinate.y) / sqrt(1.0f + df * df);

		float eps       = length(fwidth(texture_coordinate.xy));
		float thickness = 4.f;

		float alpha = smoothstep((0.5f * thickness + 2.0f) * eps, (0.5f * thickness + 0.0f) * eps, de);
		out_colour = vec4(bounding_box_colour.xyz, alpha);
	}break;

	case 2:
	case 3:
	{
		float t = clamp(sdf_wire_box_outside(p, vec3(1.0f), bounding_box_fraction) /  bounding_box_fraction, 0, 1);

		out_colour = vec4(t * vec3(data) + (1 - t) * bounding_box_colour.xyz, 1);
		//if (u_solid_bb) out_colour = u_bb_colour;
	}break;
	}

	//out_colour = vec4(textureQueryLod(u_texture, texture_coordinate).y, 0, 0, 1);
	//out_colour = vec4(abs(normal), 1);
	//out_colour = vec4(1, 1, 1, smp);
	//out_colour = vec4(smp * abs(normal), 1);
}
