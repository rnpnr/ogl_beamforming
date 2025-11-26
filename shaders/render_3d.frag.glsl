/* See LICENSE for license details. */

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

void main()
{
	vec4  value  = vec4(0);
	ivec3 P      = ivec3((u_image_points - 1) * texture_coordinate);
	int   offset = u_image_components * (u_image_points.x * u_image_points.y * P.z + u_image_points.x * P.y + P.x);
	for (int c = 0; c < u_image_components; c++)
		value[c] = image_data[offset + c];

	float smp = length(value);
	float threshold_val = pow(10.0f, u_threshold / 20.0f);
	smp = clamp(smp, 0.0f, threshold_val);
	smp = smp / threshold_val;
	smp = pow(smp, u_gamma);

	//float t = test_texture_coordinate.y;
	//smp = smp * smoothstep(-0.4, 1.1, t) * u_gain;

	if (u_log_scale) {
		smp = 20 * log(smp) / log(10);
		smp = clamp(smp, -u_db_cutoff, 0) / -u_db_cutoff;
		smp = 1 - smp;
	}

	vec3  p = 2.0f * test_texture_coordinate - 1.0f;
	float t = clamp(sdf_wire_box_outside(p, vec3(1.0f), u_bb_fraction) / u_bb_fraction, 0, 1);

	out_colour = vec4(t * vec3(smp) + (1 - t) * u_bb_colour.xyz, 1);
	if (u_solid_bb) out_colour = u_bb_colour;

	//out_colour = vec4(textureQueryLod(u_texture, texture_coordinate).y, 0, 0, 1);
	//out_colour = vec4(abs(normal), 1);
	//out_colour = vec4(1, 1, 1, smp);
	//out_colour = vec4(smp * abs(normal), 1);
}
