/* See LICENSE for license details. */

/* input:  h [0,360] | s,v [0, 1] *
 * output: rgb [0,1]              */
vec3 hsv2rgb(vec3 hsv)
{
	vec3 k = mod(vec3(5, 3, 1) + hsv.x / 60, 6);
	k = max(min(min(k, 4 - k), 1), 0);
	return hsv.z - hsv.z * hsv.y * k;
}

bool bounding_box_test(vec3 coord, float p)
{
	bool result = false;
	bvec3 tests = bvec3(1 - step(vec3(p), coord) * step(coord, vec3(1 - p)));
	if ((tests.x && tests.y) || (tests.x && tests.z) || (tests.y && tests.z))
		result = true;
	return result;
}

void main()
{
	float smp = length(texture(u_texture, texture_coordinate).xy);
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

	if (bounding_box_test(test_texture_coordinate, u_bb_fraction)) {
		out_colour = u_bb_colour;
	} else {
		out_colour = vec4(smp, smp, smp, 1);
	}

	//out_colour = vec4(textureQueryLod(u_texture, texture_coordinate).y, 0, 0, 1);
	//out_colour = vec4(abs(normal), 1);
	//out_colour = vec4(1, 1, 1, smp);
	//out_colour = vec4(smp * abs(normal), 1);
}
