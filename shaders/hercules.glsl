/* See LICENSE for license details. */
layout(local_size_x = 32, local_size_y = 1, local_size_z = 32) in;

layout(std430, binding = 1) readonly restrict buffer buffer_1 {
	vec2 rf_data[];
};

layout(rg32f, binding = 0) writeonly uniform image3D u_out_data_tex;

layout(location = 2) uniform int   u_volume_export_pass;
layout(location = 3) uniform ivec3 u_volume_export_dim_offset;
layout(location = 4) uniform mat4  u_xdc_transform;
layout(location = 5) uniform int   u_xdc_index;

#define C_SPLINE 0.5

#define TX_ROWS 0
#define TX_COLS 1

#if 1
/* NOTE: interpolation is unnecessary if the data has been demodulated and not decimated */
vec2 cubic(uint ridx, float t)
{
	return rf_data[ridx + uint(floor(t))];
}
#else
/* NOTE: See: https://cubic.org/docs/hermite.htm */
vec2 cubic(uint ridx, float x)
{
	mat4 h = mat4(
		 2, -3,  0, 1,
		-2,  3,  0, 0,
		 1, -2,  1, 0,
		 1, -1,  0, 0
	);

	uint  xk = uint(floor(x));
	float t  = (x  - float(xk));
	vec4  S  = vec4(t * t * t, t * t, t, 1);

	vec2 P1 = rf_data[ridx + xk];
	vec2 P2 = rf_data[ridx + xk + 1];
	vec2 T1 = C_SPLINE * (P2 - rf_data[ridx + xk - 1]);
	vec2 T2 = C_SPLINE * (rf_data[ridx + xk + 2] - P1);

	vec4 C1 = vec4(P1.x, P2.x, T1.x, T2.x);
	vec4 C2 = vec4(P1.y, P2.y, T1.y, T2.y);
	return vec2(dot(S, h * C1), dot(S, h * C2));
}
#endif

vec3 calc_image_point(vec3 voxel)
{
	ivec3 out_data_dim = imageSize(u_out_data_tex);
	vec4 output_size   = abs(output_max_coord - output_min_coord);
	vec4 image_point   = vec4(output_min_coord.xyz + voxel * output_size.xyz / out_data_dim, 1);

	if (u_volume_export_pass == 0)
		image_point.y = off_axis_pos;

	/* NOTE: move the image point into xdc space */
	image_point = u_xdc_transform * image_point;

	return image_point.xyz;
}

void main()
{
	vec3  voxel      = vec3(gl_GlobalInvocationID.xyz)  + vec3(u_volume_export_dim_offset);
	ivec3 out_coord  = ivec3(gl_GlobalInvocationID.xyz) + u_volume_export_dim_offset;

	/* NOTE: Convert voxel to physical coordinates */
	vec3 edge1       = xdc_corner1[u_xdc_index].xyz - xdc_origin[u_xdc_index].xyz;
	vec3 edge2       = xdc_corner2[u_xdc_index].xyz - xdc_origin[u_xdc_index].xyz;
	vec3 image_point = calc_image_point(voxel);
	vec3 delta;
	/* TODO: there should be a smarter way of detecting this */
	if (edge2.x != 0) delta = vec3(edge2.x, edge1.y, 0) / float(dec_data_dim.y);
	else              delta = vec3(edge1.x, edge2.y, 0) / float(dec_data_dim.y);

	/* NOTE: used for constant F# dynamic receive apodization. This is implemented as:
	 *
	 *                  /        |x_e - x_i|\
	 *    a(x, z) = cos(F# * π * ----------- ) ^ 2
	 *                  \        |z_e - z_i|/
	 *
	 * where x,z_e are transducer element positions and x,z_i are image positions. */
	float apod_arg = f_number * 0.5 * radians(360) / abs(image_point.z);

	vec2 sum   = vec2(0);
	vec3 rdist = image_point;

	/* TODO: pass this in (there is a problem in that it depends on the orientation
	 * of the array relative to the target/subject). */
	int   transmit_orientation = TX_ROWS;
	float transmit_dist;
	if (isinf(focal_depth)) {
		/* NOTE: plane wave */
		transmit_dist = image_point.z;
	} else {
		/* NOTE: cylindrical diverging wave */
		if (transmit_orientation == TX_ROWS)
			transmit_dist = length(vec2(image_point.y, image_point.z - focal_depth));
		else
			transmit_dist = length(vec2(image_point.x, image_point.z - focal_depth));
	}

	/* NOTE: skip over channels corresponding to other arrays */
	uint ridx      = u_xdc_index * (dec_data_dim.y / xdc_count) * dec_data_dim.x * dec_data_dim.z;
	int  direction = beamform_plane * (u_volume_export_pass ^ 1);
	/* NOTE: For Each Acquistion in Raw Data */
	for (uint i = 0; i < dec_data_dim.z; i++) {
		/* NOTE: For Each Virtual Source */
		for (uint j = 0; j < dec_data_dim.y; j++) {
			float dist = transmit_dist + length(rdist);
			float time = dist / speed_of_sound + time_offset;

			/* NOTE: apodization value for this transducer element */
			float a  = cos(clamp(abs(apod_arg * rdist.x), 0, 0.25 * radians(360)));
			a        = a * a;

			float sidx = time * sampling_frequency;
			vec2 valid = vec2(sidx < dec_data_dim.x);
			vec2 p     = cubic(ridx, sidx);
			/* NOTE: tribal knowledge; this is a problem with the imaging sequence */
			if (i == 0) p *= inversesqrt(128);
			sum += p * a;

			rdist[direction] -= delta[direction];
			ridx             += dec_data_dim.x;
		}

		rdist[direction]      = image_point[direction];
		rdist[direction ^ 1] -= delta[direction ^ 1];
	}
	imageStore(u_out_data_tex, out_coord, vec4(sum.x, sum.y, 0, 0));
}
