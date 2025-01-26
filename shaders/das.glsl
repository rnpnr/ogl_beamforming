/* See LICENSE for license details. */
layout(local_size_x = 32, local_size_y = 1, local_size_z = 32) in;

layout(std430, binding = 1) readonly restrict buffer buffer_1 {
	vec2 rf_data[];
};

layout(rg32f, binding = 0) writeonly uniform image3D u_out_data_tex;

layout(location = 2) uniform ivec3 u_voxel_offset;
layout(location = 3) uniform float u_cycle_t;

#define C_SPLINE 0.5

#define TX_ROWS 0
#define TX_COLS 1

#define TX_MODE_TX_COLS(a) (((a) & 2) != 0)
#define TX_MODE_RX_COLS(a) (((a) & 1) != 0)

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
	vec3 image_point   = output_min_coord.xyz + voxel * output_size.xyz / out_data_dim;

	switch (das_shader_id) {
	case DAS_ID_FORCES:
	case DAS_ID_UFORCES:
		/* TODO: fix the math so that the image plane can be aritrary */
		image_point.y = 0;
		break;
	case DAS_ID_HERCULES:
	case DAS_ID_RCA_TPW:
	case DAS_ID_RCA_VLS:
		image_point.y = off_axis_pos;
		break;
	}

	return image_point;
}

vec2 apodize(vec2 value, float apodization_arg, float distance)
{
	/* NOTE: apodization value for this transducer element */
	float a  = cos(clamp(abs(apodization_arg * distance), 0, 0.25 * radians(360)));
	return value * a * a;
}

vec3 orientation_projection(bool rows)
{
	return vec3(float(!rows), float(rows), 1);
}

vec3 world_space_to_rca_space(vec3 image_point, bool rx_rows)
{
	vec3 result  = (xdc_transform * vec4(image_point, 1)).xyz;
	result      *= orientation_projection(rx_rows);
	return result;
}

float sample_index(float distance)
{
	float  time = distance / speed_of_sound + time_offset;
	return time * sampling_frequency;
}

float planewave_transmit_distance(vec3 point, float transmit_angle, bool tx_rows)
{
	return dot(point * orientation_projection(tx_rows),
	           vec3(sin(transmit_angle), sin(transmit_angle), cos(transmit_angle)));
}

float cylindricalwave_transmit_distance(vec3 point, float focal_depth, float transmit_angle, bool tx_rows)
{
	vec3 f = focal_depth * vec3(sin(transmit_angle), sin(transmit_angle), cos(transmit_angle));
	return length((point - f) * orientation_projection(tx_rows));
}

vec2 RCA(vec3 image_point, vec3 delta, float apodization_arg)
{
	uint ridx      = 0;
	int  direction = beamform_plane;
	if (direction != TX_ROWS) image_point = image_point.yxz;

	bool tx_col = TX_MODE_TX_COLS(transmit_mode);
	bool rx_col = TX_MODE_RX_COLS(transmit_mode);

	vec3 recieve_point = world_space_to_rca_space(image_point, !rx_col);
	delta *= orientation_projection(!rx_col);

	vec2 sum = vec2(0);
	/* NOTE: For Each Acquistion in Raw Data */
	// uint i = (dec_data_dim.z - 1) * uint(clamp(u_cycle_t, 0, 1)); {
	for (uint i = 0; i < dec_data_dim.z; i++) {
		uint base_idx = i / 4;
		uint sub_idx  = i % 4;

		float focal_depth    = focal_depths[base_idx][sub_idx];
		float transmit_angle = transmit_angles[base_idx][sub_idx];

		float transmit_distance;
		if (isinf(focal_depth)) {
			transmit_distance = planewave_transmit_distance(image_point, transmit_angle,
			                                                !tx_col);
		} else {
			transmit_distance = cylindricalwave_transmit_distance(image_point, focal_depth,
			                                                      transmit_angle,
			                                                      !tx_col);
		}

		vec3 receive_distance = recieve_point;
		/* NOTE: For Each Receiver */
		// uint j = (dec_data_dim.z - 1) * uint(clamp(u_cycle_t, 0, 1)); {
		for (uint j = 0; j < dec_data_dim.y; j++) {
			float sidx  = sample_index(transmit_distance + length(receive_distance));
			vec2 valid  = vec2(sidx >= 0) * vec2(sidx < dec_data_dim.x);
			sum        += apodize(cubic(ridx, sidx), apodization_arg, length(receive_distance.xy)) * valid;
			receive_distance   -= delta;
			ridx       += dec_data_dim.x;
		}
	}
	return sum;
}

vec2 HERCULES(vec3 image_point, vec3 delta, float apodization_arg)
{
	uint ridx      = 0;
	int  direction = beamform_plane;
	if (direction != TX_ROWS) image_point = image_point.yxz;

	bool tx_col = TX_MODE_TX_COLS(transmit_mode);
	bool rx_col = TX_MODE_RX_COLS(transmit_mode);
	float focal_depth    = focal_depths[0][0];
	float transmit_angle = transmit_angles[0][0];

	vec3 recieve_point = (xdc_transform * vec4(image_point, 1)).xyz;

	float transmit_distance;
	if (isinf(focal_depth)) {
		transmit_distance = planewave_transmit_distance(image_point, transmit_angle,
		                                                !tx_col);
	} else {
		transmit_distance = cylindricalwave_transmit_distance(image_point, focal_depth,
		                                                      transmit_angle,
		                                                      !tx_col);
	}

	vec2 sum = vec2(0);
	/* NOTE: For Each Acquistion in Raw Data */
	for (uint i = 0; i < dec_data_dim.z; i++) {
		/* NOTE: For Each Virtual Source */
		for (uint j = 0; j < dec_data_dim.y; j++) {
			vec3 element_position;
			if (rx_col) element_position = vec3(j, i, 0) * delta;
			else        element_position = vec3(i, j, 0) * delta;
			vec3 receive_distance = recieve_point - element_position;
			float sidx  = sample_index(transmit_distance + length(receive_distance));
			vec2 valid  = vec2(sidx >= 0) * vec2(sidx < dec_data_dim.x);

			/* NOTE: tribal knowledge */
			if (i == 0) valid *= inversesqrt(dec_data_dim.z);

			sum  += apodize(cubic(ridx, sidx), apodization_arg,
			                length(receive_distance.xy)) * valid;
			ridx += dec_data_dim.x;
		}
	}
	return sum;
}

vec2 uFORCES(vec3 image_point, vec3 delta, float apodization_arg)
{
	/* NOTE: skip first acquisition in uforces since its garbage */
	uint uforces = uint(das_shader_id == DAS_ID_UFORCES);
	uint ridx    = dec_data_dim.y * dec_data_dim.x * uforces;

	image_point  = (xdc_transform * vec4(image_point, 1)).xyz;

	int transmit_orientation = TX_ROWS;
	if (transmit_orientation == TX_ROWS)
		delta = delta.yxz;

	vec3 focal_point_offset = vec3(0, delta.y * floor(dec_data_dim.y / 2), 0);
	delta.y = 0;

	vec2 sum = vec2(0);
	for (uint i = uforces; i < dec_data_dim.z; i++) {
		uint base_idx = ((i - uforces) / 8);
		uint sub_idx  = ((i - uforces) % 8) / 2;
		uint shift    = (~(i - uforces) & 1u) * 16u;
		uint channel  = (uforces_channels[base_idx][sub_idx] << shift) >> 16u;

		vec2  rdist         = vec2(image_point.x, image_point.z);
		vec3  focal_point   = channel * delta + focal_point_offset;
		float transmit_dist = distance(image_point, focal_point);

		for (uint j = 0; j < dec_data_dim.y; j++) {
			float sidx  = sample_index(transmit_dist + length(rdist));
			vec2 valid  = vec2(sidx >= 0) * vec2(sidx < dec_data_dim.x);
			sum        += apodize(cubic(ridx, sidx), apodization_arg, rdist.x) * valid;
			rdist.x    -= delta.x;
			ridx       += dec_data_dim.x;
		}
	}
	return sum;
}

void main()
{

	/* NOTE: Convert voxel to physical coordinates */
	ivec3 out_coord   = ivec3(gl_GlobalInvocationID) + u_voxel_offset;
	vec3  image_point = calc_image_point(vec3(out_coord));

	/* NOTE: used for constant F# dynamic receive apodization. This is implemented as:
	 *
	 *                  /        |x_e - x_i|\
	 *    a(x, z) = cos(F# * π * ----------- ) ^ 2
	 *                  \        |z_e - z_i|/
	 *
	 * where x,z_e are transducer element positions and x,z_i are image positions. */
	float apod_arg = f_number * radians(180) / abs(image_point.z);

	/* NOTE: skip over channels corresponding to other arrays */
	vec2 sum;
	switch (das_shader_id) {
	case DAS_ID_FORCES:
	case DAS_ID_UFORCES:
		sum = uFORCES(image_point, vec3(xdc_element_pitch, 0), apod_arg);
		break;
	case DAS_ID_HERCULES:
		sum = HERCULES(image_point, vec3(xdc_element_pitch, 0), apod_arg);
		break;
	case DAS_ID_RCA_TPW:
	case DAS_ID_RCA_VLS:
		sum = RCA(image_point, vec3(xdc_element_pitch, 0), apod_arg);
		break;
	}

	imageStore(u_out_data_tex, out_coord, vec4(sum, 0, 0));
}
