/* See LICENSE for license details. */
layout(std430, binding = 1) readonly restrict buffer buffer_1 {
	vec2 rf_data[];
};

layout(rg32f, binding = 0) writeonly restrict uniform image3D  u_out_data_tex;
layout(r16i,  binding = 1) readonly  restrict uniform iimage1D sparse_elements;
layout(rg32f, binding = 2) readonly  restrict uniform image1D  focal_vectors;

#define C_SPLINE 0.5

#define TX_ROWS 0
#define TX_COLS 1

#define TX_MODE_TX_COLS(a) (((a) & 2) != 0)
#define TX_MODE_RX_COLS(a) (((a) & 1) != 0)

/* NOTE: See: https://cubic.org/docs/hermite.htm */
vec2 cubic(int ridx, float x)
{
	mat4 h = mat4(
		 2, -3,  0, 1,
		-2,  3,  0, 0,
		 1, -2,  1, 0,
		 1, -1,  0, 0
	);

	int   xk = int(floor(x));
	float t  = x - floor(x);
	vec4  S  = vec4(t * t * t, t * t, t, 1);

	vec2 P1 = rf_data[ridx + xk];
	vec2 P2 = rf_data[ridx + xk + 1];
	vec2 T1 = C_SPLINE * (P2 - rf_data[ridx + xk - 1]);
	vec2 T2 = C_SPLINE * (rf_data[ridx + xk + 2] - P1);

	mat2x4 C = mat2x4(vec4(P1.x, P2.x, T1.x, T2.x), vec4(P1.y, P2.y, T1.y, T2.y));
	return S * h * C;
}

vec2 sample_rf(int ridx, float t)
{
	vec2 result;
	if (interpolate) result = cubic(ridx, t);
	else             result = rf_data[ridx + int(floor(t))];
	return result;
}

vec3 calc_image_point(vec3 voxel)
{
	vec3 out_data_dim  = vec3(imageSize(u_out_data_tex));
	vec4 output_size   = abs(output_max_coordinate - output_min_coordinate);
	vec3 image_point   = output_min_coordinate.xyz + voxel * output_size.xyz / out_data_dim;

	switch (das_shader_id) {
	case DAS_ID_FLASH:
	case DAS_ID_FORCES:
	case DAS_ID_UFORCES:
		/* TODO: fix the math so that the image plane can be aritrary */
		image_point.y = 0;
		break;
	case DAS_ID_HERCULES:
	case DAS_ID_RCA_TPW:
	case DAS_ID_RCA_VLS:
		/* TODO(rnp): this can be removed when we use an abitrary plane transform */
		if (!all(greaterThan(out_data_dim, vec3(1, 1, 1))))
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

vec3 orientation_projection(vec3 point, bool rows)
{
	return point * vec3(!rows, rows, 1);
}

vec3 world_space_to_rca_space(vec3 image_point, bool rx_rows)
{
	return orientation_projection((xdc_transform * vec4(image_point, 1)).xyz, rx_rows);
}

float sample_index(float distance)
{
	float  time = distance / speed_of_sound + time_offset;
	return time * sampling_frequency;
}

float planewave_transmit_distance(vec3 point, float transmit_angle, bool tx_rows)
{
	return dot(orientation_projection(point, tx_rows),
	           vec3(sin(transmit_angle), sin(transmit_angle), cos(transmit_angle)));
}

float cylindricalwave_transmit_distance(vec3 point, float focal_depth, float transmit_angle, bool tx_rows)
{
	vec3 f = focal_depth * vec3(sin(transmit_angle), sin(transmit_angle), cos(transmit_angle));
	return length(orientation_projection(point - f, tx_rows));
}

vec3 RCA(vec3 image_point, vec3 delta, float apodization_arg)
{
	int ridx      = 0;
	int direction = beamform_plane;
	if (direction != TX_ROWS) image_point = image_point.yxz;

	bool tx_col = TX_MODE_TX_COLS(transmit_mode);
	bool rx_col = TX_MODE_RX_COLS(transmit_mode);

	vec3 receive_point = world_space_to_rca_space(image_point, !rx_col);
	delta = orientation_projection(delta, !rx_col);

	vec3 sum = vec3(0);
	/* NOTE: For Each Acquistion in Raw Data */
	// uint i = (dec_data_dim.z - 1) * uint(clamp(u_cycle_t, 0, 1)); {
	for (int i = 0; i < dec_data_dim.z; i++) {
		float transmit_angle = radians(imageLoad(focal_vectors, i).x);
		float focal_depth    = imageLoad(focal_vectors, i).y;

		float transmit_distance;
		if (isinf(focal_depth)) {
			transmit_distance = planewave_transmit_distance(image_point, transmit_angle,
			                                                !tx_col);
		} else {
			transmit_distance = cylindricalwave_transmit_distance(image_point, focal_depth,
			                                                      transmit_angle,
			                                                      !tx_col);
		}

		vec3 receive_distance = receive_point;
		/* NOTE: For Each Receiver */
		// uint j = (dec_data_dim.z - 1) * uint(clamp(u_cycle_t, 0, 1)); {
		for (uint j = 0; j < dec_data_dim.y; j++) {
			float sidx  = sample_index(transmit_distance + length(receive_distance));
			vec2 valid  = vec2(sidx >= 0) * vec2(sidx < dec_data_dim.x);
			vec2 samp   = valid * apodize(sample_rf(ridx, sidx), apodization_arg,
			                              length(receive_distance.xy));
			sum += vec3(samp, length(samp));
			receive_distance -= delta;
			ridx             += int(dec_data_dim.x);
		}
	}
	return sum;
}

vec3 HERCULES(vec3 image_point, vec3 delta, float apodization_arg)
{
	int uhercules  = int(das_shader_id == DAS_ID_UHERCULES);
	int ridx       = int(dec_data_dim.y * dec_data_dim.x * uhercules);
	int  direction = beamform_plane;
	if (direction != TX_ROWS) image_point = image_point.yxz;

	bool tx_col = TX_MODE_TX_COLS(transmit_mode);
	bool rx_col = TX_MODE_RX_COLS(transmit_mode);
	float transmit_angle = radians(imageLoad(focal_vectors, 0).x);
	float focal_depth    = imageLoad(focal_vectors, 0).y;

	vec3 receive_point = (xdc_transform * vec4(image_point, 1)).xyz;

	float transmit_distance;
	if (isinf(focal_depth)) {
		transmit_distance = planewave_transmit_distance(image_point, transmit_angle,
		                                                !tx_col);
	} else {
		transmit_distance = cylindricalwave_transmit_distance(image_point, focal_depth,
		                                                      transmit_angle,
		                                                      !tx_col);
	}

	vec3 sum = vec3(0);
	/* NOTE: For Each Acquistion in Raw Data */
	for (int i = uhercules; i < dec_data_dim.z; i++) {
		int channel = imageLoad(sparse_elements, i - uhercules).x;
		/* NOTE: For Each Virtual Source */
		for (uint j = 0; j < dec_data_dim.y; j++) {
			vec3 element_position;
			if (rx_col) element_position = vec3(j, channel, 0) * delta;
			else        element_position = vec3(channel, j, 0) * delta;
			vec3 receive_distance = receive_point - element_position;
			float sidx  = sample_index(transmit_distance + length(receive_distance));
			vec2 valid  = vec2(sidx >= 0) * vec2(sidx < dec_data_dim.x);

			/* NOTE: tribal knowledge */
			if (i == 0) valid *= inversesqrt(dec_data_dim.z);

			vec2 samp = valid * apodize(sample_rf(ridx, sidx), apodization_arg,
			                            length(receive_distance.xy));
			sum  += vec3(samp, length(samp));
			ridx += int(dec_data_dim.x);
		}
	}
	return sum;
}

vec3 uFORCES(vec3 image_point, vec3 delta, float apodization_arg)
{
	/* NOTE: skip first acquisition in uforces since its garbage */
	int uforces = int(das_shader_id == DAS_ID_UFORCES);
	int ridx    = int(dec_data_dim.y * dec_data_dim.x * uforces);

	image_point  = (xdc_transform * vec4(image_point, 1)).xyz;

	vec3 sum = vec3(0);
	for (int i = uforces; i < dec_data_dim.z; i++) {
		int   channel        = imageLoad(sparse_elements, i - uforces).x;
		vec2  recieve_point  = image_point.xz;
		vec3  element_center = delta * vec3(channel, floor(dec_data_dim.y / 2), 0);
		float transmit_dist  = distance(image_point, element_center);

		for (uint j = 0; j < dec_data_dim.y; j++) {
			float sidx  = sample_index(transmit_dist + length(recieve_point));
			vec2 valid  = vec2(sidx >= 0) * vec2(sidx < dec_data_dim.x);
			vec2 samp   = valid * apodize(sample_rf(ridx, sidx), apodization_arg,
			                              recieve_point.x);
			sum        += vec3(samp, length(samp));
			ridx       += int(dec_data_dim.x);
			recieve_point.x -= delta.x;
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
	vec3 sum;
	switch (das_shader_id) {
	case DAS_ID_FORCES:
	case DAS_ID_UFORCES:
		sum = uFORCES(image_point, vec3(xdc_element_pitch, 0), apod_arg);
		break;
	case DAS_ID_HERCULES:
	case DAS_ID_UHERCULES:
		sum = HERCULES(image_point, vec3(xdc_element_pitch, 0), apod_arg);
		break;
	case DAS_ID_FLASH:
	case DAS_ID_RCA_TPW:
	case DAS_ID_RCA_VLS:
		sum = RCA(image_point, vec3(xdc_element_pitch, 0), apod_arg);
		break;
	}

	/* TODO(rnp): scale such that brightness remains ~constant */
	if (coherency_weighting) sum.xy *= sum.xy / (sum.z + float(sum.z == 0));

	imageStore(u_out_data_tex, out_coord, vec4(sum.xy, 0, 0));
}
