/* See LICENSE for license details. */
layout(std430, binding = 1) readonly restrict buffer buffer_1 {
	vec2 rf_data[];
};

#if DAS_FAST
layout(rg32f, binding = 0)           restrict uniform image3D  u_out_data_tex;
#else
layout(rg32f, binding = 0) writeonly restrict uniform image3D  u_out_data_tex;
#endif

layout(r16i,  binding = 1) readonly  restrict uniform iimage1D sparse_elements;
layout(rg32f, binding = 2) readonly  restrict uniform image1D  focal_vectors;

#define C_SPLINE 0.5

#define TX_ROWS 0
#define TX_COLS 1

#define TX_MODE_TX_COLS(a) (((a) & 2) != 0)
#define TX_MODE_RX_COLS(a) (((a) & 1) != 0)

vec2 rotate_iq(vec2 iq, float time)
{
	vec2 result = iq;
	if (center_frequency > 0) {
		float arg    = radians(360) * center_frequency * time;
		mat2  phasor = mat2( cos(arg), sin(arg),
		                    -sin(arg), cos(arg));
		result = phasor * iq;
	}
	return result;
}

/* NOTE: See: https://cubic.org/docs/hermite.htm */
vec2 cubic(int base_index, float x)
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

	vec2 samples[4] = {
		rf_data[base_index + xk - 1],
		rf_data[base_index + xk + 0],
		rf_data[base_index + xk + 1],
		rf_data[base_index + xk + 2],
	};

	vec2 P1 = samples[1];
	vec2 P2 = samples[2];
	vec2 T1 = C_SPLINE * (P2 - samples[0]);
	vec2 T2 = C_SPLINE * (samples[3] - P1);

	mat2x4 C = mat2x4(vec4(P1.x, P2.x, T1.x, T2.x), vec4(P1.y, P2.y, T1.y, T2.y));
	vec2 result = rotate_iq(S * h * C, x / sampling_frequency);
	return result;
}

vec2 sample_rf(int channel, int transmit, float t)
{
	vec2 result;
	int base_index = int(channel * dec_data_dim.x * dec_data_dim.z + transmit * dec_data_dim.x);
	if (interpolate) result = cubic(base_index, t);
	else             result = rotate_iq(rf_data[base_index + int(round(t))], t / sampling_frequency);
	return result;
}

float sample_index(float distance)
{
	float  time = distance / speed_of_sound + time_offset;
	return time * sampling_frequency;
}

float apodize(float arg)
{
	/* NOTE: used for constant F# dynamic receive apodization. This is implemented as:
	 *
	 *                  /        |x_e - x_i|\
	 *    a(x, z) = cos(F# * π * ----------- ) ^ 2
	 *                  \        |z_e - z_i|/
	 *
	 * where x,z_e are transducer element positions and x,z_i are image positions. */
	float a = cos(clamp(abs(arg), 0, 0.25 * radians(360)));
	return a * a;
}

vec2 rca_plane_projection(vec3 point, bool rows)
{
	vec2 result = vec2(point[int(rows)], point[2]);
	return result;
}

float plane_wave_transmit_distance(vec3 point, float transmit_angle, bool tx_rows)
{
	return dot(rca_plane_projection(point, tx_rows), vec2(sin(transmit_angle), cos(transmit_angle)));
}

float cylindrical_wave_transmit_distance(vec3 point, float focal_depth, float transmit_angle, bool tx_rows)
{
	vec2 f = focal_depth * vec2(sin(transmit_angle), cos(transmit_angle));
	return distance(rca_plane_projection(point, tx_rows), f);
}

#if DAS_FAST
vec3 RCA(vec3 world_point)
{
	bool  tx_rows         = !TX_MODE_TX_COLS(transmit_mode);
	bool  rx_rows         = !TX_MODE_RX_COLS(transmit_mode);
	vec2  xdc_world_point = rca_plane_projection((xdc_transform * vec4(world_point, 1)).xyz, rx_rows);
	vec2  focal_vector    = imageLoad(focal_vectors, u_channel).xy;
	float transmit_angle  = radians(focal_vector.x);
	float focal_depth     = focal_vector.y;

	float transmit_distance;
	if (isinf(focal_depth)) {
		transmit_distance = plane_wave_transmit_distance(world_point, transmit_angle, tx_rows);
	} else {
		transmit_distance = cylindrical_wave_transmit_distance(world_point, focal_depth,
		                                                       transmit_angle, tx_rows);
	}

	vec2 result = vec2(0);
	for (int channel = 0; channel < dec_data_dim.y; channel++) {
		vec2  receive_vector   = xdc_world_point - rca_plane_projection(vec3(channel * xdc_element_pitch, 0), rx_rows);
		float receive_distance = length(receive_vector);
		float apodization      = apodize(f_number * radians(180) / abs(xdc_world_point.y) * receive_vector.x);

		if (apodization > 0) {
			float sidx   = sample_index(transmit_distance + receive_distance);
			vec2  valid  = vec2(sidx >= 0) * vec2(sidx < dec_data_dim.x);
			result      += valid * apodization * sample_rf(channel, u_channel, sidx);
		}
	}
	return vec3(result, 0);
}
#else
vec3 RCA(vec3 world_point)
{
	bool tx_rows         = !TX_MODE_TX_COLS(transmit_mode);
	bool rx_rows         = !TX_MODE_RX_COLS(transmit_mode);
	vec2 xdc_world_point = rca_plane_projection((xdc_transform * vec4(world_point, 1)).xyz, rx_rows);

	vec3 sum = vec3(0);
	for (int transmit = 0; transmit < dec_data_dim.z; transmit++) {
		vec2  focal_vector   = imageLoad(focal_vectors, transmit).xy;
		float transmit_angle = radians(focal_vector.x);
		float focal_depth    = focal_vector.y;

		float transmit_distance;
		if (isinf(focal_depth)) {
			transmit_distance = plane_wave_transmit_distance(world_point, transmit_angle, tx_rows);
		} else {
			transmit_distance = cylindrical_wave_transmit_distance(world_point, focal_depth,
			                                                       transmit_angle, tx_rows);
		}

		for (int rx_channel = 0; rx_channel < dec_data_dim.y; rx_channel++) {
			vec3  rx_center      = vec3(rx_channel * xdc_element_pitch, 0);
			vec2  receive_vector = xdc_world_point - rca_plane_projection(rx_center, rx_rows);
			float apodization    = apodize(f_number * radians(180) / abs(xdc_world_point.y) * receive_vector.x);

			if (apodization > 0) {
				float sidx   = sample_index(transmit_distance + length(receive_vector));
				vec2  valid  = vec2(sidx >= 0) * vec2(sidx < dec_data_dim.x);
				vec2  value  = valid * apodization * sample_rf(rx_channel, transmit, sidx);
				sum         += vec3(value, length(value));
			}
		}
	}
	return sum;
}
#endif

#if DAS_FAST
vec3 HERCULES(vec3 world_point)
{
	bool  uhercules       = das_shader_id == DAS_ID_UHERCULES;
	vec3  xdc_world_point = (xdc_transform * vec4(world_point, 1)).xyz;
	bool  tx_rows         = !TX_MODE_TX_COLS(transmit_mode);
	bool  rx_cols         =  TX_MODE_RX_COLS(transmit_mode);
	vec2  focal_vector    = imageLoad(focal_vectors, 0).xy;
	float transmit_angle  = radians(focal_vector.x);
	float focal_depth     = focal_vector.y;

	float transmit_distance;
	if (isinf(focal_depth)) {
		transmit_distance = plane_wave_transmit_distance(world_point, transmit_angle, tx_rows);
	} else {
		transmit_distance = cylindrical_wave_transmit_distance(world_point, focal_depth,
		                                                       transmit_angle, tx_rows);
	}

	vec2 result = vec2(0);
	for (int transmit = int(uhercules); transmit < dec_data_dim.z; transmit++) {
		int tx_channel = uhercules ? imageLoad(sparse_elements, transmit - int(uhercules)).x : transmit;
		vec3 element_position;
		if (rx_cols) element_position = vec3(u_channel,  tx_channel, 0) * vec3(xdc_element_pitch, 0);
		else         element_position = vec3(tx_channel, u_channel,  0) * vec3(xdc_element_pitch, 0);

		float apodization = apodize(f_number * radians(180) / abs(xdc_world_point.z) *
		                            distance(xdc_world_point.xy, element_position.xy));
		if (apodization > 0) {
			/* NOTE: tribal knowledge */
			if (transmit == 0) apodization *= inversesqrt(dec_data_dim.z);

			float sidx   = sample_index(transmit_distance + distance(xdc_world_point, element_position));
			vec2  valid  = vec2(sidx >= 0) * vec2(sidx < dec_data_dim.x);
			result      += valid * apodization * sample_rf(u_channel, transmit, sidx);
		}
	}
	return vec3(result, 0);
}
#else
vec3 HERCULES(vec3 world_point)
{
	bool  uhercules       = das_shader_id == DAS_ID_UHERCULES;
	vec3  xdc_world_point = (xdc_transform * vec4(world_point, 1)).xyz;
	bool  tx_rows         = !TX_MODE_TX_COLS(transmit_mode);
	bool  rx_cols         =  TX_MODE_RX_COLS(transmit_mode);
	vec2  focal_vector    = imageLoad(focal_vectors, 0).xy;
	float transmit_angle  = radians(focal_vector.x);
	float focal_depth     = focal_vector.y;

	float transmit_distance;
	if (isinf(focal_depth)) {
		transmit_distance = plane_wave_transmit_distance(world_point, transmit_angle, tx_rows);
	} else {
		transmit_distance = cylindrical_wave_transmit_distance(world_point, focal_depth,
		                                                       transmit_angle, tx_rows);
	}

	vec3 result = vec3(0);
	for (int transmit = int(uhercules); transmit < dec_data_dim.z; transmit++) {
		int tx_channel = uhercules ? imageLoad(sparse_elements, transmit - int(uhercules)).x : transmit;
		for (int rx_channel = 0; rx_channel < dec_data_dim.y; rx_channel++) {
			vec3 element_position;
			if (rx_cols) element_position = vec3(rx_channel, tx_channel, 0) * vec3(xdc_element_pitch, 0);
			else         element_position = vec3(tx_channel, rx_channel, 0) * vec3(xdc_element_pitch, 0);

			float apodization = apodize(f_number * radians(180) / abs(xdc_world_point.z) *
			                            distance(xdc_world_point.xy, element_position.xy));
			if (apodization > 0) {
				/* NOTE: tribal knowledge */
				if (transmit == 0) apodization *= inversesqrt(dec_data_dim.z);

				float sidx   = sample_index(transmit_distance + distance(xdc_world_point, element_position));
				vec2  valid  = vec2(sidx >= 0) * vec2(sidx < dec_data_dim.x);
				vec2  value  = valid * apodization * sample_rf(rx_channel, transmit, sidx);
				result      += vec3(value, length(value));
			}
		}
	}
	return result;
}
#endif

#if DAS_FAST
vec3 FORCES(vec3 world_point)
{
	bool  uforces          = das_shader_id == DAS_ID_UFORCES;
	vec3  xdc_world_point  = (xdc_transform * vec4(world_point, 1)).xyz;
	float receive_distance = distance(xdc_world_point.xz, vec2(u_channel * xdc_element_pitch.x, 0));
	float apodization      = apodize(f_number * radians(180) / abs(xdc_world_point.z) *
	                                 (xdc_world_point.x - u_channel * xdc_element_pitch.x));

	vec2 result = vec2(0);
	if (apodization > 0) {
		for (int transmit = int(uforces); transmit < dec_data_dim.z; transmit++) {
			int   tx_channel      = uforces ? imageLoad(sparse_elements, transmit - int(uforces)).x : transmit;
			vec3  transmit_center = vec3(xdc_element_pitch * vec2(tx_channel, floor(dec_data_dim.y / 2)), 0);

			float sidx   = sample_index(distance(xdc_world_point, transmit_center) + receive_distance);
			vec2  valid  = vec2(sidx >= 0) * vec2(sidx < dec_data_dim.x);
			result      += valid * apodization * sample_rf(u_channel, transmit, sidx);
		}
	}
	return vec3(result, 0);
}
#else
vec3 FORCES(vec3 world_point)
{
	bool uforces         = das_shader_id == DAS_ID_UFORCES;
	vec3 xdc_world_point = (xdc_transform * vec4(world_point, 1)).xyz;

	vec3 result = vec3(0);
	for (int rx_channel = 0; rx_channel < dec_data_dim.y; rx_channel++) {
		float receive_distance = distance(xdc_world_point.xz, vec2(rx_channel * xdc_element_pitch.x, 0));
		float apodization      = apodize(f_number * radians(180) / abs(xdc_world_point.z) *
		                                 (xdc_world_point.x - rx_channel * xdc_element_pitch.x));
		if (apodization > 0) {
			for (int transmit = int(uforces); transmit < dec_data_dim.z; transmit++) {
				int   tx_channel      = uforces ? imageLoad(sparse_elements, transmit - int(uforces)).x : transmit;
				vec3  transmit_center = vec3(xdc_element_pitch * vec2(tx_channel, floor(dec_data_dim.y / 2)), 0);

				float sidx   = sample_index(distance(xdc_world_point, transmit_center) + receive_distance);
				vec2  valid  = vec2(sidx >= 0) * vec2(sidx < dec_data_dim.x);
				vec2  value  = valid * apodization * sample_rf(rx_channel, tx_channel, sidx);
				result      += vec3(value, length(value));
			}
		}
	}
	return result;
}
#endif

void main()
{
	ivec3 out_voxel = ivec3(gl_GlobalInvocationID);
#if DAS_FAST
	vec3 sum = vec3(imageLoad(u_out_data_tex, out_voxel).xy, 0);
#else
	vec3 sum = vec3(0);
	out_voxel += u_voxel_offset;
#endif

	vec3 world_point = (u_voxel_transform * vec4(out_voxel, 1)).xyz;

	switch (das_shader_id) {
	case DAS_ID_FORCES:
	case DAS_ID_UFORCES:
	{
		sum += FORCES(world_point);
	}break;
	case DAS_ID_HERCULES:
	case DAS_ID_UHERCULES:
	{
		sum += HERCULES(world_point);
	}break;
	case DAS_ID_FLASH:
	case DAS_ID_RCA_TPW:
	case DAS_ID_RCA_VLS:
	{
		sum += RCA(world_point);
	}break;
	}

	/* TODO(rnp): scale such that brightness remains ~constant */
	if (coherency_weighting) sum.xy *= sum.xy / (sum.z + float(sum.z == 0));

	imageStore(u_out_data_tex, out_voxel, vec4(sum.xy, 0, 0));
}
