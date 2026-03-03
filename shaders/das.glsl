/* See LICENSE for license details. */
#if   DataKind == DataKind_Float32
  #if CoherencyWeighting
    #define RESULT_TYPE               vec2
    #define RESULT_COHERENT_CAST(a)   (a).x
    #define RESULT_INCOHERENT_CAST(a) (a).y
  #endif
  #define SAMPLE_TYPE  float
#elif DataKind == DataKind_Float32Complex
  #if CoherencyWeighting
    #define RESULT_TYPE               vec3
    #define RESULT_COHERENT_CAST(a)   (a).xy
    #define RESULT_INCOHERENT_CAST(a) (a).z
  #endif
  #define SAMPLE_TYPE  vec2
#else
  #error DataKind unsupported for DAS
#endif

#ifndef RESULT_TYPE
  #define RESULT_TYPE SAMPLE_TYPE
#endif

#ifndef RESULT_COHERENT_CAST
  #define RESULT_COHERENT_CAST(a) (a)
#endif

#if CoherencyWeighting
  #define RESULT_STORE(a) RESULT_TYPE(RESULT_COHERENT_CAST(a), length(a))
#else
  #define RESULT_STORE(a) (a)
#endif

layout(set = ShaderResourceKind_Buffer, binding = ShaderBufferSlot_PingPong) readonly buffer RF {
	SAMPLE_TYPE rf[];
};

layout(set = ShaderResourceKind_Buffer, binding = ShaderBufferSlot_BeamformedData) buffer Output {
	SAMPLE_TYPE output_data[];
};

layout(set = ShaderResourceKind_Buffer, binding = ShaderBufferSlot_BeamformedData) buffer IncoherentOutput {
	float incoherent_data[];
};

layout(std430, buffer_reference) restrict readonly buffer ArrayParameters {
	DASArrayParameters data;
};

#define RX_ORIENTATION(tx_rx) bitfieldExtract((tx_rx), 0, 4)
#define TX_ORIENTATION(tx_rx) bitfieldExtract((tx_rx), 4, 4)

#define C_SPLINE 0.5

#if DataKind == DataKind_Float32Complex
vec2 rotate_iq(const vec2 iq, const float time)
{
	float arg    = radians(360) * DemodulationFrequency * time;
	mat2  phasor = mat2( cos(arg), sin(arg),
	                    -sin(arg), cos(arg));
	vec2 result = phasor * iq;
	return result;
}
#else
  #define rotate_iq(a, b) (a)
#endif

/* NOTE: See: https://cubic.org/docs/hermite.htm */
SAMPLE_TYPE cubic(const int offset, const float t)
{
	const mat4 h = mat4(
		 2, -3,  0, 1,
		-2,  3,  0, 0,
		 1, -2,  1, 0,
		 1, -1,  0, 0
	);

	SAMPLE_TYPE samples[4] = {
		rf[offset + 0],
		rf[offset + 1],
		rf[offset + 2],
		rf[offset + 3],
	};

	vec4        S  = vec4(t * t * t, t * t, t, 1);
	SAMPLE_TYPE P1 = samples[1];
	SAMPLE_TYPE P2 = samples[2];
	SAMPLE_TYPE T1 = C_SPLINE * (P2 - samples[0]);
	SAMPLE_TYPE T2 = C_SPLINE * (samples[3] - P1);

#if   DataKind == DataKind_Float32
	vec4 C = vec4(P1.x, P2.x, T1.x, T2.x);
	float result = dot(S, h * C);
#elif DataKind == DataKind_Float32Complex
	mat2x4 C = mat2x4(vec4(P1.x, P2.x, T1.x, T2.x), vec4(P1.y, P2.y, T1.y, T2.y));
	vec2 result = S * h * C;
#endif
	return result;
}

SAMPLE_TYPE sample_rf(const int rf_offset, const float index)
{
	SAMPLE_TYPE result = SAMPLE_TYPE(0);
	switch (InterpolationMode) {
	case InterpolationMode_Nearest:{
		if (int(index) >= 0 && int(round(index)) < SampleCount)
			result = rotate_iq(rf[rf_offset + int(round(index))], index / SamplingFrequency);
	}break;
	case InterpolationMode_Linear:{
		if (int(index) >= 0 && int(index) < SampleCount - 1) {
			float tk, t = modf(index, tk);
			int n = rf_offset + int(tk);
			result = (1 - t) * rf[n] + t * rf[n + 1];
			result = rotate_iq(result, index / SamplingFrequency);
		}
	}break;
	case InterpolationMode_Cubic:{
		if (int(index) > 0 && int(index) < SampleCount - 2) {
			float tk, t = modf(index, tk);
			result = rotate_iq(cubic(rf_offset + int(index), t), index / SamplingFrequency);
		}
	}break;
	}
	return result;
}

float sample_index(const float distance)
{
	float  time = distance / SpeedOfSound + TimeOffset;
	return time * SamplingFrequency;
}

uint32_t output_index(uint32_t x, uint32_t y, uint32_t z)
{
	uint32_t result = output_size_x * output_size_y * z + output_size_x * y + x;
	return result;
}

float apodize(const float arg)
{
	/* IMPORTANT: do not move calculation of arg into this function. It will generate a
	 * conditional move resulting in cos always being evaluated causing a slowdown */

	/* NOTE: constant F# dynamic receive apodization. This is implemented as:
	 *
	 *                  /        |x_e - x_i|\
	 *    a(x, z) = cos(F# * π * ----------- ) ^ 2
	 *                  \        |z_e - z_i|/
	 *
	 * where x,z_e are transducer element positions and x,z_i are image positions. */
	float a = cos(radians(180) * arg);
	return a * a;
}

vec2 rca_plane_projection(const vec3 point, const bool rows)
{
	vec2 result = vec2(point[int(rows)], point[2]);
	return result;
}

float plane_wave_transmit_distance(const vec3 point, const float transmit_angle, const bool tx_rows)
{
	return dot(rca_plane_projection(point, tx_rows), vec2(sin(transmit_angle), cos(transmit_angle)));
}

float cylindrical_wave_transmit_distance(const vec3 point, const float focal_depth,
                                         const float transmit_angle, const bool tx_rows)
{
	vec2 f = focal_depth * vec2(sin(transmit_angle), cos(transmit_angle));
	return distance(rca_plane_projection(point, tx_rows), f);
}

uint16_t tx_rx_orientation_for_acquisition(const int16_t acquisition)
{
	uint16_t result = uint16_t(TransmitReceiveOrientation);
	if (!bool(SingleOrientation))
		result = ArrayParameters(array_parameters).data.transmit_receive_orientations[acquisition];
	return result;
}

vec2 focal_vector_for_acquisition(const int16_t acquisition)
{
	vec2 result = bool(SingleFocus) ? vec2(TransmitAngle, FocusDepth)
	                                : ArrayParameters(array_parameters).data.focal_vectors[acquisition];
	return result;
}

float rca_transmit_distance(const vec3 world_point, const vec2 focal_vector, const uint16_t transmit_receive_orientation)
{
	float result = 0;
	if (TX_ORIENTATION(transmit_receive_orientation) != RCAOrientation_None) {
		bool  tx_rows        = TX_ORIENTATION(transmit_receive_orientation) == RCAOrientation_Rows;
		float transmit_angle = radians(focal_vector.x);
		float focal_depth    = focal_vector.y;

		if (isinf(focal_depth)) {
			result = plane_wave_transmit_distance(world_point, transmit_angle, tx_rows);
		} else {
			result = cylindrical_wave_transmit_distance(world_point, focal_depth, transmit_angle, tx_rows);
		}
	}
	return result;
}

RESULT_TYPE RCA(const vec3 world_point)
{
	RESULT_TYPE result = RESULT_TYPE(0);
	for (int16_t acquisition = int16_t(0); acquisition < int16_t(AcquisitionCount); acquisition++) {
		const uint16_t tx_rx_orientation = tx_rx_orientation_for_acquisition(acquisition);
		const bool     rx_rows           = RX_ORIENTATION(tx_rx_orientation) == RCAOrientation_Rows;
		const vec2     focal_vector      = focal_vector_for_acquisition(acquisition);
		vec2  xdc_world_point   = rca_plane_projection((xdc_transform * vec4(world_point, 1)).xyz, rx_rows);
		float transmit_distance = rca_transmit_distance(world_point, focal_vector, tx_rx_orientation);

		int rf_offset  = int(rf_element_offset) + acquisition * SampleCount;
		rf_offset     -= int(InterpolationMode == InterpolationMode_Cubic);
		for (int chunk_channel = 0; chunk_channel < ChannelChunkCount; chunk_channel++) {
			int   rx_channel     = channel_offset + chunk_channel;
			vec3  rx_center      = vec3(rx_channel * xdc_element_pitch, 0);
			vec2  receive_vector = xdc_world_point - rca_plane_projection(rx_center, rx_rows);
			float a_arg          = abs(FNumber * receive_vector.x / abs(xdc_world_point.y));

			if (a_arg < 0.5f) {
				float       sidx  = sample_index(transmit_distance + length(receive_vector));
				SAMPLE_TYPE value = apodize(a_arg) * sample_rf(rf_offset, sidx);
				result += RESULT_STORE(value);
			}
			rf_offset += SampleCount * AcquisitionCount;
		}
	}
	return result;
}

RESULT_TYPE HERCULES(const vec3 world_point)
{
	const uint16_t tx_rx_orientation = tx_rx_orientation_for_acquisition(int16_t(0));
	const bool     rx_cols           = RX_ORIENTATION(tx_rx_orientation) == RCAOrientation_Columns;
	const vec2     focal_vector      = focal_vector_for_acquisition(int16_t(0));
	const vec3     xdc_world_point   = (xdc_transform * vec4(world_point, 1)).xyz;

	const float transmit_index   = sample_index(rca_transmit_distance(world_point, focal_vector, tx_rx_orientation));
	const float z_delta_squared  = xdc_world_point.z * xdc_world_point.z;
	const float f_number_over_z  = abs(FNumber / xdc_world_point.z);
	const vec2  xy_world_point   = xdc_world_point.xy;
	const float apodization_test = 0.25f / (f_number_over_z * f_number_over_z);

	RESULT_TYPE result = RESULT_TYPE(0);
	for (float chunk_channel = 0; chunk_channel < float(ChannelChunkCount); chunk_channel += 1.0f) {
		float rx_channel = float(channel_offset) + chunk_channel;
		int rf_offset   = int(rf_element_offset) + int(chunk_channel) * SampleCount * AcquisitionCount + Sparse * SampleCount;
		rf_offset      -= int(InterpolationMode == InterpolationMode_Cubic);

		// NOTE(rnp): this wouldn't be so messy if we just forced an orientation like with FORCES
		vec2 element_receive_delta_squared = xy_world_point;
		if (rx_cols) element_receive_delta_squared.x -= rx_channel * xdc_element_pitch.x;
		else         element_receive_delta_squared.y -= rx_channel * xdc_element_pitch.y;

		if (rx_cols) element_receive_delta_squared.x *= element_receive_delta_squared.x;
		else         element_receive_delta_squared.y *= element_receive_delta_squared.y;

		for (int transmit = Sparse; transmit < AcquisitionCount; transmit++) {
			int tx_channel = bool(Sparse) ? ArrayParameters(array_parameters).data.sparse_elements[transmit - Sparse]
			                              : transmit;

			if (rx_cols) element_receive_delta_squared.y  = xy_world_point.y - tx_channel * xdc_element_pitch.y;
			else         element_receive_delta_squared.x  = xy_world_point.x - tx_channel * xdc_element_pitch.x;

			if (rx_cols) element_receive_delta_squared.y *= element_receive_delta_squared.y;
			else         element_receive_delta_squared.x *= element_receive_delta_squared.x;

			float element_delta_squared = element_receive_delta_squared.x + element_receive_delta_squared.y;
			if (element_delta_squared < apodization_test) {
				/* NOTE: tribal knowledge */
				float apodization = transmit == 0 ? inversesqrt(float(AcquisitionCount)) : 1.0f;
				apodization *= apodize(f_number_over_z * sqrt(element_delta_squared));

				float index = transmit_index + sqrt(z_delta_squared + element_delta_squared) * SamplingFrequency / SpeedOfSound;
				SAMPLE_TYPE value = apodization * sample_rf(rf_offset, index);
				result += RESULT_STORE(value);
			}

			rf_offset += SampleCount;
		}
	}
	return result;
}

RESULT_TYPE FORCES(const vec3 xdc_world_point)
{
	RESULT_TYPE result = RESULT_TYPE(0);

	float z_delta_squared     = xdc_world_point.z * xdc_world_point.z;
	float transmit_y_delta    = xdc_world_point.y - xdc_element_pitch.y * ChannelCount / 2;
	float transmit_yz_squared = transmit_y_delta * transmit_y_delta + z_delta_squared;

	for (float chunk_channel = 0; chunk_channel < float(ChannelChunkCount); chunk_channel += 1.0f) {
		float rx_channel      = float(channel_offset) + chunk_channel;
		float receive_x_delta = xdc_world_point.x - rx_channel * xdc_element_pitch.x;
		float a_arg           = abs(FNumber * receive_x_delta / xdc_world_point.z);

		if (a_arg < 0.5f) {
			int rf_offset  = int(rf_element_offset) + int(chunk_channel) * SampleCount * AcquisitionCount + Sparse * SampleCount;
			rf_offset     -= int(InterpolationMode == InterpolationMode_Cubic);

			float receive_index = sample_index(sqrt(receive_x_delta * receive_x_delta + z_delta_squared));
			float apodization   = apodize(a_arg);
			for (int transmit = Sparse; transmit < AcquisitionCount; transmit++) {
				int tx_channel = bool(Sparse) ? ArrayParameters(array_parameters).data.sparse_elements[transmit - Sparse]
				                               : transmit;
				float transmit_x_delta = xdc_world_point.x - xdc_element_pitch.x * tx_channel;
				float transmit_index   = sqrt(transmit_yz_squared + transmit_x_delta * transmit_x_delta) * SamplingFrequency / SpeedOfSound;

				SAMPLE_TYPE value = apodization * sample_rf(rf_offset, receive_index + transmit_index);
				result    += RESULT_STORE(value);
				rf_offset += SampleCount;
			}
		}
	}
	return result;
}

void main()
{
	uvec3 out_voxel = gl_GlobalInvocationID;
	if (!all(lessThan(out_voxel, uvec3(output_size_x, output_size_y, output_size_z))))
		return;

	vec3 image_points = vec3(output_size_x, output_size_y, output_size_z) - 1.0f;
	vec3 point        = vec3(out_voxel) / max(vec3(1.0f), image_points);
	vec3 world_point  = (voxel_transform * vec4(point, 1)).xyz;

	uint32_t out_index = output_index(out_voxel.x, out_voxel.y, out_voxel.z);

	RESULT_TYPE sum = RESULT_TYPE(0);
	switch (AcquisitionKind) {
	case AcquisitionKind_FORCES:
	case AcquisitionKind_UFORCES:
	{
		sum = FORCES(world_point);
	}break;
	case AcquisitionKind_HERCULES:
	case AcquisitionKind_UHERCULES:
	case AcquisitionKind_HERO_PA:
	{
		sum = HERCULES(world_point);
	}break;
	case AcquisitionKind_Flash:
	case AcquisitionKind_RCA_TPW:
	case AcquisitionKind_RCA_VLS:
	{
		sum = RCA(world_point);
	}break;
	}

	#if CoherencyWeighting
	incoherent_data[incoherent_element_offset + out_index] += RESULT_INCOHERENT_CAST(sum);
	#endif

	output_data[output_element_offset + out_index] += RESULT_COHERENT_CAST(sum);
}
