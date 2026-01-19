/* See LICENSE for license details. */
#if   DataKind == DataKind_Float32
  #if CoherencyWeighting
    #define RESULT_TYPE               vec2
    #define RESULT_COHERENT_CAST(a)   (a).x
    #define RESULT_INCOHERENT_CAST(a) (a).y
  #endif
  #define SAMPLE_TYPE float
#elif DataKind == DataKind_Float32Complex
  #if CoherencyWeighting
    #define RESULT_TYPE               vec3
    #define RESULT_COHERENT_CAST(a)   (a).xy
    #define RESULT_INCOHERENT_CAST(a) (a).z
  #endif
  #define SAMPLE_TYPE vec2
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

layout(std430, buffer_reference, buffer_reference_align = 64) restrict readonly buffer RF {
	SAMPLE_TYPE values[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict readonly buffer SparseElements {
	int16_t values[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict readonly buffer TransmitReceiveOrientations {
	uint16_t values[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict readonly buffer FocalVectors {
	vec2 values[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict buffer Output {
	SAMPLE_TYPE values[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict buffer IncoherentOutput {
	float values[];
};

#define RX_ORIENTATION(tx_rx) (((tx_rx) >> 0) & 0x0F)
#define TX_ORIENTATION(tx_rx) (((tx_rx) >> 4) & 0x0F)
//#define RX_ORIENTATION(tx_rx) bitfieldExtract((tx_rx), 0, 4)
//#define TX_ORIENTATION(tx_rx) bitfieldExtract((tx_rx), 4, 4)

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
SAMPLE_TYPE cubic(const int base_index, const float index)
{
	const mat4 h = mat4(
		 2, -3,  0, 1,
		-2,  3,  0, 0,
		 1, -2,  1, 0,
		 1, -1,  0, 0
	);

	float tk, t = modf(index, tk);
	SAMPLE_TYPE samples[4] = {
		RF(rf_data).values[base_index + int(tk) - 1],
		RF(rf_data).values[base_index + int(tk) + 0],
		RF(rf_data).values[base_index + int(tk) + 1],
		RF(rf_data).values[base_index + int(tk) + 2],
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

SAMPLE_TYPE sample_rf(const int channel, const int transmit, const float index)
{
	SAMPLE_TYPE result = SAMPLE_TYPE(0);
	int base_index = int(channel * SampleCount * AcquisitionCount + transmit * SampleCount);
	switch (InterpolationMode) {
	case InterpolationMode_Nearest:{
		if (index >= 0 && int(round(index)) < SampleCount)
			result = rotate_iq(RF(rf_data).values[base_index + int(round(index))], index / SamplingFrequency);
	}break;
	case InterpolationMode_Linear:{
		if (index >= 0 && round(index) < SampleCount) {
			float tk, t = modf(index, tk);
			int n = base_index + int(tk);
			result = (1 - t) * RF(rf_data).values[n] + t * RF(rf_data).values[n + 1];
		}
		result = rotate_iq(result, index / SamplingFrequency);
	}break;
	case InterpolationMode_Cubic:{
		if (index >= 0 && (int(index) + 2) < SampleCount)
			result = rotate_iq(cubic(base_index, index), index / SamplingFrequency);
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
	uint16_t result = bool(SingleOrientation) ? uint16_t(TransmitReceiveOrientation)
	                                          : TransmitReceiveOrientations(transmit_receive_orientations).values[acquisition];
	return result;
}

vec2 focal_vector_for_acquisition(const int16_t acquisition)
{
	vec2 result = bool(SingleFocus) ? vec2(TransmitAngle, FocusDepth)
	                                : FocalVectors(focal_vectors).values[acquisition];
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
	const int16_t acquisition_start = int16_t(channel_t);
	const int16_t acquisition_end   = int16_t(channel_t + 1);
	RESULT_TYPE result = RESULT_TYPE(0);
	for (int16_t acquisition = acquisition_start; acquisition < acquisition_end; acquisition++) {
		const uint16_t tx_rx_orientation = tx_rx_orientation_for_acquisition(acquisition);
		const bool     rx_rows           = RX_ORIENTATION(tx_rx_orientation) == RCAOrientation_Rows;
		const vec2     focal_vector      = focal_vector_for_acquisition(acquisition);
		vec2  xdc_world_point   = rca_plane_projection((xdc_transform * vec4(world_point, 1)).xyz, rx_rows);
		float transmit_distance = rca_transmit_distance(world_point, focal_vector, tx_rx_orientation);

		for (int16_t rx_channel = int16_t(0); rx_channel < int16_t(ChannelCount); rx_channel++) {
			vec3  rx_center      = vec3(rx_channel * xdc_element_pitch, 0);
			vec2  receive_vector = xdc_world_point - rca_plane_projection(rx_center, rx_rows);
			float a_arg          = abs(FNumber * receive_vector.x / abs(xdc_world_point.y));

			if (a_arg < 0.5f) {
				float       sidx  = sample_index(transmit_distance + length(receive_vector));
				SAMPLE_TYPE value = apodize(a_arg) * sample_rf(rx_channel, acquisition, sidx);
				result += RESULT_STORE(value);
			}
		}
	}
	return result;
}

RESULT_TYPE HERCULES(const vec3 world_point)
{
	const uint16_t tx_rx_orientation = tx_rx_orientation_for_acquisition(int16_t(0));
	const bool     rx_cols           = RX_ORIENTATION(tx_rx_orientation) == RCAOrientation_Columns;
	const vec2     focal_vector      = focal_vector_for_acquisition(int16_t(0));
	const float    transmit_distance = rca_transmit_distance(world_point, focal_vector, tx_rx_orientation);
	const vec3     xdc_world_point   = (xdc_transform * vec4(world_point, 1)).xyz;

	RESULT_TYPE result = RESULT_TYPE(0);
	for (int16_t transmit = int16_t(Sparse); transmit < int16_t(AcquisitionCount); transmit++) {
		const int16_t tx_channel = bool(Sparse) ? SparseElements(sparse_elements).values[transmit - Sparse]
		                                        : transmit;
		const int16_t rx_channel = int16_t(channel_t);
		{
			vec3 element_position;
			if (rx_cols) element_position = vec3(rx_channel, tx_channel, 0) * vec3(xdc_element_pitch, 0);
			else         element_position = vec3(tx_channel, rx_channel, 0) * vec3(xdc_element_pitch, 0);

			float a_arg = abs(FNumber * distance(xdc_world_point.xy, element_position.xy) /
			                  abs(xdc_world_point.z));
			if (a_arg < 0.5f) {
				float apodization = apodize(a_arg);
				/* NOTE: tribal knowledge */
				if (transmit == 0) apodization *= inversesqrt(float(AcquisitionCount));

				float       sidx  = sample_index(transmit_distance + distance(xdc_world_point, element_position));
				SAMPLE_TYPE value = apodization * sample_rf(rx_channel, transmit, sidx);
				result += RESULT_STORE(value);
			}
		}
	}
	return result;
}

RESULT_TYPE FORCES(const vec3 world_point)
{
	const int16_t rx_channel_start = int16_t(channel_t);
	const int16_t rx_channel_end   = int16_t(channel_t + 1);

	RESULT_TYPE result = RESULT_TYPE(0);
	vec3 xdc_world_point = (xdc_transform * vec4(world_point, 1)).xyz;
	for (int16_t rx_channel = rx_channel_start; rx_channel < rx_channel_end; rx_channel++) {
		float receive_distance = distance(xdc_world_point.xz, vec2(rx_channel * xdc_element_pitch.x, 0));
		float a_arg = abs(FNumber * (xdc_world_point.x - rx_channel * xdc_element_pitch.x) /
		                  abs(xdc_world_point.z));
		if (a_arg < 0.5f) {
			float apodization = apodize(a_arg);
			for (int16_t transmit = int16_t(Sparse); transmit < int16_t(AcquisitionCount); transmit++) {
				int16_t tx_channel = bool(Sparse) ? SparseElements(sparse_elements).values[transmit - Sparse]
				                                  : transmit;
				vec3    transmit_center = vec3(xdc_element_pitch * vec2(tx_channel, int(ChannelCount / 2)), 0);

				float       sidx  = sample_index(distance(xdc_world_point, transmit_center) + receive_distance);
				SAMPLE_TYPE value = apodization * sample_rf(rx_channel, transmit, sidx);
				result += RESULT_STORE(value);
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

	RESULT_TYPE sum = RESULT_TYPE(0);

	vec3 world_point = (voxel_transform * vec4(out_voxel, 1)).xyz;

	uint32_t out_index = output_index(out_voxel.x, out_voxel.y, out_voxel.z);

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
	IncoherentOutput(incoherent_output).values[out_index] += RESULT_INCOHERENT_CAST(sum);
	#endif

	Output(output_data).values[out_index] += RESULT_COHERENT_CAST(sum);
}
