/* See LICENSE for license details. */
#if   DataKind == DataKind_Float32
  #define SAMPLE_TYPE           float
  #define TEXTURE_KIND          r32f
  #define RESULT_TYPE_CAST(a)   (a).x
  #define OUTPUT_TYPE_CAST(a)   vec4((a).x, 0, 0, 0)
  #if (ShaderFlags & ShaderFlags_Fast) == 0
    #define RESULT_TYPE         vec2
    #define RESULT_LAST_INDEX   1
  #endif
#elif DataKind == DataKind_Float32Complex
  #define SAMPLE_TYPE           vec2
  #define TEXTURE_KIND          rg32f
  #define RESULT_TYPE_CAST(a)   (a).xy
  #define OUTPUT_TYPE_CAST(a)   vec4((a).xy, 0, 0)
  #if (ShaderFlags & ShaderFlags_Fast) == 0
    #define RESULT_TYPE         vec3
    #define RESULT_LAST_INDEX   2
  #endif
#else
  #error DataKind unsupported for DAS
#endif

layout(std430, binding = 1) readonly restrict buffer buffer_1 {
	SAMPLE_TYPE rf_data[];
};

#ifndef RESULT_TYPE
  #define RESULT_TYPE SAMPLE_TYPE
#endif

#if (ShaderFlags & ShaderFlags_Fast)
  #define RESULT_STORE(a, length_a) RESULT_TYPE(a)
#else
  #define RESULT_STORE(a, length_a) RESULT_TYPE(a, length_a)
#endif

const bool fast                = bool(ShaderFlags & ShaderFlags_Fast);
const bool sparse              = bool(ShaderFlags & ShaderFlags_Sparse);
const bool interpolate         = bool(ShaderFlags & ShaderFlags_Interpolate);
const bool coherency_weighting = bool(ShaderFlags & ShaderFlags_CoherencyWeighting);
const bool receive_only        = bool(ShaderFlags & ShaderFlags_ReceiveOnly);

#if (ShaderFlags & ShaderFlags_Fast)
layout(TEXTURE_KIND, binding = 0)           restrict uniform image3D  u_out_data_tex;
#else
layout(TEXTURE_KIND, binding = 0) writeonly restrict uniform image3D  u_out_data_tex;
#endif

layout(r16i,  binding = 1) readonly  restrict uniform iimage1D sparse_elements;
layout(rg32f, binding = 2) readonly  restrict uniform image1D  focal_vectors;
layout(r8i,   binding = 3) readonly  restrict uniform iimage1D transmit_receive_orientations;

#define RX_ORIENTATION_MASK (1 << 0)
#define TX_ORIENTATION_MASK (1 << 1)

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
		rf_data[base_index + int(tk) - 1],
		rf_data[base_index + int(tk) + 0],
		rf_data[base_index + int(tk) + 1],
		rf_data[base_index + int(tk) + 2],
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
	SAMPLE_TYPE result = SAMPLE_TYPE(index >= 0.0f) * SAMPLE_TYPE((int(index) + 1 + int(interpolate)) < SampleCount);
	int base_index = int(channel * SampleCount * AcquisitionCount + transmit * SampleCount);
	if (interpolate) result *= cubic(base_index, index);
	else             result *= rf_data[base_index + int(round(index))];
	result = rotate_iq(result, index / SamplingFrequency);
	return result;
}

float sample_index(const float distance)
{
	float  time = distance / SpeedOfSound + TimeOffset;
	return time * SamplingFrequency;
}

float apodize(const float arg)
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

float rca_transmit_distance(const vec3 world_point, const vec2 focal_vector, const int transmit_receive_orientation)
{
	float result = 0;
	if (!receive_only) {
		bool  tx_rows        = (transmit_receive_orientation & TX_ORIENTATION_MASK) == 0;
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
	const int acquisition_start = fast? u_channel     : 0;
	const int acquisition_end   = fast? u_channel + 1 : AcquisitionCount;
	RESULT_TYPE result = RESULT_TYPE(0);
	for (int acquisition = acquisition_start; acquisition < acquisition_end; acquisition++) {
		int   transmit_receive_orientation = imageLoad(transmit_receive_orientations, acquisition).x;
		bool  rx_rows           = (transmit_receive_orientation & RX_ORIENTATION_MASK) == 0;
		vec2  xdc_world_point   = rca_plane_projection((xdc_transform * vec4(world_point, 1)).xyz, rx_rows);
		float transmit_distance = rca_transmit_distance(world_point, imageLoad(focal_vectors, acquisition).xy,
		                                                transmit_receive_orientation);

		for (int rx_channel = 0; rx_channel < ChannelCount; rx_channel++) {
			vec3  rx_center      = vec3(rx_channel * xdc_element_pitch, 0);
			vec2  receive_vector = xdc_world_point - rca_plane_projection(rx_center, rx_rows);
			float apodization    = apodize(FNumber * radians(180) / abs(xdc_world_point.y) * receive_vector.x);

			if (apodization > 0) {
				float       sidx  = sample_index(transmit_distance + length(receive_vector));
				SAMPLE_TYPE value = apodization * sample_rf(rx_channel, acquisition, sidx);
				result += RESULT_STORE(value, length(value));
			}
		}
	}
	return result;
}

RESULT_TYPE HERCULES(const vec3 world_point)
{
	const int rx_channel_start = fast? u_channel     : 0;
	const int rx_channel_end   = fast? u_channel + 1 : ChannelCount;

	int   transmit_receive_orientation = imageLoad(transmit_receive_orientations, 0).x;
	vec3  xdc_world_point   = (xdc_transform * vec4(world_point, 1)).xyz;
	bool  rx_cols           = (transmit_receive_orientation & RX_ORIENTATION_MASK) != 0;
	float transmit_distance = rca_transmit_distance(world_point, imageLoad(focal_vectors, 0).xy,
	                                                transmit_receive_orientation);

	RESULT_TYPE result = RESULT_TYPE(0);
	for (int transmit = int(sparse); transmit < AcquisitionCount; transmit++) {
		int tx_channel = sparse ? imageLoad(sparse_elements, transmit - int(sparse)).x : transmit;
		for (int rx_channel = rx_channel_start; rx_channel < rx_channel_end; rx_channel++) {
			vec3 element_position;
			if (rx_cols) element_position = vec3(rx_channel, tx_channel, 0) * vec3(xdc_element_pitch, 0);
			else         element_position = vec3(tx_channel, rx_channel, 0) * vec3(xdc_element_pitch, 0);

			float apodization = apodize(FNumber * radians(180) / abs(xdc_world_point.z) *
			                            distance(xdc_world_point.xy, element_position.xy));
			if (apodization > 0) {
				/* NOTE: tribal knowledge */
				if (transmit == 0) apodization *= inversesqrt(AcquisitionCount);

				float       sidx  = sample_index(transmit_distance + distance(xdc_world_point, element_position));
				SAMPLE_TYPE value = apodization * sample_rf(rx_channel, transmit, sidx);
				result += RESULT_STORE(value, length(value));
			}
		}
	}
	return result;
}

RESULT_TYPE FORCES(const vec3 world_point)
{
	const int rx_channel_start = fast? u_channel     : 0;
	const int rx_channel_end   = fast? u_channel + 1 : ChannelCount;

	RESULT_TYPE result = RESULT_TYPE(0);
	vec3 xdc_world_point = (xdc_transform * vec4(world_point, 1)).xyz;
	for (int rx_channel = rx_channel_start; rx_channel < rx_channel_end; rx_channel++) {
		float receive_distance = distance(xdc_world_point.xz, vec2(rx_channel * xdc_element_pitch.x, 0));
		float apodization      = apodize(FNumber * radians(180) / abs(xdc_world_point.z) *
		                                 (xdc_world_point.x - rx_channel * xdc_element_pitch.x));
		if (apodization > 0) {
			for (int transmit = int(sparse); transmit < AcquisitionCount; transmit++) {
				int   tx_channel      = sparse ? imageLoad(sparse_elements, transmit - int(sparse)).x : transmit;
				vec3  transmit_center = vec3(xdc_element_pitch * vec2(tx_channel, floor(ChannelCount / 2)), 0);

				float       sidx  = sample_index(distance(xdc_world_point, transmit_center) + receive_distance);
				SAMPLE_TYPE value = apodization * sample_rf(rx_channel, transmit, sidx);
				result += RESULT_STORE(value, length(value));
			}
		}
	}
	return result;
}

void main()
{
	ivec3 out_voxel = ivec3(gl_GlobalInvocationID);
	if (!all(lessThan(out_voxel, imageSize(u_out_data_tex))))
		return;

#if (ShaderFlags & ShaderFlags_Fast)
	RESULT_TYPE sum = RESULT_TYPE_CAST(imageLoad(u_out_data_tex, out_voxel));
#else
	RESULT_TYPE sum = RESULT_TYPE(0);
	out_voxel += u_voxel_offset;
#endif

	vec3 world_point = (voxel_transform * vec4(out_voxel, 1)).xyz;

	switch (AcquisitionKind) {
	case AcquisitionKind_FORCES:
	case AcquisitionKind_UFORCES:
	{
		sum += FORCES(world_point);
	}break;
	case AcquisitionKind_HERCULES:
	case AcquisitionKind_UHERCULES:
	case AcquisitionKind_HERO_PA:
	{
		sum += HERCULES(world_point);
	}break;
	case AcquisitionKind_Flash:
	case AcquisitionKind_RCA_TPW:
	case AcquisitionKind_RCA_VLS:
	{
		sum += RCA(world_point);
	}break;
	}

	#if (ShaderFlags & ShaderFlags_Fast) == 0
	/* TODO(rnp): scale such that brightness remains ~constant */
	if (coherency_weighting) {
		float denominator = sum[RESULT_LAST_INDEX] + float(sum[RESULT_LAST_INDEX] == 0);
		RESULT_TYPE_CAST(sum) *= RESULT_TYPE_CAST(sum) / denominator;
	}
	#endif

	imageStore(u_out_data_tex, out_voxel, OUTPUT_TYPE_CAST(sum));
}
