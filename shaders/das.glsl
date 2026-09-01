/* See LICENSE for license details. */
#if   InputDataKind == DataKind_Float32 || InputDataKind == DataKind_Float16
  #if CoherencyWeighting
    #define RESULT_TYPE               vec2
    #define RESULT_COHERENT_CAST(a)   (a).x
    #define RESULT_INCOHERENT_CAST(a) (a).y
  #endif
  #define SAMPLE_TYPE f32
#elif InputDataKind == DataKind_Float32Complex || InputDataKind == DataKind_Float16Complex
  #if CoherencyWeighting
    #define RESULT_TYPE               vec3
    #define RESULT_COHERENT_CAST(a)   (a).xy
    #define RESULT_INCOHERENT_CAST(a) (a).z
  #endif
  #define SAMPLE_TYPE f32vec2
#else
  #error InputDataKind unsupported for DAS
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

// NOTE(rnp): we don't want das to get recompiled when it isn't actually using the Heap
// but we also don't want to check everywhere in here for the existence of the Heap
#ifndef HeapBase
  #define HeapBase u64(0)
#endif

layout(std430, buffer_reference) buffer Input  { InputDataType x[]; };
layout(std430, buffer_reference) buffer Output { OutputDataType x[]; };

layout(std430, buffer_reference) buffer IncoherentOutput {
	f32 x[];
};

layout(std430, buffer_reference) buffer F16   { f16     x[]; };
layout(std430, buffer_reference) buffer F32   { f32     x[]; };
layout(std430, buffer_reference) buffer S16   { s16     x[]; };
layout(std430, buffer_reference) buffer U8    { u8      x[]; };
layout(std430, buffer_reference) buffer U32V4 { u32vec4 x[]; };
layout(std430, buffer_reference) buffer F32V2 { f32vec2 x[]; };
layout(std430, buffer_reference) buffer F32V4 { f32vec4 x[]; };
layout(std430, buffer_reference) buffer F16V2 { f16vec2 x[]; };
layout(std430, buffer_reference) buffer F16V4 { f16vec4 x[]; };
layout(std430, buffer_reference) buffer M4    { mat4    x[]; };

#define RX_ORIENTATION(tx_rx) bitfieldExtract((tx_rx), 0, 4)
#define TX_ORIENTATION(tx_rx) bitfieldExtract((tx_rx), 4, 4)

#define C_SPLINE 0.5

#if InputDataKind == DataKind_Float32Complex || InputDataKind == DataKind_Float16Complex
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

// NOTE(rnp): while the input RF buffer is padded such that we could continue reading
// DAS is very expensive so we want to avoid any extra work possible.
u32 batch_channel_count()
{
	const bool safe   = (ReceiveChannelCount % ChunkChannelCount) == 0;
	const u32  result = safe ? ChunkChannelCount : min(ReceiveChannelCount - channel_offset, ChunkChannelCount);
	return result;
}

/* NOTE: See: https://cubic.org/docs/hermite.htm */
SAMPLE_TYPE cubic(const u64 rf_pointer, const f32 t)
{
	const mat4 h = mat4(
		 2, -3,  0, 1,
		-2,  3,  0, 0,
		 1, -2,  1, 0,
		 1, -1,  0, 0
	);

	#if InputDataKind == DataKind_Float32
		f32vec4 samples = F32V4(rf_pointer).x[0];
	#elif InputDataKind == DataKind_Float16
		f16vec4 samples = F16V4(rf_pointer).x[0];
	#elif InputDataKind == DataKind_Float16Complex
		f32vec2 samples[4];
		uvec4 load = U32V4(rf_pointer).x[0];
		samples[0] = unpackHalf2x16(load[0]);
		samples[1] = unpackHalf2x16(load[1]);
		samples[2] = unpackHalf2x16(load[2]);
		samples[3] = unpackHalf2x16(load[3]);
	#else
		f32vec2 samples[4];
		vec4 load1 = F32V4(rf_pointer).x[0];
		vec4 load2 = F32V4(rf_pointer).x[1];
		samples[0] = load1.xy;
		samples[1] = load1.zw;
		samples[2] = load2.xy;
		samples[3] = load2.zw;
	#endif

	vec4        Sh = vec4(t * t * t, t * t, t, 1) * h;
	SAMPLE_TYPE P1 = samples[1];
	SAMPLE_TYPE P2 = samples[2];
	SAMPLE_TYPE T1 = C_SPLINE * (P2 - samples[0]);
	SAMPLE_TYPE T2 = C_SPLINE * (samples[3] - P1);

	#if   InputDataKind == DataKind_Float32 || InputDataKind == DataKind_Float16
		SAMPLE_TYPE result = dot(Sh, vec4(P1, P2, T1, T2));
	#else
		mat2x4 C = mat2x4(vec4(P1.x, P2.x, T1.x, T2.x), vec4(P1.y, P2.y, T1.y, T2.y));
		SAMPLE_TYPE result = Sh * C;
	#endif
	return result;
}

SAMPLE_TYPE sample_rf(const u64 rf_pointer, const f32 index)
{
	SAMPLE_TYPE result = SAMPLE_TYPE(0);

	switch (InterpolationMode) {
	case InterpolationMode_Nearest:{
		if (index >= 0.f && index < (f32(SampleCount) - 0.5f))
			result = rotate_iq(Input(rf_pointer + InputDataKindByteSize * u32(round(index))).x[0], index / SamplingFrequency);
	}break;
	case InterpolationMode_Linear:{
		if (index >= 0.f && index < f32(SampleCount - 1)) {
			#if InputDataKind == DataKind_Float32
				f32vec2 rf = F32V2(rf_pointer + InputDataKindByteSize * u32(index)).x[0];
			#elif InputDataKind == DataKind_Float16
				f16vec2 rf = F16V2(rf_pointer + InputDataKindByteSize * u32(index)).x[0];
			#elif InputDataKind == DataKind_Float16Complex
				f16vec4 load  = F16V4(rf_pointer + InputDataKindByteSize * u32(index)).x[0];
				f16vec2 rf[2] = {load.xy, load.zw};
			#else
				f32vec4 load  = F32V4(rf_pointer + InputDataKindByteSize * u32(index)).x[0];
				f32vec2 rf[2] = {load.xy, load.zw};
			#endif

			f32 t  = fract(index);
			result = (1 - t) * rf[0] + t * rf[1];
			result = rotate_iq(result, index / SamplingFrequency);
		}
	}break;
	case InterpolationMode_Cubic:{
		if (index >= 1.f && index < f32(SampleCount - 2))
			result = rotate_iq(cubic(rf_pointer + InputDataKindByteSize * u32(index), fract(index)), index / SamplingFrequency);
	}break;
	}
	return result;
}

float sample_index(const float distance)
{
	float  time = distance / SpeedOfSound + TimeOffset;
	return time * SamplingFrequency;
}

u32 output_index(const u32 x, const u32 y, const u32 z)
{
	u32 result = OutputSizeX * OutputSizeY * z + OutputSizeX * y + x;
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

u8 tx_rx_orientation_for_acquisition(const s32 acquisition)
{
	u8 result = u8(TransmitReceiveOrientation);
	if (!SingleOrientation) result = U8(HeapBase + TransmitReceiveOrientations).x[acquisition];
	return result;
}

f32vec2 focal_vector_for_acquisition(const s32 acquisition)
{
	f32vec2 result = SingleFocus ? f32vec2(TransmitAngle, FocusDepth) : F32V2(HeapBase + FocalVectors).x[acquisition];
	return result;
}

f32 rca_transmit_distance(const vec3 world_point, const vec2 focal_vector, const u8 transmit_receive_orientation)
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

mat4 transducer_transform(const u32 acquisition)
{
	const u32 receive_tile_channels = ReceiveChannelCount / ReceiveTileCount;
	const u32 receive_index = channel_offset / receive_tile_channels;

	const u32 transmit_tile_channels = TransmitChannelCount / TransmitTileCount;
	const u32 transmit_index = acquisition / transmit_tile_channels;

	mat4 result = M4(HeapBase + TransducerTransforms).x[transmit_index * ReceiveTileCount + receive_index];
	return result;
}

RESULT_TYPE RCA(const vec3 world_point)
{
	RESULT_TYPE result = RESULT_TYPE(0);

	const mat4 xdc_transform = transducer_transform(0);
	for (s32 acquisition = 0; acquisition < s32(AcquisitionCount); acquisition++) {
		const u8   tx_rx_orientation = tx_rx_orientation_for_acquisition(acquisition);
		const bool rx_rows           = RX_ORIENTATION(tx_rx_orientation) == RCAOrientation_Rows;
		const vec2 focal_vector      = focal_vector_for_acquisition(acquisition);
		vec2  xdc_world_point   = rca_plane_projection((xdc_transform * vec4(world_point, 1)).xyz, rx_rows);
		float transmit_distance = rca_transmit_distance(world_point, focal_vector, tx_rx_orientation);

		u64 rf_pointer  = RFData + InputDataKindByteSize * acquisition * SampleCount;
		rf_pointer     -= InputDataKindByteSize * u32(InterpolationMode == InterpolationMode_Cubic);

		for (f32 chunk_channel = 0.f; chunk_channel < f32(batch_channel_count()); chunk_channel += 1.f) {
			f32  rx_channel     = f32(channel_offset) + chunk_channel;
			vec3 rx_center      = vec3(rx_channel * xdc_element_pitch, 0);
			vec2 receive_vector = xdc_world_point - rca_plane_projection(rx_center, rx_rows);
			f32  a_arg          = abs(FNumber * receive_vector.x / abs(xdc_world_point.y));

			if (a_arg < 0.5f) {
				f32         index = sample_index(transmit_distance + length(receive_vector));
				SAMPLE_TYPE value = apodize(a_arg) * sample_rf(rf_pointer, index);
				result += RESULT_STORE(value);
			}
			rf_pointer += InputDataKindByteSize * SampleCount * AcquisitionCount;
		}
	}
	return result;
}

RESULT_TYPE HERCULES(const vec3 world_point)
{
	// TODO(rnp): this whole thing needs have an outer loop over transmit tiles
	const mat4 xdc_transform     = transducer_transform(0);

	const u8   tx_rx_orientation = tx_rx_orientation_for_acquisition(0);
	const bool rx_cols           = RX_ORIENTATION(tx_rx_orientation) == RCAOrientation_Columns;
	const vec2 focal_vector      = focal_vector_for_acquisition(0);
	const vec3 xdc_world_point   = (xdc_transform * vec4(world_point, 1)).xyz;

	const f32 transmit_index   = sample_index(rca_transmit_distance(world_point, focal_vector, tx_rx_orientation));
	const f32 z_delta_squared  = xdc_world_point.z * xdc_world_point.z;
	const f32 f_number_over_z  = abs(FNumber / xdc_world_point.z);
	const f32 apodization_test = 0.25f / (f_number_over_z * f_number_over_z);

	const f32 rx_world_point   = xdc_world_point[s32(!rx_cols)];
	const f32 tx_world_point   = xdc_world_point[s32(rx_cols)];
	const f32 rx_pitch         = xdc_element_pitch[s32(!rx_cols)];
	const f32 tx_pitch         = xdc_element_pitch[s32(rx_cols)];

	RESULT_TYPE result = RESULT_TYPE(0);
	for (f32 chunk_channel = 0.f; chunk_channel < f32(batch_channel_count()); chunk_channel += 1.f) {
		f32 rx_channel  = f32(channel_offset) + chunk_channel;

		f32 element_receive_delta_squared = rx_world_point - rx_channel * rx_pitch;
		element_receive_delta_squared *= element_receive_delta_squared;

		u64 rf_pointer  = RFData + InputDataKindByteSize * (u32(chunk_channel) * SampleCount * AcquisitionCount + u32(Sparse) * SampleCount);
		rf_pointer     -= InputDataKindByteSize * u32(InterpolationMode == InterpolationMode_Cubic);

		for (f32 transmit = f32(Sparse); transmit < f32(AcquisitionCount); transmit += 1.f) {
			f32 tx_channel = Sparse ? f32(S16(HeapBase + SparseElements).x[s32(transmit) - s32(Sparse)]) : transmit;

			f32 element_transmit_delta_squared = tx_world_point - tx_channel * tx_pitch;
			element_transmit_delta_squared *= element_transmit_delta_squared;

			f32 element_delta_squared = element_transmit_delta_squared + element_receive_delta_squared;
			if (element_delta_squared < apodization_test) {
				/* NOTE: tribal knowledge */
				float apodization = transmit == 0 ? inversesqrt(float(AcquisitionCount)) : 1.0f;
				apodization *= apodize(f_number_over_z * sqrt(element_delta_squared));

				float index = transmit_index + sqrt(z_delta_squared + element_delta_squared) * SamplingFrequency / SpeedOfSound;
				SAMPLE_TYPE value = apodization * sample_rf(rf_pointer, index);
				result += RESULT_STORE(value);
			}

			rf_pointer += InputDataKindByteSize * SampleCount;
		}
	}
	return result;
}

RESULT_TYPE FORCES(const vec3 world_point)
{
	RESULT_TYPE result = RESULT_TYPE(0);

	const mat4 xdc_transform   = transducer_transform(0);
	const vec3 xdc_world_point = (xdc_transform * vec4(world_point, 1)).xyz;

	// TODO(rnp): the sign of the origin offset might be flipped
	f32 origin_offset       = FocusDepth * tan(radians(TransmitAngle));
	f32 transmit_y_delta    = world_point.y + origin_offset;
	f32 z_delta_squared     = xdc_world_point.z * xdc_world_point.z;
	f32 transmit_yz_squared = transmit_y_delta * transmit_y_delta + z_delta_squared;

	for (f32 chunk_channel = 0; chunk_channel < f32(batch_channel_count()); chunk_channel += 1.f) {
		f32 rx_channel      = f32(channel_offset) + chunk_channel;
		f32 receive_x_delta = xdc_world_point.x - rx_channel * xdc_element_pitch.x;
		f32 a_arg           = abs(FNumber * receive_x_delta / xdc_world_point.z);

		if (a_arg < 0.5f) {
			u64 rf_pointer  = RFData + InputDataKindByteSize * (u32(chunk_channel) * SampleCount * AcquisitionCount + u32(Sparse) * SampleCount);
			rf_pointer     -= InputDataKindByteSize * u32(InterpolationMode == InterpolationMode_Cubic);

			f32 receive_index = sample_index(sqrt(receive_x_delta * receive_x_delta + z_delta_squared));
			f32 apodization   = apodize(a_arg);
			for (f32 transmit = f32(Sparse); transmit < f32(AcquisitionCount); transmit += 1.f) {
				f32 tx_channel = Sparse ? f32(S16(HeapBase + SparseElements).x[s32(transmit) - s32(Sparse)]) : transmit;
				f32 transmit_x_delta = xdc_world_point.x - xdc_element_pitch.x * tx_channel;
				f32 transmit_index   = sqrt(transmit_yz_squared + transmit_x_delta * transmit_x_delta) * SamplingFrequency / SpeedOfSound;

				SAMPLE_TYPE value = apodization * sample_rf(rf_pointer, receive_index + transmit_index);
				result     += RESULT_STORE(value);
				rf_pointer += InputDataKindByteSize * SampleCount;
			}
		}
	}
	return result;
}

RESULT_TYPE READI_FORCES(const vec3 world_point)
{
	RESULT_TYPE result = RESULT_TYPE(0);

	const mat4 xdc_transform   = transducer_transform(0);
	const vec3 xdc_world_point = (xdc_transform * vec4(world_point, 1)).xyz;

	// TODO(rnp): the sign of the origin offset might be flipped
	f32 origin_offset       = FocusDepth * tan(radians(TransmitAngle));
	f32 transmit_y_delta    = world_point.y + origin_offset;
	f32 z_delta_squared     = xdc_world_point.z * xdc_world_point.z;
	f32 transmit_yz_squared = transmit_y_delta * transmit_y_delta + z_delta_squared;

	// NOTE(tkh): The row we use matches the acquisition group, the column is the element group we are beamforming.
	s32 hadamard_offset = s32(readi_group) * s32(ReadiGroupCount);

	for (f32 chunk_channel = 0; chunk_channel < f32(batch_channel_count()); chunk_channel += 1.f) {
		f32 rx_channel      = f32(channel_offset) + chunk_channel;
		f32 receive_x_delta = xdc_world_point.x - rx_channel * xdc_element_pitch.x;
		f32 a_arg           = abs(FNumber * receive_x_delta / xdc_world_point.z);

		if (a_arg < 0.5f) {
			u64 channel_rf_pointer  = RFData + InputDataKindByteSize * u32(chunk_channel) * SampleCount * AcquisitionCount;
			channel_rf_pointer     -= InputDataKindByteSize * u32(InterpolationMode == InterpolationMode_Cubic);

			f32 receive_index = sample_index(sqrt(receive_x_delta * receive_x_delta + z_delta_squared));
			f32 apodization   = apodize(a_arg);

			// NOTE(tkh): Iterating over groups of tx elements, each group is AcquisitionCount
			// sequential elements. The first element in each group is beamformed using the first
			// acquisition, the second element in each group is beamformed using the second acquisition, etc.
			for (s32 tx_group = 0; tx_group < s32(ReadiGroupCount); tx_group++) {
				f32 group_apodization = apodization * F16(HeapBase + Hadamard).x[hadamard_offset + tx_group];
				u64 rf_pointer = channel_rf_pointer;

				for (f32 tx_event = 0; tx_event < f32(AcquisitionCount); tx_event += 1.f) {
					f32 tx_element       = f32(tx_group) * f32(AcquisitionCount) + tx_event;
					f32 transmit_x_delta = xdc_world_point.x - xdc_element_pitch.x * tx_element;
					f32 transmit_index   = sqrt(transmit_yz_squared + transmit_x_delta * transmit_x_delta) * SamplingFrequency / SpeedOfSound;

					SAMPLE_TYPE value = group_apodization * sample_rf(rf_pointer, receive_index + transmit_index);
					result     += RESULT_STORE(value);
					rf_pointer += InputDataKindByteSize * SampleCount;
				}
			}
		}
	}
	return result;
}

void main()
{
	uvec3 out_voxel = gl_GlobalInvocationID;
	if (!all(lessThan(out_voxel, uvec3(OutputSizeX, OutputSizeY, OutputSizeZ))))
		return;

	vec3 image_points = vec3(OutputSizeX, OutputSizeY, OutputSizeZ) - 1.0f;
	vec3 point        = vec3(out_voxel) / max(vec3(1.0f), image_points);
	vec3 world_point  = (voxel_transform * vec4(point, 1)).xyz;

	uint32_t out_index = output_index(out_voxel.x, out_voxel.y, out_voxel.z);

	RESULT_TYPE sum = RESULT_TYPE(0);
	switch (AcquisitionKind) {
	case AcquisitionKind_FORCES:
	case AcquisitionKind_UFORCES:
	{
		sum = ReadiGroupCount > 1 ? READI_FORCES(world_point)
		                          : FORCES(world_point);
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
	IncoherentOutput(HeapBase + IncoherentFrame).x[out_index] += RESULT_INCOHERENT_CAST(sum);
	#endif

	Output(output_frame).x[out_index] += RESULT_COHERENT_CAST(sum);
}
