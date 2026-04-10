/* See LICENSE for license details. */
#if  (InputDataKind == DataKind_Int16Complex          || \
      (InputDataKind == DataKind_Int16 && Demodulate) || \
      (InputDataKind == DataKind_Float16 && Demodulate))
  #define SAMPLE_TYPE f16vec2
#elif InputDataKind == DataKind_Int16
  #define SAMPLE_TYPE f16
#elif InputDataKind == DataKind_Float32 && Demodulate
  #define SAMPLE_TYPE f32vec2
#endif

#ifndef SAMPLE_TYPE
  #define SAMPLE_TYPE InputDataType
#endif

#define ComplexSampleType (InputDataKind == DataKind_Float32Complex || \
                           InputDataKind == DataKind_Float16Complex || \
                           InputDataKind == DataKind_Int16Complex   || \
                           Demodulate)
#if ComplexSampleType
  #define RESULT_TYPE f32vec2
#else
  #define RESULT_TYPE f32
#endif

#if ComplexFilter
  #define FILTER_TYPE f32vec2
#else
  #define FILTER_TYPE f32
#endif

#if ComplexFilter && ComplexSampleType
  #define apply_filter(iq, h) complex_mul(f32vec2(iq), f32vec2(h))
#else
  #define apply_filter(iq, h) ((iq) * (h))
#endif

layout(std430, buffer_reference, buffer_reference_align = 64) restrict readonly buffer Input {
	InputDataType x[];
};

layout(set = ShaderResourceKind_Buffer, binding = ShaderBufferSlot_PingPong) buffer Output {
	OutputDataType output_data[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict readonly buffer Filter {
	FILTER_TYPE values[FilterLength];
};

f32vec2 complex_mul(f32vec2 a, f32vec2 b)
{
	mat2 m = mat2(b.x, b.y, -b.y, b.x);
	f32vec2 result = m * a;
	return result;
}

#if Demodulate
SAMPLE_TYPE rotate_iq(SAMPLE_TYPE iq, uint index)
{
	float arg          = radians(360) * DemodulationFrequency * index / SamplingFrequency;
	SAMPLE_TYPE result = SAMPLE_TYPE(complex_mul(iq, f32vec2(cos(arg), -sin(arg))));
	return result;
}
#endif

shared SAMPLE_TYPE rf[DecimationRate * gl_WorkGroupSize.x + FilterLength - 1];

void main()
{
	uint out_sample = gl_GlobalInvocationID.x;
	uint channel    = gl_GlobalInvocationID.y;
	uint transmit   = gl_GlobalInvocationID.z;

	uint thread_index = gl_LocalInvocationIndex;
	uint thread_count = gl_WorkGroupSize.x * gl_WorkGroupSize.y * gl_WorkGroupSize.z;
	/////////////////////////
	// NOTE: sample caching
	{
		bool offset_wraps = (DecimationRate * gl_WorkGroupID.x * gl_WorkGroupSize.x) < (FilterLength - 1);

		u32 in_offset = InputDataKindByteSize * (InputChannelStride * channel + InputTransmitStride * transmit);
		// NOTE(rnp): when demodulating we want to load 2 elements at a time but the
		// input strides were specified in terms of a single element. therefore we
		// must divide this by two. by doing this here we can gracefully handle
		// the case where there are an odd number of samples (this drops the last one).
		if (Demodulate != 0)
			in_offset /= 2;

		// NOTE(rnp): broken out to avoid overflow from the subtraction
		u64 input_address = input_data + in_offset;
		input_address += InputDataKindByteSize * (DecimationRate * gl_WorkGroupID.x * gl_WorkGroupSize.x);
		input_address -= InputDataKindByteSize * (FilterLength - 1);

		uint total_samples       = rf.length();
		uint samples_per_thread  = total_samples / thread_count;
		uint leftover_count      = total_samples % thread_count;
		uint samples_this_thread = samples_per_thread + uint(thread_index < leftover_count);

		const SAMPLE_TYPE scale = SAMPLE_TYPE(bool(ComplexFilter) ? 1 : sqrt(2.0f));
		for (uint i = 0; i < samples_this_thread; i++) {
			uint index = thread_count * i + thread_index;
			SAMPLE_TYPE s = SAMPLE_TYPE(0);
			if (!offset_wraps || index >= FilterLength - 1) {
				s = SAMPLE_TYPE(Input(input_address).x[index]);
				#if Demodulate
				s = scale * rotate_iq(s * SAMPLE_TYPE(1, -1), index);
				#endif
			}
			rf[index] = s;
		}
	}
	barrier();

	if (out_sample < SampleCount / DecimationRate) {
		RESULT_TYPE result = RESULT_TYPE(0);
		uint offset = DecimationRate * thread_index;
		for (uint j = 0; j < FilterLength; j++)
			result += apply_filter(rf[offset + j], Filter(filter_coefficients).values[j]);

		u32 out_offset = OutputChannelStride  * channel +
		                 OutputTransmitStride * transmit +
		                 OutputSampleStride   * out_sample +
		                 output_element_offset;

		#if BatchSampleCount
		// NOTE(rnp): deinterleave
		output_data[out_offset] = OutputDataType(result.x);
		out_offset += BatchSampleCount;
		output_data[out_offset] = OutputDataType(result.y);
		#else
		output_data[out_offset] = OutputDataType(result);
		#endif
	}
}
