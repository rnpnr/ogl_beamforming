/* See LICENSE for license details. */
#if   DataKind == DataKind_Float32Complex || (DataKind == DataKind_Float32 && Demodulate)
  #define INPUT_TYPE  f32vec2
  #define SAMPLE_TYPE f32vec2
  #if BatchSampleCount
    #define OUTPUT_TYPE f32
  #else
    #define OUTPUT_TYPE f32vec2
  #endif
#elif DataKind == DataKind_Float32
  #define INPUT_TYPE  f32
  #define SAMPLE_TYPE f32
  #define OUTPUT_TYPE f32
#elif DataKind == DataKind_Float16Complex || (DataKind == DataKind_Float16 && Demodulate)
  #define INPUT_TYPE  f16vec2
  #define SAMPLE_TYPE f16vec2
  #if OutputFloats
    #if BatchSampleCount
      #define OUTPUT_TYPE f32
    #else
      #define OUTPUT_TYPE f32vec2
    #endif
  #else
    #if BatchSampleCount
      #define OUTPUT_TYPE f16
    #else
      #define OUTPUT_TYPE f16vec2
    #endif
  #endif
#elif DataKind == DataKind_Float16
  #define INPUT_TYPE  f16
  #define SAMPLE_TYPE f16
  #define OUTPUT_TYPE f16
#elif DataKind == DataKind_Int16Complex   || (DataKind == DataKind_Int16   && Demodulate)
  #define INPUT_TYPE  s16vec2
  #define SAMPLE_TYPE f16vec2
  #if OutputFloats
    #if BatchSampleCount
      #define OUTPUT_TYPE f32
    #else
      #define OUTPUT_TYPE f32vec2
    #endif
  #else
    #if BatchSampleCount
      #define OUTPUT_TYPE f16
    #else
      #define OUTPUT_TYPE f16vec2
    #endif
  #endif
#elif DataKind == DataKind_Int16
  #define INPUT_TYPE  s16
  #define SAMPLE_TYPE f16
  #define OUTPUT_TYPE f16
#else
  #error unsupported data kind
#endif

#define ComplexSampleType (DataKind == DataKind_Float32Complex || \
                           DataKind == DataKind_Float16Complex || \
                           DataKind == DataKind_Int16Complex || \
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
	INPUT_TYPE x[];
};

layout(set = ShaderResourceKind_Buffer, binding = ShaderBufferSlot_PingPong) buffer Output {
	OUTPUT_TYPE output_data[];
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

SAMPLE_TYPE sample_rf(uint index)
{
	SAMPLE_TYPE result = SAMPLE_TYPE(Input(input_data).x[index]);
	return result;
}

shared SAMPLE_TYPE rf[DecimationRate * gl_WorkGroupSize.x + FilterLength - 1];

void main()
{
	uint out_sample = gl_GlobalInvocationID.x;
	uint channel    = gl_GlobalInvocationID.y;
	uint transmit   = gl_GlobalInvocationID.z;

	uint in_offset    = InputChannelStride * channel + InputTransmitStride * transmit;
	uint thread_index = gl_LocalInvocationIndex;
	uint thread_count = gl_WorkGroupSize.x * gl_WorkGroupSize.y * gl_WorkGroupSize.z;
	/////////////////////////
	// NOTE: sample caching
	{
		bool offset_wraps = (DecimationRate * gl_WorkGroupID.x * gl_WorkGroupSize.x) < (FilterLength - 1);

		in_offset += DecimationRate * gl_WorkGroupID.x * gl_WorkGroupSize.x - (FilterLength - 1);

		uint total_samples       = rf.length();
		uint samples_per_thread  = total_samples / thread_count;
		uint leftover_count      = total_samples % thread_count;
		uint samples_this_thread = samples_per_thread + uint(thread_index < leftover_count);

		const SAMPLE_TYPE scale = SAMPLE_TYPE(bool(ComplexFilter) ? 1 : sqrt(2.0f));
		for (uint i = 0; i < samples_this_thread; i++) {
			uint index = thread_count * i + thread_index;
			if (offset_wraps && index < FilterLength - 1) {
				rf[index] = SAMPLE_TYPE(0);
			} else {
				#if Demodulate
					rf[index] = scale * rotate_iq(sample_rf(in_offset + index) * SAMPLE_TYPE(1, -1), index);
				#else
					rf[index] = sample_rf(in_offset + index);
				#endif
			}
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
		                 OutputSampleStride   * out_sample;

		#if BatchSampleCount
		// NOTE(rnp): deinterleave
		output_data[output_element_offset + out_offset] = OUTPUT_TYPE(result.x);
		out_offset += BatchSampleCount;
		output_data[output_element_offset + out_offset] = OUTPUT_TYPE(result.y);
		#else
		output_data[output_element_offset + out_offset] = OUTPUT_TYPE(result);
		#endif
	}
}
