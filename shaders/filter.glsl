/* See LICENSE for license details. */
/* TODO(rnp): bug: this won't filter RF data correctly */
#define SAMPLE_TYPE vec2
#if DataKind == DataKind_Float32
  #define DATA_TYPE           vec2
  #define RESULT_TYPE_CAST(v) (v)
  #define SAMPLE_TYPE_CAST(v) (v)
#else
  #define DATA_TYPE           uint
  #define SAMPLE_TYPE_CAST(v) unpackSnorm2x16(v)
  #if OutputFloats
    #define OUT_DATA_TYPE       vec2
    #define RESULT_TYPE_CAST(v) (clamp((v), -1.0, 1.0) * 32767.0f)
  #else
    #define RESULT_TYPE_CAST(v) packSnorm2x16(v)
  #endif
#endif

#ifndef OUT_DATA_TYPE
  #define OUT_DATA_TYPE DATA_TYPE
#endif

#if ComplexFilter
	#define FILTER_TYPE         vec2
	#define apply_filter(iq, h) complex_mul((iq), (h))
#else
	#define FILTER_TYPE         float
	#define apply_filter(iq, h) ((iq) * (h))
#endif

layout(std430, buffer_reference, buffer_reference_align = 64) restrict readonly buffer Input {
	DATA_TYPE values[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict writeonly buffer Output {
	OUT_DATA_TYPE values[];
};

layout(std430, buffer_reference, buffer_reference_align = 64) restrict readonly buffer Filter {
	FILTER_TYPE values[FilterLength];
};

vec2 complex_mul(vec2 a, vec2 b)
{
	mat2 m = mat2(b.x, b.y, -b.y, b.x);
	vec2 result = m * a;
	return result;
}

#if Demodulate
vec2 rotate_iq(vec2 iq, uint index)
{
	float arg    = radians(360) * DemodulationFrequency * index / SamplingFrequency;
	vec2  result = complex_mul(iq, vec2(cos(arg), -sin(arg)));
	return result;
}
#endif

SAMPLE_TYPE sample_rf(uint index)
{
	SAMPLE_TYPE result = SAMPLE_TYPE_CAST(Input(input_data).values[index]);
	return result;
}

shared SAMPLE_TYPE rf[DecimationRate * gl_WorkGroupSize.x + FilterLength - 1];

void main()
{
	uint out_sample = gl_GlobalInvocationID.x;
	uint channel    = gl_GlobalInvocationID.y;
	uint transmit   = gl_GlobalInvocationID.z;

	uint in_offset  = InputChannelStride * channel + InputTransmitStride * transmit;
	uint out_offset = OutputChannelStride  * channel +
	                  OutputTransmitStride * transmit +
	                  OutputSampleStride   * out_sample;

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

		const float scale = bool(ComplexFilter) ? 1 : sqrt(2.0f);
		for (uint i = 0; i < samples_this_thread; i++) {
			uint index = thread_count * i + thread_index;
			if (offset_wraps && index < FilterLength - 1) {
				rf[index] = SAMPLE_TYPE(0);
			} else {
				#if Demodulate
					rf[index] = scale * rotate_iq(sample_rf(in_offset + index) * vec2(1, -1), index);
				#else
					rf[index] = sample_rf(in_offset + index);
				#endif
			}
		}
	}
	barrier();

	if (out_sample < SampleCount / DecimationRate) {
		SAMPLE_TYPE result = SAMPLE_TYPE(0);
		uint offset = DecimationRate * thread_index;
		for (uint j = 0; j < FilterLength; j++)
			result += apply_filter(rf[offset + j], Filter(filter_coefficients).values[j]);
		Output(output_data).values[out_offset] = RESULT_TYPE_CAST(result);
	}
}
