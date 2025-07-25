/* See LICENSE for license details. */

/* NOTE(rnp): invoked with samples x channels x transmits
 * Each instance extracts a single time sample from a single channel for all transmits
 * and does a dot product with the appropriate row of the bound hadamard matrix
 * (unless decode_mode == DECODE_MODE_NONE). The result of this dot product is stored in the
 * output. In bulk this has the effect of computing a matrix multiply of the
 * sample-transmit plane with the bound hadamard matrix.
 */

#if   defined(INPUT_DATA_TYPE_FLOAT)
	#define INPUT_DATA_TYPE      float
	#define RF_SAMPLES_PER_INDEX 1
	#define RESULT_TYPE_CAST(x)  vec4((x), 0, 0, 0)
	#define SAMPLE_DATA_TYPE     float
	#define SAMPLE_TYPE_CAST(x)  (x)
#elif defined(INPUT_DATA_TYPE_FLOAT_COMPLEX)
	#define INPUT_DATA_TYPE      vec2
	#define RF_SAMPLES_PER_INDEX 1
	#define RESULT_TYPE_CAST(x)  vec4((x), 0, 0)
	#define SAMPLE_DATA_TYPE     vec2
	#define SAMPLE_TYPE_CAST(x)  (x)
#elif defined(INPUT_DATA_TYPE_INT16_COMPLEX)
	#define INPUT_DATA_TYPE      int
	#define RF_SAMPLES_PER_INDEX 1
	#define RESULT_TYPE_CAST(x)  vec4((x), 0, 0)
	#define SAMPLE_DATA_TYPE     vec2
	#define SAMPLE_TYPE_CAST(x)  vec2(((x) << 16) >> 16, (x) >> 16)
#else
	#define INPUT_DATA_TYPE      int
	#define RESULT_TYPE_CAST(x)  (x)
	/* NOTE(rnp): for i16 rf_data we decode 2 samples at once */
	#define RF_SAMPLES_PER_INDEX 2
	#define SAMPLE_DATA_TYPE     vec4
	#if defined(OUTPUT_DATA_TYPE_FLOAT)
		#define SAMPLE_TYPE_CAST(x)  vec4(((x) << 16) >> 16, (x) >> 16, 0, 0)
	#else
		#define SAMPLE_TYPE_CAST(x)  vec4(((x) << 16) >> 16, 0, (x) >> 16, 0)
	#endif
#endif

layout(std430, binding = 1) readonly restrict buffer buffer_1 {
	INPUT_DATA_TYPE rf_data[];
};

layout(std430, binding = 2) writeonly restrict buffer buffer_2 {
	INPUT_DATA_TYPE out_rf_data[];
};

layout(std430, binding = 3) writeonly restrict buffer buffer_3 {
	vec2 out_data[];
};

layout(r8i,  binding = 0) readonly restrict uniform iimage2D hadamard;
layout(r16i, binding = 1) readonly restrict uniform iimage1D channel_mapping;

SAMPLE_DATA_TYPE sample_rf_data(uint index)
{
	SAMPLE_DATA_TYPE result = SAMPLE_TYPE_CAST(rf_data[index]);
	return result;
}

void main()
{
	uint time_sample = gl_GlobalInvocationID.x * RF_SAMPLES_PER_INDEX;
	uint channel     = gl_GlobalInvocationID.y;
	uint transmit    = gl_GlobalInvocationID.z;

	uint rf_offset = (input_channel_stride * channel + transmit_count * time_sample) / RF_SAMPLES_PER_INDEX;
	if (u_first_pass) {
		if (time_sample < input_transmit_stride) {
			uint in_off = input_channel_stride  * imageLoad(channel_mapping, int(channel)).x +
			              input_transmit_stride * transmit +
			              input_sample_stride   * time_sample;
			out_rf_data[rf_offset + transmit] = rf_data[in_off / RF_SAMPLES_PER_INDEX];
		}
	} else {
		#if defined(OUTPUT_DATA_TYPE_FLOAT)
		/* NOTE(rnp): when outputting floats do not dilate the out time sample;
		 * output should end up densely packed */
		time_sample = gl_GlobalInvocationID.x;
		#endif
		if (time_sample < output_transmit_stride) {
			uint out_off = output_channel_stride  * channel +
			               output_transmit_stride * transmit +
			               output_sample_stride   * time_sample;

			vec4 result = vec4(0);
			switch (decode_mode) {
			case DECODE_MODE_NONE: {
				result = RESULT_TYPE_CAST(sample_rf_data(rf_offset + transmit));
			} break;
			case DECODE_MODE_HADAMARD: {
				SAMPLE_DATA_TYPE sum = SAMPLE_DATA_TYPE(0);
				for (int i = 0; i < imageSize(hadamard).x; i++)
					sum += imageLoad(hadamard, ivec2(i, transmit)).x * sample_rf_data(rf_offset++);
				result = RESULT_TYPE_CAST(sum) / float(imageSize(hadamard).x);
			} break;
			}
			out_data[out_off + 0] = result.xy;
			#if RF_SAMPLES_PER_INDEX == 2 && !defined(OUTPUT_DATA_TYPE_FLOAT)
			out_data[out_off + 1] = result.zw;
			#endif
		}
	}
}
