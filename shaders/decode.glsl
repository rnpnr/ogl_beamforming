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
	#define RF_SAMPLES_PER_INDEX 2
	#define RESULT_TYPE_CAST(x)  (x)
	#define SAMPLE_DATA_TYPE     vec4
	/* NOTE(rnp): for i16 rf_data we decode 2 samples at once */
	#define SAMPLE_TYPE_CAST(x)  vec4(((x) << 16) >> 16, 0, (x) >> 16, 0)
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

	uint rf_offset = (channel * rf_raw_dim.x + time_sample * dec_data_dim.z) / RF_SAMPLES_PER_INDEX;
	if (u_first_pass) {
		int  rf_channel = imageLoad(channel_mapping, int(channel)).x;
		uint in_off     = rf_channel * rf_raw_dim.x + transmit * dec_data_dim.x + time_sample;
		out_rf_data[rf_offset + transmit] = rf_data[in_off / RF_SAMPLES_PER_INDEX];
	} else {
		/* NOTE(rnp): stores output as a 3D matrix with ordering of {samples, channels, transmits} */
		uint out_off = dec_data_dim.x * dec_data_dim.z * channel + dec_data_dim.x * transmit + time_sample;

		vec4 result = vec4(0);
		switch (decode) {
		case DECODE_MODE_NONE: {
			result = RESULT_TYPE_CAST(sample_rf_data(rf_offset + transmit));
		} break;
		case DECODE_MODE_HADAMARD: {
			SAMPLE_DATA_TYPE sum = SAMPLE_DATA_TYPE(0);
			for (int i = 0; i < dec_data_dim.z; i++)
				sum += imageLoad(hadamard, ivec2(i, transmit)).x * sample_rf_data(rf_offset++);
			result = RESULT_TYPE_CAST(sum) / float(dec_data_dim.z);
		} break;
		}
		out_data[out_off + 0] = result.xy;
		#if RF_SAMPLES_PER_INDEX == 2
		out_data[out_off + 1] = result.zw;
		#endif
	}
}
