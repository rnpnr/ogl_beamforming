/* See LICENSE for license details. */

/* NOTE(rnp): invoked with samples x channels x transmits
 * Each instance extracts a single time sample from a single channel for all transmits
 * and does a dot product with the appropriate row of the bound hadamard matrix
 * (unless decode_mode == DECODE_MODE_NONE). The result of this dot product is stored in the
 * output. In bulk this has the effect of computing a matrix multiply of the
 * sample-transmit plane with the bound hadamard matrix.
 */

#if   DataKind == DataKind_Float32
	#define INPUT_DATA_TYPE      float
	#define SAMPLE_DATA_TYPE     float
	#define SAMPLE_TYPE_CAST(x)  (x)
#elif DataKind == DataKind_Float32Complex
	#define INPUT_DATA_TYPE      vec2
	#define SAMPLE_DATA_TYPE     vec2
	#define SAMPLE_TYPE_CAST(x)  (x)
#elif DataKind == DataKind_Int16Complex
	#define INPUT_DATA_TYPE      int
	#define SAMPLE_DATA_TYPE     vec2
	#define SAMPLE_TYPE_CAST(x)  vec2(((x) << 16) >> 16, (x) >> 16)
#elif DataKind == DataKind_Int16
	#define INPUT_DATA_TYPE      int
	#define RF_SAMPLES_PER_INDEX 2
	#if (ShaderFlags & ShaderFlags_DilateOutput)
		#define SAMPLE_DATA_TYPE    vec4
		#define SAMPLE_TYPE_CAST(x) vec4(((x) << 16) >> 16, 0, (x) >> 16, 0)
	#else
		#define SAMPLE_DATA_TYPE    vec2
		#define SAMPLE_TYPE_CAST(x) vec2(((x) << 16) >> 16, (x) >> 16)
		#define OUTPUT_SAMPLES_PER_INDEX 2
	#endif
#else
	#error unsupported data kind for Decode
#endif

#ifndef OUTPUT_SAMPLES_PER_INDEX
	#define OUTPUT_SAMPLES_PER_INDEX 1
#endif

#ifndef RF_SAMPLES_PER_INDEX
	#define RF_SAMPLES_PER_INDEX 1
#endif

layout(std430, binding = 1) readonly restrict buffer buffer_1 {
	INPUT_DATA_TYPE rf_data[];
};

layout(std430, binding = 2) writeonly restrict buffer buffer_2 {
	INPUT_DATA_TYPE out_rf_data[];
};

layout(std430, binding = 3) writeonly restrict buffer buffer_3 {
	SAMPLE_DATA_TYPE out_data[];
};

layout(r32f, binding = 0) readonly restrict uniform image2D  hadamard;
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

	uint rf_offset = (InputChannelStride * channel + TransmitCount * time_sample) / RF_SAMPLES_PER_INDEX;
	if (u_first_pass) {
		if (time_sample < InputTransmitStride) {
			uint in_off = InputChannelStride  * imageLoad(channel_mapping, int(channel)).x +
			              InputTransmitStride * transmit +
			              InputSampleStride   * time_sample;
			out_rf_data[rf_offset + transmit] = rf_data[in_off / RF_SAMPLES_PER_INDEX];
		}
	} else {
		if (time_sample < OutputTransmitStride) {
			uint out_off = OutputChannelStride  * channel +
			               OutputTransmitStride * transmit +
			               OutputSampleStride   * time_sample;

			SAMPLE_DATA_TYPE result = SAMPLE_DATA_TYPE(0);
			switch (DecodeMode) {
			case DecodeMode_None:{
				result = sample_rf_data(rf_offset + transmit);
			}break;
			case DecodeMode_Hadamard:{
				SAMPLE_DATA_TYPE sum = SAMPLE_DATA_TYPE(0);
				for (int i = 0; i < TransmitCount; i++)
					sum += imageLoad(hadamard, ivec2(i, transmit)).x * sample_rf_data(rf_offset++);
				result = sum / float(TransmitCount);
			}break;
			}
			out_data[out_off / OUTPUT_SAMPLES_PER_INDEX] = result;
		}
	}
}
