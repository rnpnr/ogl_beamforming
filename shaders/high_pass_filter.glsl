/* See LICENSE for license details. */
layout(local_size_x = 16, local_size_y = 1, local_size_z = 16) in;

#if   DataKind == DataKind_Float32
  #define SAMPLE_TYPE           float
  #define TEXTURE_KIND          r32f
  #define RESULT_TYPE_CAST(a)   (a).x
  #define OUTPUT_TYPE_CAST(a)   vec4((a).x, 0, 0, 0)
#elif DataKind == DataKind_Float32Complex
  #define SAMPLE_TYPE           vec2
  #define TEXTURE_KIND          rg32f
  #define RESULT_TYPE_CAST(a)   (a).xy
  #define OUTPUT_TYPE_CAST(a)   vec4((a).xy, 0, 0)
#else
  #error DataKind unsupported for HighPassFilter
#endif

layout(TEXTURE_KIND, binding = 0) writeonly restrict uniform image3D u_out_img;

/* NOTE: 20 input textures for the circular buffer frames */
layout(TEXTURE_KIND, binding = 1)  readonly restrict uniform image3D u_in_img_0;
layout(TEXTURE_KIND, binding = 2)  readonly restrict uniform image3D u_in_img_1;
layout(TEXTURE_KIND, binding = 3)  readonly restrict uniform image3D u_in_img_2;
layout(TEXTURE_KIND, binding = 4)  readonly restrict uniform image3D u_in_img_3;
layout(TEXTURE_KIND, binding = 5)  readonly restrict uniform image3D u_in_img_4;
layout(TEXTURE_KIND, binding = 6)  readonly restrict uniform image3D u_in_img_5;
layout(TEXTURE_KIND, binding = 7)  readonly restrict uniform image3D u_in_img_6;
layout(TEXTURE_KIND, binding = 8)  readonly restrict uniform image3D u_in_img_7;
layout(TEXTURE_KIND, binding = 9)  readonly restrict uniform image3D u_in_img_8;
layout(TEXTURE_KIND, binding = 10) readonly restrict uniform image3D u_in_img_9;
layout(TEXTURE_KIND, binding = 11) readonly restrict uniform image3D u_in_img_10;
layout(TEXTURE_KIND, binding = 12) readonly restrict uniform image3D u_in_img_11;
layout(TEXTURE_KIND, binding = 13) readonly restrict uniform image3D u_in_img_12;
layout(TEXTURE_KIND, binding = 14) readonly restrict uniform image3D u_in_img_13;
layout(TEXTURE_KIND, binding = 15) readonly restrict uniform image3D u_in_img_14;
layout(TEXTURE_KIND, binding = 16) readonly restrict uniform image3D u_in_img_15;
layout(TEXTURE_KIND, binding = 17) readonly restrict uniform image3D u_in_img_16;
layout(TEXTURE_KIND, binding = 18) readonly restrict uniform image3D u_in_img_17;
layout(TEXTURE_KIND, binding = 19) readonly restrict uniform image3D u_in_img_18;
layout(TEXTURE_KIND, binding = 20) readonly restrict uniform image3D u_in_img_19;

/* NOTE: FIR high pass filter coefficients for 20-tap filter
 * This implements a simple high pass: output = current - mean(previous frames)
 * For a more sophisticated filter, coefficients can be designed using
 * standard FIR design methods (e.g., windowed sinc, Parks-McClellan)
 */
#define FILTER_LENGTH 20

SAMPLE_TYPE sample_frame(int frame_index, ivec3 voxel)
{
	switch (frame_index) {
	case 0:  return RESULT_TYPE_CAST(imageLoad(u_in_img_0,  voxel));
	case 1:  return RESULT_TYPE_CAST(imageLoad(u_in_img_1,  voxel));
	case 2:  return RESULT_TYPE_CAST(imageLoad(u_in_img_2,  voxel));
	case 3:  return RESULT_TYPE_CAST(imageLoad(u_in_img_3,  voxel));
	case 4:  return RESULT_TYPE_CAST(imageLoad(u_in_img_4,  voxel));
	case 5:  return RESULT_TYPE_CAST(imageLoad(u_in_img_5,  voxel));
	case 6:  return RESULT_TYPE_CAST(imageLoad(u_in_img_6,  voxel));
	case 7:  return RESULT_TYPE_CAST(imageLoad(u_in_img_7,  voxel));
	case 8:  return RESULT_TYPE_CAST(imageLoad(u_in_img_8,  voxel));
	case 9:  return RESULT_TYPE_CAST(imageLoad(u_in_img_9,  voxel));
	case 10: return RESULT_TYPE_CAST(imageLoad(u_in_img_10, voxel));
	case 11: return RESULT_TYPE_CAST(imageLoad(u_in_img_11, voxel));
	case 12: return RESULT_TYPE_CAST(imageLoad(u_in_img_12, voxel));
	case 13: return RESULT_TYPE_CAST(imageLoad(u_in_img_13, voxel));
	case 14: return RESULT_TYPE_CAST(imageLoad(u_in_img_14, voxel));
	case 15: return RESULT_TYPE_CAST(imageLoad(u_in_img_15, voxel));
	case 16: return RESULT_TYPE_CAST(imageLoad(u_in_img_16, voxel));
	case 17: return RESULT_TYPE_CAST(imageLoad(u_in_img_17, voxel));
	case 18: return RESULT_TYPE_CAST(imageLoad(u_in_img_18, voxel));
	case 19: return RESULT_TYPE_CAST(imageLoad(u_in_img_19, voxel));
	default: return SAMPLE_TYPE(0);
	}
}

void main()
{
	ivec3 voxel = ivec3(gl_GlobalInvocationID);
	if (!all(lessThan(voxel, imageSize(u_out_img))))
		return;

	/* NOTE: High pass filter implementation
	 * Simple approach: output = current_frame - mean(all_frames)
	 * This removes the DC/low-frequency component
	 * 
	 * The frames are bound in temporal order (oldest to newest):
	 * - binding 1 = oldest frame
	 * - binding 20 = newest frame (current frame)
	 */
	SAMPLE_TYPE sum = SAMPLE_TYPE(0);
	for (int i = 0; i < FILTER_LENGTH; i++) {
		sum += sample_frame(i, voxel);
	}
	SAMPLE_TYPE mean = sum / float(FILTER_LENGTH);

	/* NOTE: The most recent frame is at index 19 (binding 20, last in buffer)
	 * High pass: subtract mean from current frame
	 */
	SAMPLE_TYPE current = sample_frame(19, voxel);
	SAMPLE_TYPE result = current - mean;

	imageStore(u_out_img, voxel, OUTPUT_TYPE_CAST(result));
}

