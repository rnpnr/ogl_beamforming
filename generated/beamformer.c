/* See LICENSE for license details. */

// GENERATED CODE

// NOTE: Constants (Integer)
#define BeamformerChunkChannelCount        (16)
#define BeamformerFilterSlots              (4)
#define BeamformerMaxBacklogFrames         (4096)
#define BeamformerMaxChannelCount          (256)
#define BeamformerMaxEmissionsCount        (256)
#define BeamformerMaxComputeShaderStages   (16)
#define BeamformerMaxParameterBlocks       (16)
#define BeamformerMaxRawDataFramesInFlight (3)
#define BeamformerMaxHadamardElements      (65536)

typedef enum {
	BeamformerShaderResourceKind_Buffer = 0,
	BeamformerShaderResourceKind_Count,
} BeamformerShaderResourceKind;

typedef enum {
	BeamformerShaderBufferSlot_BeamformedData = 0,
	BeamformerShaderBufferSlot_PingPong       = 1,
	BeamformerShaderBufferSlot_Count,
} BeamformerShaderBufferSlot;

typedef enum {
	BeamformerDecodeMode_None     = 0,
	BeamformerDecodeMode_Hadamard = 1,
	BeamformerDecodeMode_Count,
} BeamformerDecodeMode;

typedef enum {
	BeamformerRCAOrientation_None    = 0,
	BeamformerRCAOrientation_Rows    = 1,
	BeamformerRCAOrientation_Columns = 2,
	BeamformerRCAOrientation_Count,
} BeamformerRCAOrientation;

typedef enum {
	BeamformerSamplingMode_2X = 0,
	BeamformerSamplingMode_4X = 1,
	BeamformerSamplingMode_Count,
} BeamformerSamplingMode;

typedef enum {
	BeamformerDataKind_Int16          = 0,
	BeamformerDataKind_Int16Complex   = 1,
	BeamformerDataKind_Float32        = 2,
	BeamformerDataKind_Float32Complex = 3,
	BeamformerDataKind_Float16        = 4,
	BeamformerDataKind_Float16Complex = 5,
	BeamformerDataKind_Count,
} BeamformerDataKind;

typedef enum {
	BeamformerContrastMode_None = 0,
	BeamformerContrastMode_A1S2 = 1,
	BeamformerContrastMode_Count,
} BeamformerContrastMode;

typedef enum {
	BeamformerEmissionKind_Sine  = 0,
	BeamformerEmissionKind_Chirp = 1,
	BeamformerEmissionKind_Count,
} BeamformerEmissionKind;

typedef enum {
	BeamformerInterpolationMode_Nearest = 0,
	BeamformerInterpolationMode_Linear  = 1,
	BeamformerInterpolationMode_Cubic   = 2,
	BeamformerInterpolationMode_Count,
} BeamformerInterpolationMode;

typedef enum {
	BeamformerViewPlaneTag_XZ        = 0,
	BeamformerViewPlaneTag_YZ        = 1,
	BeamformerViewPlaneTag_XY        = 2,
	BeamformerViewPlaneTag_Arbitrary = 3,
	BeamformerViewPlaneTag_Count,
} BeamformerViewPlaneTag;

typedef enum {
	BeamformerAcquisitionKind_FORCES         = 0,
	BeamformerAcquisitionKind_UFORCES        = 1,
	BeamformerAcquisitionKind_HERCULES       = 2,
	BeamformerAcquisitionKind_RCA_VLS        = 3,
	BeamformerAcquisitionKind_RCA_TPW        = 4,
	BeamformerAcquisitionKind_UHERCULES      = 5,
	BeamformerAcquisitionKind_RACES          = 6,
	BeamformerAcquisitionKind_EPIC_FORCES    = 7,
	BeamformerAcquisitionKind_EPIC_UFORCES   = 8,
	BeamformerAcquisitionKind_EPIC_UHERCULES = 9,
	BeamformerAcquisitionKind_Flash          = 10,
	BeamformerAcquisitionKind_HERO_PA        = 11,
	BeamformerAcquisitionKind_ULM            = 12,
	BeamformerAcquisitionKind_Count,
} BeamformerAcquisitionKind;

typedef enum {
	BeamformerFilterKind_Kaiser       = 0,
	BeamformerFilterKind_MatchedChirp = 1,
	BeamformerFilterKind_Count,
} BeamformerFilterKind;

typedef enum {
	BeamformerLiveFeedbackFlags_ImagePlaneOffsets = 0,
	BeamformerLiveFeedbackFlags_TransmitPower     = 1,
	BeamformerLiveFeedbackFlags_TGCControlPoints  = 2,
	BeamformerLiveFeedbackFlags_SaveData          = 3,
	BeamformerLiveFeedbackFlags_SaveNameTag       = 4,
	BeamformerLiveFeedbackFlags_StopImaging       = 5,
	BeamformerLiveFeedbackFlags_AcquisitionKind   = 6,
	BeamformerLiveFeedbackFlags_Count,
} BeamformerLiveFeedbackFlags;

typedef enum {
	BeamformerComputeArrayParametersField_FocalVectors                = 0,
	BeamformerComputeArrayParametersField_SparseElements              = 1,
	BeamformerComputeArrayParametersField_TransmitReceiveOrientations = 2,
	BeamformerComputeArrayParametersField_DASHadamard                 = 3,
	BeamformerComputeArrayParametersField_DecodeHadamard              = 4,
	BeamformerComputeArrayParametersField_Count,
} BeamformerComputeArrayParametersField;

typedef enum {
	BeamformerLiveImagingDirtyFlags_ImagePlaneOffsets = 1 << 0,
	BeamformerLiveImagingDirtyFlags_TransmitPower     = 1 << 1,
	BeamformerLiveImagingDirtyFlags_TGCControlPoints  = 1 << 2,
	BeamformerLiveImagingDirtyFlags_SaveData          = 1 << 3,
	BeamformerLiveImagingDirtyFlags_SaveNameTag       = 1 << 4,
	BeamformerLiveImagingDirtyFlags_StopImaging       = 1 << 5,
	BeamformerLiveImagingDirtyFlags_AcquisitionKind   = 1 << 6,
} BeamformerLiveImagingDirtyFlags;

typedef enum {
	BeamformerDecodeCompileFlags_CooperativeMatrix = 1 << 0,
	BeamformerDecodeCompileFlags_UseSharedMemory   = 1 << 1,
} BeamformerDecodeCompileFlags;

typedef enum {
	BeamformerFilterCompileFlags_ComplexFilter = 1 << 0,
	BeamformerFilterCompileFlags_Demodulate    = 1 << 1,
} BeamformerFilterCompileFlags;

typedef enum {
	BeamformerDASCompileFlags_CoherencyWeighting = 1 << 0,
} BeamformerDASCompileFlags;

typedef enum {
	BeamformerReshapeCompileFlags_Deinterleave = 1 << 0,
	BeamformerReshapeCompileFlags_Interleave   = 1 << 1,
} BeamformerReshapeCompileFlags;

typedef enum {
	BeamformerShaderKind_Decode             = 0,
	BeamformerShaderKind_Filter             = 1,
	BeamformerShaderKind_Demodulate         = 2,
	BeamformerShaderKind_DAS                = 3,
	BeamformerShaderKind_Sum                = 4,
	BeamformerShaderKind_MinMax             = 5,
	BeamformerShaderKind_Hilbert            = 6,
	BeamformerShaderKind_CoherencyWeighting = 7,
	BeamformerShaderKind_Reshape            = 8,
	BeamformerShaderKind_RenderBeamformed   = 9,
	BeamformerShaderKind_Count,

	BeamformerShaderKind_ComputeFirst        = BeamformerShaderKind_Decode,
	BeamformerShaderKind_ComputeLast         = BeamformerShaderKind_Hilbert,
	BeamformerShaderKind_ComputeCount        = 7,
	BeamformerShaderKind_ComputeHelpersFirst = BeamformerShaderKind_CoherencyWeighting,
	BeamformerShaderKind_ComputeHelpersLast  = BeamformerShaderKind_Reshape,
	BeamformerShaderKind_ComputeHelpersCount = 2,
	BeamformerShaderKind_RenderFirst         = BeamformerShaderKind_RenderBeamformed,
	BeamformerShaderKind_RenderLast          = BeamformerShaderKind_RenderBeamformed,
	BeamformerShaderKind_RenderCount         = 1,
} BeamformerShaderKind;

typedef struct {
	u64 HadamardBuffer;
	u32 DecodeMode;
	u32 OutputChannelStride;
	u32 OutputSampleStride;
	u32 OutputTransmitStride;
	u32 ToProcess;
	u32 TransmitCount;
	u32 ChunkChannelCount;
	u32 CooperativeMatrixM;
	u32 CooperativeMatrixN;
	u32 CooperativeMatrixK;
} BeamformerDecodeBakeParameters;

typedef struct {
	u64 FilterCoefficients;
	u32 FilterLength;
	f32 SamplingFrequency;
	f32 DemodulationFrequency;
	u32 DecimationRate;
	u32 SampleCount;
	u32 BatchSampleCount;
	u32 InputChannelStride;
	u32 InputSampleStride;
	u32 InputTransmitStride;
	u32 OutputChannelStride;
	u32 OutputSampleStride;
	u32 OutputTransmitStride;
} BeamformerFilterBakeParameters;

typedef struct {
	u64 ArrayParameters;
	u32 AcquisitionKind;
	b32 Sparse;
	i32 AcquisitionCount;
	i32 ChannelCount;
	i32 ChunkChannelCount;
	i32 SampleCount;
	f32 SamplingFrequency;
	f32 DemodulationFrequency;
	f32 SpeedOfSound;
	f32 TimeOffset;
	u32 InterpolationMode;
	f32 FNumber;
	b32 SingleOrientation;
	u32 TransmitReceiveOrientation;
	b32 SingleFocus;
	f32 FocusDepth;
	f32 TransmitAngle;
	u32 ReadiGroupCount;
} BeamformerDASBakeParameters;

typedef struct {
	u32 SizeX;
	u32 SizeY;
	u32 SizeZ;
	u32 InputStrideX;
	u32 InputStrideY;
	u32 InputStrideZ;
	u32 OutputStrideX;
	u32 OutputStrideY;
	u32 OutputStrideZ;
} BeamformerReshapeBakeParameters;

typedef struct {
	u64 rf_buffer;
	u64 output_buffer;
} BeamformerDecodePushConstants;

typedef struct {
	u64 input_data;
	u32 output_element_offset;
} BeamformerFilterPushConstants;

typedef struct {
	m4  xdc_transform;
	m4  voxel_transform;
	v2  xdc_element_pitch;
	u64 output_frame;
	u64 incoherent_frame;
	u32 rf_element_offset;
	u32 output_size_x;
	u32 output_size_y;
	u32 output_size_z;
	i32 channel_offset;
	u32 readi_group;
} BeamformerDASPushConstants;

typedef struct {
	u64 output_data;
	u64 input_data;
	u32 image_elements;
	f32 scale;
} BeamformerSumPushConstants;

typedef struct {
	u64 left_side_buffer;
	u64 right_side_buffer;
	f32 scale;
	u32 output_size_x;
	u32 output_size_y;
	u32 output_size_z;
} BeamformerCoherencyWeightingPushConstants;

typedef struct {
	u64 output_buffer;
	u64 left_input_buffer;
	u64 right_input_buffer;
} BeamformerReshapePushConstants;

typedef struct {
	m4  mvp_matrix;
	u64 positions;
	u64 normals;
	v4  bounding_box_colour;
	f32 bounding_box_fraction;
	f32 db_cutoff;
	f32 threshold;
	f32 gamma;
	u64 input_data;
	u32 input_size_x;
	u32 input_size_y;
	u32 input_size_z;
	u32 data_kind;
} BeamformerRenderBeamformedPushConstants;

typedef struct {
	f32 cycles;
	f32 frequency;
} BeamformerSineParameters;

typedef struct {
	f32 duration;
	f32 min_frequency;
	f32 max_frequency;
} BeamformerChirpParameters;

typedef struct {
	BeamformerEmissionKind kind;
	union {
		BeamformerSineParameters  sine;
		BeamformerChirpParameters chirp;
	};
} BeamformerEmissionParameters;

typedef struct {
	f32 cutoff_frequency;
	f32 beta;
	u32 length;
} BeamformerKaiserFilterParameters;

typedef struct {
	f32 duration;
	f32 min_frequency;
	f32 max_frequency;
} BeamformerMatchedChirpFilterParameters;

typedef struct {
	BeamformerFilterKind kind;
	f32                  sampling_frequency;
	b32                  complex;
	union {
		BeamformerKaiserFilterParameters       kaiser;
		BeamformerMatchedChirpFilterParameters matched_chirp;
	};
} BeamformerFilterParameters;

typedef struct {
	m4                        das_voxel_transform;
	m4                        xdc_transform;
	v2                        xdc_element_pitch;
	uv2                       raw_data_dimensions;
	v2                        focal_vector;
	u32                       transmit_receive_orientation;
	u32                       sample_count;
	u32                       channel_count;
	u32                       acquisition_count;
	BeamformerAcquisitionKind acquisition_kind;
	BeamformerDecodeMode      decode_mode;
	BeamformerSamplingMode    sampling_mode;
	f32                       time_offset;
	b32                       single_focus;
	b32                       single_orientation;
} BeamformerParametersHead;

typedef struct {
	iv4                         output_points;
	f32                         sampling_frequency;
	f32                         demodulation_frequency;
	f32                         speed_of_sound;
	f32                         f_number;
	BeamformerInterpolationMode interpolation_mode;
	b32                         coherency_weighting;
	u32                         decimation_rate;
} BeamformerUIParameters;

typedef struct {
	BeamformerContrastMode       contrast_mode;
	BeamformerEmissionParameters emission_parameters;
	u32                          readi_group_count;
	u32                          readi_group;
} BeamformerExtraParameters;

typedef struct {
	m4                           das_voxel_transform;
	m4                           xdc_transform;
	v2                           xdc_element_pitch;
	uv2                          raw_data_dimensions;
	v2                           focal_vector;
	u32                          transmit_receive_orientation;
	u32                          sample_count;
	u32                          channel_count;
	u32                          acquisition_count;
	BeamformerAcquisitionKind    acquisition_kind;
	BeamformerDecodeMode         decode_mode;
	BeamformerSamplingMode       sampling_mode;
	f32                          time_offset;
	b32                          single_focus;
	b32                          single_orientation;
	iv4                          output_points;
	f32                          sampling_frequency;
	f32                          demodulation_frequency;
	f32                          speed_of_sound;
	f32                          f_number;
	BeamformerInterpolationMode  interpolation_mode;
	b32                          coherency_weighting;
	u32                          decimation_rate;
	BeamformerContrastMode       contrast_mode;
	BeamformerEmissionParameters emission_parameters;
	u32                          readi_group_count;
	u32                          readi_group;
} BeamformerParameters;

typedef struct {
	m4                           das_voxel_transform;
	m4                           xdc_transform;
	v2                           xdc_element_pitch;
	uv2                          raw_data_dimensions;
	v2                           focal_vector;
	u32                          transmit_receive_orientation;
	u32                          sample_count;
	u32                          channel_count;
	u32                          acquisition_count;
	BeamformerAcquisitionKind    acquisition_kind;
	BeamformerDecodeMode         decode_mode;
	BeamformerSamplingMode       sampling_mode;
	f32                          time_offset;
	b32                          single_focus;
	b32                          single_orientation;
	iv4                          output_points;
	f32                          sampling_frequency;
	f32                          demodulation_frequency;
	f32                          speed_of_sound;
	f32                          f_number;
	BeamformerInterpolationMode  interpolation_mode;
	b32                          coherency_weighting;
	u32                          decimation_rate;
	BeamformerContrastMode       contrast_mode;
	BeamformerEmissionParameters emission_parameters;
	u32                          readi_group_count;
	u32                          readi_group;
	i16                          channel_mapping[BeamformerMaxChannelCount];
	i16                          sparse_elements[BeamformerMaxEmissionsCount];
	u8                           transmit_receive_orientations[BeamformerMaxEmissionsCount];
	f32                          steering_angles[BeamformerMaxEmissionsCount];
	f32                          focal_depths[BeamformerMaxEmissionsCount];
	i32                          compute_stages[BeamformerMaxComputeShaderStages];
	i32                          compute_stage_parameters[BeamformerMaxComputeShaderStages];
	u32                          compute_stages_count;
	BeamformerDataKind           data_kind;
} BeamformerSimpleParameters;

typedef struct {
	u32 active;
	u32 save_enabled;
	u32 save_active;
	u32 acquisition_kind;
	u64 acquisition_kind_enabled_flags;
	f32 transmit_power;
	f32 image_plane_offsets[BeamformerViewPlaneTag_Count];
	f32 tgc_control_points[8];
	i32 save_name_tag_length;
	u8  save_name_tag[128];
} BeamformerLiveImagingParameters;

typedef struct {
	v2  focal_vectors[BeamformerMaxChannelCount];
	i16 sparse_elements[BeamformerMaxChannelCount];
	u16 transmit_receive_orientations[BeamformerMaxChannelCount];
	f16 das_hadamard[BeamformerMaxHadamardElements];
	f16 decode_hadamard[BeamformerMaxHadamardElements];
} BeamformerComputeArrayParameters;

typedef union {
	BeamformerDecodeBakeParameters  Decode;
	BeamformerFilterBakeParameters  Filter;
	BeamformerDASBakeParameters     DAS;
	BeamformerReshapeBakeParameters Reshape;
} BeamformerShaderBakeParameters;

read_only global u32 beamformer_compute_array_parameter_sizes[] = {
	sizeof(v2)  * BeamformerMaxChannelCount,
	sizeof(i16) * BeamformerMaxChannelCount,
	sizeof(u16) * BeamformerMaxChannelCount,
	sizeof(f16) * BeamformerMaxHadamardElements,
	sizeof(f16) * BeamformerMaxHadamardElements,
};

read_only global u32 beamformer_compute_array_parameter_offsets[] = {
	offsetof(BeamformerComputeArrayParameters, focal_vectors),
	offsetof(BeamformerComputeArrayParameters, sparse_elements),
	offsetof(BeamformerComputeArrayParameters, transmit_receive_orientations),
	offsetof(BeamformerComputeArrayParameters, das_hadamard),
	offsetof(BeamformerComputeArrayParameters, decode_hadamard),
};

read_only global u8 beamformer_data_kind_element_size[] = {
	2,
	2,
	4,
	4,
	2,
	2,
};

read_only global u8 beamformer_data_kind_element_count[] = {
	1,
	2,
	1,
	2,
	1,
	2,
};

read_only global u8 beamformer_data_kind_byte_size[] = {
	2 * 1,
	2 * 2,
	4 * 1,
	4 * 2,
	2 * 1,
	2 * 2,
};

read_only global b8 beamformer_data_kind_complex[] = {
	0,
	1,
	0,
	1,
	0,
	1,
};

read_only global str8 beamformer_data_kind_glsl_type[] = {
	str8_comp("int16_t"),
	str8_comp("i16vec2"),
	str8_comp("float32_t"),
	str8_comp("f32vec2"),
	str8_comp("float16_t"),
	str8_comp("f16vec2"),
};

read_only global str8 beamformer_data_kind_str8[] = {
	str8_comp("Int16"),
	str8_comp("Int16Complex"),
	str8_comp("Float32"),
	str8_comp("Float32Complex"),
	str8_comp("Float16"),
	str8_comp("Float16Complex"),
};

read_only global u8 beamformer_contrast_mode_samples[] = {
	1,
	3,
};

read_only global str8 beamformer_contrast_mode_strings[] = {
	str8_comp("None"),
	str8_comp("A1S2"),
};

read_only global str8 beamformer_view_plane_tag_strings[] = {
	str8_comp("XZ"),
	str8_comp("YZ"),
	str8_comp("XY"),
	str8_comp("Arbitrary"),
};

read_only global u8 beamformer_acquisition_kind_has_fixed_transmits[] = {
	1,
	0,
	1,
	0,
	0,
	0,
	1,
	1,
	0,
	0,
	0,
	0,
	0,
};

read_only global str8 beamformer_acquisition_kind_strings[] = {
	str8_comp("FORCES"),
	str8_comp("UFORCES"),
	str8_comp("HERCULES"),
	str8_comp("VLS"),
	str8_comp("TPW"),
	str8_comp("UHERCULES"),
	str8_comp("RACES"),
	str8_comp("EPIC-FORCES"),
	str8_comp("EPIC-UFORCES"),
	str8_comp("EPIC-UHERCULES"),
	str8_comp("Flash"),
	str8_comp("HERO-PA"),
	str8_comp("ULM"),
};

read_only global str8 beamformer_filter_kind_strings[] = {
	str8_comp("Kaiser"),
	str8_comp("MatchedChirp"),
};

read_only global str8 beamformer_interpolation_mode_strings[] = {
	str8_comp("Nearest"),
	str8_comp("Linear"),
	str8_comp("Cubic"),
};

read_only global str8 beamformer_shader_resource_kind_strings[] = {
	str8_comp("Buffer"),
};

typedef enum {
	BeamformerStructKind_DecodeBakeParameters  = 0,
	BeamformerStructKind_FilterBakeParameters  = 1,
	BeamformerStructKind_DASBakeParameters     = 2,
	BeamformerStructKind_ReshapeBakeParameters = 3,
	BeamformerStructKind_Count,
} BeamformerStructKind;

read_only global MetaStructMember *meta_struct_members_by_id[] = {
	(MetaStructMember []){
		{17, 0,  1, 0},
		{18, 8,  1, 0},
		{18, 12, 1, 0},
		{18, 16, 1, 0},
		{18, 20, 1, 0},
		{18, 24, 1, 0},
		{18, 28, 1, 0},
		{18, 32, 1, 0},
		{18, 36, 1, 0},
		{18, 40, 1, 0},
		{18, 44, 1, 0},
	},
	(MetaStructMember []){
		{17, 0,  1, 0},
		{18, 8,  1, 0},
		{8,  12, 1, 0},
		{8,  16, 1, 0},
		{18, 20, 1, 0},
		{18, 24, 1, 0},
		{18, 28, 1, 0},
		{18, 32, 1, 0},
		{18, 36, 1, 0},
		{18, 40, 1, 0},
		{18, 44, 1, 0},
		{18, 48, 1, 0},
		{18, 52, 1, 0},
	},
	(MetaStructMember []){
		{17, 0,  1, 0},
		{18, 8,  1, 0},
		{14, 12, 1, 0},
		{10, 16, 1, 0},
		{10, 20, 1, 0},
		{10, 24, 1, 0},
		{10, 28, 1, 0},
		{8,  32, 1, 0},
		{8,  36, 1, 0},
		{8,  40, 1, 0},
		{8,  44, 1, 0},
		{18, 48, 1, 0},
		{8,  52, 1, 0},
		{14, 56, 1, 0},
		{18, 60, 1, 0},
		{14, 64, 1, 0},
		{8,  68, 1, 0},
		{8,  72, 1, 0},
		{18, 76, 1, 0},
	},
	(MetaStructMember []){
		{18, 0,  1, 0},
		{18, 4,  1, 0},
		{18, 8,  1, 0},
		{18, 12, 1, 0},
		{18, 16, 1, 0},
		{18, 20, 1, 0},
		{18, 24, 1, 0},
		{18, 28, 1, 0},
		{18, 32, 1, 0},
	},
};

read_only global str8 *meta_struct_member_names_by_id[] = {
	(str8 []){
		str8_comp("HadamardBuffer"),
		str8_comp("DecodeMode"),
		str8_comp("OutputChannelStride"),
		str8_comp("OutputSampleStride"),
		str8_comp("OutputTransmitStride"),
		str8_comp("ToProcess"),
		str8_comp("TransmitCount"),
		str8_comp("ChunkChannelCount"),
		str8_comp("CooperativeMatrixM"),
		str8_comp("CooperativeMatrixN"),
		str8_comp("CooperativeMatrixK"),
	},
	(str8 []){
		str8_comp("FilterCoefficients"),
		str8_comp("FilterLength"),
		str8_comp("SamplingFrequency"),
		str8_comp("DemodulationFrequency"),
		str8_comp("DecimationRate"),
		str8_comp("SampleCount"),
		str8_comp("BatchSampleCount"),
		str8_comp("InputChannelStride"),
		str8_comp("InputSampleStride"),
		str8_comp("InputTransmitStride"),
		str8_comp("OutputChannelStride"),
		str8_comp("OutputSampleStride"),
		str8_comp("OutputTransmitStride"),
	},
	(str8 []){
		str8_comp("ArrayParameters"),
		str8_comp("AcquisitionKind"),
		str8_comp("Sparse"),
		str8_comp("AcquisitionCount"),
		str8_comp("ChannelCount"),
		str8_comp("ChunkChannelCount"),
		str8_comp("SampleCount"),
		str8_comp("SamplingFrequency"),
		str8_comp("DemodulationFrequency"),
		str8_comp("SpeedOfSound"),
		str8_comp("TimeOffset"),
		str8_comp("InterpolationMode"),
		str8_comp("FNumber"),
		str8_comp("SingleOrientation"),
		str8_comp("TransmitReceiveOrientation"),
		str8_comp("SingleFocus"),
		str8_comp("FocusDepth"),
		str8_comp("TransmitAngle"),
		str8_comp("ReadiGroupCount"),
	},
	(str8 []){
		str8_comp("SizeX"),
		str8_comp("SizeY"),
		str8_comp("SizeZ"),
		str8_comp("InputStrideX"),
		str8_comp("InputStrideY"),
		str8_comp("InputStrideZ"),
		str8_comp("OutputStrideX"),
		str8_comp("OutputStrideY"),
		str8_comp("OutputStrideZ"),
	},
};

read_only global MetaStructInfo meta_struct_info_by_id[] = {
	{str8_comp("DecodeBakeParameters"),  11, 48, 0},
	{str8_comp("FilterBakeParameters"),  13, 56, 0},
	{str8_comp("DASBakeParameters"),     19, 80, 0},
	{str8_comp("ReshapeBakeParameters"), 9,  36, 0},
};

read_only global str8 beamformer_shader_names[] = {
	str8_comp("Decode"),
	str8_comp("Filter"),
	str8_comp("Demodulate"),
	str8_comp("DAS"),
	str8_comp("Sum"),
	str8_comp("MinMax"),
	str8_comp("Hilbert"),
	str8_comp("CoherencyWeighting"),
	str8_comp("Reshape"),
	str8_comp("RenderBeamformed"),
};

read_only global BeamformerShaderKind beamformer_reloadable_shader_kinds[] = {
	BeamformerShaderKind_Decode,
	BeamformerShaderKind_Filter,
	BeamformerShaderKind_DAS,
	BeamformerShaderKind_Sum,
	BeamformerShaderKind_MinMax,
	BeamformerShaderKind_CoherencyWeighting,
	BeamformerShaderKind_Reshape,
	BeamformerShaderKind_RenderBeamformed,
};

read_only global str8 *beamformer_reloadable_shader_files[] = {
	(str8 []){str8_comp("decode.glsl")},
	(str8 []){str8_comp("filter.glsl")},
	(str8 []){str8_comp("das.glsl")},
	(str8 []){str8_comp("sum.glsl")},
	(str8 []){str8_comp("min_max.glsl")},
	(str8 []){str8_comp("coherency_weighting.glsl")},
	(str8 []){str8_comp("reshape.glsl")},
	(str8 []){str8_comp("render_3d.vert.glsl"), str8_comp("render_3d.frag.glsl")},
};

read_only global i32 beamformer_shader_reloadable_index_by_shader[] = {
	0,
	1,
	1,
	2,
	3,
	4,
	-1,
	5,
	6,
	7,
};

read_only global i32 beamformer_reloadable_compute_shader_info_indices[] = {
	0,
	1,
	2,
	3,
	4,
};

read_only global i32 beamformer_reloadable_compute_helpers_shader_info_indices[] = {
	5,
	6,
};

read_only global i32 beamformer_reloadable_render_shader_info_indices[] = {
	7,
};

read_only global str8 beamformer_shader_global_header_strings[] = {
	str8_comp(""
	"#define DecodeMode_None     0\n"
	"#define DecodeMode_Hadamard 1\n"
	"\n"),
	str8_comp(""
	"#define CooperativeMatrix ((CompileFlags & (1 << 0)) != 0)\n"
	"#define UseSharedMemory   ((CompileFlags & (1 << 1)) != 0)\n"
	"\n"),
	str8_comp(""
	"layout(push_constant, std430) uniform PushConstants {\n"
	"  uint64_t rf_buffer;\n"
	"  uint64_t output_buffer;\n"
	"};\n"
	"\n"),
	str8_comp(""
	"#define ShaderBufferSlot_BeamformedData 0\n"
	"#define ShaderBufferSlot_PingPong       1\n"
	"\n"),
	str8_comp(""
	"#define ShaderResourceKind_Buffer 0\n"
	"\n"),
	str8_comp(""
	"#define ComplexFilter ((CompileFlags & (1 << 0)) != 0)\n"
	"#define Demodulate    ((CompileFlags & (1 << 1)) != 0)\n"
	"\n"),
	str8_comp(""
	"layout(push_constant, std430) uniform PushConstants {\n"
	"  uint64_t input_data;\n"
	"  uint32_t output_element_offset;\n"
	"};\n"
	"\n"),
	str8_comp("#define MaxChannelCount (256)\n\n"),
	str8_comp("#define MaxHadamardElements (65536)\n\n"),
	str8_comp(""
	"#define AcquisitionKind_FORCES         0\n"
	"#define AcquisitionKind_UFORCES        1\n"
	"#define AcquisitionKind_HERCULES       2\n"
	"#define AcquisitionKind_RCA_VLS        3\n"
	"#define AcquisitionKind_RCA_TPW        4\n"
	"#define AcquisitionKind_UHERCULES      5\n"
	"#define AcquisitionKind_RACES          6\n"
	"#define AcquisitionKind_EPIC_FORCES    7\n"
	"#define AcquisitionKind_EPIC_UFORCES   8\n"
	"#define AcquisitionKind_EPIC_UHERCULES 9\n"
	"#define AcquisitionKind_Flash          10\n"
	"#define AcquisitionKind_HERO_PA        11\n"
	"#define AcquisitionKind_ULM            12\n"
	"\n"),
	str8_comp(""
	"#define InterpolationMode_Nearest 0\n"
	"#define InterpolationMode_Linear  1\n"
	"#define InterpolationMode_Cubic   2\n"
	"\n"),
	str8_comp(""
	"#define RCAOrientation_None    0\n"
	"#define RCAOrientation_Rows    1\n"
	"#define RCAOrientation_Columns 2\n"
	"\n"),
	str8_comp(""
	"struct ComputeArrayParameters {\n"
	"  f32vec2   focal_vectors[MaxChannelCount];\n"
	"  int16_t   sparse_elements[MaxChannelCount];\n"
	"  uint16_t  transmit_receive_orientations[MaxChannelCount];\n"
	"  float16_t das_hadamard[MaxHadamardElements];\n"
	"  float16_t decode_hadamard[MaxHadamardElements];\n"
	"};\n"
	"layout(std430, buffer_reference) buffer ComputeArrayParametersReference {\n"
	"  f32vec2   focal_vectors[MaxChannelCount];\n"
	"  int16_t   sparse_elements[MaxChannelCount];\n"
	"  uint16_t  transmit_receive_orientations[MaxChannelCount];\n"
	"  float16_t das_hadamard[MaxHadamardElements];\n"
	"  float16_t decode_hadamard[MaxHadamardElements];\n"
	"};\n"
	"\n"),
	str8_comp(""
	"#define CoherencyWeighting ((CompileFlags & (1 << 0)) != 0)\n"
	"\n"),
	str8_comp(""
	"layout(push_constant, std430) uniform PushConstants {\n"
	"  f32mat4  xdc_transform;\n"
	"  f32mat4  voxel_transform;\n"
	"  f32vec2  xdc_element_pitch;\n"
	"  uint64_t output_frame;\n"
	"  uint64_t incoherent_frame;\n"
	"  uint32_t rf_element_offset;\n"
	"  uint32_t output_size_x;\n"
	"  uint32_t output_size_y;\n"
	"  uint32_t output_size_z;\n"
	"  int32_t  channel_offset;\n"
	"  uint32_t readi_group;\n"
	"};\n"
	"\n"),
	str8_comp(""
	"layout(push_constant, std430) uniform PushConstants {\n"
	"  uint64_t  output_data;\n"
	"  uint64_t  input_data;\n"
	"  uint32_t  image_elements;\n"
	"  float32_t scale;\n"
	"};\n"
	"\n"),
	str8_comp(""
	"layout(push_constant, std430) uniform PushConstants {\n"
	"  uint64_t  left_side_buffer;\n"
	"  uint64_t  right_side_buffer;\n"
	"  float32_t scale;\n"
	"  uint32_t  output_size_x;\n"
	"  uint32_t  output_size_y;\n"
	"  uint32_t  output_size_z;\n"
	"};\n"
	"\n"),
	str8_comp(""
	"#define Deinterleave ((CompileFlags & (1 << 0)) != 0)\n"
	"#define Interleave   ((CompileFlags & (1 << 1)) != 0)\n"
	"\n"),
	str8_comp(""
	"layout(push_constant, std430) uniform PushConstants {\n"
	"  uint64_t output_buffer;\n"
	"  uint64_t left_input_buffer;\n"
	"  uint64_t right_input_buffer;\n"
	"};\n"
	"\n"),
	str8_comp(""
	"layout(push_constant, std430) uniform PushConstants {\n"
	"  f32mat4   mvp_matrix;\n"
	"  uint64_t  positions;\n"
	"  uint64_t  normals;\n"
	"  f32vec4   bounding_box_colour;\n"
	"  float32_t bounding_box_fraction;\n"
	"  float32_t db_cutoff;\n"
	"  float32_t threshold;\n"
	"  float32_t gamma;\n"
	"  uint64_t  input_data;\n"
	"  uint32_t  input_size_x;\n"
	"  uint32_t  input_size_y;\n"
	"  uint32_t  input_size_z;\n"
	"  uint32_t  data_kind;\n"
	"};\n"
	"\n"),
};

read_only global b8 beamformer_shader_has_primitive[] = {
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	1,
};

read_only global b8 beamformer_shader_primitive_is_vertex[] = {
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	1,
};

read_only global i32 *beamformer_shader_header_vectors[] = {
	(i32 []){0, 1, 2},
	(i32 []){3, 4, 5, 6},
	(i32 []){7, 8, 9, 10, 11, 3, 4, 12, 13, 14},
	(i32 []){15},
	0,
	(i32 []){16},
	(i32 []){17, 18},
	(i32 []){19},
};

read_only global i32 beamformer_shader_header_vector_lengths[] = {
	3,
	4,
	10,
	1,
	0,
	1,
	2,
	1,
};

read_only global str8 *beamformer_shader_compile_flag_names[] = {
	(str8 []){
		str8_comp("CooperativeMatrix"),
		str8_comp("UseSharedMemory"),
	},
	(str8 []){
		str8_comp("ComplexFilter"),
		str8_comp("Demodulate"),
	},
	(str8 []){
		str8_comp("CoherencyWeighting"),
	},
	0,
	0,
	0,
	(str8 []){
		str8_comp("Deinterleave"),
		str8_comp("Interleave"),
	},
	0,
};

read_only global u8 beamformer_shader_compile_flag_counts[] = {
	2,
	2,
	1,
	0,
	0,
	0,
	2,
	0,
};

read_only global i32 beamformer_base_shader_to_bake_struct_id[] = {
	0,
	1,
	2,
	-1,
	-1,
	-1,
	3,
	-1,
};

read_only global u8 beamformer_shader_push_constant_sizes[] = {
	sizeof(BeamformerDecodePushConstants),
	sizeof(BeamformerFilterPushConstants),
	sizeof(BeamformerDASPushConstants),
	sizeof(BeamformerSumPushConstants),
	0,
	sizeof(BeamformerCoherencyWeightingPushConstants),
	sizeof(BeamformerReshapePushConstants),
	sizeof(BeamformerRenderBeamformedPushConstants),
};

