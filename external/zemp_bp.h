/*
 * ISC License (ISC)
 *
 * © 2024-2026 Randy Palamar <randy@rnpnr.xyz>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

// GENERATED CODE

#include <stdint.h>

#define ZBP_HeaderMagic (0x5042504d455afecaULL)
#define ZBP_OffsetAlignment (0x04U)

typedef enum {
	ZBP_RCAOrientation_None    = 0,
	ZBP_RCAOrientation_Rows    = 1,
	ZBP_RCAOrientation_Columns = 2,
	ZBP_RCAOrientation_Count,
} ZBP_RCAOrientation;

typedef enum {
	ZBP_DecodeMode_None     = 0,
	ZBP_DecodeMode_Hadamard = 1,
	ZBP_DecodeMode_Walsh    = 2,
	ZBP_DecodeMode_Count,
} ZBP_DecodeMode;

typedef enum {
	ZBP_SamplingMode_Standard = 0,
	ZBP_SamplingMode_Bandpass = 1,
	ZBP_SamplingMode_Count,
} ZBP_SamplingMode;

typedef enum {
	ZBP_AcquisitionKind_FORCES         = 0,
	ZBP_AcquisitionKind_UFORCES        = 1,
	ZBP_AcquisitionKind_HERCULES       = 2,
	ZBP_AcquisitionKind_RCA_VLS        = 3,
	ZBP_AcquisitionKind_RCA_TPW        = 4,
	ZBP_AcquisitionKind_UHERCULES      = 5,
	ZBP_AcquisitionKind_RACES          = 6,
	ZBP_AcquisitionKind_EPIC_FORCES    = 7,
	ZBP_AcquisitionKind_EPIC_UFORCES   = 8,
	ZBP_AcquisitionKind_EPIC_UHERCULES = 9,
	ZBP_AcquisitionKind_Flash          = 10,
	ZBP_AcquisitionKind_HERO_PA        = 11,
	ZBP_AcquisitionKind_Count,
} ZBP_AcquisitionKind;

typedef enum {
	ZBP_ContrastMode_None = 0,
	ZBP_ContrastMode_Count,
} ZBP_ContrastMode;

typedef enum {
	ZBP_EmissionKind_Sine  = 0,
	ZBP_EmissionKind_Chirp = 1,
	ZBP_EmissionKind_Count,
} ZBP_EmissionKind;

typedef enum {
	ZBP_DataKind_Int16          = 0,
	ZBP_DataKind_Int16Complex   = 1,
	ZBP_DataKind_Float32        = 2,
	ZBP_DataKind_Float32Complex = 3,
	ZBP_DataKind_Float16        = 4,
	ZBP_DataKind_Float16Complex = 5,
	ZBP_DataKind_Count,
} ZBP_DataKind;

typedef enum {
	ZBP_DataCompressionKind_None = 0,
	ZBP_DataCompressionKind_ZSTD = 1,
	ZBP_DataCompressionKind_Count,
} ZBP_DataCompressionKind;

typedef struct ZBP_BaseHeader {
	uint64_t magic;
	uint32_t major;
	uint32_t minor;
} ZBP_BaseHeader;

typedef struct ZBP_HeaderV1 {
	uint64_t magic;
	uint32_t version;
	int16_t  decode_mode;
	int16_t  beamform_mode;
	uint32_t raw_data_dimension[4];
	uint32_t sample_count;
	uint32_t channel_count;
	uint32_t receive_event_count;
	uint32_t frame_count;
	float    transducer_element_pitch[2];
	float    transducer_transform_matrix[16];
	int16_t  channel_mapping[256];
	float    steering_angles[256];
	float    focal_depths[256];
	int16_t  sparse_elements[256];
	int16_t  hadamard_rows[256];
	float    speed_of_sound;
	float    demodulation_frequency;
	float    sampling_frequency;
	float    time_offset;
	uint32_t transmit_mode;
} ZBP_HeaderV1;

typedef struct ZBP_HeaderV2 {
	uint64_t magic;
	uint32_t major;
	uint32_t minor;
	uint32_t raw_data_dimension[4];
	int32_t  raw_data_kind;
	int32_t  raw_data_offset;
	int32_t  raw_data_compression_kind;
	int32_t  decode_mode;
	int32_t  sampling_mode;
	float    sampling_frequency;
	float    demodulation_frequency;
	float    speed_of_sound;
	int32_t  channel_mapping_offset;
	uint32_t sample_count;
	uint32_t channel_count;
	uint32_t receive_event_count;
	float    transducer_transform_matrix[16];
	float    transducer_element_pitch[2];
	float    time_offset;
	float    group_acquisition_time;
	float    ensemble_repitition_interval;
	int32_t  acquisition_mode;
	int32_t  acquisition_parameters_offset;
	int32_t  contrast_mode;
	int32_t  contrast_parameters_offset;
	int32_t  emission_descriptors_offset;
} ZBP_HeaderV2;

typedef struct ZBP_EmissionDescriptor {
	int32_t emission_kind;
	int32_t parameters_offset;
} ZBP_EmissionDescriptor;

typedef struct ZBP_EmissionSineParameters {
	float cycles;
	float frequency;
} ZBP_EmissionSineParameters;

typedef struct ZBP_EmissionChirpParameters {
	float duration;
	float min_frequency;
	float max_frequency;
} ZBP_EmissionChirpParameters;

typedef struct ZBP_RCATransmitFocus {
	float    focal_depth;
	float    steering_angle;
	float    origin_offset;
	uint32_t transmit_receive_orientation;
} ZBP_RCATransmitFocus;

typedef struct ZBP_FORCESParameters {
	ZBP_RCATransmitFocus transmit_focus;
} ZBP_FORCESParameters;

typedef struct ZBP_uFORCESParameters {
	ZBP_RCATransmitFocus transmit_focus;
	int32_t              sparse_elements_offset;
} ZBP_uFORCESParameters;

typedef struct ZBP_HERCULESParameters {
	ZBP_RCATransmitFocus transmit_focus;
} ZBP_HERCULESParameters;

typedef struct ZBP_uHERCULESParameters {
	ZBP_RCATransmitFocus transmit_focus;
	int32_t              sparse_elements_offset;
} ZBP_uHERCULESParameters;

typedef struct ZBP_TPWParameters {
	int32_t tilting_angles_offset;
	int32_t transmit_receive_orientations_offset;
} ZBP_TPWParameters;

typedef struct ZBP_VLSParameters {
	int32_t focal_depths_offset;
	int32_t origin_offsets_offset;
	int32_t transmit_receive_orientations_offset;
} ZBP_VLSParameters;
