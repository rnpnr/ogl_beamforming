/* See LICENSE for license details. */
#define BASE_EXPORT           function
#define BASE_IMPORT           function
#define BEAMFORMER_LIB_EXPORT function
#include "base_platform.h"
#include "ogl_beamformer_lib.c"

#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <zstd.h>

#include "external/zemp_bp.h"

#define RXOrientation(tx_rx) ((ZBP_RCAOrientation)((tx_rx) >> 0 & 0x0F))
#define TXOrientation(tx_rx) ((ZBP_RCAOrientation)((tx_rx) >> 4 & 0x0F))

typedef struct {
	ZBP_DataKind            kind;
	ZBP_DataCompressionKind compression_kind;
	str8                    bytes;
} ZBP_Data;

#define shift_n(v, c, n) v += n, c -= n
#define shift(v, c) shift_n(v, c, 1)

#define die(...) die_((char *)__func__, __VA_ARGS__)
function no_return void
die_(char *function_name, char *format, ...)
{
	if (function_name)
		fprintf(stderr, "%s: ", function_name);

	va_list ap;

	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);

	os_exit(1);
}

function void
beamformer_simple_parameters_equip_channel_mapping(BeamformerSimpleParameters *bp, u8 *raw, u32 channel_count, i32 channel_mapping_offset)
{
	if (channel_mapping_offset != -1) {
		memory_copy(bp->channel_mapping, raw + channel_mapping_offset, sizeof(*bp->channel_mapping) * channel_count);
	} else {
		for EachIndex(channel_count, it)
			bp->channel_mapping[it] = it;
	}
}

function void
beamformer_simple_parameters_equip_emission(BeamformerSimpleParameters *bp, u8 *raw, i32 emission_descriptors_offset)
{
	// NOTE/TODO(rnp): only considering the first data frame right now
	ZBP_EmissionDescriptor *ed = (ZBP_EmissionDescriptor *)(raw + emission_descriptors_offset);
	switch (ed->emission_kind) {

	case ZBP_EmissionKind_Sine:{
		ZBP_EmissionSineParameters *ep = (ZBP_EmissionSineParameters *)(raw + ed->parameters_offset);
		bp->emission_parameters.kind           = BeamformerEmissionKind_Sine;
		bp->emission_parameters.sine.cycles    = ep->cycles;
		bp->emission_parameters.sine.frequency = ep->frequency;
	}break;

	case ZBP_EmissionKind_Chirp:{
		ZBP_EmissionChirpParameters *ep = (ZBP_EmissionChirpParameters *)(raw + ed->parameters_offset);
		bp->emission_parameters.kind                = BeamformerEmissionKind_Chirp;
		bp->emission_parameters.chirp.duration      = ep->duration;
		bp->emission_parameters.chirp.min_frequency = ep->min_frequency;
		bp->emission_parameters.chirp.max_frequency = ep->max_frequency;
	}break;

	InvalidDefaultCase;
	static_assert(ZBP_EmissionKind_Count == (ZBP_EmissionKind_Chirp + 1), "");
	}
}

function void
beamformer_simple_parameters_equip_acquisition(BeamformerSimpleParameters *bp, u8 *raw,
                                               BeamformerAcquisitionKind acquisition_kind,
                                               i32 acquisition_parameters_offset)
{
	switch (acquisition_kind) {
	case ZBP_AcquisitionKind_FORCES:{
		ZBP_FORCESParameters *p = (ZBP_FORCESParameters *)(raw + acquisition_parameters_offset);
		bp->transmit_receive_orientation = p->transmit_focus.transmit_receive_orientation;
		bp->focal_vector.E[0] = p->transmit_focus.steering_angle;
		bp->focal_vector.E[1] = p->transmit_focus.focal_depth;

		bp->single_focus       = 1;
		bp->single_orientation = 1;
	}break;

	case ZBP_AcquisitionKind_HERCULES:{
		ZBP_HERCULESParameters *p = (ZBP_HERCULESParameters *)(raw + acquisition_parameters_offset);
		bp->transmit_receive_orientation = p->transmit_focus.transmit_receive_orientation;
		bp->focal_vector.E[0] = p->transmit_focus.steering_angle;
		bp->focal_vector.E[1] = p->transmit_focus.focal_depth;

		bp->single_focus       = 1;
		bp->single_orientation = 1;
	}break;

	case ZBP_AcquisitionKind_UFORCES:{
		ZBP_uFORCESParameters *p = (ZBP_uFORCESParameters *)(raw + acquisition_parameters_offset);
		bp->transmit_receive_orientation = p->transmit_focus.transmit_receive_orientation;
		bp->focal_vector.E[0] = p->transmit_focus.steering_angle;
		bp->focal_vector.E[1] = p->transmit_focus.focal_depth;

		bp->single_focus       = 1;
		bp->single_orientation = 1;

		memory_copy(bp->sparse_elements, raw + p->sparse_elements_offset,
		            sizeof(*bp->sparse_elements) * bp->acquisition_count);
	}break;

	case ZBP_AcquisitionKind_UHERCULES:{
		ZBP_uHERCULESParameters *p = (ZBP_uHERCULESParameters *)(raw + acquisition_parameters_offset);
		bp->transmit_receive_orientation = p->transmit_focus.transmit_receive_orientation;
		bp->focal_vector.E[0] = p->transmit_focus.steering_angle;
		bp->focal_vector.E[1] = p->transmit_focus.focal_depth;

		bp->single_focus       = 1;
		bp->single_orientation = 1;

		memory_copy(bp->sparse_elements, raw + p->sparse_elements_offset,
		            sizeof(*bp->sparse_elements) * bp->acquisition_count);
	}break;

	case ZBP_AcquisitionKind_RCA_TPW:{
		ZBP_TPWParameters *p = (ZBP_TPWParameters *)(raw + acquisition_parameters_offset);

		memory_copy(bp->transmit_receive_orientations, raw + p->transmit_receive_orientations_offset,
		            sizeof(*bp->transmit_receive_orientations) * bp->acquisition_count);
		memory_copy(bp->steering_angles, raw + p->tilting_angles_offset,
		            sizeof(*bp->steering_angles) * bp->acquisition_count);

		for EachIndex(bp->acquisition_count, it)
			bp->focal_depths[it] = inf32();
	}break;

	case ZBP_AcquisitionKind_RCA_VLS:{
		ZBP_VLSParameters *p = (ZBP_VLSParameters *)(raw + acquisition_parameters_offset);

		memory_copy(bp->transmit_receive_orientations, raw + p->transmit_receive_orientations_offset,
		            sizeof(*bp->transmit_receive_orientations) * bp->acquisition_count);

		f32 *focal_depths   = (f32 *)(raw + p->focal_depths_offset);
		f32 *origin_offsets = (f32 *)(raw + p->origin_offsets_offset);

		for EachIndex(bp->acquisition_count, it) {
			f32 sign   = Sign(focal_depths[it]);
			f32 depth  = focal_depths[it];
			f32 origin = origin_offsets[it];
			bp->steering_angles[it] = atan2_f32(origin, -depth) * 180.0f / PI;
			bp->focal_depths[it]    = sign * sqrt_f32(depth * depth + origin * origin);
		}
	}break;

	InvalidDefaultCase;
	}
}

function b32
beamformer_simple_parameters_from_zbp_file(Arena *arena, BeamformerSimpleParameters *bp, char *path, ZBP_Data *raw_data, u32 *data_frame_count)
{
	str8 raw = os_read_entire_file(arena, path);
	if (raw.length < (i64)sizeof(ZBP_BaseHeader) || ((ZBP_BaseHeader *)raw.data)->magic != ZBP_HeaderMagic)
		return 0;

	read_only BeamformerSamplingMode zbp_sampling_mode_to_beamformer[] = {
		[ZBP_SamplingMode_Standard] = BeamformerSamplingMode_4X,
		[ZBP_SamplingMode_Bandpass] = BeamformerSamplingMode_2X,
	};

	switch (((ZBP_BaseHeader *)raw.data)->major) {

	case 1:{
		ZBP_HeaderV1 *header       = (ZBP_HeaderV1 *)raw.data;

		bp->sample_count           = header->sample_count;
		bp->channel_count          = header->channel_count;
		bp->acquisition_count      = header->receive_event_count;

		bp->sampling_mode          = BeamformerSamplingMode_4X;
		bp->acquisition_kind       = header->beamform_mode;
		bp->decode_mode            = header->decode_mode;
		bp->sampling_frequency     = header->sampling_frequency;
		bp->demodulation_frequency = header->sampling_frequency / 4;
		bp->speed_of_sound         = header->speed_of_sound;
		bp->time_offset            = header->time_offset;

		memory_copy(bp->channel_mapping,       header->channel_mapping,             sizeof(*bp->channel_mapping) * bp->channel_count);
		memory_copy(bp->xdc_transform.E,       header->transducer_transform_matrix, sizeof(bp->xdc_transform));
		memory_copy(bp->xdc_element_pitch.E,   header->transducer_element_pitch,    sizeof(bp->xdc_element_pitch));
		// NOTE(rnp): ignores emission count and ensemble count
		memory_copy(bp->raw_data_dimensions.E, header->raw_data_dimension,          sizeof(bp->raw_data_dimensions));
		if (data_frame_count) *data_frame_count = header->raw_data_dimension[2];
		//if (data_frame_count) *data_frame_count = header->raw_data_dimension[3];

		bp->data_kind              = (BeamformerDataKind)ZBP_DataKind_Int16;
		raw_data->kind             = ZBP_DataKind_Int16;
		raw_data->compression_kind = ZBP_DataCompressionKind_ZSTD;

		read_only u8 transmit_mode_to_orientation[] = {
			[0] = (ZBP_RCAOrientation_Rows    << 4) | ZBP_RCAOrientation_Rows,
			[1] = (ZBP_RCAOrientation_Rows    << 4) | ZBP_RCAOrientation_Columns,
			[2] = (ZBP_RCAOrientation_Columns << 4) | ZBP_RCAOrientation_Rows,
			[3] = (ZBP_RCAOrientation_Columns << 4) | ZBP_RCAOrientation_Columns,
		};
		if (header->transmit_mode >= countof(transmit_mode_to_orientation))
			return 0;

		bp->transmit_receive_orientation = transmit_mode_to_orientation[header->transmit_mode];

		ZBP_AcquisitionKind acquisition_kind = header->beamform_mode;
		if (acquisition_kind == ZBP_AcquisitionKind_FORCES   ||
		    acquisition_kind == ZBP_AcquisitionKind_HERCULES ||
		    acquisition_kind == ZBP_AcquisitionKind_UFORCES  ||
		    acquisition_kind == ZBP_AcquisitionKind_UHERCULES)
		{
			bp->single_focus       = 1;
			bp->single_orientation = 1;
			bp->focal_vector.E[0]  = header->steering_angles[0];
			bp->focal_vector.E[1]  = header->focal_depths[0];
		}

		if (acquisition_kind == ZBP_AcquisitionKind_UFORCES ||
		    acquisition_kind == ZBP_AcquisitionKind_UHERCULES)
		{
			memory_copy(bp->sparse_elements, header->sparse_elements, sizeof(*bp->sparse_elements) * bp->acquisition_count);
		}

		if (acquisition_kind == ZBP_AcquisitionKind_RCA_TPW ||
		    acquisition_kind == ZBP_AcquisitionKind_RCA_VLS)
		{
			memory_copy(bp->focal_depths,    header->focal_depths,    sizeof(*bp->focal_depths) * bp->acquisition_count);
			memory_copy(bp->steering_angles, header->steering_angles, sizeof(*bp->steering_angles) * bp->acquisition_count);
			for EachIndex(bp->acquisition_count, it)
				bp->transmit_receive_orientations[it] = bp->transmit_receive_orientation;
		}

		bp->emission_parameters.kind           = BeamformerEmissionKind_Sine;
		bp->emission_parameters.sine.cycles    = 2;
		bp->emission_parameters.sine.frequency = bp->demodulation_frequency;
	}break;

	case 2:{
		ZBP_HeaderV2 *header       = (ZBP_HeaderV2 *)raw.data;
		bp->sample_count           = header->sample_count;
		bp->channel_count          = header->channel_count;
		bp->acquisition_count      = header->receive_event_count;
		bp->sampling_mode          = zbp_sampling_mode_to_beamformer[header->sampling_mode];
		bp->acquisition_kind       = (BeamformerAcquisitionKind)header->acquisition_mode;
		bp->decode_mode            = (BeamformerDecodeMode)header->decode_mode;
		bp->sampling_frequency     = header->sampling_frequency;
		bp->demodulation_frequency = header->demodulation_frequency;
		bp->speed_of_sound         = header->speed_of_sound;
		bp->time_offset            = header->time_offset;
		bp->contrast_mode          = (BeamformerContrastMode)header->contrast_mode;
		bp->data_kind              = (BeamformerDataKind)header->raw_data_kind;

		beamformer_simple_parameters_equip_channel_mapping(bp, raw.data, bp->channel_count, header->channel_mapping_offset);
		beamformer_simple_parameters_equip_emission(bp, raw.data, header->emission_descriptors_offset);
		beamformer_simple_parameters_equip_acquisition(bp, raw.data, bp->acquisition_kind, header->acquisition_parameters_offset);

		memory_copy(bp->xdc_transform.E,       header->transducer_transform_matrix, sizeof(bp->xdc_transform));
		memory_copy(bp->xdc_element_pitch.E,   header->transducer_element_pitch,    sizeof(bp->xdc_element_pitch));
		// NOTE(rnp): ignores group count and ensemble count
		memory_copy(bp->raw_data_dimensions.E, header->raw_data_dimension,          sizeof(bp->raw_data_dimensions));
		if (data_frame_count) *data_frame_count = header->raw_data_dimension[2];
		//if (data_frame_count) *data_frame_count = header->raw_data_dimension[3];

		raw_data->kind             = header->raw_data_kind;
		raw_data->compression_kind = header->raw_data_compression_kind;

		if (header->raw_data_offset != -1) {
			raw_data->bytes.data = raw.data + header->raw_data_offset;
			if (raw_data->compression_kind == ZBP_DataCompressionKind_ZSTD) {
				// NOTE(rnp): limitation in the header format
				raw_data->bytes.length  = raw.length - header->raw_data_offset;
			} else {
				raw_data->bytes.length  = header->raw_data_dimension[0] * header->raw_data_dimension[1] *
				                          header->raw_data_dimension[2] * header->raw_data_dimension[3];
				raw_data->bytes.length *= beamformer_data_kind_byte_size[header->raw_data_kind];
			}
		}
	}break;

	case 3:{
		ZBP_HeaderV3 *header   = (ZBP_HeaderV3 *)raw.data;
		bp->sample_count       = header->sample_count;
		bp->channel_count      = header->channel_count;
		bp->acquisition_count  = header->receive_event_count;
		bp->sampling_mode      = zbp_sampling_mode_to_beamformer[header->sampling_mode];
		bp->acquisition_kind   = (BeamformerAcquisitionKind)header->acquisition_mode;
		bp->decode_mode        = (BeamformerDecodeMode)header->decode_mode;
		bp->sampling_frequency = header->sampling_frequency;
		bp->speed_of_sound     = header->speed_of_sound;
		bp->contrast_mode      = (BeamformerContrastMode)header->contrast_mode;
		bp->data_kind          = (BeamformerDataKind)header->raw_data_kind;

		memory_copy(bp->xdc_element_pitch.E,   header->transducer_element_pitch,    sizeof(bp->xdc_element_pitch));
		memory_copy(bp->raw_data_dimensions.E, header->raw_data_dimension,          sizeof(bp->raw_data_dimensions));
		if (data_frame_count) *data_frame_count = header->raw_data_dimension[2];

		beamformer_simple_parameters_equip_channel_mapping(bp, raw.data, bp->channel_count, header->channel_mapping_offset);
		beamformer_simple_parameters_equip_emission(bp, raw.data, header->emission_descriptors_offset);
		beamformer_simple_parameters_equip_acquisition(bp, raw.data, bp->acquisition_kind, header->acquisition_parameters_offset);

		// TODO(rnp): for now we only consider first data frame
		memory_copy(&bp->time_offset, raw.data + header->time_delays_offset, sizeof(f32));
		memory_copy(&bp->demodulation_frequency , raw.data + header->demodulation_frequencies_offset, sizeof(f32));

		// TODO(rnp): for now we only consider first tile
		memory_copy(&bp->xdc_transform, raw.data + header->transducer_transforms_offset, sizeof(m4));

		raw_data->kind             = header->raw_data_kind;
		raw_data->compression_kind = header->raw_data_compression_kind;
		if (header->raw_data_offset != -1) {
			raw_data->bytes.length = header->raw_data_size;
			raw_data->bytes.data   = raw.data + header->raw_data_offset;
		}
	}break;

	default:{return 0;}break;
	}

	return 1;
}

function void
stream_ensure_termination(Stream *s, u8 byte)
{
	b32 found = 0;
	if (!s->errors && s->widx > 0)
		found = s->data[s->widx - 1] == byte;
	if (!found) {
		s->errors |= s->cap - 1 < s->widx;
		if (!s->errors)
			s->data[s->widx++] = byte;
	}
}

function str8
zstd_decompress_data(Arena *arena, str8 raw)
{
	u64 requested_size = ZSTD_getFrameContentSize(raw.data, (u64)raw.length);
	void *out          = push_array_no_zero(arena, u8, requested_size);
	str8 result = {.data = out};
	u64 decompressed  = ZSTD_decompress(out, requested_size, raw.data, (u64)raw.length);
	if (decompressed == requested_size) result.length = requested_size;
	return result;
}

function void *
zbp_data_pointer(Arena *arena, ZBP_Data *raw, Stream path, u32 frame_number)
{
	void *result = 0;
	if (raw->bytes.length == 0) {
		// NOTE(rnp): strip ".bp"
		stream_reset(&path, path.widx - 3);

		b32 compressed = raw->compression_kind == ZBP_DataCompressionKind_ZSTD;
		stream_append_byte(&path, '_');
		stream_append_u64_width(&path, frame_number, 2);
		stream_append_str8(&path, compressed ? str8(".zst") : str8(".bin"));
		stream_ensure_termination(&path, 0);
		str8 compressed_data = os_read_entire_file(arena, (char *)path.data);

		str8 bytes = compressed_data;
		if (compressed) {
			bytes = zstd_decompress_data(arena, compressed_data);
			if (!bytes.length)
				die("failed to decompress data: %s\n", path.data);
		}
		result = bytes.data;
	} else {
		if (raw->compression_kind == ZBP_DataCompressionKind_ZSTD) {
			str8 bytes = zstd_decompress_data(arena, raw->bytes);
			result = bytes.data;
		} else {
			result = raw->bytes.data;
		}
	}
	return result;
}
