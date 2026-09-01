/* See LICENSE for license details. */

#define BASE_EXPORT           function
#define BASE_IMPORT           function
#define BEAMFORMER_LIB_EXPORT function
#include "base_platform.h"
#include "ogl_beamformer_lib.c"
#include "threads.c"

#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <zstd.h>

//global iv3 output_points    = {{512, 1, 1024}};
global v2  axial_extent     = {{ 0e-3f, 120e-3f}};
global v2  lateral_extent   = {{-60e-3f,  60e-3f}};
global f32 f_number         = 0.5f;

#define DATA_FILE "/home/rnp/doc/school/grad/src/others/ogl_beamforming/data/"\
                  "260820_Tiled1_Tiled_Data_2_FORCES-Rx-Columns_combined.bp"

typedef union {
	struct {
		f32 alpha, beta, gamma;
		f32 x_translation;
		f32 y_translation;
	};
	f32 E[5];
} TileParameters;

global Arena          *arena;

global u32 wire_points = 1024;
global v2 wire_targets[] = {
	{{  1.3e-3f, 105.5e-3f}},
	{{  1.5e-3f, 86.4e-3f}},
	{{  3.6e-3f,  20.2e-3f}},
	//{{  3.4e-3f,  58.4e-3f}},
	{{ 23.2e-3f,  48.9e-3f}},
	{{-15.1e-3f,  48.7e-3f}},
};
global f32 region_width = 12e-3;

#define OutputImages (countof(wire_targets) * 2)

global u64 iteration_count;
global f32 max_output_values[OutputImages];
global f32 resolutions[OutputImages];

#define TileCount 2
global TileParameters tile_parameters_log[TileCount][1 << 14];

global b32 g_should_exit;

#include "external/zemp_bp.h"

typedef struct {
	ZBP_DataKind            kind;
	ZBP_DataCompressionKind compression_kind;
	str8                    bytes;
} ZBP_Data;

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

function str8
decompress_zstd_data(Arena *arena, str8 raw)
{
	str8 result = {.length = ZSTD_getFrameContentSize(raw.data, (u64)raw.length)};
	result.data = push_array_no_zero(arena, u8, result.length, .align = 64);
	u64 decompressed = ZSTD_decompress(result.data, result.length, raw.data, (u64)raw.length);
	if (decompressed != (u64)result.length)
		die("failed to decompress data\n");
	return result;
}

function b32
beamformer_simple_parameters_from_zbp_file(char *path, BeamformerSimpleParameters *bp, ZBP_Data *raw_data)
{
	str8 raw = os_read_entire_file(arena, path);
	if (raw.length < (i64)sizeof(ZBP_BaseHeader) || ((ZBP_BaseHeader *)raw.data)->magic != ZBP_HeaderMagic)
		return 0;

	switch (((ZBP_BaseHeader *)raw.data)->major) {

	case 1:{
		ZBP_HeaderV1 *header       = (ZBP_HeaderV1 *)raw.data;

		bp->sample_count            = header->sample_count;
		bp->receive_channel_count   = header->channel_count;
		bp->transmit_channel_count  = header->channel_count;
		bp->xdc_receive_tile_count  = 1;
		bp->xdc_transmit_tile_count = 1;
		bp->acquisition_count       = header->receive_event_count;

		bp->sampling_mode          = BeamformerSamplingMode_4X;
		bp->acquisition_kind       = header->beamform_mode;
		bp->decode_mode            = header->decode_mode;
		bp->sampling_frequency     = header->sampling_frequency;
		bp->demodulation_frequency = header->sampling_frequency / 4;
		bp->speed_of_sound         = header->speed_of_sound;
		bp->time_offset            = header->time_offset;

		memory_copy(bp->xdc_transform_matrices + 0, header->transducer_transform_matrix, sizeof(header->transducer_transform_matrix));
		memory_copy(bp->channel_mapping,       header->channel_mapping,             sizeof(*bp->channel_mapping) * bp->receive_channel_count);
		memory_copy(bp->xdc_element_pitch.E,   header->transducer_element_pitch,    sizeof(bp->xdc_element_pitch));
		// NOTE(rnp): ignores emission count and ensemble count
		memory_copy(bp->raw_data_dimensions.E, header->raw_data_dimension,          sizeof(bp->raw_data_dimensions));

		bp->data_kind              = (BeamformerDataKind)ZBP_DataKind_Int16;
		raw_data->kind             = ZBP_DataKind_Int16;
		raw_data->compression_kind = ZBP_DataCompressionKind_ZSTD;

		read_only local_persist u8 transmit_mode_to_orientation[] = {
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

		bp->sample_count            = header->sample_count;
		bp->receive_channel_count   = header->channel_count;
		bp->transmit_channel_count  = header->channel_count;
		bp->xdc_receive_tile_count  = 1;
		bp->xdc_transmit_tile_count = 1;
		bp->acquisition_count       = header->receive_event_count;

		read_only local_persist BeamformerSamplingMode zbp_sampling_mode_to_beamformer[] = {
			[ZBP_SamplingMode_Standard] = BeamformerSamplingMode_4X,
			[ZBP_SamplingMode_Bandpass] = BeamformerSamplingMode_2X,
		};
		bp->sampling_mode = zbp_sampling_mode_to_beamformer[header->sampling_mode];

		bp->acquisition_kind       = header->acquisition_mode;
		bp->decode_mode            = header->decode_mode;
		bp->sampling_frequency     = header->sampling_frequency;
		bp->demodulation_frequency = header->demodulation_frequency;
		bp->speed_of_sound         = header->speed_of_sound;
		bp->time_offset            = header->time_offset;

		bp->contrast_mode          = header->contrast_mode;

		if (header->channel_mapping_offset != -1) {
			memory_copy(bp->channel_mapping, raw.data + header->channel_mapping_offset,
			         sizeof(*bp->channel_mapping) * bp->receive_channel_count);
		} else {
			for EachIndex(bp->receive_channel_count, it)
				bp->channel_mapping[it] = it;
		}

		memory_copy(bp->xdc_transform_matrices + 0, header->transducer_transform_matrix, sizeof(header->transducer_transform_matrix));
		memory_copy(bp->xdc_element_pitch.E,   header->transducer_element_pitch,    sizeof(bp->xdc_element_pitch));
		// NOTE(rnp): ignores group count and ensemble count
		memory_copy(bp->raw_data_dimensions.E, header->raw_data_dimension,          sizeof(bp->raw_data_dimensions));

		{
			f32 gap = 9.6e-3f;
			bp->xdc_receive_tile_count = 2;

			m4 transform;
			memory_copy(transform.E, header->transducer_transform_matrix, sizeof(transform));

			static_assert(TileCount == 2, "");

			u32 iteration = iteration_count++;
			tile_parameters_log[0][iteration].x_translation = transform.c[3].x + gap / 2;
			tile_parameters_log[0][iteration].y_translation = transform.c[3].y;
			tile_parameters_log[1][iteration].x_translation = 0.5f * transform.c[3].x + 1.5f * gap;
			tile_parameters_log[1][iteration].y_translation = transform.c[3].y;

			transform = m4_translation((v3){.x = tile_parameters_log[0][iteration].x_translation,
			                                .y = tile_parameters_log[0][iteration].y_translation});
			memory_copy(bp->xdc_transform_matrices + 0, transform.E, sizeof(transform));

			transform = m4_translation((v3){.x = tile_parameters_log[1][iteration].x_translation,
			                                .y = tile_parameters_log[1][iteration].y_translation});
			memory_copy(bp->xdc_transform_matrices + 1, transform.E, sizeof(transform));
		}

		bp->data_kind              = header->raw_data_kind;
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

		// NOTE(rnp): only look at the first emission descriptor, other cases aren't currently relevant
		{
			ZBP_EmissionDescriptor *ed = (ZBP_EmissionDescriptor *)(raw.data + header->emission_descriptors_offset);
			switch (ed->emission_kind) {

			case ZBP_EmissionKind_Sine:{
				ZBP_EmissionSineParameters *ep = (ZBP_EmissionSineParameters *)(raw.data + ed->parameters_offset);
				bp->emission_parameters.kind           = BeamformerEmissionKind_Sine;
				bp->emission_parameters.sine.cycles    = ep->cycles;
				bp->emission_parameters.sine.frequency = ep->frequency;
			}break;

			case ZBP_EmissionKind_Chirp:{
				ZBP_EmissionChirpParameters *ep = (ZBP_EmissionChirpParameters *)(raw.data + ed->parameters_offset);
				bp->emission_parameters.kind                = BeamformerEmissionKind_Chirp;
				bp->emission_parameters.chirp.duration      = ep->duration;
				bp->emission_parameters.chirp.min_frequency = ep->min_frequency;
				bp->emission_parameters.chirp.max_frequency = ep->max_frequency;
			}break;

			InvalidDefaultCase;
			static_assert(ZBP_EmissionKind_Count == (ZBP_EmissionKind_Chirp + 1), "");
			}
		}

		switch (header->acquisition_mode) {
		case ZBP_AcquisitionKind_FORCES:{}break;

		case ZBP_AcquisitionKind_HERCULES:{
			ZBP_HERCULESParameters *p = (ZBP_HERCULESParameters *)(raw.data + header->acquisition_parameters_offset);
			bp->transmit_receive_orientation = p->transmit_focus.transmit_receive_orientation;
			bp->focal_vector.E[0] = p->transmit_focus.steering_angle;
			bp->focal_vector.E[1] = p->transmit_focus.focal_depth;

			bp->single_focus       = 1;
			bp->single_orientation = 1;
		}break;

		case ZBP_AcquisitionKind_UFORCES:{
			ZBP_uFORCESParameters *p = (ZBP_uFORCESParameters *)(raw.data + header->acquisition_parameters_offset);
			memory_copy(bp->sparse_elements, raw.data + p->sparse_elements_offset,
			         sizeof(*bp->sparse_elements) * bp->acquisition_count);
		}break;

		case ZBP_AcquisitionKind_UHERCULES:{
			ZBP_uHERCULESParameters *p = (ZBP_uHERCULESParameters *)(raw.data + header->acquisition_parameters_offset);
			bp->transmit_receive_orientation = p->transmit_focus.transmit_receive_orientation;
			bp->focal_vector.E[0] = p->transmit_focus.steering_angle;
			bp->focal_vector.E[1] = p->transmit_focus.focal_depth;

			bp->single_focus       = 1;
			bp->single_orientation = 1;

			memory_copy(bp->sparse_elements, raw.data + p->sparse_elements_offset,
			         sizeof(*bp->sparse_elements) * bp->acquisition_count);
		}break;

		case ZBP_AcquisitionKind_RCA_TPW:{
			ZBP_TPWParameters *p = (ZBP_TPWParameters *)(raw.data + header->acquisition_parameters_offset);

			memory_copy(bp->transmit_receive_orientations, raw.data + p->transmit_receive_orientations_offset,
			         sizeof(*bp->transmit_receive_orientations) * bp->acquisition_count);
			memory_copy(bp->steering_angles, raw.data + p->tilting_angles_offset,
			         sizeof(*bp->steering_angles) * bp->acquisition_count);

			for EachIndex(bp->acquisition_count, it)
				bp->focal_depths[it] = inf32();
		}break;

		case ZBP_AcquisitionKind_RCA_VLS:{
			ZBP_VLSParameters *p = (ZBP_VLSParameters *)(raw.data + header->acquisition_parameters_offset);

			memory_copy(bp->transmit_receive_orientations, raw.data + p->transmit_receive_orientations_offset,
			         sizeof(*bp->transmit_receive_orientations) * bp->acquisition_count);

			f32 *focal_depths   = (f32 *)(raw.data + p->focal_depths_offset);
			f32 *origin_offsets = (f32 *)(raw.data + p->origin_offsets_offset);

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

	}break;

	default:{return 0;}break;
	}

	return 1;
}

function b32
send_frame(void *restrict data, BeamformerSimpleParameters *restrict bp, BeamformerViewPlaneTag tag, u32 slot)
{
	u32 data_size = bp->raw_data_dimensions.E[0] * bp->raw_data_dimensions.E[1]
	                * beamformer_data_kind_byte_size[bp->data_kind];
	b32 result    = beamformer_push_data_with_compute(data, data_size, tag, slot);
	if (!result && !g_should_exit) printf("lib error: %s\n", beamformer_get_last_error_string());

	return result;
}

function void
load_parameters(BeamformerSimpleParameters *bp, ZBP_Data *raw_data)
{
	if (!beamformer_simple_parameters_from_zbp_file(DATA_FILE, bp, raw_data))
		die("failed to load parameters file: " DATA_FILE "\n");

	bp->output_points.x    = wire_points;
	bp->f_number           = f_number;
	bp->interpolation_mode = BeamformerInterpolationMode_Cubic;

	bp->decimation_rate = 1;

	if (bp->data_kind != BeamformerDataKind_Float32Complex &&
	    bp->data_kind != BeamformerDataKind_Int16Complex)
	{
		bp->compute_stages[bp->compute_stages_count++] = BeamformerShaderKind_Demodulate;
	}
	bp->compute_stages[bp->compute_stages_count++] = BeamformerShaderKind_Decode;
	bp->compute_stages[bp->compute_stages_count++] = BeamformerShaderKind_DAS;

	BeamformerFilterParameters filter = {.sampling_frequency = bp->sampling_frequency / 2};
	{
		BeamformerEmissionParameters *ep = &bp->emission_parameters;
		switch (bp->emission_parameters.kind) {

		case BeamformerEmissionKind_Sine:{
			filter.kind                    = BeamformerFilterKind_Kaiser;
			filter.kaiser.beta             = 5.65f;
			filter.kaiser.cutoff_frequency = 0.5f * ep->sine.frequency;
			filter.kaiser.length           = 36;
		}break;

		case BeamformerEmissionKind_Chirp:{
			filter.kind                        = BeamformerFilterKind_MatchedChirp;
			filter.matched_chirp.duration      = ep->chirp.duration;
			filter.matched_chirp.min_frequency = ep->chirp.min_frequency - bp->demodulation_frequency;
			filter.matched_chirp.max_frequency = ep->chirp.max_frequency - bp->demodulation_frequency;
			filter.complex                     = 1;

			//bp.time_offset += ep->chirp.duration / 2;
		}break;

		InvalidDefaultCase;
		}

		beamformer_create_filter(&filter, 0, 0);

		bp->compute_stage_parameters[0] = 0;
	}

	beamformer_push_simple_parameters_at(bp, 0);

	beamformer_set_global_timeout(1000);

	if (raw_data->bytes.length == 0)
		die("bp file must contain embedded raw data\n");

	if (raw_data->compression_kind == ZBP_DataCompressionKind_ZSTD) {
		raw_data->bytes = decompress_zstd_data(arena, raw_data->bytes);
		raw_data->compression_kind = ZBP_DataCompressionKind_None;
	}
}

function void
sigint(i32 _signo)
{
	g_should_exit = 1;
}

global u64 g_random_state[4];

function void
init_random(void)
{
	u64 clock = os_timer_count();
	g_random_state[0] = (u64)&g_should_exit ^ ror_u64(clock, 0);
	g_random_state[1] = (u64)&main          ^ ror_u64(clock, 11);
	g_random_state[2] = (u64)&die_          ^ ror_u64(clock, 17);
	g_random_state[3] = (u64)&sigint        ^ ror_u64(clock, 23);
}

function u64
xoroshiro256star(u64 s[4])
{
	u64 ts1 = s[1] * 5;
	u64 result = ((ts1 << 7) | (ts1 >> 57)) * 9;
	u64 t = s[1] << 17;
	s[2] ^= s[0];
	s[3] ^= s[1];
	s[1] ^= s[2];
	s[0] ^= s[3];
	s[2] ^= t;
	s[3] = ((s[3] << 45) | (s[3] >> 19));
	return result;
}

function f64
random_uniform(void)
{
	return xoroshiro256star(g_random_state) / (f64)UINT64_MAX;
}

function void
start_beamforming(BeamformerSimpleParameters *restrict bp, void *restrict data)
{
	bp->output_points.x = wire_points;
	bp->output_points.y = 1;
	bp->output_points.z = 1;

	for EachElement(wire_targets, wire) {
		v3 p = {.x = wire_targets[wire].x, .z = wire_targets[wire].y};
		bp->das_voxel_transform = das_transform_1d(v3_add(p, (v3){.x = -region_width / 2.f}),
		                                           v3_add(p, (v3){.x =  region_width / 2.f}));
		beamformer_push_parameters_at((BeamformerParameters *)bp, 0);
		send_frame(data, bp, BeamformerViewPlaneTag_XY, 0);

		bp->das_voxel_transform = das_transform_1d(v3_add(p, (v3){.z = -region_width / 2.f}),
		                                           v3_add(p, (v3){.z =  region_width / 2.f}));
		beamformer_push_parameters_at((BeamformerParameters *)bp, 0);
		send_frame(data, bp, BeamformerViewPlaneTag_YZ, 0);
	}
}

function void
iteration(BeamformerSimpleParameters *bp)
{
	// NOTE(rnp): make sure first iteration is always an improvement
	// so we don't need to special case it
	local_persist f32 last_average_resolution = 100.f;

	// NOTE(rnp): temperature for annealing; starting value doesn't matter too much
	local_persist f64 T = 1e2;

	f32 average_resolution = 0;
	for EachElement(resolutions, it)
		//average_resolution += resolutions[it] / (f32)countof(resolutions);
		average_resolution = Max(average_resolution, resolutions[it]);

	f32 dResolution = average_resolution - last_average_resolution;
	last_average_resolution = average_resolution;

	u32 iteration = iteration_count++;
	for EachIndex(TileCount, tile) {
		if (dResolution < 0) {
			// NOTE(rnp): improvement; keep last parameters
			tile_parameters_log[tile][iteration] = tile_parameters_log[tile][iteration - 1];
		} else {
			// NOTE(rnp): worse result; use annealed probability to determine acceptance
			f64 P = exp_f64(-dResolution / T);
			f64 R = random_uniform();
			if (R < P)
				tile_parameters_log[tile][iteration] = tile_parameters_log[tile][iteration - 1];
			else
				tile_parameters_log[tile][iteration] = tile_parameters_log[tile][iteration - 2];
		}
	}

	// NOTE(rnp): reduce temperature for next iteration
	T *= 0.99;

	static_assert(countof(tile_parameters_log[0][0].E) == 5, "");
	// NOTE(rnp): alpha, beta, gamma, x, y
	f32 scales[5] = {0.03f, 0.03f, 0.03f, 0.0005f, 0.f};
	//for EachIndex(TileCount, tile) {
	{ u32 tile = 1;
		TileParameters *t = tile_parameters_log[tile] + iteration;
		for EachElement(t->E, it)
			t->E[it] += scales[it] * (2.0 * random_uniform() - 1.0);
	}

	for EachIndex(TileCount, tile) {
		TileParameters *t = tile_parameters_log[tile] + iteration;
		m4 R = m4_rotation_from_euler(t->alpha, t->beta, t->gamma);
		m4 T = m4_translation((v3){.x = t->x_translation, .y = t->y_translation});
		m4 transform = m4_mul(T, R);
		memory_copy(bp->xdc_transform_matrices + tile, transform.E, sizeof(transform));
	}

	beamformer_push_simple_parameters_at(bp, 0);
}

function void
optimize(void)
{
	BeamformerSimpleParameters bp = {0};
	ZBP_Data raw_data = {0};

	// NOTE(rnp): load dataset, setup initial parameters
	if (lane_index() == 0) {
		load_parameters(&bp, &raw_data);
		start_beamforming(&bp, raw_data.bytes.data);
	}

	u64 image_size = AlignUpPowerOfTwo(sizeof(v2) * wire_points, 64);
	v2 *frame_readback_buffer;
	if (lane_index() == 0)
	{
		frame_readback_buffer = arena_alloc(arena, .size = image_size, .count = OutputImages,
		                                    .align = 64, .flags = ArenaAllocateFlags_NoZero);
	}

	lane_sync_u64(&frame_readback_buffer, 0);

	for (;iteration_count < countof(*tile_parameters_log) && !g_should_exit;) {
		////////////////////////////
		// NOTE(rnp): update parameters
		lane_sync();
		if (lane_index() == 0)
			iteration(&bp);

		////////////////////////////
		// NOTE(rnp): fetch results
		if (lane_index() == 0) {
			u64 readback_buffer_size = image_size * OutputImages;
			b32 result = beamformer_get_last_frames(frame_readback_buffer, readback_buffer_size, OutputImages);
			if unlikely(!result)
				printf("lib error: %s\n", beamformer_get_last_error_string());
		}

		////////////////////////////
		// NOTE(rnp): start next iteration
		if (lane_index() == 0)
			start_beamforming(&bp, raw_data.bytes.data);

		lane_sync();

		////////////////////////////
		// NOTE(rnp): compute metrics
		RangeU64 range = lane_range(OutputImages);
		for (u64 frame = range.start; frame < range.stop; frame++) {
			f32  max_value = 0;
			v2  *image     = (v2 *)((u8 *)frame_readback_buffer + image_size * frame);
			for EachIndex(wire_points, index) {
				f32 value = v2_magnitude_squared(image[index]);
				max_value = Max(max_value, value);
			}
			max_output_values[frame] = max_value;

			u32 first_resolution_index = 0;
			for EachIndex(wire_points, index) {
				f32 value = v2_magnitude_squared(image[index]);
				if (value >= 0.5f * max_value) {
					first_resolution_index = index;
					break;
				}
			}

			u32 last_resolution_index = 0;
			for EachIndex(wire_points, index) {
				f32 value = v2_magnitude_squared(image[wire_points - 1 - index]);
				if (value >= 0.5f * max_value) {
					last_resolution_index = wire_points - 1 - index;
					break;
				}
			}

			f32 axis_inc = region_width / (wire_points - 1);
			if (last_resolution_index > first_resolution_index)
				resolutions[frame] = axis_inc * (last_resolution_index - first_resolution_index);
			else
				resolutions[frame] = 10.f;

			///u32 max_value_index = 0;
			///for EachIndex(wire_points, index) {
			///	f32 value = v2_magnitude_squared(image[index]);
			///	if (value == max_value) {
			///		max_value_index = index;
			///		break;
			///	}
			///}
			///u32 wire_bin = frame / 2;
			///u32 dir_bin  = frame % 2;
			///wire_targets[wire_bin].E[dir_bin] = wire_targets[wire_bin].E[dir_bin] - region_width / 2.f + axis_inc * max_value_index;
		}
	}

	if (lane_index() == 0) {
		v2 min_coordinate = {.x = lateral_extent.x, .y = axial_extent.x};
		v2 max_coordinate = {.x = lateral_extent.y, .y = axial_extent.y};
		bp.output_points.x = 512;
		bp.output_points.y = 512;
		bp.das_voxel_transform = das_transform_2d_xz(min_coordinate, max_coordinate, 0);
		beamformer_push_parameters_at((BeamformerParameters *)&bp, 0);

		send_frame(raw_data.bytes.data, &bp, BeamformerViewPlaneTag_XZ, 0);
	}
}

function OS_THREAD_ENTRY_POINT_FN(thread_entry_point)
{
	lane_context(user_context);
	optimize();
	return 0;
}

BASE_IMPORT void
entry_point(i32 argc, char *argv[])
{
	signal(SIGINT, sigint);

	init_random();

	arena = arena_create();

	u32 thread_count = Min(OutputImages, os_system_info()->logical_processor_count);
	ThreadContext *threads = push_array(arena, ThreadContext, thread_count);
	OSBarrier      barrier = os_barrier_alloc(thread_count);

	local_persist u64 broadcast_memory;
	for EachIndex(thread_count, it) {
		Stream sb = stream_from_buffer(threads[it].name, countof(threads[it].name) - 1);
		stream_append_str8(&sb, str8("[worker "));
		stream_append_u64(&sb, it);
		stream_append_byte(&sb, ']');

		threads[it].lane_context.count   = thread_count;
		threads[it].lane_context.index   = it;
		threads[it].lane_context.barrier = barrier;
		threads[it].lane_context.broadcast_memory = &broadcast_memory;

		if (it != 0) os_create_thread((char *)threads[it].name, threads + it, thread_entry_point);
	}

	thread_entry_point(threads + 0);
}
