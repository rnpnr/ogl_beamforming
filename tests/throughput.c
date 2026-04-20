/* See LICENSE for license details. */
/* TODO(rnp):
 * [ ]: for finer grained evaluation of throughput latency just queue a data upload
 *      without replacing the data.
 * [ ]: bug: we aren't inserting rf data between each frame
 */

#define BEAMFORMER_LIB_EXPORT function
#include "ogl_beamformer_lib.c"

#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <zstd.h>

global iv3 g_output_points    = {{512, 1, 1024}};
global v2  g_axial_extent     = {{ 10e-3f, 165e-3f}};
global v2  g_lateral_extent   = {{-60e-3f,  60e-3f}};
global f32 g_f_number         = 0.5f;

typedef struct {
	b32 loop;
	b32 cuda;
	u32 frame_number;

	char **remaining;
	i32    remaining_count;
} Options;

#include "external/zemp_bp.h"

typedef struct {
	ZBP_DataKind            kind;
	ZBP_DataCompressionKind compression_kind;
	s8                      bytes;
} ZBP_Data;

global b32 g_should_exit;

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

#if OS_LINUX

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

function s8
os_read_file_simp(char *fname)
{
	s8 result;
	i32 fd = open(fname, O_RDONLY);
	if (fd < 0)
		die("couldn't open file: %s\n", fname);

	struct stat st;
	if (stat(fname, &st) < 0)
		die("couldn't stat file\n");

	result.len  = st.st_size;
	result.data = malloc((uz)st.st_size);
	if (!result.data)
		die("couldn't alloc space for reading\n");

	iz rlen = read(fd, result.data, (u32)st.st_size);
	close(fd);

	if (rlen != st.st_size)
		die("couldn't read file: %s\n", fname);

	return result;
}

#elif OS_WINDOWS

function s8
os_read_file_simp(char *fname)
{
	s8 result;
	iptr h = CreateFileA(fname, GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);
	if (h == INVALID_FILE)
		die("couldn't open file: %s\n", fname);

	w32_file_info fileinfo;
	if (!GetFileInformationByHandle(h, &fileinfo))
		die("couldn't get file info\n", stderr);

	result.len  = fileinfo.nFileSizeLow;
	result.data = malloc(fileinfo.nFileSizeLow);
	if (!result.data)
		die("couldn't alloc space for reading\n");

	i32 rlen = 0;
	if (!ReadFile(h, result.data, (i32)fileinfo.nFileSizeLow, &rlen, 0) && rlen != (i32)fileinfo.nFileSizeLow)
		die("couldn't read file: %s\n", fname);
	CloseHandle(h);

	return result;
}

#else
#error Unsupported Platform
#endif

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

function void *
decompress_zstd_data(s8 raw)
{
	uz requested_size = ZSTD_getFrameContentSize(raw.data, (uz)raw.len);
	void *out         = malloc(requested_size);
	if (out) {
		uz decompressed = ZSTD_decompress(out, requested_size, raw.data, (uz)raw.len);
		if (decompressed != requested_size) {
			free(out);
			out = 0;
		}
	}
	return out;
}

function b32
beamformer_simple_parameters_from_zbp_file(BeamformerSimpleParameters *bp, char *path, ZBP_Data *raw_data)
{
	s8 raw = os_read_file_simp(path);
	if (raw.len < (iz)sizeof(ZBP_BaseHeader) || ((ZBP_BaseHeader *)raw.data)->magic != ZBP_HeaderMagic)
		return 0;

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

		mem_copy(bp->channel_mapping,       header->channel_mapping,             sizeof(*bp->channel_mapping) * bp->channel_count);
		mem_copy(bp->xdc_transform.E,       header->transducer_transform_matrix, sizeof(bp->xdc_transform));
		mem_copy(bp->xdc_element_pitch.E,   header->transducer_element_pitch,    sizeof(bp->xdc_element_pitch));
		// NOTE(rnp): ignores emission count and ensemble count
		mem_copy(bp->raw_data_dimensions.E, header->raw_data_dimension,          sizeof(bp->raw_data_dimensions));

		bp->data_kind              = ZBP_DataKind_Int16;
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
			mem_copy(bp->sparse_elements, header->sparse_elements, sizeof(*bp->sparse_elements) * bp->acquisition_count);
		}

		if (acquisition_kind == ZBP_AcquisitionKind_RCA_TPW ||
		    acquisition_kind == ZBP_AcquisitionKind_RCA_VLS)
		{
			mem_copy(bp->focal_depths,    header->focal_depths,    sizeof(*bp->focal_depths) * bp->acquisition_count);
			mem_copy(bp->steering_angles, header->steering_angles, sizeof(*bp->steering_angles) * bp->acquisition_count);
			for EachIndex(bp->acquisition_count, it)
				bp->transmit_receive_orientations[it] = bp->transmit_receive_orientation;
		}

		bp->emission_kind = BeamformerEmissionKind_Sine;
		bp->emission_parameters.sine.cycles    = 2;
		bp->emission_parameters.sine.frequency = bp->demodulation_frequency;
	}break;

	case 2:{
		ZBP_HeaderV2 *header       = (ZBP_HeaderV2 *)raw.data;

		bp->sample_count           = header->sample_count;
		bp->channel_count          = header->channel_count;
		bp->acquisition_count      = header->receive_event_count;

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
			mem_copy(bp->channel_mapping, raw.data + header->channel_mapping_offset,
			         sizeof(*bp->channel_mapping) * bp->channel_count);
		} else {
			for EachIndex(bp->channel_count, it)
				bp->channel_mapping[it] = it;
		}

		mem_copy(bp->xdc_transform.E,       header->transducer_transform_matrix, sizeof(bp->xdc_transform));
		mem_copy(bp->xdc_element_pitch.E,   header->transducer_element_pitch,    sizeof(bp->xdc_element_pitch));
		// NOTE(rnp): ignores group count and ensemble count
		mem_copy(bp->raw_data_dimensions.E, header->raw_data_dimension,          sizeof(bp->raw_data_dimensions));

		bp->data_kind              = header->raw_data_kind;
		raw_data->kind             = header->raw_data_kind;
		raw_data->compression_kind = header->raw_data_compression_kind;

		if (header->raw_data_offset != -1) {
			raw_data->bytes.data = raw.data + header->raw_data_offset;
			if (raw_data->compression_kind == ZBP_DataCompressionKind_ZSTD) {
				// NOTE(rnp): limitation in the header format
				raw_data->bytes.len  = raw.len - header->raw_data_offset;
			} else {
				raw_data->bytes.len  = header->raw_data_dimension[0] * header->raw_data_dimension[1] *
				                       header->raw_data_dimension[2] * header->raw_data_dimension[3];
				raw_data->bytes.len *= beamformer_data_kind_byte_size[header->raw_data_kind];
			}
		}

		// NOTE(rnp): only look at the first emission descriptor, other cases aren't currently relevant
		{
			ZBP_EmissionDescriptor *ed = (ZBP_EmissionDescriptor *)(raw.data + header->emission_descriptors_offset);
			switch (ed->emission_kind) {

			case ZBP_EmissionKind_Sine:{
				ZBP_EmissionSineParameters *ep = (ZBP_EmissionSineParameters *)(raw.data + ed->parameters_offset);
				bp->emission_kind = BeamformerEmissionKind_Sine;
				bp->emission_parameters.sine.cycles    = ep->cycles;
				bp->emission_parameters.sine.frequency = ep->frequency;
			}break;

			case ZBP_EmissionKind_Chirp:{
				ZBP_EmissionChirpParameters *ep = (ZBP_EmissionChirpParameters *)(raw.data + ed->parameters_offset);
				bp->emission_kind = BeamformerEmissionKind_Chirp;
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
			mem_copy(bp->sparse_elements, raw.data + p->sparse_elements_offset,
			         sizeof(*bp->sparse_elements) * bp->acquisition_count);
		}break;

		case ZBP_AcquisitionKind_UHERCULES:{
			ZBP_uHERCULESParameters *p = (ZBP_uHERCULESParameters *)(raw.data + header->acquisition_parameters_offset);
			bp->transmit_receive_orientation = p->transmit_focus.transmit_receive_orientation;
			bp->focal_vector.E[0] = p->transmit_focus.steering_angle;
			bp->focal_vector.E[1] = p->transmit_focus.focal_depth;

			bp->single_focus       = 1;
			bp->single_orientation = 1;

			mem_copy(bp->sparse_elements, raw.data + p->sparse_elements_offset,
			         sizeof(*bp->sparse_elements) * bp->acquisition_count);
		}break;

		case ZBP_AcquisitionKind_RCA_TPW:{
			ZBP_TPWParameters *p = (ZBP_TPWParameters *)(raw.data + header->acquisition_parameters_offset);

			mem_copy(bp->transmit_receive_orientations, raw.data + p->transmit_receive_orientations_offset,
			         sizeof(*bp->transmit_receive_orientations) * bp->acquisition_count);
			mem_copy(bp->steering_angles, raw.data + p->tilting_angles_offset,
			         sizeof(*bp->steering_angles) * bp->acquisition_count);

			for EachIndex(bp->acquisition_count, it)
				bp->focal_depths[it] = F32_INFINITY;
		}break;

		case ZBP_AcquisitionKind_RCA_VLS:{
			ZBP_VLSParameters *p = (ZBP_VLSParameters *)(raw.data + header->acquisition_parameters_offset);

			mem_copy(bp->transmit_receive_orientations, raw.data + p->transmit_receive_orientations_offset,
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

#define shift_n(v, c, n) v += n, c -= n
#define shift(v, c) shift_n(v, c, 1)

function void
usage(char *argv0)
{
	die("%s [--loop] [--cuda] [--frame n] parameters_file\n"
	    "    --loop:    reupload data forever\n"
	    "    --cuda:    use cuda for decoding\n"
	    "    --frame n: use frame n of the data for display\n",
	    argv0);
}

function Options
parse_argv(i32 argc, char *argv[])
{
	Options result = {0};

	char *argv0 = argv[0];
	shift(argv, argc);

	while (argc > 0) {
		s8 arg = c_str_to_s8(*argv);

		if (s8_equal(arg, s8("--loop"))) {
			shift(argv, argc);
			result.loop = 1;
		} else if (s8_equal(arg, s8("--cuda"))) {
			shift(argv, argc);
			result.cuda = 1;
		} else if (s8_equal(arg, s8("--frame"))) {
			shift(argv, argc);
			if (argc) {
				result.frame_number = (u32)atoi(*argv);
				shift(argv, argc);
			}
		} else if (arg.len > 0 && arg.data[0] == '-') {
			usage(argv0);
		} else {
			break;
		}
	}

	result.remaining       = argv;
	result.remaining_count = argc;

	return result;
}

function b32
send_frame(void *restrict data, BeamformerSimpleParameters *restrict bp)
{
	u32 data_size = bp->raw_data_dimensions.E[0] * bp->raw_data_dimensions.E[1]
	                * beamformer_data_kind_byte_size[bp->data_kind];
	b32 result    = beamformer_push_data_with_compute(data, data_size, BeamformerViewPlaneTag_XZ, 0);
	if (!result && !g_should_exit) printf("lib error: %s\n", beamformer_get_last_error_string());

	return result;
}

function void
execute_study(Arena arena, Stream path, Options *options)
{
	i32 path_work_index = path.widx;
	stream_ensure_termination(&path, 0);

	ZBP_Data raw_data = {0};
	BeamformerSimpleParameters bp = {0};
	if (!beamformer_simple_parameters_from_zbp_file(&bp, (char *)path.data, &raw_data))
		die("failed to load parameters file: %s\n", (char *)path.data);

	v3 min_coordinate = (v3){{g_lateral_extent.x, g_axial_extent.x, 0}};
	v3 max_coordinate = (v3){{g_lateral_extent.y, g_axial_extent.y, 0}};
	bp.das_voxel_transform = das_transform(min_coordinate, max_coordinate, &g_output_points);

	bp.output_points.xyz = g_output_points;
	bp.output_points.w   = 1;

	bp.f_number           = g_f_number;
	bp.interpolation_mode = BeamformerInterpolationMode_Cubic;

	bp.decimation_rate = 1;

	if (bp.data_kind != BeamformerDataKind_Float32Complex &&
	    bp.data_kind != BeamformerDataKind_Int16Complex)
	{
		bp.compute_stages[bp.compute_stages_count++] = BeamformerShaderKind_Demodulate;
	}
	if (options->cuda) bp.compute_stages[bp.compute_stages_count++] = BeamformerShaderKind_CudaDecode;
	else               bp.compute_stages[bp.compute_stages_count++] = BeamformerShaderKind_Decode;
	bp.compute_stages[bp.compute_stages_count++] = BeamformerShaderKind_DAS;

	{
		BeamformerFilterParameters filter = {0};
		BeamformerFilterKind filter_kind  = 0;
		b32 complex = 0;
		u32 size    = 0;

		BeamformerEmissionParameters *ep = &bp.emission_parameters;
		switch (bp.emission_kind) {

		case BeamformerEmissionKind_Sine:{
			filter_kind = BeamformerFilterKind_Kaiser;
			filter.kaiser.beta             = 5.65f;
			filter.kaiser.cutoff_frequency = 0.5f * ep->sine.frequency;
			filter.kaiser.length           = 36;
			size = sizeof(filter.kaiser);
		}break;

		case BeamformerEmissionKind_Chirp:{
			filter_kind = BeamformerFilterKind_MatchedChirp;

			filter.matched_chirp.duration      = ep->chirp.duration;
			filter.matched_chirp.min_frequency = ep->chirp.min_frequency - bp.demodulation_frequency;
			filter.matched_chirp.max_frequency = ep->chirp.max_frequency - bp.demodulation_frequency;
			size = sizeof(filter.matched_chirp);
			complex = 1;

			//bp.time_offset += ep->chirp.duration / 2;
		}break;

		InvalidDefaultCase;
		}

		beamformer_create_filter(filter_kind, (f32 *)&filter.kaiser, size, bp.sampling_frequency / 2,
		                         complex, 0, 0);

		bp.compute_stage_parameters[0] = 0;
	}

	beamformer_push_simple_parameters(&bp);

	beamformer_set_global_timeout(1000);

	void *data = 0;
	if (raw_data.bytes.len == 0) {
		// NOTE(rnp): strip ".bp"
		stream_reset(&path, path_work_index - 3);

		stream_append_byte(&path, '_');
		stream_append_u64_width(&path, options->frame_number, 2);
		stream_append_s8(&path, s8(".zst"));
		stream_ensure_termination(&path, 0);
		s8 compressed_data = os_read_file_simp((char *)path.data);

		data = decompress_zstd_data(compressed_data);
		if (!data)
			die("failed to decompress data: %s\n", path.data);
		free(compressed_data.data);
	} else {
		if (raw_data.compression_kind == ZBP_DataCompressionKind_ZSTD) {
			data = decompress_zstd_data(raw_data.bytes);
			if (!data)
				die("failed to decompress data: %s\n", path.data);
		} else {
			data = raw_data.bytes.data;
		}
	}

	if (options->loop) {
		BeamformerLiveImagingParameters lip = {.active = 1, .save_enabled = 1};
		s8 short_name = s8("Throughput");
		mem_copy(lip.save_name_tag, short_name.data, (uz)short_name.len);
		lip.save_name_tag_length = (i32)short_name.len;
		beamformer_set_live_parameters(&lip);

		u32 frame = 0;
		f32 times[32] = {0};
		f32 data_size = (f32)(bp.raw_data_dimensions.E[0] * bp.raw_data_dimensions.E[1]
		                      * beamformer_data_kind_byte_size[bp.data_kind]);
		u64 start = os_timer_count();
		f64 frequency = os_timer_frequency();
		for (;!g_should_exit;) {
			if (send_frame(data, &bp)) {
				u64 now   = os_timer_count();
				f64 delta = (now - start) / frequency;
				start = now;

				if ((frame % 16) == 0) {
					f32 sum = 0;
					for (u32 i = 0; i < countof(times); i++)
						sum += times[i] / countof(times);
					printf("Frame Time: %8.3f [ms] | 32-Frame Average: %8.3f [ms] | %8.3f GB/s\n",
					       delta * 1e3, sum * 1e3, data_size / (sum * (GB(1))));
				}

				times[frame % countof(times)] = delta;
				frame++;
			}
			i32 flag = beamformer_live_parameters_get_dirty_flag();
			if (flag != -1 && (1 << flag) == BeamformerLiveImagingDirtyFlags_StopImaging)
				break;
		}

		lip.active = 0;
		beamformer_set_live_parameters(&lip);
	} else {
		send_frame(data, &bp);
	}
}

function void
sigint(i32 _signo)
{
	g_should_exit = 1;
}

extern i32
main(i32 argc, char *argv[])
{
	Options options = parse_argv(argc, argv);

	if (options.remaining_count != 1)
		usage(argv[0]);

	signal(SIGINT, sigint);

	Arena arena = os_alloc_arena(KB(8));
	Stream path = stream_alloc(&arena, KB(4));
	stream_append_s8(&path, c_str_to_s8(options.remaining[0]));

	execute_study(arena, path, &options);

	return 0;
}
