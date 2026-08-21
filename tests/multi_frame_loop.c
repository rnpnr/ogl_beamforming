/* See LICENSE for license details. */
#include "test_common.c"

global iv3 g_output_points    = {{512, 1, 256}};
global v2  g_axial_extent     = {{ 8e-3f, 27e-3f}};
global v2  g_lateral_extent   = {{-22e-3f,  22e-3f}};
//global v2  g_axial_extent     = {{ 20e-3f,  48e-3f}};
//global v2  g_lateral_extent   = {{-26e-3f,  26e-3f}};
global f32 g_f_number         = 0.5f;

global b32 g_should_exit;

function void
usage(char *argv0)
{
	die("%s parameters_file\n", argv0);
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
execute_study(Arena *arena, Stream path)
{
	i32 path_work_index = path.widx;
	stream_ensure_termination(&path, 0);

	u32 data_frame_count;
	ZBP_Data raw_data = {0};
	BeamformerSimpleParameters bp = {0};
	if (!beamformer_simple_parameters_from_zbp_file(arena, &bp, (char *)path.data, &raw_data, &data_frame_count))
		die("failed to load parameters file: %s\n", (char *)path.data);
	stream_reset(&path, path_work_index);

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
	bp.compute_stages[bp.compute_stages_count++] = BeamformerShaderKind_Decode;
	bp.compute_stages[bp.compute_stages_count++] = BeamformerShaderKind_DAS;

	BeamformerFilterParameters filter = {.sampling_frequency = bp.sampling_frequency / 2};
	{
		BeamformerEmissionParameters *ep = &bp.emission_parameters;
		switch (bp.emission_parameters.kind) {

		case BeamformerEmissionKind_Sine:{
			filter.kind                    = BeamformerFilterKind_Kaiser;
			filter.kaiser.beta             = 5.65f;
			filter.kaiser.cutoff_frequency = 0.5f * ep->sine.frequency;
			filter.kaiser.length           = 36 * ep->sine.cycles / 2.f;
		}break;

		case BeamformerEmissionKind_Chirp:{
			filter.kind                        = BeamformerFilterKind_MatchedChirp;
			filter.matched_chirp.duration      = ep->chirp.duration;
			filter.matched_chirp.min_frequency = ep->chirp.min_frequency - bp.demodulation_frequency;
			filter.matched_chirp.max_frequency = ep->chirp.max_frequency - bp.demodulation_frequency;
			filter.complex                     = 1;

			//bp.time_offset += ep->chirp.duration / 2;
		}break;

		InvalidDefaultCase;
		}

		beamformer_create_filter(&filter, 0, 0);

		bp.compute_stage_parameters[0] = 0;
	}

	bp.doppler = 1;
	bp.doppler_ensemble = 16;
	bp.doppler_order    = 2;

	beamformer_push_simple_parameters(&bp);

	beamformer_set_global_timeout(1000);

	void *data = zbp_data_pointer(arena, &raw_data, path, 0);

	BeamformerLiveImagingParameters lip = {
		.active = 1,
		.acquisition_kind = bp.acquisition_kind,
		.save_enabled = 1,
		.acquisition_kind_enabled_flags = 1 << bp.acquisition_kind,
	};

	str8 short_name = str8("MultiFrame");
	memory_copy(lip.save_name_tag, short_name.data, (u64)short_name.length);
	lip.save_name_tag_length = (i32)short_name.length;
	beamformer_set_live_parameters(&lip);

	u32 frame = 0;
	f32 times[32] = {0};

	u32 data_bytes = (bp.raw_data_dimensions.E[0] * bp.raw_data_dimensions.E[1]
	                  * beamformer_data_kind_byte_size[bp.data_kind]);
	f32 data_size  = (f32)(data_bytes);
	u64 start = os_timer_count();
	f64 frequency = os_timer_frequency();
	for (;!g_should_exit;) {
		void *current_data = (u8 *)data + data_bytes * (frame % data_frame_count);
		if (send_frame(current_data, &bp, BeamformerViewPlaneTag_XZ, 0)) {
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
}

function void
sigint(i32 _signo)
{
	g_should_exit = 1;
}

BASE_IMPORT void
entry_point(i32 argc, char *argv[])
{
	if (argc != 2)
		usage(argv[0]);

	signal(SIGINT, sigint);

	Arena  *arena = arena_create();
	Stream  path  = stream_alloc(arena, KB(4));
	stream_append_str8(&path, str8_from_c_str(argv[1]));

	execute_study(arena, path);
}
