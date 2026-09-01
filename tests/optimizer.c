/* See LICENSE for license details. */

#include "test_common.c"
#include "threads.c"

#include <inttypes.h>

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
	//{{  1.3e-3f, 105.5e-3f}},
	{{  4.2e-3f, 86.4e-3f}},
	//{{  3.6e-3f,  20.2e-3f}},
	//{{  3.4e-3f,  58.4e-3f}},
	//{{ 23.2e-3f,  48.9e-3f}},
	//{{-15.1e-3f,  48.7e-3f}},
};
global f32 region_width = 8e-3;

#define OutputImages (countof(wire_targets) * 2)

global u64 iteration_count;
global f32 max_output_values[OutputImages];
global f32 resolutions[OutputImages];

#define TileCount 2
global TileParameters tile_parameters_log[TileCount][1 << 14];

global b32 g_should_exit;

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
	if (!beamformer_simple_parameters_from_zbp_file(arena, bp, DATA_FILE, raw_data, 0))
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
		send_frame(data, bp, BeamformerViewPlaneTag_XZ, 0);
	}
}

function void
iteration(BeamformerSimpleParameters *bp)
{
	printf("Resolutions:\n");
	for EachElement(resolutions, it)
		printf("%0.3e\n", resolutions[it]);
	// NOTE(rnp): make sure first iteration is always an improvement
	// so we don't need to special case it

	// NOTE(rnp): temperature for annealing; starting value doesn't matter too much
	local_persist f64 T = 1e-5;

	local_persist f32 last_average_resolution = 100.f;
	f32 average_resolution = 0;
	for EachElement(resolutions, it)
		average_resolution += resolutions[it] / (f32)countof(resolutions);
		//average_resolution = Max(average_resolution, resolutions[it]);

	f32 dResolution = average_resolution - last_average_resolution;
	last_average_resolution = average_resolution;

	local_persist f32 last_average_maximum = 0.f;
	f32 average_maximum = max_output_values[0];
	for EachElement(max_output_values, it)
		average_maximum += max_output_values[it] / (f32)countof(max_output_values);
		//average_maximum = Min(max_output_values[it], average_maximum);

	f32 dMax = last_average_maximum - average_maximum;
	last_average_maximum = average_maximum;

	(void)dMax; (void)dResolution;
	f32 dTest = dMax;

	u32 iteration = iteration_count++;
	for EachIndex(TileCount, tile) {
		if (dTest < 0) {
			// NOTE(rnp): improvement; keep last parameters
			tile_parameters_log[tile][iteration] = tile_parameters_log[tile][iteration - 1];
		} else {
			// NOTE(rnp): worse result; use annealed probability to determine acceptance
			f64 P = exp_f64(-dTest / T);
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
	f32 scales[5] = {0.02f, 0.02f, 0.02f, 0.001f, 0.f};
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

#if ARCH_X64 && defined(__AVX512F__)
function force_inline __m512
packed_v2_magnitude_squared(__m512 v)
{
	__m512 result;
	result = _mm512_mul_ps(v, v);
	result = _mm512_add_ps(result, _mm512_permute_ps(result, 0xB1));
	return result;
}
#endif

function void
optimize(void)
{
	BeamformerSimpleParameters bp = {0};
	ZBP_Data raw_data = {0};

	// NOTE(rnp): load dataset, setup initial parameters
	if (lane_index() == 0) {
		load_parameters(&bp, &raw_data);
		if (raw_data.bytes.length == 0)
			die("bp file must contain embedded raw data\n");
		start_beamforming(&bp, zbp_data_pointer(arena, &raw_data, (Stream){0}, 0));
	}

	u64 image_size = AlignUpPowerOfTwo(sizeof(v2) * wire_points, 64);
	v2 *frame_readback_buffer = 0;
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
			f32 max_value = 0;

			#if ARCH_X64 && defined(__AVX512F__)
			f32 *image = (f32 *)((u8 *)frame_readback_buffer + image_size * frame);
			{
				__m512 maxv = {0};
				for (u32 index = 0; index < 2 * wire_points; index += 16) {
					__m512 value = packed_v2_magnitude_squared(_mm512_load_ps(image + index));
					maxv = _mm512_max_ps(maxv, value);
				}
				max_value = _mm512_reduce_max_ps(maxv);
			}
			#else
			v2 *image = (v2 *)((u8 *)frame_readback_buffer + image_size * frame);
			{
				for EachIndex(wire_points, index) {
					f32 value = v2_magnitude_squared(image[index]);
					max_value = Max(max_value, value);
				}
			}
			#endif

			max_output_values[frame] = max_value;

			i32 first_resolution_index = -1;
			i32 last_resolution_index  = -1;

			f32 fwhm_fraction = 0.5f;

			#if ARCH_X64 && defined(__AVX512F__)
			__m512 threshv = _mm512_set1_ps(fwhm_fraction * max_value);
			for (u32 index = 0; index < 2 * wire_points; index += 16) {
				__m512    value = packed_v2_magnitude_squared(_mm512_load_ps(image + index));
				__mmask16 mask  = _mm512_cmp_ps_mask(value, threshv, _CMP_GE_OQ);
				if (mask != 0) {
					first_resolution_index = (i32)(index + ctz_u64(mask));
					break;
				}
			}
			#else
			for EachIndex(wire_points, index) {
				f32 value = v2_magnitude_squared(image[index]);
				if (value >= fwhm_fraction * max_value) {
					first_resolution_index = index;
					break;
				}
			}
			#endif

			if (first_resolution_index >= 0) {
				#if ARCH_X64 && defined(__AVX512F__)
				u32 start_chunk = (first_resolution_index / 16) * 16;
				for (u32 index = start_chunk; index < 2 * wire_points; index += 16) {
					__m512    value = packed_v2_magnitude_squared(_mm512_load_ps(image + index));
					__mmask16 mask  = _mm512_cmp_ps_mask(value, threshv, _CMP_LE_OQ);

					// NOTE(rnp): if we are still in the same chunk we need to mask
					// off values before the first crossing
					if (index == start_chunk)
						mask &= ~((1u << ((first_resolution_index % 16) + 1)) - 1);

					if (mask != 0) {
						last_resolution_index = (i32)(index + ctz_u64(mask));
						break;
					}
				}
				// NOTE(rnp): we computed f32 indices but we need v2 indices
				first_resolution_index /= 2;
				last_resolution_index  /= 2;
				#else
				for EachIndex(wire_points, index) {
					f32 value = v2_magnitude_squared(image[wire_points - 1 - index]);
					if (value >= 0.5f * max_value) {
						last_resolution_index = wire_points - 1 - index;
						break;
					}
				}
				#endif
			}

			f32 axis_inc = region_width / (wire_points - 1);
			if (last_resolution_index > 0)
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

		send_frame(raw_data.bytes.data, &bp, BeamformerViewPlaneTag_YZ, 0);
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

	printf("Seed: {0x%016"PRIu64", 0x%016"PRIu64", 0x%016"PRIu64", 0x%016"PRIu64"}\n",
	       g_random_state[0], g_random_state[1], g_random_state[2], g_random_state[3]);

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

	printf("Iteration Count: %"PRIu64"\n", iteration_count);
}
