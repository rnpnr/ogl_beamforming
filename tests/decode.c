/* See LICENSE for license details. */
#define LIB_FN function
#include "ogl_beamformer_lib.c"

#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define AVERAGE_SAMPLES countof(((BeamformerComputeStatsTable *)0)->times)
//#define RF_TIME_SAMPLES 2432
#define RF_TIME_SAMPLES 4096

read_only global u32 decode_transmit_counts[] = {
	2, 4, 8, 12, 16, 20, 24, 32, 40, 48, 64, 80, 96, 128, 160, 192, 256
};

typedef struct {
	b32 loop;
	b32 cuda;
	b32 once;
	b32 dump;
	b32 full_aperture;

	u32 warmup_count;

	char *outdir;

	char **remaining;
	i32    remaining_count;
} Options;

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

function void
os_make_directory(char *name)
{
	mkdir(name, 0770);
}

#elif OS_WINDOWS

W32(b32) CreateDirectoryA(c8 *, void *);

function void
os_make_directory(char *name)
{
	CreateDirectoryA(name, 0);
}

#else
#error Unsupported Platform
#endif

#define shift_n(v, c, n) v += n, c -= n
#define shift(v, c)   shift_n(v, c, 1)
#define unshift(v, c) shift_n(v, c, -1)

function void
usage(char *argv0)
{
	die("%s [--loop] [--once] [--full-aperture] [--cuda] [--warmup n] [--dump dir]\n"
	    "    --loop:          reupload data forever\n"
	    "    --once:          only run a single frame\n"
	    "    --full-aperture: recieve on full 256 channel aperture\n"
	    "    --cuda:          use cuda for decoding\n"
	    "    --warmup:        warmup with n runs\n"
	    "    --dump:          dump output stats files to dir\n",
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
		shift(argv, argc);

		if (s8_equal(arg, s8("--loop"))) {
			result.loop = 1;
		} else if (s8_equal(arg, s8("--full-aperture"))) {
			result.full_aperture = 1;
		} else if (s8_equal(arg, s8("--cuda"))) {
			result.cuda = 1;
		} else if (s8_equal(arg, s8("--dump"))) {
			if (argc) {
				result.outdir = *argv;
				result.dump   = 1;
				shift(argv, argc);
			} else {
				die("expected directory to dump to\n");
			}
		} else if (s8_equal(arg, s8("--once"))) {
			result.once = 1;
		} else if (s8_equal(arg, s8("--warmup"))) {
			if (argc) {
				result.warmup_count = (u32)atoi(*argv);
				shift(argv, argc);
			}
		} else if (arg.len > 0 && arg.data[0] == '-') {
			usage(argv0);
		} else {
			unshift(argv, argc);
			break;
		}
	}

	result.remaining       = argv;
	result.remaining_count = argc;

	return result;
}

function b32
send_frame(i16 *restrict i16_data, u32 data_size)
{
	b32 result = beamformer_push_data_with_compute(i16_data, data_size, BeamformerViewPlaneTag_XZ, 0);
	if (!result && !g_should_exit) printf("lib error: %s\n", beamformer_get_last_error_string());
	return result;
}

function uv4
decoded_data_dim(u32 transmit_count, b32 full_aperture)
{
	u32 max_transmits = decode_transmit_counts[countof(decode_transmit_counts) - 1];
	uv4 result = {{RF_TIME_SAMPLES, full_aperture? max_transmits: transmit_count, transmit_count, 1}};
	return result;
}

function uv2
raw_data_dim(u32 transmit_count, b32 full_aperture)
{
	uv4 dec = decoded_data_dim(transmit_count, full_aperture);
	uv2 result = {{dec.x * transmit_count,  256}};
	return result;
}

function u32
data_size_for_transmit_count(u32 transmit_count, b32 full_aperture)
{
	uv2 rf_dim = raw_data_dim(transmit_count, full_aperture);
	u32 result = rf_dim.x * rf_dim.y * sizeof(i16);
	return result;
}

function i16 *
generate_test_data_for_transmit_count(u32 transmit_count, b32 full_aperture)
{
	uz  rf_size = data_size_for_transmit_count(transmit_count, full_aperture);
	i16 *result = malloc(rf_size);
	if (!result) die("malloc\n");
	return result;
}

function void
dump_stats(BeamformerComputeStatsTable *stats, Options *options, u32 transmit_count)
{
	char path_buffer[1024];
	Stream sb = {.data = (u8 *)path_buffer, .cap = sizeof(path_buffer)};
	stream_append_s8s(&sb, c_str_to_s8(options->outdir), s8(OS_PATH_SEPARATOR), s8("decode_"));
	if (options->cuda) stream_append_s8(&sb, s8("cuda_"));
	stream_append_u64(&sb, transmit_count);
	stream_append_s8(&sb, s8(".bin"));
	stream_append_byte(&sb, 0);
	os_write_new_file(path_buffer, (s8){.len = sizeof(*stats), .data = (u8 *)stats});
}

function void
send_parameters(Options *options, u32 transmit_count)
{
	BeamformerParameters bp = {0};
	bp.decode_mode    = BeamformerDecodeMode_Hadamard;
	b32 full_aperture = options->full_aperture;
	uv3 dec_data_dim  = decoded_data_dim(transmit_count, full_aperture).xyz;
	bp.sample_count      = dec_data_dim.x;
	bp.channel_count     = dec_data_dim.y;
	bp.acquisition_count = dec_data_dim.z;

	bp.raw_data_dimensions = raw_data_dim(transmit_count, full_aperture);
	beamformer_push_parameters(&bp);

	/* NOTE(rnp): use real channel mapping so that we still get ~random~ access pattern */
	read_only local_persist i16 channel_mapping[] = {
		217, 129, 212, 188, 255, 131, 237, 190, 241, 130, 248, 187, 219, 128, 218, 181,
		216, 134, 247, 180, 220, 132, 238, 178, 246, 133, 240, 179, 221, 135, 239, 173,
		231, 137, 211, 172, 222, 139, 213, 170, 249, 138, 210, 171, 223, 136, 232, 189,
		233, 142, 209, 164, 224, 140, 214, 186, 254, 141, 208, 163, 225, 143, 215, 185,
		230, 145, 204, 162, 226, 147, 206, 165, 229, 146, 207, 161, 227, 144, 205, 182,
		234, 150, 203, 160, 228, 148, 201, 166, 236, 149, 200, 159, 235, 175, 202, 177,
		242, 151, 196, 191, 243, 155, 198, 167, 245, 154, 199, 158, 244, 176, 197, 174,
		250, 168, 195, 184, 251, 156, 193, 152, 253, 153, 192, 157, 252, 183, 194, 169,
		102,  62,  71,   3, 100,  60,  82,   1,  78,  61,  72,   4,  64,  63, 101,  10,
		103,  57, 107,  11,  99,  59,  81,  13,  73,  58,  79,  12,  98,  56,  80,  18,
		 88,  54, 108,  19,  97,  52, 106,  21,  70,  53, 109,  20,  96,  55,  87,   2,
		 86,  49, 110,  27,  95,  51, 105,   5,  65,  50, 111,  28,  94,  48, 104,   6,
		 89,  46, 115,  29,  93,  44, 113,  26,  90,  45, 112,  30,  92,  47, 114,   9,
		 85,  41, 116,  31,  91,  43, 118,  25,  83,  42, 119,  32,  84,  16, 117,  14,
		 77,  40, 123,   0,  76,  36, 121,  24,  74,  37, 120,  33,  75,  15, 122,  17,
		 69,  23, 124,   7,  68,  35, 126,  39,  66,  38, 127,  34,  67,   8, 125,  22,
	};
	beamformer_push_channel_mapping(channel_mapping, countof(channel_mapping));

	i32 shader_stages[1];
	if (options->cuda) shader_stages[0] = BeamformerShaderKind_CudaDecode;
	else               shader_stages[0] = BeamformerShaderKind_Decode;
	beamformer_push_pipeline(shader_stages, 1, BeamformerDataKind_Int16);
	beamformer_set_global_timeout(1000);
}

function f32
execute_study(Options *options, u32 transmit_count, i16 *restrict data)
{
	send_parameters(options, transmit_count);
	u32 data_size = data_size_for_transmit_count(transmit_count, options->full_aperture);
	for (u32 i = 0; !g_should_exit && i < options->warmup_count; i++)
		send_frame(data, data_size);

	u64 start     = os_timer_count();
	f64 frequency = os_timer_frequency();
	for (u32 i = 0; !g_should_exit && i < AVERAGE_SAMPLES; i++)
		send_frame(data, data_size);
	f32 result = (os_timer_count() - start) / frequency / (f32)AVERAGE_SAMPLES;

	return result;
}

function void
print_result(u32 transmit_count, f32 time)
{
	printf("decode %3u | %uF Average: %8.3f [ms]\n", transmit_count, (u32)AVERAGE_SAMPLES, time * 1e3);
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

	if (options.remaining_count)
		usage(argv[0]);

	if (options.dump) os_make_directory(options.outdir);

	signal(SIGINT, sigint);

	BeamformerLiveImagingParameters lip = {.active = 1, .save_enabled = 1};
	s8 short_name = s8("Decode Bench");
	mem_copy(lip.save_name_tag, short_name.data, (uz)short_name.len);
	lip.save_name_tag_length = (i32)short_name.len;
	beamformer_set_live_parameters(&lip);

	u32 max_transmit_count = decode_transmit_counts[countof(decode_transmit_counts) - 1];
	i16 *data = generate_test_data_for_transmit_count(max_transmit_count, options.full_aperture);
	if (options.loop) {
		for (;!g_should_exit;) {
			u32 transmit_count = decode_transmit_counts[0];
			f32 time = execute_study(&options, transmit_count, data);
			if (!g_should_exit) print_result(transmit_count, time);
		}
	} else if (options.once) {
		u32 transmit_count = decode_transmit_counts[0];
		u32 data_size = data_size_for_transmit_count(transmit_count, options.full_aperture);
		send_parameters(&options, transmit_count);
		send_frame(data, data_size);
	} else {
		BeamformerComputeStatsTable stats = {0};
		for (iz i = 0; i < countof(decode_transmit_counts); i++) {
			u32 transmit_count = decode_transmit_counts[i];
			f32 time = execute_study(&options, transmit_count, data);
			if (options.dump) {
				beamformer_compute_timings(&stats, 1000);
				dump_stats(&stats, &options, transmit_count);
			}
			if (!g_should_exit) print_result(transmit_count, time);
		}
	}

	lip.active = 0;
	beamformer_set_live_parameters(&lip);

	return 0;
}
