#include "ogl_beamformer_lib.c"

/* NOTE(rnp): I want these activated here even in release mode */
#ifndef assert
#define assert(c) do { if (!(c)) debugbreak(); } while (0);
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <fftw3.h>
#include <zstd.h>

//#define TIME_FFT
/* NOTE: main thread will also do work */
#define THREAD_COUNT (1 - 1)
#if THREAD_COUNT
#include <pthread.h>
#endif

#define PI   3.14159265358979323846f
#define cosf __builtin_cosf
#define sinf __builtin_sinf

global u32 g_output_points[4] = {512, 1, 1024, 1};

typedef struct {
	s8  output_path;
	uv4 output_points;
	v2  axial_extent;
	v2  lateral_extent;
	i32 frame_count;
	b32 coherency_weighting;
	f32 f_number;
	f32 center_frequency;
	f32 low_pass_cutoff_frequency;
	u32 low_pass_filter_order;
} WorkGroupSettings;

typedef struct {
	s8 study;
	WorkGroupSettings settings;
} WorkFrame;

/* TODO(rnp): file dir iterator */
#define LAST_SAVED_FRAME_NUMBER 8

#define low_freq_res_settings (WorkGroupSettings){ \
	.output_path    = s8("low_freq_res"),         \
	.axial_extent   = {.x =  5e-3,  .y = 170e-3}, \
	.lateral_extent = {.x = -50e-3, .y = 50e-3},  \
	.frame_count    = 1,                          \
	.f_number       = 0.5,                        \
}

#define low_freq_cyst_settings (WorkGroupSettings){ \
	.output_path    = s8("low_freq_cysts"),       \
	.axial_extent   = {.x =  10e-3, .y = 165e-3}, \
	.lateral_extent = {.x = -60e-3, .y = 60e-3},  \
	.frame_count    = 1,                          \
	.f_number       = 0.5,                        \
}

#define low_freq_cyst_xplane (WorkGroupSettings){ \
	.output_path    = s8("low_freq_cyst_xplane"), \
	.axial_extent   = {.x =  10e-3, .y = 165e-3}, \
	.lateral_extent = {.x = -60e-3, .y = 60e-3},  \
	.frame_count    = 8,                          \
	.f_number       = 0.5,                        \
}

#define high_freq_res_settings (WorkGroupSettings){ \
	.output_path    = s8("high_freq_res"),           \
	.axial_extent   = {.x =  5e-3,  .y = 55e-3},     \
	.lateral_extent = {.x = -18.5e-3, .y = 18.5e-3}, \
	.frame_count    = 1,                             \
	.f_number       = 0.75,                          \
}

#define high_freq_cyst_settings (WorkGroupSettings){ \
	.output_path    = s8("high_freq_cysts"),         \
	.axial_extent   = {.x =  5e-3,    .y = 55e-3},   \
	.lateral_extent = {.x = -18.5e-3, .y = 18.5e-3}, \
	.frame_count    = 8,                             \
	.f_number       = 1,                             \
}

#define high_freq_cyst_xplane (WorkGroupSettings){ \
	.output_path    = s8("high_freq_cyst_xplane"),   \
	.axial_extent   = {.x =  5e-3,  .y = 60e-3},     \
	.lateral_extent = {.x = -18.5e-3, .y = 18.5e-3}, \
	.frame_count    = 8,                             \
	.f_number       = 1,                             \
}

#define fetal_pig_settings (WorkGroupSettings){ \
	.output_path    = s8("fetal_pig"),               \
	.axial_extent   = {.x =  5e-3,  .y = 55e-3},     \
	.lateral_extent = {.x = -18.5e-3, .y = 18.5e-3}, \
	.frame_count    = 1,                             \
	.f_number       = 0.5,                           \
	.coherency_weighting = 1,                        \
}

#define mouse_settings (WorkGroupSettings){ \
	.output_path    = s8("mouse"),                   \
	.axial_extent   = {.x =  5e-3,    .y = 37.5e-3}, \
	.lateral_extent = {.x = -18.5e-3, .y = 18.5e-3}, \
	.frame_count    = 1,                             \
	.f_number       = 1,                             \
	.coherency_weighting = 0,                        \
}

#define mouse_28_settings (WorkGroupSettings){ \
	.output_path    = s8("mouse_28_column_tx"),      \
	.axial_extent   = {.x =  10e-3,   .y = 34.5e-3}, \
	.lateral_extent = {.x = -16.0e-3, .y = 16.0e-3}, \
	.frame_count    = 1,                             \
	.f_number       = 1,                             \
	.coherency_weighting = 1,                        \
}

#define walking_cyst_settings (WorkGroupSettings){ \
	.output_path    = s8("walking_cysts_rotated"),   \
	.axial_extent   = {.x =  5e-3,    .y = 42e-3},   \
	.lateral_extent = {.x = -18.5e-3, .y = 18.5e-3}, \
	.frame_count    = 1,                             \
	.f_number       = 1,                             \
}

global WorkFrame work[] = {
	/* MN45-1 - 3.3MHz */
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_FORCES-TxRow"),     low_freq_cyst_xplane},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_FORCES-TxColumn"),  low_freq_cyst_xplane},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_FORCES-TxRow"),           low_freq_cyst_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_TPW-128-TxColumn"),       low_freq_cyst_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Cyst_VLS-128-TxColumn"),       low_freq_cyst_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Resolution_FORCES-TxRow"),     low_freq_res_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Resolution_TPW-128-TxColumn"), low_freq_res_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Resolution_VLS-128-TxColumn"), low_freq_res_settings},
	//{s8("250317_MN45-1_3.30MHz_ATS539_Resolution_uFORCES-32-TxRow"), low_freq_res_settings},

	/* A06 - 7.8MHz */
	//{s8("250327_A06_7.80MHz_35mm_ATS539_Cyst_FORCES-TxColumn"), high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_ATS539_Cyst_uFORCES-32-TxRow"),        high_freq_cyst_xplane},
	//{s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxColumn"),     high_freq_cyst_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxRow"),        high_freq_cyst_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Cyst_TPW-128-TxColumn"),    high_freq_cyst_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Cyst_VLS-128-TxColumn"),    high_freq_cyst_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Resolution_FORCES-TxRow"),     high_freq_res_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Resolution_VLS-128-TxColumn"), high_freq_res_settings},
	//{s8("250319_A06_7.80MHz_ATS539_Resolution_TPW-128-TxColumn"), high_freq_res_settings},

	/* Cyst X-Plane */
	//{s8("250327_A06_7.80MHz_15mm_ATS539_Cyst_FORCES-TxColumn"), high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_25mm_ATS539_Cyst_FORCES-TxColumn"), high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_35mm_ATS539_Cyst_FORCES-TxColumn"), high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_45mm_ATS539_Cyst_FORCES-TxColumn"), high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_15mm_ATS539_Cyst_FORCES-TxRow"),    high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_25mm_ATS539_Cyst_FORCES-TxRow"),    high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_35mm_ATS539_Cyst_FORCES-TxRow"),    high_freq_cyst_xplane},
	//{s8("250327_A06_7.80MHz_45mm_ATS539_Cyst_FORCES-TxRow"),    high_freq_cyst_xplane},

	/* Fetal Pig */
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Tx-Row"),    fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Tx-Column"), fetal_pig_settings},

	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Row-1_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Row-2_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Row-3_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Row-4_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Row-5_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Row-6_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Row-7_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Row-8_reshaped"), fetal_pig_settings}

	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Column-9_reshaped"),  fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Column-10_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Column-11_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Column-12_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Column-13_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Column-14_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Column-15_reshaped"), fetal_pig_settings},
	//{s8("250417_A06_Pig_Fetal_Abdomen_FORCES-Walking-64-Tx-Column-16_reshaped"), fetal_pig_settings},

	/* Mouse */
	//{s8("250204_MN32-1_M18_Side_force_FORCES-TxColumn"),          mouse_settings},
	//{s8("250204_MN32-1_M18_Side_forces_hanning_FORCES-TxColumn"), mouse_settings},
	//{s8("250204_MN32-1_M18_Side_force_FORCES-TxRow"),             mouse_settings},
	//{s8("250204_MN32-1_M18_Side_forces_hanning_FORCES-TxRow"),    mouse_settings},

	/* Mouse-28 */
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Row-1_reshaped"),     mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Row-2_reshaped"),     mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Row-3_reshaped"),     mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Row-4_reshaped"),     mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Row-5_reshaped"),     mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Row-6_reshaped"),     mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Row-7_reshaped"),     mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Row-8_reshaped"),     mouse_28_settings},

	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Column-9_reshaped"),  mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Column-10_reshaped"), mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Column-11_reshaped"), mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Column-12_reshaped"), mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Column-13_reshaped"), mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Column-14_reshaped"), mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Column-15_reshaped"), mouse_28_settings},
	//{s8("250416_A06_Mouse_ID28_Abdomen_FORCES-Walking-64-Tx-Column-16_reshaped"), mouse_28_settings},

	/* Walking Cysts */
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Row-1_reshaped"),     walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Row-2_reshaped"),     walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Row-3_reshaped"),     walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Row-4_reshaped"),     walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Row-5_reshaped"),     walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Row-6_reshaped"),     walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Row-7_reshaped"),     walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Row-8_reshaped"),     walking_cyst_settings},

	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Column-9_reshaped"),  walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Column-10_reshaped"), walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Column-11_reshaped"), walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Column-12_reshaped"), walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Column-13_reshaped"), walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Column-14_reshaped"), walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Column-15_reshaped"), walking_cyst_settings},
	{s8("250520_A06_ATS539_Cyst_rotated_FORCES-Walking-64-Tx-Column-16_reshaped"), walking_cyst_settings},
};

global v3 low_freq_cyst_centers[] = {
	/* 8mm */
	{{14.5e-3, 22.25e-3, 8e-3}}, {{14.5e-3,  41.5e-3, 8e-3}}, {{14.5e-3,  60.9e-3, 8e-3}},
	{{14.5e-3,  80.3e-3, 8e-3}}, {{14.8e-3, 99.15e-3, 8e-3}}, {{14.6e-3, 118.6e-3, 8e-3}},
	{{14.8e-3, 137.8e-3, 8e-3}}, {{14.8e-3, 157.8e-3, 8e-3}},

	/* 6mm */
	{{0.2e-3,   22.6e-3, 6e-3}}, {{0.2e-3,  41.7e-3, 6e-3}}, {{0.2e-3,  60.9e-3, 6e-3}},
	{{0.3e-3,   80.2e-3, 6e-3}}, {{0.4e-3,  99.5e-3, 6e-3}}, {{0.4e-3, 118.7e-3, 6e-3}},
	{{0.4e-3,  138.7e-3, 6e-3}}, {{0.4e-3, 157.5e-3, 6e-3}},

	/* 4mm */
	{{-9.7e-3,   22.3e-3, 4e-3}}, {{-9.4e-3,  32.1e-3, 4e-3}}, {{-9.7e-3,  41.8e-3, 4e-3}},
	{{-9.5e-3,   51.3e-3, 4e-3}}, {{-9.5e-3,  61.0e-3, 4e-3}}, {{-9.5e-3,  70.5e-3, 4e-3}},
	{{-9.5e-3,  80.25e-3, 4e-3}}, {{-9.3e-3,  90.1e-3, 4e-3}}, {{-9.3e-3,  99.7e-3, 4e-3}},
	{{-9.0e-3,  108.9e-3, 4e-3}}, {{-9.0e-3, 118.6e-3, 4e-3}}, {{-9.5e-3, 128.1e-3, 4e-3}},
	{{-9.0e-3,  138.0e-3, 4e-3}}, {{-9.0e-3, 147.5e-3, 4e-3}}, {{-9.0e-3, 157.8e-3, 4e-3}},
};

global v3 high_freq_cyst_centers[] = {
	/* 4mm */
	{{-10e-3,  13e-3, 4e-3}}, {{-9.8e-3, 22.4e-3, 4e-3}}, {{-9.6e-3, 32.2e-3, 4e-3}},
	{{-9.4e-3, 42e-3, 4e-3}},
	/* 3mm */
	{{-0.5e-3, 12.5e-3, 3e-3}}, {{-0.4e-3, 22.4e-3, 3e-3}}, {{-0.2e-3, 32e-3, 3e-3}},
	{{-0.2e-3, 41.8e-3, 3e-3}},
	/* 2mm */
	{{9.7e-3, 12.6e-3, 2e-3}}, {{9.5e-3, 22.3e-3, 2e-3}}, {{9.4e-3, 31.8e-3, 2e-3}},
	{{9.4e-3, 41.8e-3, 2e-3}},
};

#define low_freq_contrast_settings (WorkGroupSettings){ \
	.output_points  = {{512, 1, 512, 1}},      \
	.output_path    = s8("low_freq_contrast"), \
	.frame_count    = 1,                       \
	.f_number       = 1,                       \
}

#define high_freq_contrast_settings (WorkGroupSettings){ \
	.output_points  = {{512, 1, 512, 1}},       \
	.output_path    = s8("high_freq_contrast"), \
	.frame_count    = 1,                        \
	.f_number       = 1,                        \
}

typedef struct {
	s8 study;
	WorkGroupSettings settings;

	s8  method;
	v3 *cysts;
	i32 cysts_count;
} ContrastWorkFrame;

global ContrastWorkFrame gCNR_work[] = {
	/* MN45-1 (Low-Frequency) */
	{
		.study       = s8("250317_MN45-1_3.30MHz_ATS539_Cyst_FORCES-TxRow"),
		.settings    = low_freq_contrast_settings,
		.method      = s8("FORCES"),
		.cysts       = low_freq_cyst_centers,
		.cysts_count = countof(low_freq_cyst_centers),
	},
	{
		.study       = s8("250317_MN45-1_3.30MHz_ATS539_Cyst_TPW-128-TxColumn"),
		.settings    = low_freq_contrast_settings,
		.method      = s8("TPW"),
		.cysts       = low_freq_cyst_centers,
		.cysts_count = countof(low_freq_cyst_centers),
	},
	{
		.study       = s8("250317_MN45-1_3.30MHz_ATS539_Cyst_VLS-128-TxColumn"),
		.settings    = low_freq_contrast_settings,
		.method      = s8("VLS"),
		.cysts       = low_freq_cyst_centers,
		.cysts_count = countof(low_freq_cyst_centers),
	},

	/* A06 (High-Frequency) */
	{
		.study       = s8("250319_A06_7.80MHz_ATS539_Cyst_FORCES-TxRow"),
		.settings    = high_freq_contrast_settings,
		.method      = s8("FORCES"),
		.cysts       = high_freq_cyst_centers,
		.cysts_count = countof(high_freq_cyst_centers),
	},
	{
		.study       = s8("250319_A06_7.80MHz_ATS539_Cyst_TPW-128-TxColumn"),
		.settings    = high_freq_contrast_settings,
		.method      = s8("TPW"),
		.cysts       = high_freq_cyst_centers,
		.cysts_count = countof(high_freq_cyst_centers),
	},
	{
		.study       = s8("250319_A06_7.80MHz_ATS539_Cyst_VLS-128-TxColumn"),
		.settings    = high_freq_contrast_settings,
		.method      = s8("VLS"),
		.cysts       = high_freq_cyst_centers,
		.cysts_count = countof(high_freq_cyst_centers),
	},
};

global v2 low_freq_wire_centers[] = {
	{{-24.90e-3,   8.00e-3}},
	{{-19.90e-3,   9.10e-3}}, {{-15.00e-3,  10.00e-3}}, {{-10.40e-3,  10.90e-3}},
	{{-05.60e-3,  11.75e-3}}, {{-00.75e-3,  12.70e-3}}, {{-00.60e-3,  22.40e-3}},
	{{-00.50e-3,  32.00e-3}}, {{-00.42e-3,  41.50e-3}}, {{-00.39e-3,  51.10e-3}},
	{{-00.40e-3,  60.95e-3}}, {{-00.23e-3,  70.35e-3}}, {{ 00.05e-3,  80.00e-3}},
	{{ 00.05e-3,  89.45e-3}}, {{ 00.10e-3,  99.15e-3}}, {{ 00.20e-3, 108.50e-3}},
	{{ 00.35e-3, 118.25e-3}}, {{ 00.40e-3, 127.75e-3}}, {{ 00.20e-3, 137.70e-3}},
	{{ 00.45e-3, 146.90e-3}}, {{ 00.20e-3, 156.90e-3}}, {{ 00.25e-3, 166.25e-3}},
};

global v2 high_freq_wire_centers[] = {
	{{ 1.10e-3, 11.20e-3}}, {{ 5.75e-3, 10.25e-3}}, {{ 10.40e-3,  9.40e-3}},
	{{-3.65e-3, 12.15e-3}}, {{-8.45e-3, 13.20e-3}}, {{-08.45e-3, 22.95e-3}},
	{{-8.45e-3, 32.50e-3}}, {{-8.35e-3, 42.15e-3}},
};

typedef struct {
	s8 study;
	WorkGroupSettings settings;

	s8  method;
	v2 *wires;
	i32 wires_count;
} ResolutionWorkFrame;

#define low_freq_wire_settings (WorkGroupSettings){ \
	.output_points  = {{512, 1, 512, 1}},        \
	.output_path    = s8("low_freq_resolution"), \
	.frame_count    = 1,                         \
	.f_number       = 0.5,                       \
}

#define high_freq_wire_settings (WorkGroupSettings){ \
	.output_points  = {{512, 1, 512, 1}},         \
	.output_path    = s8("high_freq_resolution"), \
	.frame_count    = 1,                          \
	.f_number       = 0.5,                        \
}

global ResolutionWorkFrame resolution_work[] = {
	/* MN45-1 (Low-Frequency) */
	{
		.study       = s8("250317_MN45-1_3.30MHz_ATS539_Resolution_FORCES-TxRow"),
		.settings    = low_freq_wire_settings,
		.method      = s8("FORCES"),
		.wires       = low_freq_wire_centers,
		.wires_count = countof(low_freq_wire_centers),
	},
	{
		.study       = s8("250317_MN45-1_3.30MHz_ATS539_Resolution_TPW-128-TxColumn"),
		.settings    = low_freq_wire_settings,
		.method      = s8("TPW"),
		.wires       = low_freq_wire_centers,
		.wires_count = countof(low_freq_wire_centers),
	},
	{
		.study       = s8("250317_MN45-1_3.30MHz_ATS539_Resolution_VLS-128-TxColumn"),
		.settings    = low_freq_wire_settings,
		.method      = s8("VLS"),
		.wires       = low_freq_wire_centers,
		.wires_count = countof(low_freq_wire_centers),
	},

	/* A06 (High-Frequency) */
	{
		.study       = s8("250319_A06_7.80MHz_ATS539_Resolution_FORCES-TxRow"),
		.settings    = high_freq_wire_settings,
		.method      = s8("FORCES"),
		.wires       = high_freq_wire_centers,
		.wires_count = countof(high_freq_wire_centers),
	},
	{
		.study       = s8("250319_A06_7.80MHz_ATS539_Resolution_TPW-128-TxColumn"),
		.settings    = high_freq_wire_settings,
		.method      = s8("TPW"),
		.wires       = high_freq_wire_centers,
		.wires_count = countof(high_freq_wire_centers),
	},
	{
		.study       = s8("250319_A06_7.80MHz_ATS539_Resolution_VLS-128-TxColumn"),
		.settings    = high_freq_wire_settings,
		.method      = s8("VLS"),
		.wires       = high_freq_wire_centers,
		.wires_count = countof(high_freq_wire_centers),
	},
};

typedef struct {
	f32 *output;
	iz   output_size;

	b32 export;
	b32 analytic;
	b32 low_pass;
	b32 matched_filter;
	b32 beamform_plane;

	s8  output_path_prefix;

	Stream path;
} WorkContext;

#define ZEMP_BP_MAGIC (uint64_t)0x5042504D455AFECAull
typedef struct {
	u64 magic;
	u32 version;
	u16 decode_mode;
	u16 beamform_mode;
	u32 raw_data_dim[4];
	u32 decoded_data_dim[4];
	f32 xdc_element_pitch[2];
	f32 xdc_transform[16]; /* NOTE: column major order */
	i16 channel_mapping[256];
	f32 transmit_angles[256];
	f32 focal_depths[256];
	i16 sparse_elements[256];
	i16 hadamard_rows[256];
	f32 speed_of_sound;
	f32 center_frequency;
	f32 sampling_frequency;
	f32 time_offset;
	u32 transmit_mode;
} zemp_bp_v1;

#define die(...) die_((char *)__func__, __VA_ARGS__)
function void __attribute__((noreturn))
die_(char *function_name, char *format, ...)
{
	if (function_name)
		fprintf(stderr, "%s: ", function_name);

	va_list ap;

	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);

	exit(1);
}

#if OS_LINUX

function void
os_make_directory(char *name)
{
	mkdir(name, 0770);
}

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
	result.data = malloc(st.st_size);
	if (!result.data)
		die("couldn't alloc space for reading\n");

	iz rlen = read(fd, result.data, st.st_size);
	close(fd);

	if (rlen != st.st_size)
		die("couldn't read file: %s\n", fname);

	return result;
}

#elif OS_WINDOWS

W32(b32) CreateDirectoryA(c8 *, void *);

function void
os_make_directory(char *name)
{
	CreateDirectoryA(name, 0);
}

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
	if (!ReadFile(h, result.data, fileinfo.nFileSizeLow, &rlen, 0) && rlen != fileinfo.nFileSizeLow)
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

function void
stream_push_u64_width(Stream *s, u64 n, u64 min_width)
{
	u8 tmp[64];
	u8 *end = tmp + sizeof(tmp);
	u8 *beg = end;
	min_width = MIN(sizeof(tmp), min_width);

	do { *--beg = '0' + (n % 10); } while (n /= 10);
	while (end - beg > 0 && end - beg < min_width)
		*--beg = '0';

	stream_append(s, beg, end - beg);
}

function void
stream_push_file_end_at_index(Stream *s, u32 index)
{
	stream_append_byte(s, '_');
	stream_push_u64_width(s, index, 2);
	stream_append_s8(s, s8(".zst"));
	stream_ensure_termination(s, 0);
}

function void
stream_printf(Stream *s, const char *format, ...)
{
	va_list ap;

	va_start(ap, format);
	i32 length = vsnprintf(0, 0, format, ap);
	s->errors |= (s->cap - s->widx) < (length + 1);
	if (!s->errors) {
		vsnprintf((char *)(s->data + s->widx), s->cap - s->widx, format, ap);
		s->widx += length;
	}
	va_end(ap);
}

function void *
decompress_zstd_data(s8 raw)
{
	iz requested_size = ZSTD_getFrameContentSize(raw.data, raw.len);
	void *out         = malloc(requested_size);
	if (out) {
		iz decompressed = ZSTD_decompress(out, requested_size, raw.data, raw.len);
		if (decompressed != requested_size) {
			free(out);
			out = 0;
		}
	}
	return out;
}

function void
s8_split_at(s8 s, u8 byte, s8 *left, s8 *right)
{
	b32 found;
	iz off = 0;
	while (off < s.len && !(found = s.data[off++] == byte)) {}
	if (left)  *left  = (s8){.len = off - found, .data = s.data};
	if (right) *right = (s8){.len = s.len - off, .data = s.data + off};
}

function void
make_full_directory(Stream *p)
{
	s8 start = stream_to_s8(p);
	s8 left, right, tmp;
	for (s8_split_at(start, OS_PATH_SEPARATOR_CHAR, &left, &right);
	     left.len != start.len;
	     s8_split_at(right, OS_PATH_SEPARATOR_CHAR, &tmp, &right), left.len += tmp.len + 1)
	{
		assert(left.data[left.len] == OS_PATH_SEPARATOR_CHAR);
		left.data[left.len] = 0;
		os_make_directory((char *)left.data);
		left.data[left.len] = OS_PATH_SEPARATOR_CHAR;
	}
	os_make_directory((char *)start.data);
}

function void
write_output_data(s8 output_prefix, s8 output_path, s8 study, f32 *data, u32 points[3],
                  v4 min_coord, v4 max_coord)
{
	u8 buf[2048];
	Stream path = {.data = buf, .cap = sizeof(buf)};

	stream_append_s8(&path, output_prefix);
	stream_ensure_termination(&path, OS_PATH_SEPARATOR_CHAR);
	stream_append_s8(&path, output_path);
	stream_append_byte(&path, 0);
	make_full_directory(&path);
	stream_commit(&path, -1);

	stream_ensure_termination(&path, OS_PATH_SEPARATOR_CHAR);
	stream_append_s8(&path, study);
	iz sidx = path.widx;

	stream_append_s8(&path, s8("_beamformed.bin"));
	stream_append_byte(&path, 0);

	s8 raw_data = {.len = points[0] * points[1] * points[2] * 2 * sizeof(f32), .data = (u8 *)data};
	if (path.errors || !os_write_new_file((c8 *)path.data, raw_data)) {
		printf("failed to write output data: %s\n", (char *)path.data);
	} else {
		printf("wrote data to:   %s\n", (c8 *)path.data);
	}

	stream_reset(&path, sidx);
	stream_append_s8(&path, s8("_params.csv"));
	stream_append_byte(&path, 0);

	u8 buf2[2048];
	Stream o = {.data = buf2, .cap = sizeof(buf2)};
	stream_append_s8(&o, s8("min_coord,max_coord,size\n"));
	stream_printf(&o, "%f,%f,%u\n", min_coord.x, max_coord.x, points[0]);
	stream_printf(&o, "%f,%f,%u\n", min_coord.y, max_coord.y, points[1]);
	stream_printf(&o, "%f,%f,%u\n", min_coord.z, max_coord.z, points[2]);
	if (o.errors || !os_write_new_file((c8 *)path.data, stream_to_s8(&o))) {
		printf("failed to write output parameters: %s\n", (c8 *)path.data);
	} else {
		printf("wrote params to: %s\n", (c8 *)path.data);
	}
}

function zemp_bp_v1 *
read_zemp_bp_v1(u8 *path)
{
	s8 raw = os_read_file_simp((char *)path);
	zemp_bp_v1 *result = 0;
	if (raw.len == sizeof(zemp_bp_v1) && *(u64 *)raw.data == ZEMP_BP_MAGIC) {
		if (((zemp_bp_v1 *)raw.data)->version == 1)
			result = (zemp_bp_v1 *)raw.data;
	}
	return result;
}

function void
fill_beamformer_parameters_from_zemp_bp_v1(zemp_bp_v1 *zbp, BeamformerParametersV0 *out)
{
	mem_copy(out->channel_mapping,   zbp->channel_mapping,   sizeof(out->channel_mapping));
	mem_copy(out->focal_depths,      zbp->focal_depths,      sizeof(out->focal_depths));
	mem_copy(out->transmit_angles,   zbp->transmit_angles,   sizeof(out->transmit_angles));
	mem_copy(out->xdc_transform,     zbp->xdc_transform,     sizeof(out->xdc_transform));
	mem_copy(out->dec_data_dim,      zbp->decoded_data_dim,  sizeof(out->dec_data_dim));
	mem_copy(out->xdc_element_pitch, zbp->xdc_element_pitch, sizeof(out->xdc_element_pitch));
	mem_copy(out->rf_raw_dim,        zbp->raw_data_dim,      sizeof(out->rf_raw_dim));

	if (zbp->sparse_elements[0] == -1) {
		for (u32 i = 0; i < zbp->decoded_data_dim[2]; i++)
			out->uforces_channels[i] = i;
	} else {
		mem_copy(out->uforces_channels, zbp->sparse_elements, sizeof(out->uforces_channels));
	}

	out->transmit_mode      = zbp->transmit_mode;
	out->decode             = zbp->decode_mode;
	out->das_shader_id      = zbp->beamform_mode;
	out->time_offset        = zbp->time_offset;
	out->sampling_frequency = zbp->sampling_frequency;
	out->center_frequency   = zbp->center_frequency;
	out->speed_of_sound     = zbp->speed_of_sound;
}

#define shift_n(v, c, n) v += n, c -= n
#define shift(v, c) shift_n(v, c, 1)

function void
usage(char *argv0)
{
	die("%s [--analytic] [--low-pass] [--export path] [--swap-plane] base_path\n"
	    "    --export  path:   export data to path\n"
	    "    --analytic:       use analytic signal for beamforming\n"
	    "    --low-pass:       apply low pass filter to input data\n"
	    "    --matched-filter: apply matched filtering to input data\n"
	    "    --swap-plane:     beamform the YZ plane\n",
	    argv0);
}

function b32
s8_equal(s8 a, s8 b)
{
	b32 result = a.len == b.len;
	while (result && a.len) {
		result &= a.data[0] == b.data[0];
		shift(a.data, a.len);
		shift(b.data, b.len);
	}
	return result;
}

function i32
parse_argv(WorkContext *wc, i32 argc, char *argv[])
{
	i32 start_argc = argc;

	char *argv0 = argv[0];
	shift(argv, argc);

	while (argc > 0) {
		s8 arg = c_str_to_s8(*argv);

		if (s8_equal(arg, s8("--analytic"))) {
			shift(argv, argc);
			wc->analytic = 1;
		} else if (s8_equal(arg, s8("--low-pass"))) {
			shift(argv, argc);
			wc->low_pass = 1;
		} else if (s8_equal(arg, s8("--matched-filter"))) {
			shift(argv, argc);
			wc->matched_filter = 1;
		} else if (s8_equal(arg, s8("--swap-plane"))) {
			shift(argv, argc);
			wc->beamform_plane = 1;
		} else if (s8_equal(arg, s8("--export"))) {
			shift(argv, argc);
			wc->export = 1;
			wc->output_path_prefix = c_str_to_s8(*argv);
			shift(argv, argc);
		} else if (arg.len > 0 && arg.data[0] == '-') {
			usage(argv0);
		} else {
			break;
		}
	}

	return start_argc - argc;
}

function i16 *
decompress_data_at_work_index(Stream path_base, u32 index)
{
	stream_push_file_end_at_index(&path_base, index);
	s8 compressed_data = os_read_file_simp((c8 *)path_base.data);
	i16 *result = decompress_zstd_data(compressed_data);
	if (!result)
		die("failed to decompress data: %s\n", path_base.data);
	free(compressed_data.data);

	return result;
}

/* NOTE(rnp): I was going to write something fancy here but the function is so simple
 * that the compiler will generate the fast version with march=native and O >= 2 */
function void
convert_i16_to_f32_fast(i16 *restrict in, f32 *restrict out, uz elements)
{
	for (uz i = 0; i < elements; i++)
		out[i] = in[i];
}

global u32 channel_index;

typedef struct {
	f32 *input;
	fftwf_complex *fft, *ifft, *filter;
	fftwf_plan fplan, bplan;
	u32 filter_length;
} fftw_context;

typedef struct {
	i32 samples;
	f32 sampling_frequency;
	f32 transmit_frequency;
	f32 die_center_frequency;
	f32 low_pass_cutoff_frequency;
	i32 low_pass_filter_order;
	b32 analytic;
} fft_filter_context;

typedef struct {
	fftw_context fftw;
	u32 samples, channels, transmits;
	u32 channel_stride, channel_offset;
} work_thread_work;

function void
fftw_context_release(fftw_context *fftw)
{
	fftwf_destroy_plan(fftw->fplan);
	fftwf_destroy_plan(fftw->bplan);
	fftwf_free(fftw->input);
	fftwf_free(fftw->fft);
	fftwf_free(fftw->ifft);
	fftwf_free(fftw->filter);
}

function void
hadamard_product_1d(fftwf_complex *restrict a, fftwf_complex *restrict b, u32 n)
{
	for (u32 i = 0; i < n; i++) {
		f32 x = (a[i][0] * b[i][0] - a[i][1] * b[i][1]);
		f32 y = (a[i][0] * b[i][1] + a[i][1] * b[i][0]);
		a[i][0] = x;
		a[i][1] = y;
	}
}

function f32
fftw_create_filter(fftw_context *ctx, fft_filter_context filter_ctx)
{
	f32 result = 0;
	ctx->filter        = fftwf_alloc_complex(filter_ctx.samples);
	ctx->filter_length = filter_ctx.samples / 2;

	if (!ctx->filter)
		die("failed to allocate space for signal filter\n");

	ctx->filter[0][0] = 1;
	for (u32 i = 1; i < ctx->filter_length - 1; i++)
		ctx->filter[i][0] = 1 + filter_ctx.analytic;
	ctx->filter[ctx->filter_length - 1][0] = 1;

	f32           *f_in  = 0;
	fftwf_complex *f_out = 0;
	fftwf_plan     plan  = 0;

	f32 fs = filter_ctx.sampling_frequency;
	if (filter_ctx.transmit_frequency || filter_ctx.low_pass_cutoff_frequency) {
		f_in  = fftwf_alloc_real(filter_ctx.samples);
		f_out = fftwf_alloc_complex(filter_ctx.samples);
		plan  = fftwf_plan_dft_r2c_1d(filter_ctx.samples, f_in, f_out, FFTW_MEASURE);
		if (!f_in || !f_out)
			die("failed to allocate temporary space for filter\n");
	}

	if (filter_ctx.low_pass_cutoff_frequency) {
		i32 order    = filter_ctx.low_pass_filter_order;
		i32 midpoint = (order - 1) / 2;
		f32 wc = 2 * PI * filter_ctx.low_pass_cutoff_frequency / fs;
		for (i32 i = 0; i < order; i++) {
			f32 window = 0.5 - 0.5 * cosf(2 * PI * i / (order - 1));
			f32 value;
			if (i == midpoint) value = wc / PI;
			else               value = sinf(wc * (i - midpoint)) / (PI * (i - midpoint));
			f_in[i]  = value * window;
		}

		fftwf_execute_dft_r2c(plan, f_in, f_out);
		hadamard_product_1d(ctx->filter, f_out, ctx->filter_length);

		/* TODO(rnp): this is inaccurate for larger orders */
		result += (order - 1) / (4 * PI * filter_ctx.low_pass_cutoff_frequency);
	}

	if (filter_ctx.transmit_frequency) {
		mem_clear(f_in, 0, filter_ctx.samples);
		/* NOTE: impulse response - 1 cycle sine with hamming applied */
		i32 length = (fs / filter_ctx.die_center_frequency) + 1;
		f32 wc     = 2 * PI * filter_ctx.die_center_frequency / fs;
		for (i32 i = 0; i < length; i++) {
			f32 window = 0.5 - 0.5 * cosf(2 * PI * i / (length - 1));
			f32 value  = sinf(wc * i);
			f_in[i]    = value * window;
		}

		fftwf_complex *f_out_copy = fftwf_alloc_complex(filter_ctx.samples);
		if (!f_out_copy)
			die("failed to allocate space for pulse copy\n");
		fftwf_plan bplan = fftwf_plan_many_dft(1, &filter_ctx.samples, 1,
		                                       f_out_copy, 0, 1, filter_ctx.samples,
		                                       f_out,      0, 1, filter_ctx.samples,
		                                       FFTW_BACKWARD, FFTW_MEASURE);

		fftwf_execute_dft_r2c(plan, f_in, f_out_copy);
		hadamard_product_1d(ctx->filter, f_out_copy, ctx->filter_length);

		mem_clear(f_in, 0, length);

		/* TODO(rnp): refactor to include filtering for chirps */
		/* NOTE: transmit waveform - (2 cycle sine at transmit center frequency) */
		length = (i32)(2 * (fs / filter_ctx.transmit_frequency)) + 1;
		wc     = 2 * PI * filter_ctx.transmit_frequency / fs;
		for (i32 i = 0; i < length; i++)
			f_in[i] = sinf(wc * i);

		fftwf_execute_dft_r2c(plan, f_in, f_out);
		hadamard_product_1d(ctx->filter, f_out, ctx->filter_length);

		hadamard_product_1d(f_out_copy, f_out, ctx->filter_length);
		for (u32 i = 1; i < ctx->filter_length - 1; i++)
			f_out_copy[i][0] *= 2;

		fftwf_execute_dft(bplan, f_out_copy, f_out);

		f32 max_magnitude = 0;
		u32 max_index     = 0;
		for (u32 i = 0; i < ctx->filter_length; i++) {
			f32 magnitude  = f_out[i][0] * f_out[i][0] + f_out[i][1] * f_out[i][1];
			max_magnitude  = MAX(max_magnitude, magnitude);
			if (max_magnitude == magnitude)
				max_index = i;
		}

		//result += (f32)max_index / fs;
		result += 1.5 * ((f32)length - 1) / fs;
		//result += 1 / filter_ctx.transmit_frequency;
		//result += ((f32)length - 1) / (2 * fs);

		fftwf_free(f_out_copy);
		fftwf_destroy_plan(bplan);
	}

	fftwf_free(f_in);
	fftwf_free(f_out);
	fftwf_destroy_plan(plan);
	return result;
}

function void *
fft_thread(void *ctx_)
{
	work_thread_work ctx  = *(work_thread_work *)ctx_;
	fftw_context     fftw = ctx.fftw;
	for (;;) {
		#if THREAD_COUNT
		u32 channel = atomic_inc(&channel_index, 1);
		#else
		u32 channel = channel_index++;
		#endif
		if (channel >= ctx.channels)
			break;
		channel += ctx.channel_offset;

		fftwf_execute_dft_r2c(fftw.fplan, fftw.input + (channel * ctx.channel_stride),
		                      fftw.fft + (channel * ctx.channel_stride));

		if (fftw.filter_length) {
			assert(fftw.filter_length == ctx.samples / 2);
			/* NOTE(rnp): dft_r2c won't actually compute anything for the second half
			 * of the array since the input was real (Hermetian Symmetry) - we don't
			 * need to zero it ourselves */
			u32 base_off = channel * ctx.channel_stride;
			for (u32 transmit = 0; transmit < ctx.transmits; transmit++) {
				u32 sample_off = transmit * ctx.samples;
				hadamard_product_1d(fftw.fft + base_off + sample_off,
				                    fftw.filter, fftw.filter_length);
			}
		}

		fftwf_execute_dft(fftw.bplan, fftw.fft + (channel * ctx.channel_stride),
		                  fftw.ifft + (channel * ctx.channel_stride));

		f32 scale = 1.0f / (f32)ctx.samples;
		for (u32 i = 0; i < ctx.channel_stride; i++) {
			fftw.ifft[channel * ctx.channel_stride + i][0] *= scale;
			fftw.ifft[channel * ctx.channel_stride + i][1] *= scale;
		}
	}
	return 0;
}

function void
run_fft_work(fftw_context *fftw, u32 dec_data_dim[3], u32 channel_stride, u32 channel_offset)
{
	work_thread_work thread_ctx = {
		.fftw           = *fftw,
		.samples        = dec_data_dim[0],
		.channels       = dec_data_dim[1],
		.transmits      = dec_data_dim[2],
		.channel_stride = channel_stride,
		.channel_offset = channel_offset,
	};

	channel_index = 0;

	#ifdef TIME_FFT
	clock_t start = clock();
	#endif
	#if THREAD_COUNT
	pthread_t threads[THREAD_COUNT];
	for (u32 i = 0; i < THREAD_COUNT; i++)
		pthread_create(threads + i, 0, fft_thread, &thread_ctx);
	#endif

	fft_thread(&thread_ctx);

	#if THREAD_COUNT
	void *out;
	for (u32 i = 0; i < THREAD_COUNT; i++)
		pthread_join(threads[i], &out);
	#endif
	#ifdef TIME_FFT
	clock_t end = clock();
	printf("run_fft_work: took: %0.02f [ms]\n", 1000.0f * (f32)(end - start) / (f32)(CLOCKS_PER_SEC));
	#endif
}

function b32
send_frame(i16 *restrict i16_data, fftw_context *restrict fftw, f32 *restrict out,
           BeamformerParametersV0 *restrict bp)
{
	b32 result = 0;
	void *data = i16_data;
	u32   data_size = bp->rf_raw_dim[0] * bp->rf_raw_dim[1] * sizeof(i16);
	if (fftw) {
		u32 elements = bp->rf_raw_dim[0] * bp->rf_raw_dim[1];
		u32 offset   = 0;
		if ((bp->transmit_mode & 1u) == 0 && bp->rf_raw_dim[1] != bp->dec_data_dim[1])
			offset = bp->dec_data_dim[1];
		convert_i16_to_f32_fast(i16_data, fftw->input, elements);
		run_fft_work(fftw, bp->dec_data_dim, bp->rf_raw_dim[0], offset);
		data      = (f32 *)fftw->ifft;
		data_size = elements * 2 * sizeof(f32);
	}
	if (out) result = beamform_data_synchronized(data, data_size, bp->output_points, out, 100000);
	else     result = send_data(data, data_size);
	return result;
}

function b32
allocate_output_buffer(WorkContext *wc, u32 output_points[3])
{
	iz output_size = output_points[0] * output_points[1] * output_points[2] * 2 * sizeof(f32);
	b32 result = wc->output && output_size < wc->output_size;
	if (!result) {
		wc->output = realloc(wc->output, output_size);
		result     = wc->output != 0;
		if (result) wc->output_size = output_size;
		else        wc->output_size = 0;
	}
	return result;
}

function b32
execute_work_item(WorkContext *wc, WorkFrame *w, Arena arena, Stream path, s8 output_name)
{
	stream_append_s8(&path, w->study);
	stream_ensure_termination(&path, OS_PATH_SEPARATOR_CHAR);
	stream_append_s8(&path, w->study);
	iz path_work_index = path.widx;

	stream_append_s8(&path, s8(".bp"));
	stream_ensure_termination(&path, 0);

	zemp_bp_v1 *zbp = read_zemp_bp_v1(path.data);
	if (!zbp) die("failed to unpack parameters file\n");

	uv4 raw_data_dim = uv4_from_u32_array(zbp->raw_data_dim);
	BeamformerParametersV0 bp = {0};
	fill_beamformer_parameters_from_zemp_bp_v1(zbp, &bp);
	free(zbp);

	if (!uv3_equal(w->settings.output_points.xyz, (uv3){0}))
		mem_copy(bp.output_points, w->settings.output_points.E, sizeof(bp.output_points));
	else
		mem_copy(bp.output_points, g_output_points, sizeof(bp.output_points));

	bp.output_min_coordinate[0] = w->settings.lateral_extent.x;
	bp.output_min_coordinate[1] = 0;
	bp.output_min_coordinate[2] = w->settings.axial_extent.x;
	bp.output_min_coordinate[3] = 0;

	bp.output_max_coordinate[0] = w->settings.lateral_extent.y;
	bp.output_max_coordinate[1] = 0;
	bp.output_max_coordinate[2] = w->settings.axial_extent.y;
	bp.output_max_coordinate[3] = 0;

	bp.f_number            = w->settings.f_number;
	bp.coherency_weighting = w->settings.coherency_weighting;
	bp.beamform_plane      = wc->beamform_plane;
	bp.interpolate         = 1;

	if (wc->export && !allocate_output_buffer(wc, bp.output_points))
		die("failed to allocate buffer for export data\n");

	b32 do_fft = wc->analytic || wc->low_pass || wc->matched_filter;
	fftw_context *fftw = 0;
	if (do_fft) {
		fftw = push_struct(&arena, fftw_context);

		iz total_samples = bp.rf_raw_dim[0] * bp.rf_raw_dim[1];
		fftw->input  = fftwf_alloc_real(total_samples);
		fftw->fft    = fftwf_alloc_complex(total_samples);
		fftw->ifft   = fftwf_alloc_complex(total_samples);

		if (!fftw->input || !fftw->fft || !fftw->ifft)
			die("failed to alloc space for converted f32 input data\n");

		fft_filter_context filter = {
			.samples            = bp.dec_data_dim[0],
			.sampling_frequency = bp.sampling_frequency,
			.analytic           = wc->analytic,
		};

		if (wc->low_pass) {
			filter.low_pass_cutoff_frequency = w->settings.low_pass_cutoff_frequency;
			filter.low_pass_filter_order     = w->settings.low_pass_filter_order;
		}

		if (wc->matched_filter) {
			filter.transmit_frequency   = bp.center_frequency;
			filter.die_center_frequency = bp.center_frequency;
		}

		i32 samples = bp.dec_data_dim[0];
		bp.time_offset += fftw_create_filter(fftw, filter);
		fftw->fplan = fftwf_plan_many_dft_r2c(1, &samples, bp.dec_data_dim[2],
		                                      fftw->input, 0, 1, bp.dec_data_dim[0],
		                                      fftw->fft,   0, 1, bp.dec_data_dim[0],
		                                      FFTW_MEASURE);
		fftw->bplan = fftwf_plan_many_dft(1, &samples, bp.dec_data_dim[2],
		                                  fftw->fft,  0, 1, bp.dec_data_dim[0],
		                                  fftw->ifft, 0, 1, bp.dec_data_dim[0],
		                                  FFTW_BACKWARD, FFTW_MEASURE);
	}

	i32 valid_frame_indices[LAST_SAVED_FRAME_NUMBER + 1];
	i32 valid_frames = 0;
	for (i32 frame = 0; frame <= LAST_SAVED_FRAME_NUMBER; frame++) {
		stream_reset(&path, path_work_index);
		stream_push_file_end_at_index(&path, frame);
		if (os_file_exists((char *)path.data))
			valid_frame_indices[valid_frames++] = frame;
	}

	bp.output_points[3] = MIN(valid_frames, w->settings.frame_count);

	set_beamformer_parameters(&bp);

	i32 shader_stages[16];
	i32 shader_stage_count = 0;
	if (do_fft) shader_stages[shader_stage_count++] = CS_DECODE_FLOAT_COMPLEX;
	else        shader_stages[shader_stage_count++] = CS_DECODE;
	shader_stages[shader_stage_count++] = CS_DAS;
	if (bp.output_points[3] > 1) shader_stages[shader_stage_count++] = CS_SUM;

	set_beamformer_pipeline(shader_stages, shader_stage_count);

	stream_reset(&path, path_work_index);

	b32 result = 1;
	i16 *data;
	if (raw_data_dim.z > 1) {
		Stream sb = arena_stream(arena);
		stream_append_s8(&sb, output_name);
		stream_append_byte(&sb, '_');
		iz sb_idx = sb.widx;

		data = decompress_data_at_work_index(path, valid_frame_indices[0]);
		for (u32 i = 0; i < raw_data_dim.z; i++) {
			sb.widx = sb_idx;
			stream_append_u64(&sb, i);
			i32 offset = i * raw_data_dim.x * raw_data_dim.y;
			result = send_frame(data + offset, fftw, wc->export ? wc->output : 0, &bp);
			if (wc->export && result) {
				write_output_data(wc->output_path_prefix, w->settings.output_path,
				                  stream_to_s8(&sb), wc->output, bp.output_points,
				                  v4_from_f32_array(bp.output_min_coordinate),
				                  v4_from_f32_array(bp.output_max_coordinate));
			}
			if (!wc->export) {
				fprintf(stderr, "press enter to display next frame...");
				if (fgetc(stdin) == EOF)
					break;
			}
		}
	} else {
		for (i32 frame = 0; frame < (i32)bp.output_points[3] - 1; frame++) {
			data = decompress_data_at_work_index(path, valid_frame_indices[frame]);
			send_frame(data, fftw, 0, &bp);
			free(data);
		}

		data   = decompress_data_at_work_index(path, bp.output_points[3] - 1);
		result = send_frame(data, fftw, wc->output, &bp);
		if (wc->export && result) {
			write_output_data(wc->output_path_prefix, w->settings.output_path,
			                  output_name, wc->output, bp.output_points,
			                  v4_from_f32_array(bp.output_min_coordinate),
			                  v4_from_f32_array(bp.output_max_coordinate));
		}
	}
	free(data);

	if (fftw) fftw_context_release(fftw);

	return result;
}

function void
work_loop(WorkContext *wc, Arena arena)
{
	b32 exit = 1;
	do {
		for (i32 i = 0; i < countof(work); i++) {
			WorkFrame *w = work + i;
			fprintf(stderr, "showing: %.*s\n", (i32)w->study.len, w->study.data);
			b32 result = execute_work_item(wc, w, arena, wc->path, w->study);
			if (wc->export && !result)
				fprintf(stderr, "failed to export. will retry...\n");
			if (!wc->export) {
				fprintf(stderr, "press enter to continue...");
				exit = fgetc(stdin) == EOF;
			}
		}
	} while (!exit);
}

function Rect
gen_cyst_region(v3 cyst)
{
	Rect result = {0};
	/* NOTE(rnp): to measure gCNR we want the area in the center of the cyst to match
	 * the area of a ring on the outer portion of cyst and that all needs to be in view */
	f32 r = 1.25 * sqrt_f32(2) * cyst.z / 2;
	result.pos.x  = cyst.x - r;
	result.pos.y  = cyst.x + r;
	result.size.x = cyst.y - r;
	result.size.y = cyst.y + r;
	return result;
}

function void
contrast_work_loop(WorkContext *wc, Arena arena)
{
	b32 exit = 1;
	do {
		for (i32 i = 0; i < countof(gCNR_work); i++) {
			ContrastWorkFrame *w = gCNR_work + i;
			for (i32 j = 0; j < w->cysts_count; j++) {
				v3 cyst = w->cysts[j];
				//fprintf(stderr, "\t%.*s\n\tcyst: %.02f @ {%.02f, %.02f} mm\n",
				//        (i32)w->study.len, w->study.data,
				//        cyst.z * 1e3, cyst.x * 1e3, cyst.y * 1e3);

				Arena a = arena;
				s8 saved_path = w->settings.output_path;

				Stream buf = arena_stream(a);
				stream_append_s8s(&buf, saved_path, s8(OS_PATH_SEPARATOR), w->method);
				stream_append_i64(&buf, cyst.z * 1e3 + 0.5);
				stream_append_s8(&buf, s8("mm"));
				w->settings.output_path = arena_stream_commit(&a, &buf);

				buf = arena_stream(a);
				stream_append_s8(&buf, w->study);
				stream_append_byte(&buf, '_');
				stream_append_f64(&buf, cyst.z * 1e3, 10);
				stream_append_s8(&buf, s8("mm_("));
				stream_append_f64(&buf, cyst.x * 1e3, 10);
				stream_append_byte(&buf, ',');
				stream_append_f64(&buf, cyst.y * 1e3, 10);
				stream_append_s8(&buf, s8(")"));

				Rect rect = gen_cyst_region(cyst);
				w->settings.lateral_extent = rect.pos;
				w->settings.axial_extent   = rect.size;

				s8 output_name_part = arena_stream_commit(&a, &buf);
				b32 result = execute_work_item(wc, (WorkFrame *)w, a, wc->path,
				                               output_name_part);
				if (wc->export && !result)
					fprintf(stderr, "failed to export. will retry...\n");
				if (!wc->export) {
					fprintf(stderr, "press enter to continue...");
					exit = fgetc(stdin) == EOF;
				}
				w->settings.output_path = saved_path;
			}
		}
	} while (!exit);
}

function Rect
gen_wire_region(v2 wire)
{
	Rect result = {0};
	f32 r = 2.5e-3;
	result.pos.x  = wire.x - r;
	result.pos.y  = wire.x + r;
	result.size.x = wire.y - r;
	result.size.y = wire.y + r;
	return result;
}

function void
resolution_work_loop(WorkContext *wc, Arena arena)
{
	b32 exit = 1;
	do {
		for (i32 i = 0; i < countof(resolution_work); i++) {
			ResolutionWorkFrame *w = resolution_work + i;
			for (i32 j = 0; j < w->wires_count; j++) {
				v2 wire = w->wires[j];
				Arena a = arena;
				s8 saved_path = w->settings.output_path;

				Stream buf = arena_stream(a);
				stream_append_s8s(&buf, saved_path, s8(OS_PATH_SEPARATOR), w->method);
				w->settings.output_path = arena_stream_commit(&a, &buf);

				buf = arena_stream(a);
				stream_append_s8s(&buf, w->study, s8("_("));
				stream_append_f64(&buf, wire.x * 1e3, 100);
				stream_append_byte(&buf, ',');
				stream_append_f64(&buf, wire.y * 1e3, 100);
				stream_append_s8(&buf, s8(")"));

				Rect rect = gen_wire_region(wire);
				w->settings.lateral_extent = rect.pos;
				w->settings.axial_extent   = rect.size;

				s8 output_name_part = arena_stream_commit(&a, &buf);
				b32 result = execute_work_item(wc, (WorkFrame *)w, a, wc->path,
				                               output_name_part);
				if (wc->export && !result)
					fprintf(stderr, "failed to export. will retry...\n");
				if (!wc->export) {
					fprintf(stderr, "press enter to continue...");
					exit = fgetc(stdin) == EOF;
				}
				w->settings.output_path = saved_path;
			}
		}
	} while (!exit);
}

extern i32
main(i32 argc, char *argv[])
{
	WorkContext wc = {0};
	i32 parsed = parse_argv(&wc, argc, argv);

	if (argc - parsed > 1)
		usage(argv[0]);
	shift_n(argv, argc, parsed);

	Arena arena = os_alloc_arena((Arena){0}, KB(8));
	wc.path     = stream_alloc(&arena, KB(4));
	stream_append_s8(&wc.path, c_str_to_s8(argv[0]));
	stream_ensure_termination(&wc.path, OS_PATH_SEPARATOR_CHAR);

	work_loop(&wc, arena);
	//contrast_work_loop(&wc, arena);
	//resolution_work_loop(&wc, arena);

	return 0;
}
