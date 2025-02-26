/* See LICENSE for license details. */
#include <stddef.h>
#include <stdint.h>

typedef char      c8;
typedef uint8_t   u8;
typedef int16_t   i16;
typedef uint16_t  u16;
typedef int32_t   i32;
typedef uint32_t  u32;
typedef uint32_t  b32;
typedef uint64_t  u64;
typedef float     f32;
typedef double    f64;
typedef ptrdiff_t size;
typedef ptrdiff_t iptr;

#if defined(_WIN32)
#define LIB_FN __declspec(dllexport)
#else
#define LIB_FN
#endif

typedef struct { f32 x, y; }       v2;
typedef struct { f32 x, y, z, w; } v4;
typedef struct { u32 x, y; }       uv2;
typedef struct { u32 x, y, z; }    uv3;
typedef struct { u32 x, y, z, w; } uv4;

#include "../beamformer_parameters.h"

LIB_FN b32 set_beamformer_parameters(char *shm_name, BeamformerParameters *);
LIB_FN b32 set_beamformer_pipeline(char *shm_name, i32 *stages, i32 stages_count);
LIB_FN b32 send_data(char *pipe_name, char *shm_name, i16 *data, uv2 data_dim);

/* NOTE: sends data and waits for (complex) beamformed data to be returned.
 * out_data: must be allocated by the caller as 2 f32s per output point. */
LIB_FN b32 beamform_data_synchronized_i16(char *pipe_name, char *shm_name, i16 *data, uv2 data_dim,
                                          uv4 output_points, f32 *out_data, i32 timeout_ms);

LIB_FN b32 beamform_data_synchronized_f32(char *pipe_name, char *shm_name, f32 *data, uv2 data_dim,
                                          uv4 output_points, f32 *out_data, i32 timeout_ms);
