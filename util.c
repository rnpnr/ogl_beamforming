/* See LICENSE for license details. */

#ifndef _UTIL_C_
#define _UTIL_C_

#include <stdint.h>
#include <stddef.h>

#ifdef _DEBUG
#define ASSERT(c) do { if (!(c)) asm("int3; nop"); } while (0);
#define DEBUG_EXPORT
#else
#define ASSERT(c)
#define DEBUG_EXPORT static
#endif

typedef uint8_t   u8;
typedef int32_t   i32;
typedef uint32_t  u32;
typedef uint32_t  b32;
typedef float     f32;
typedef double    f64;
typedef ptrdiff_t size;

typedef struct { u8 *beg, *end; } Arena;

typedef struct { size len; u8 *data; } s8;

typedef union {
	struct { u32 x, y; };
	struct { u32 w, h; };
	u32 E[2];
} uv2;

typedef union {
	struct { u32 x, y, z; };
	struct { u32 w, h, d; };
	u32 E[3];
} uv3;

typedef union {
	struct { f32 x, y; };
	f32 E[2];
	Vector2 rl;
} v2;

typedef union {
	struct { f32 x, y, z, w; };
	struct { f32 r, g, b, a; };
	f32 E[4];
	Vector4 rl;
} v4;

enum compute_shaders {
//	CS_FORCES,
//	CS_HADAMARD_DECODE,
//	CS_HERCULES,
//	CS_MIN_MAX,
	CS_UFORCES,
	CS_LAST
};

enum program_flags {
	RELOAD_SHADERS = 1 << 0,
};

typedef struct {
	u32 programs[CS_LAST];

	/* need 3 storage buffers: incoming rf data, currently decoding rf data, decoded data.
	 * last buffer will always be for decoded data. other two will swap everytime there
	 * is new data. current operating idx is stored in rf_data_idx (0 or 1) which needs
	 * to be accessed atomically
	 */
	u32 rf_data_ssbos[3];
	_Atomic u32 rf_data_idx;

	uv3 rf_data_dim;
	i32 rf_data_dim_id;
	i32 out_data_dim_id;
} ComputeShaderCtx;

typedef struct {
	Shader    shader;
	Texture2D output;
	i32       out_data_dim_id;
} FragmentShaderCtx;

typedef struct {
	uv2 window_size;
	u32 flags;

	Color bg, fg;

	u32 out_data_ssbo;
	uv3 out_data_dim;

	ComputeShaderCtx  csctx;
	FragmentShaderCtx fsctx;
} BeamformerCtx;

#define MEGABYTE (1024ULL * 1024ULL)
#define GIGABYTE (1024ULL * 1024ULL * 1024ULL)

#define ARRAY_COUNT(a) (sizeof(a) / sizeof(*a))
#define ABS(x)         ((x) < 0 ? (-x) : (x))
#define CLAMP(x, a, b) ((x) = (x) < (a) ? (a) : (x) > (b) ? (b) : (x))

static void __attribute__((noreturn))
die(char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	exit(1);
}

static void *
mem_clear(u8 *p, u8 c, size len)
{
	while (len) p[--len] = c;
	return p;
}

#define alloc(a, t, n)  (t *)alloc_(a, sizeof(t), _Alignof(t), n)
__attribute((malloc, alloc_size(4, 2), alloc_align(3)))
static void *
alloc_(Arena *a, size len, size align, size count)
{
	size padding   = -(uintptr_t)a->beg & (align - 1);
	size available = a->end - a->beg - padding;
	if (available < 0 || count > available / len) {
		ASSERT(0);
		die("arena OOM\n");
	}
	void *p = a->beg + padding;
	a->beg += padding + count * len;
	/* TODO: Performance? */
	return mem_clear(p, 0, count * len);
}

static s8
s8alloc(Arena *a, size len)
{
	return (s8){ .data = alloc(a, u8, len), .len = len };
}

#endif /*_UTIL_C_ */
