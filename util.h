/* See LICENSE for license details. */
#ifndef _UTIL_H_
#define _UTIL_H_

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#ifndef asm
#define asm __asm__
#endif

#ifndef typeof
#define typeof __typeof__
#endif

#ifndef unreachable
#ifdef _MSC_VER
	#define unreachable() __assume(0)
#else
	#define unreachable() __builtin_unreachable()
#endif
#endif

#include "intrinsics.c"

#ifdef _DEBUG
	#ifdef _WIN32
		#define DEBUG_EXPORT __declspec(dllexport)
	#else
		#define DEBUG_EXPORT
	#endif
	#define ASSERT(c) do { if (!(c)) debugbreak(); } while (0);
#else
	#define DEBUG_EXPORT static
	#define ASSERT(c)
#endif

#define INVALID_CODE_PATH ASSERT(0)

#define static_assert _Static_assert

#define ARRAY_COUNT(a) (sizeof(a) / sizeof(*a))
#define ABS(x)         ((x) < 0 ? (-x) : (x))
#define CLAMP(x, a, b) ((x) < (a) ? (a) : (x) > (b) ? (b) : (x))
#define CLAMP01(x)     CLAMP(x, 0, 1)
#define ISPOWEROF2(a)  (((a) & ((a) - 1)) == 0)
#define MIN(a, b)      ((a) < (b) ? (a) : (b))
#define MAX(a, b)      ((a) > (b) ? (a) : (b))
#define ORONE(x)       ((x)? (x) : 1)
#define SIGN(x)        ((x) < 0? -1 : 1)

#define MEGABYTE (1024ULL * 1024ULL)
#define GIGABYTE (1024ULL * 1024ULL * 1024ULL)

#define U32_MAX        (0xFFFFFFFFUL)
#define F32_INFINITY   (__builtin_inff())

typedef char      c8;
typedef uint8_t   u8;
typedef int16_t   i16;
typedef uint16_t  u16;
typedef int32_t   i32;
typedef uint32_t  u32;
typedef int64_t   i64;
typedef uint64_t  u64;
typedef uint32_t  b32;
typedef float     f32;
typedef double    f64;
typedef ptrdiff_t size;
typedef ptrdiff_t iptr;

typedef struct { u8 *beg, *end; } Arena;
typedef struct { Arena *arena; u8 *old_beg; } TempArena;

typedef struct { size len; u8 *data; } s8;
#define s8(s) (s8){.len = ARRAY_COUNT(s) - 1, .data = (u8 *)s}

/* NOTE: raylib stubs */
#ifndef RAYLIB_H
typedef struct { f32 x, y; } Vector2;
typedef struct { f32 x, y, w, h; } Rectangle;
#endif

typedef union {
	struct { i32 x, y; };
	struct { i32 w, h; };
	i32 E[2];
} iv2;

typedef union {
	struct { i32 x, y, z; };
	struct { i32 w, h, d; };
	iv2 xy;
	i32 E[3];
} iv3;

typedef union {
	struct { u32 x, y; };
	struct { u32 w, h; };
	u32 E[2];
} uv2;

typedef union {
	struct { u32 x, y, z; };
	struct { u32 w, h, d; };
	uv2 xy;
	u32 E[3];
} uv3;

typedef union {
	struct { u32 x, y, z, w; };
	struct { uv3 xyz; u32 _w; };
	u32 E[4];
} uv4;

typedef union {
	struct { f32 x, y; };
	struct { f32 w, h; };
	Vector2 rl;
	f32 E[2];
} v2;

typedef union {
	struct { f32 x, y, z; };
	struct { f32 w, h, d; };
	f32 E[3];
} v3;

typedef union {
	struct { f32 x, y, z, w; };
	struct { f32 r, g, b, a; };
	struct { v3 xyz; f32 _1; };
	struct { f32 _2; v3 yzw; };
	struct { v2 xy, zw; };
	f32 E[4];
} v4;

typedef union {
	struct { v4 x, y, z, w; };
	v4  c[4];
	f32 E[16];
} m4;

typedef union {
	struct { v2 pos, size; };
	Rectangle rl;
} Rect;
#define INVERTED_INFINITY_RECT (Rect){.pos  = {.x = -F32_INFINITY, .y = -F32_INFINITY}, \
                                      .size = {.x = -F32_INFINITY, .y = -F32_INFINITY}}

typedef struct {
	iptr  file;
	char *name;
} Pipe;
#define INVALID_FILE (-1)

typedef struct {
	size filesize;
	f64  timestamp;
} FileStats;
#define ERROR_FILE_STATS (FileStats){.filesize = -1}

typedef struct {
	u8   *data;
	u32   widx;
	u32   cap;
	b32   errors;
} Stream;

enum variable_type {
	VT_NULL,
	VT_B32,
	VT_F32,
	VT_I32,
	VT_GROUP,
};

enum variable_group_type {
	VG_LISTING,
	VG_V2,
	VG_V4,
	VG_UV4,
};

typedef struct {
	void *store;
	union {
		v2  f32_limits;
		iv2 i32_limits;
	};
	f32 display_scale;
	f32 scroll_scale;
	u32 type;
	u32 flags;
} Variable;

typedef struct {
	Variable *first;
	Variable *last;
	u32 type;
} VariableGroup;

#define NULL_VARIABLE (Variable){.store = 0, .type = VT_NULL}

#define PLATFORM_ALLOC_ARENA_FN(name) Arena name(Arena old, size capacity)
typedef PLATFORM_ALLOC_ARENA_FN(platform_alloc_arena_fn);

#define PLATFORM_CLOSE_FN(name) void name(iptr file)
typedef PLATFORM_CLOSE_FN(platform_close_fn);

#define PLATFORM_OPEN_FOR_WRITE_FN(name) iptr name(c8 *fname)
typedef PLATFORM_OPEN_FOR_WRITE_FN(platform_open_for_write_fn);

#define PLATFORM_POLL_PIPE_FN(name) b32 name(Pipe *p)

#define PLATFORM_READ_PIPE_FN(name) size name(iptr pipe, void *buf, size len)
typedef PLATFORM_READ_PIPE_FN(platform_read_pipe_fn);

#define PLATFORM_WRITE_NEW_FILE_FN(name) b32 name(char *fname, s8 raw)
typedef PLATFORM_WRITE_NEW_FILE_FN(platform_write_new_file_fn);

#define PLATFORM_WRITE_FILE_FN(name) b32 name(iptr file, s8 raw)
typedef PLATFORM_WRITE_FILE_FN(platform_write_file_fn);

#define PLATFORM_FNS      \
	X(alloc_arena)    \
	X(close)          \
	X(open_for_write) \
	X(read_pipe)      \
	X(write_new_file) \
	X(write_file)

#define X(name) platform_ ## name ## _fn *name;
typedef struct { PLATFORM_FNS } Platform;
#undef X

typedef struct {
	b32  executable_reloaded;
	b32  pipe_data_available;
	iptr pipe_handle;

	v2 mouse;
	v2 last_mouse;
} BeamformerInput;

#include "util.c"

#endif /* _UTIL_H_ */
