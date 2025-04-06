/* See LICENSE for license details. */
#ifndef _UTIL_H_
#define _UTIL_H_

#include <stddef.h>
#include <stdint.h>

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

#ifdef _DEBUG
	#ifdef _WIN32
		#define DEBUG_EXPORT __declspec(dllexport)
	#else
		#define DEBUG_EXPORT
	#endif
	#define DEBUG_DECL(a) a
	#define ASSERT(c) do { if (!(c)) debugbreak(); } while (0);
#else
	#define DEBUG_EXPORT static
	#define DEBUG_DECL(a)
	#define ASSERT(c)
#endif

#define INVALID_CODE_PATH ASSERT(0)

#define static_assert _Static_assert

/* NOTE: garbage to get the prepocessor to properly stringize the value of a macro */
#define str_(x) #x
#define str(x) str_(x)

#define ARRAY_COUNT(a)   (sizeof(a) / sizeof(*a))
#define ABS(x)           ((x) < 0 ? (-x) : (x))
#define BETWEEN(x, a, b) ((x) >= (a) && (x) <= (b))
#define CLAMP(x, a, b)   ((x) < (a) ? (a) : (x) > (b) ? (b) : (x))
#define CLAMP01(x)       CLAMP(x, 0, 1)
#define ISPOWEROF2(a)    (((a) & ((a) - 1)) == 0)
#define MIN(a, b)        ((a) < (b) ? (a) : (b))
#define MAX(a, b)        ((a) > (b) ? (a) : (b))
#define ORONE(x)         ((x)? (x) : 1)
#define SIGN(x)          ((x) < 0? -1 : 1)
#define SWAP(a, b)       {typeof(a) __tmp = (a); (a) = (b); (b) = __tmp;}

/* NOTE(rnp): no guarantees about actually getting an element */
#define SLLPop(list)     list; list = list ? list->next : 0
/* NOTE(rnp): evaluates to the old value of v->next */
#define SLLPush(v, list) (v)->next; (v)->next = (list), (list) = v

#define DLLPushDown(v, list) do { \
	(v)->next = (list);                   \
	if ((v)->next) (v)->next->prev = (v); \
	(list) = (v);                         \
} while (0)

#define DLLRemove(v) do { \
	if ((v)->next) (v)->next->prev = (v)->prev; \
	if ((v)->prev) (v)->prev->next = (v)->next; \
} while (0)

#define KB(a)            ((u64)(a) << 10ULL)
#define MB(a)            ((u64)(a) << 20ULL)
#define GB(a)            ((u64)(a) << 30ULL)

#define I32_MAX          (0x7FFFFFFFL)
#define U32_MAX          (0xFFFFFFFFUL)
#define F32_INFINITY     (__builtin_inff())

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
typedef ptrdiff_t iz;
typedef size_t    uz;
typedef ptrdiff_t iptr;
typedef size_t    uptr;

#include "intrinsics.c"

typedef struct { u8 *beg, *end; } Arena;
typedef struct { Arena *arena; u8 *old_beg; } TempArena;

typedef struct { iz len; u8 *data; } s8;
#define s8(s) (s8){.len = ARRAY_COUNT(s) - 1, .data = (u8 *)s}

typedef struct { iz len; u16 *data; } s16;

typedef struct { u32 cp, consumed; } UnicodeDecode;

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
	u8   *data;
	u32   widx;
	u32   cap;
	b32   errors;
} Stream;

typedef struct OS OS;

typedef struct {
	Arena arena;
	iptr  handle;
	iptr  window_handle;
	iptr  gl_context;
	iptr  user_context;
	i32   sync_variable;
	b32   asleep;
} GLWorkerThreadContext;

#define FILE_WATCH_CALLBACK_FN(name) b32 name(OS *os, s8 path, iptr user_data, Arena tmp)
typedef FILE_WATCH_CALLBACK_FN(file_watch_callback);

typedef struct {
	iptr user_data;
	u64  hash;
	file_watch_callback *callback;
} FileWatch;

typedef struct {
	u64       hash;
	iptr      handle;
	s8        name;
	/* TODO(rnp): just push these as a linked list */
	FileWatch file_watches[16];
	u32       file_watch_count;
	Arena     buffer;
} FileWatchDirectory;

typedef struct {
	FileWatchDirectory directory_watches[4];
	iptr               handle;
	u32                directory_watch_count;
} FileWatchContext;

#define OS_ALLOC_ARENA_FN(name) Arena name(Arena old, iz capacity)
typedef OS_ALLOC_ARENA_FN(os_alloc_arena_fn);

#define OS_ADD_FILE_WATCH_FN(name) void name(OS *os, Arena *a, s8 path, \
                                             file_watch_callback *callback, iptr user_data)
typedef OS_ADD_FILE_WATCH_FN(os_add_file_watch_fn);

#define OS_WAKE_WORKER_FN(name) void name(GLWorkerThreadContext *ctx)
typedef OS_WAKE_WORKER_FN(os_wake_worker_fn);

#define OS_CLOSE_FN(name) void name(iptr file)
typedef OS_CLOSE_FN(os_close_fn);

#define OS_OPEN_FOR_WRITE_FN(name) iptr name(c8 *fname)
typedef OS_OPEN_FOR_WRITE_FN(os_open_for_write_fn);

#define OS_READ_WHOLE_FILE_FN(name) s8 name(Arena *arena, char *file)
typedef OS_READ_WHOLE_FILE_FN(os_read_whole_file_fn);

#define OS_READ_FILE_FN(name) iz name(iptr file, void *buf, iz size)
typedef OS_READ_FILE_FN(os_read_file_fn);

#define OS_WAIT_ON_VALUE_FN(name) void name(i32 *value, i32 current, i32 timeout_ms)
typedef OS_WAIT_ON_VALUE_FN(os_wait_on_value_fn);

#define OS_WAKE_WAITERS_FN(name) void name(i32 *sync)
typedef OS_WAKE_WAITERS_FN(os_wake_waiters_fn);

#define OS_WRITE_NEW_FILE_FN(name) b32 name(char *fname, s8 raw)
typedef OS_WRITE_NEW_FILE_FN(os_write_new_file_fn);

#define OS_WRITE_FILE_FN(name) b32 name(iptr file, s8 raw)
typedef OS_WRITE_FILE_FN(os_write_file_fn);

#define OS_THREAD_ENTRY_POINT_FN(name) iptr name(iptr _ctx)
typedef OS_THREAD_ENTRY_POINT_FN(os_thread_entry_point_fn);

#define OS_FNS \
	X(add_file_watch)  \
	X(alloc_arena)     \
	X(close)           \
	X(open_for_write)  \
	X(read_file)       \
	X(read_whole_file) \
	X(wait_on_value)   \
	X(wake_waiters)    \
	X(write_new_file)  \
	X(write_file)

#define RENDERDOC_GET_API_FN(name) b32 name(u32 version, void **out_api)
typedef RENDERDOC_GET_API_FN(renderdoc_get_api_fn);

#define RENDERDOC_START_FRAME_CAPTURE_FN(name) void name(iptr gl_context, iptr window_handle)
typedef RENDERDOC_START_FRAME_CAPTURE_FN(renderdoc_start_frame_capture_fn);

#define RENDERDOC_END_FRAME_CAPTURE_FN(name) b32 name(iptr gl_context, iptr window_handle)
typedef RENDERDOC_END_FRAME_CAPTURE_FN(renderdoc_end_frame_capture_fn);

typedef __attribute__((aligned(16))) u8 RenderDocAPI[216];
#define RENDERDOC_API_FN_ADDR(a, offset) (*(iptr *)((*a) + offset))
#define RENDERDOC_START_FRAME_CAPTURE(a) (renderdoc_start_frame_capture_fn *)RENDERDOC_API_FN_ADDR(a, 152)
#define RENDERDOC_END_FRAME_CAPTURE(a)   (renderdoc_end_frame_capture_fn *)  RENDERDOC_API_FN_ADDR(a, 168)

struct OS {
#define X(name) os_ ## name ## _fn *name;
	OS_FNS
#undef X
	FileWatchContext file_watch_context;
	iptr             context;
	iptr             stderr;
	GLWorkerThreadContext compute_worker;

	DEBUG_DECL(renderdoc_start_frame_capture_fn *start_frame_capture);
	DEBUG_DECL(renderdoc_end_frame_capture_fn   *end_frame_capture);
};

#include "util.c"

#endif /* _UTIL_H_ */
