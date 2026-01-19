/* See LICENSE for license details. */
#ifndef _UTIL_H_
#define _UTIL_H_

#include "compiler.h"

#if COMPILER_MSVC
  typedef unsigned __int64  u64;
  typedef signed   __int64  i64;
  typedef unsigned __int32  u32;
  typedef signed   __int32  i32;
  typedef unsigned __int16  u16;
  typedef signed   __int16  i16;
  typedef unsigned __int8   u8;
  typedef signed   __int8   i8;
#else
  typedef __UINT64_TYPE__   u64;
  typedef __INT64_TYPE__    i64;
  typedef __UINT32_TYPE__   u32;
  typedef __INT32_TYPE__    i32;
  typedef __UINT16_TYPE__   u16;
  typedef __INT16_TYPE__    i16;
  typedef __UINT8_TYPE__    u8;
  typedef __INT8_TYPE__     i8;
#endif

typedef char   c8;
typedef u8     b8;
typedef u16    b16;
typedef u32    b32;
typedef float  f32;
typedef double f64;
typedef i64    iz;
typedef u64    uz;
typedef i64    iptr;
typedef u64    uptr;

#ifndef asm
#define asm __asm__
#endif

#ifndef typeof
#define typeof __typeof__
#endif

#if OS_WINDOWS
  #define EXPORT __declspec(dllexport)
#else
  #define EXPORT
#endif

#ifdef _DEBUG
  #define DEBUG_EXPORT EXPORT
  #ifdef _BEAMFORMER_DLL
    #if OS_WINDOWS
      #define DEBUG_IMPORT __declspec(dllimport)
    #else
      #define DEBUG_IMPORT extern
    #endif
  #else
    #define DEBUG_IMPORT DEBUG_EXPORT
  #endif
  #define DEBUG_DECL(a) a
  #define assert(c) do { if (!(c)) debugbreak(); } while (0)
#else
  #define DEBUG_IMPORT global
  #define DEBUG_EXPORT function
  #define DEBUG_DECL(a)
  #define assert(c) (void)(c)
#endif
#define ASSERT assert

#if ASAN_ACTIVE
  void __asan_poison_memory_region(void *, i64);
  void __asan_unpoison_memory_region(void *, i64);
  #define asan_poison_region(region, size)   __asan_poison_memory_region((region), (size))
  #define asan_unpoison_region(region, size) __asan_unpoison_memory_region((region), (size))
#else
  #define asan_poison_region(...)
  #define asan_unpoison_region(...)
#endif

#define InvalidCodePath assert(0)
#define InvalidDefaultCase default: assert(0); break

#define arg_list(type, ...) (type []){__VA_ARGS__}, sizeof((type []){__VA_ARGS__}) / sizeof(type)

#define function      static
#define global        static
#define local_persist static

#if COMPILER_MSVC
  #define thread_static __declspec(thread)
#elif COMPILER_CLANG || COMPILER_GCC
  #define thread_static __thread
#else
  #error thread_static not defined for this compiler
#endif

#define alignof       _Alignof
#define static_assert _Static_assert

/* NOTE: garbage to get the prepocessor to properly stringize the value of a macro */
#define str_(...) #__VA_ARGS__
#define str(...) str_(__VA_ARGS__)

#define countof(a)       (iz)(sizeof(a) / sizeof(*a))
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
#define swap(a, b)       do {typeof(a) __tmp = (a); (a) = (b); (b) = __tmp;} while(0)

#define Min(a, b)        ((a) < (b) ? (a) : (b))
#define Max(a, b)        ((a) > (b) ? (a) : (b))
#define Between(x, a, b) ((x) >= (a) && (x) <= (b))
#define Clamp(x, a, b)   ((x) < (a) ? (a) : (x) > (b) ? (b) : (x))
#define Clamp01(x)       Clamp(x, 0, 1)
#define IsPowerOfTwo(a)  (((a) & ((a) - 1)) == 0)

#define IsDigit(c)       (Between((c), '0', '9'))
#define IsUpper(c)       (((c) & 0x20u) == 0)
#define ToLower(c)       (((c) | 0x20u))
#define ToUpper(c)       (((c) & ~(0x20u)))

#define f32_cmp(x, y)    (ABS((x) - (y)) <= F32_EPSILON * MAX(1.0f, MAX(ABS(x), ABS(y))))

#define DeferLoop(begin, end)          for (i32 _i_ = ((begin), 0); !_i_; _i_ += 1, (end))
#define DeferLoopTag(begin, end, tag)  for (i32 __##tag = ((begin), 0); !__##tag ; __##tag += 1, (end))

#define EachBit(a, it)                 (u64 it = ctz_u64(a); it != 64; a &= ~(1u << (it)), it = ctz_u64(a))
#define EachElement(array, it)         (u64 it = 0; it < countof(array); it += 1)
#define EachEnumValue(type, it)        (type it = (type)0; it < type##_Count; it = (type)(it + 1))
#define EachNonZeroEnumValue(type, it) (type it = (type)1; it < type##_Count; it = (type)(it + 1))

#define spin_wait(c) while ((c)) cpu_yield()

#define DA_STRUCT(kind, name) typedef struct { \
	kind *data;     \
	iz    count;    \
	iz    capacity; \
} name ##List;

#define SLLStackPush(list, n) ((n)->next = (list), (list) = (n))
// TODO(rnp): clean this up
#define SLLPush(v, list) SLLStackPush(list, v)

/* NOTE(rnp): no guarantees about actually getting an element */
#define SLLPop(list) list; list = list ? list->next : 0

#define SLLPopFreelist(list) list; do { \
	asan_unpoison_region((list), sizeof(*(list))); \
	(void)SLLPop((list)); \
} while(0)

#define SLLPushFreelist(v, list) do { \
	SLLPush((v), (list));                  \
	asan_poison_region((v), sizeof(*(v))); \
} while(0)

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

#define I8_MAX           (0x0000007FL)
#define I32_MAX          (0x7FFFFFFFL)
#define U8_MAX           (0x000000FFUL)
#define U16_MAX          (0x0000FFFFUL)
#define U32_MAX          (0xFFFFFFFFUL)
#define U64_MAX          (0xFFFFFFFFFFFFFFFFULL)
#define F32_INFINITY     (1e+300*1e+300)
#define F32_EPSILON      (1e-6f)
#ifndef PI
  #define PI             (3.14159265358979323846f)
#endif

#include "intrinsics.c"

typedef alignas(16) union {
	u8    U8[16];
	u16   U16[8];
	u32   U32[4];
	u64   U64[2];
	u32x4 U32x4;
} u128;

typedef struct { u8 *beg, *end; } Arena;
typedef struct { Arena *arena, original_arena; } TempArena;

typedef struct { iz len; u8 *data; } s8;
#define s8(s) (s8){.len = countof(s) - 1, .data = (u8 *)s}
#define s8_comp(s) {sizeof(s) - 1, (u8 *)s}

typedef struct { iz len; u16 *data; } s16;

typedef struct { u32 cp, consumed; } UnicodeDecode;

typedef enum {
	IntegerConversionResult_Invalid,
	IntegerConversionResult_OutOfRange,
	IntegerConversionResult_Success,
} IntegerConversionResult;

typedef struct {
	IntegerConversionResult result;
	union {
		u64 U64;
		i64 S64;
	};
	s8 unparsed;
} IntegerConversion;

typedef struct { u64 start, stop; } RangeU64;

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
	struct { i32 x, y, z, w; };
	struct { iv3 xyz; i32 _w; };
	i32 E[4];
} iv4;

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

#define XZ(v) (v2){.x = v.x, .y = v.z}
#define YZ(v) (v2){.x = v.y, .y = v.z}
#define XY(v) (v2){.x = v.x, .y = v.y}

typedef union {
	struct { v4 x, y, z, w; };
	v4  c[4];
	f32 E[16];
} m4;

/* TODO(rnp): delete raylib */
typedef struct {
	v3 origin;
	v3 direction;
} ray;

typedef struct { v2 pos, size; } Rect;
#define INVERTED_INFINITY_RECT (Rect){.pos  = {.x = -F32_INFINITY, .y = -F32_INFINITY}, \
                                      .size = {.x = -F32_INFINITY, .y = -F32_INFINITY}}

typedef struct {
	u8   *data;
	i32   widx;
	i32   cap;
	b32   errors;
} Stream;

#define INVALID_FILE       (-1)

#ifndef OSInvalidHandleValue
  #define OSInvalidHandleValue ((u64)-1)
  typedef struct { u64 value[1]; } OSBarrier;
  typedef struct { u64 value[1]; } OSHandle;
  typedef struct { u64 value[1]; } OSLibrary;
  typedef struct { u64 value[1]; } OSThread;
  typedef struct { u64 value[1]; } OSW32Semaphore;
#endif

#define ValidHandle(h)     ((h).value[0] != OSInvalidHandleValue)
#define InvalidHandle(h)   ((h).value[0] == OSInvalidHandleValue)

typedef struct {
	u64        index;
	u64        count;
	OSBarrier  barrier;
	u64 *      broadcast_memory;
} LaneContext;

typedef struct {
	u8   name[16];
	u64  name_length;

	LaneContext lane_context;
} ThreadContext;

#define OS_ALLOC_ARENA_FN(name)        Arena name(iz capacity)
#define OS_READ_ENTIRE_FILE_FN(name)   i64   name(const char *file, void *buffer, i64 buffer_capacity)
#define OS_WAIT_ON_ADDRESS_FN(name)    b32   name(i32 *value, i32 current, u32 timeout_ms)
#define OS_WAKE_ALL_WAITERS_FN(name)   void  name(i32 *sync)
#define OS_THREAD_ENTRY_POINT_FN(name) u64   name(void *user_context)

#define OS_WRITE_NEW_FILE_FN(name) b32 name(char *fname, s8 raw)
typedef OS_WRITE_NEW_FILE_FN(os_write_new_file_fn);

#define RENDERDOC_GET_API_FN(name) b32 name(u32 version, void **out_api)
typedef RENDERDOC_GET_API_FN(renderdoc_get_api_fn);

#define RENDERDOC_START_FRAME_CAPTURE_FN(name) void name(void *instance_handle, iptr window_handle)
typedef RENDERDOC_START_FRAME_CAPTURE_FN(renderdoc_start_frame_capture_fn);

#define RENDERDOC_END_FRAME_CAPTURE_FN(name) b32 name(void *instance_handle, iptr window_handle)
typedef RENDERDOC_END_FRAME_CAPTURE_FN(renderdoc_end_frame_capture_fn);

#define RENDERDOC_SET_CAPTURE_PATH_TEMPLATE_FN(name) void name(const char *template)
typedef RENDERDOC_SET_CAPTURE_PATH_TEMPLATE_FN(renderdoc_set_capture_path_template_fn);

typedef alignas(16) u8 RenderDocAPI[216];
#define RENDERDOC_API_FN_ADDR(a, offset)       (*(iptr *)((*a) + offset))
#define RENDERDOC_START_FRAME_CAPTURE(a)       (renderdoc_start_frame_capture_fn *)       RENDERDOC_API_FN_ADDR(a, 152)
#define RENDERDOC_END_FRAME_CAPTURE(a)         (renderdoc_end_frame_capture_fn *)         RENDERDOC_API_FN_ADDR(a, 168)
#define RENDERDOC_SET_CAPTURE_PATH_TEMPLATE(a) (renderdoc_set_capture_path_template_fn *) RENDERDOC_API_FN_ADDR(a, 184)

#include "util.c"
#include "math.c"

#endif /* _UTIL_H_ */
