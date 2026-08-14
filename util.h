/* See LICENSE for license details. */
#ifndef _UTIL_H_
#define _UTIL_H_

#include "base_platform.h"

#define da_count i32

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

#define swap(a, b)       do {typeof(a) __tmp = (a); (a) = (b); (b) = __tmp;} while(0)

#define Abs(a)           ((a) < 0 ? -(a) : (a))
#define Sign(a)          ((a) < 0 ? -1 : 1)
#define Between(x, a, b) ((x) >= (a) && (x) <= (b))
#define Clamp(x, a, b)   ((x) < (a) ? (a) : (x) > (b) ? (b) : (x))
#define Clamp01(x)       Clamp(x, 0, 1)
#define Min(a, b)        ((a) < (b) ? (a) : (b))
#define Max(a, b)        ((a) > (b) ? (a) : (b))

#define IsPowerOfTwo(a)         (((a) & ((a) - 1)) == 0)
#define AlignUpPowerOfTwo(v, a) (((v) + (a) - 1) & (~((a) - 1)))

#define IsDigit(c)        (Between((c), '0', '9'))
#define IsUpper(c)        (((c) & 0x20u) == 0)
#define ToLower(c)        (((c) | 0x20u))
#define ToUpper(c)        (((c) & ~(0x20u)))

#define IsPunctuation(c)  (Between(c, '!', '/') || Between(c, ':', '@') || Between(c, '[', '`') || Between(c, '{', '~'))
#define IsWordBoundary(c) (((c) == ' ') || IsPunctuation(c))

#define f32_equal(x, y)  (Abs((x) - (y)) <= F32_EPSILON * Max(1.0f, Max(Abs(x), Abs(y))))

#define DeferLoop(begin, end)          for (i32 _i_ = ((begin), 0); !_i_; _i_ += 1, (end))

#define EachBit(a, it)                 (u64 _##it = a, it = ctz_u64( _##it ); it != 64; _##it &= ~(1u << (it)), it = ctz_u64( _##it ))
#define EachElement(array, it)         (u64 it = 0; it < countof(array); it += 1)
#define EachEnumValue(type, it)        (type it = (type)0; it < type##_Count; it = (type)(it + 1))
#define EachNonZeroEnumValue(type, it) (type it = (type)1; it < type##_Count; it = (type)(it + 1))
#define EachIndex(count, it)           (u64 it = 0; it < count; it += 1)

#define spin_wait(c) while ((c)) cpu_yield()

// NOTE(rnp): typically for enums, wtf is wrong with modern compilers
#define circular_add(v, add, max) (((u64)(v) + (u64)(max) + (i64)(add)) % (u64)(max))

#define DA_STRUCT(kind, name) typedef struct { \
	kind     *data;     \
	da_count  count;    \
	da_count  capacity; \
} name ##List;

#define SLLStackPush(list, n, next) ((n)->next = (list), (list) = (n))
// TODO(rnp): clean this up
#define SLLPush(v, list) SLLStackPush(list, v, next)

/* NOTE(rnp): no guarantees about actually getting an element */
#define SLLPop(l, next) (l); ((l) = (l) ? (l)->next : 0)
#define SLLStackPop(l, next) ((l) = (l)->next)

#define SLLPopFreelist(list) list; do { \
	asan_unpoison_region((list), sizeof(*(list))); \
	(void)SLLPop((list), next); \
} while(0)

#define SLLPushFreelist(v, list) do { \
	SLLPush((v), (list));                  \
	asan_poison_region((v), sizeof(*(v))); \
} while(0)

#define DLLInsert(nil, f, l, n, next, prev) (\
	((f) == 0 || (f) == nil) ? ((f) = (l) = (n), (n)->next = (n)->prev = nil) :\
	((n)->next = (f), (n)->prev = (f)->prev, (f)->prev = (n), (f) = (n)),\
	(((n)->prev && (n)->prev != nil) ? ((n)->prev->next = (n)) : (0)))

#define DLLInsertFirst(nil, f, l, n, next, prev) DLLInsert(nil, f, l, n, next, prev)
#define DLLInsertLast(nil, f, l, n, next, prev)  DLLInsert(nil, l, f, n, prev, next)

#define DLLRemove(nil, f, l, n, next, prev) (\
	((n) == (f) ? (f) = (n)->next : (0)),\
	((n) == (l) ? (l) = (n)->prev : (0)),\
	(((n)->prev != nil && (n)->prev != 0) ? (n)->prev->next = (n)->next : (0)),\
	(((n)->next != nil && (n)->next != 0) ? (n)->next->prev = (n)->prev : (0)),\
	(!(f) && (l) ? (f) = (l) : (0)),\
	(!(l) && (f) ? (l) = (f) : (0)))

typedef enum {
	Axis2_X = 0,
	Axis2_Y = 1,
	Axis2_Count,
} Axis2;
#define axis2_flip(v) (!(v))

typedef alignas(16) union {
	u8    U8[16];
	u16   U16[8];
	u32   U32[4];
	u64   U64[2];
	u32x4 U32x4;
} u128;

typedef enum {
	StringMatchFlag_CaseInsensitive = (1 << 0),
	StringMatchFlag_SloppySize      = (1 << 1),
} StringMatchFlags;

typedef struct { u32 cp, consumed; } UnicodeDecode;

typedef enum {
	NumberConversionResult_Invalid,
	NumberConversionResult_OutOfRange,
	NumberConversionResult_Success,
} NumberConversionResult;

typedef enum {
	NumberConversionKind_Invalid,
	NumberConversionKind_Integer,
	NumberConversionKind_Float,
} NumberConversionKind;

typedef struct {
	NumberConversionResult result;
	NumberConversionKind   kind;
	union {
		u64 U64;
		i64 S64;
		f64 F64;
	};
	str8 unparsed;
} NumberConversion;

#define XZ(v) (v2){.x = v.x, .y = v.z}
#define YZ(v) (v2){.x = v.y, .y = v.z}
#define XY(v) (v2){.x = v.x, .y = v.y}

/* TODO(rnp): delete raylib */
typedef struct {
	v3 origin;
	v3 direction;
} ray;

typedef struct { v2 pos, size; } Rect;

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

#define OS_THREAD_ENTRY_POINT_FN(name) u64   name(void *user_context)

#include "meta.h"
#include "util.c"
#include "math.c"

#endif /* _UTIL_H_ */
