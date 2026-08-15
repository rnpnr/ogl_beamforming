#ifndef BASE_TYPES_H
#define BASE_TYPES_H
#include "base_compiler.h"

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

#define I8_MAX           (0x0000007FL)
#define I32_MAX          (0x7FFFFFFFL)
#define S32_MAX          (0x7FFFFFFFL)
#define U8_MAX           (0x000000FFUL)
#define U16_MAX          (0x0000FFFFUL)
#define U32_MAX          (0xFFFFFFFFUL)
#define U64_MAX          (0xFFFFFFFFFFFFFFFFULL)
#define F32_EPSILON      (1e-6f)
#ifndef PI
  #define PI             (3.14159265358979323846f)
#endif

typedef char     c8;
typedef u8       b8;
typedef u16      b16;
typedef u32      b32;
typedef _Float16 f16;
typedef float    f32;
typedef double   f64;
typedef i64      iptr;
typedef u64      uptr;

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
	struct { b32 x, y, z; };
	b32 E[3];
} bv3;

typedef union {
	struct { f32 x, y; };
	struct { f32 w, h; };
	f32 E[2];
} v2;
#define V2_INFINITY (v2){{-inf32(), inf32()}}

typedef union {
	struct { f32 x,  y, z;   };
	struct { f32 w,  h, d;   };
	struct { v2  xy; f32 _1; };
	struct { f32 _2; v2 yz;  };
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

typedef struct {u8 *start, *end;} BumpArena;

typedef enum {
	ArenaFlag_NoChain = 1 << 0,

	ArenaFlag_CreationMask = ArenaFlag_NoChain,

	ArenaFlag_Sealed  = 1 << 31,
} ArenaFlags;

typedef struct {
	u64        reserve_size;
	u64        commit_size;
	ArenaFlags flags;

	void *optional_backing_store;

	char *name;
	char *allocation_site_file;
	i32   allocation_site_line;
} ArenaParameters;

typedef struct Arena Arena;
struct Arena {
	u64    position;
	u64    committed;
	u64    reserved;

	// NOTE(rnp): arena chain
	u64    base_position; // position relative to first arena in chain
	Arena *prev;
	Arena *current;

	u64        reserve_size;
	u64        commit_size;
	ArenaFlags flags;

	char *name;
	char *allocation_site_file;
	i32   allocation_site_line;
};
typedef struct { Arena *arena; u64 position; } Temp;

typedef struct { i64 length; u8 *data; } str8;
#define str8(s)        (str8){.length = countof(s) - 1, .data = (u8 *)s}
#define str8_comp(s)         {sizeof(s) - 1, (u8 *)s}
#define str8_struct(v) (str8){.length = sizeof(*v), .data = (u8 *)v}

typedef struct { i64 length; u16 *data; } str16;

#endif /* BASE_TYPES_H */
