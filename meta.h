/* See LICENSE for license details. */
#define META_KIND_LIST \
	X(M4,  m4,   f32mat4,   float,    single, 64, 16) \
	X(V4,  v4,   f32vec4,   float,    single, 16,  4) \
	X(SV4, iv4,  i32vec4,   int32_t,  int32,  16,  4) \
	X(UV4, uv4,  u32vec4,   uint32_t, uint32, 16,  4) \
	X(UV2, uv2,  u32vec2,   uint32_t, uint32,  8,  2) \
	X(V3,  v3,   f32vec3,   float,    single, 12,  3) \
	X(V2,  v2,   f32vec2,   float,    single,  8,  2) \
	X(F64, f64,  float64_t, double,   double,  8,  1) \
	X(F32, f32,  float32_t, float,    single,  4,  1) \
	X(F16, f16,  float16_t, _Float16, half,    2,  1) \
	X(S32, i32,  int32_t,   int32_t,  int32,   4,  1) \
	X(S16, i16,  int16_t,   int16_t,  int16,   2,  1) \
	X(S8,  i8,   int8_t,    int8_t,   int8,    1,  1) \
	X(B64, b64,  uint64_t,  uint64_t, uint64,  8,  1) \
	X(B32, b32,  bool,      uint32_t, uint32,  4,  1) \
	X(B16, b16,  uint16_t,  uint16_t, uint16,  2,  1) \
	X(B8,  b8,   uint8_t,   uint8_t,  uint8,   1,  1) \
	X(U64, u64,  uint64_t,  uint64_t, uint64,  8,  1) \
	X(U32, u32,  uint32_t,  uint32_t, uint32,  4,  1) \
	X(U16, u16,  uint16_t,  uint16_t, uint16,  2,  1) \
	X(U8,  u8,   uint8_t,   uint8_t,  uint8,   1,  1) \
	X(STR, str8, error,     error,    error,  16,  1) \

read_only global str8 meta_kind_glsl_types[] = {
	#define X(_k, _c, glsl, ...) str8_comp(#glsl),
	META_KIND_LIST
	#undef X
};

read_only global u8 meta_kind_byte_sizes[] = {
	#define X(_k, _c, _g, _b, _m, bytes, ...) bytes,
	META_KIND_LIST
	#undef X
};

typedef enum {
	#define X(k, ...) MetaKind_## k,
	META_KIND_LIST
	#undef X
	MetaKind_Count,
} MetaKind;

typedef enum {
	MetaStructMemberFlag_ReferenceType = 1 << 0,
} MetaStructMemberFlags;

typedef enum {
	MetaStructFlag_Union         = 1 << 0,
	MetaStructFlag_ContainsUnion = 1 << 1,
} MetaStructFlags;

typedef struct {
	u32 type_id;
	u32 offset;
	u32 elements;
	u32 flags;
} MetaStructMember;

typedef struct {
	str8            name;
	u32             member_count;
	u32             size;
	MetaStructFlags flags;
} MetaStructInfo;
