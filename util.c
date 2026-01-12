/* See LICENSE for license details. */
#if   COMPILER_CLANG
  #pragma GCC diagnostic ignored "-Winitializer-overrides"
#elif COMPILER_GCC
  #pragma GCC diagnostic ignored "-Woverride-init"
#endif

#define zero_struct(s) mem_clear(s, 0, sizeof(*s))
function void *
mem_clear(void *restrict p_, u8 c, iz size)
{
	u8 *p = p_;
	while (size > 0) p[--size] = c;
	return p;
}

function void
mem_copy(void *restrict dest, void *restrict src, uz n)
{
	u8 *s = src, *d = dest;
	#ifdef __AVX512BW__
	{
		for (; n >= 64; n -= 64, s += 64, d += 64)
			_mm512_storeu_epi8(d, _mm512_loadu_epi8(s));
		__mmask64 k = _cvtu64_mask64(_bzhi_u64(-1ULL, n));
		_mm512_mask_storeu_epi8(d, k, _mm512_maskz_loadu_epi8(k, s));
	}
	#else
		for (; n; n--) *d++ = *s++;
	#endif
}

/* IMPORTANT: this function may fault if dest, src, and n are not multiples of 64 */
function void
memory_copy_non_temporal(void *restrict dest, void *restrict src, uz n)
{
	assume(((u64)dest & 63) == 0);
	assume(((u64)src  & 63) == 0);
	assume(((u64)n    & 63) == 0);
	u8 *s = src, *d = dest;

	#if defined(__AVX512BW__)
	{
		for (; n >= 64; n -= 64, s += 64, d += 64)
			_mm512_stream_si512((__m512i *)d, _mm512_stream_load_si512((__m512i *)s));
	}
	#elif defined(__AVX2__)
	{
		for (; n >= 32; n -= 32, s += 32, d += 32)
			_mm256_stream_si256((__m256i *)d, _mm256_stream_load_si256((__m256i *)s));
	}
	#elif ARCH_ARM64 && !COMPILER_MSVC
	{
		asm volatile (
			"cbz  %2, 2f\n"
			"1: ldnp q0, q1, [%1]\n"
			"subs %2, %2, #32\n"
			"add  %1, %1, #32\n"
			"stnp q0, q1, [%0]\n"
			"add  %0, %0, #32\n"
			"b.ne 1b\n"
			"2:"
			:  "+r"(d), "+r"(s), "+r"(n)
			:: "memory", "v0", "v1"
		);
	}
	#else
		mem_copy(d, s, n);
	#endif
}

function void
mem_move(u8 *dest, u8 *src, uz n)
{
	if (dest < src) mem_copy(dest, src, n);
	else            while (n) { n--; dest[n] = src[n]; }
}

function void *
memory_scan_backwards(void *memory, u8 byte, iz n)
{
	void *result = 0;
	u8   *s      = memory;
	while (n > 0) if (s[--n] == byte) { result = s + n; break; }
	return result;
}

function Arena
arena_from_memory(void *memory, u64 size)
{
	Arena result;
	result.beg = memory;
	result.end = result.beg + size;
	return result;
}

function void *
align_pointer_up(void *p, uz alignment)
{
	uz padding = -(u64)p & (alignment - 1);
	void *result = (u8 *)p + padding;
	return result;
}

function void *
arena_aligned_start(Arena a, uz alignment)
{
	return align_pointer_up(a.beg, alignment);
}

#define arena_capacity(a, t) arena_capacity_(a, sizeof(t), alignof(t))
function iz
arena_capacity_(Arena *a, iz size, uz alignment)
{
	iz available = a->end - (u8 *)arena_aligned_start(*a, alignment);
	iz result    = available / size;
	return result;
}

function u8 *
arena_commit(Arena *a, iz size)
{
	assert(a->end - a->beg >= size);
	u8 *result = a->beg;
	a->beg += size;
	return result;
}

function void
arena_pop(Arena *a, iz length)
{
	a->beg -= length;
}

typedef enum {
	ArenaAllocateFlags_NoZero = 1 << 0,
} ArenaAllocateFlags;

typedef struct {
	iz size;
	uz align;
	iz count;
	ArenaAllocateFlags flags;
} ArenaAllocateInfo;

#define arena_alloc(a, ...)         arena_alloc_(a, (ArenaAllocateInfo){.align = 8, .count = 1, ##__VA_ARGS__})
#define push_array(a, t, n)         (t *)arena_alloc(a, .size = sizeof(t), .align = alignof(t), .count = n)
#define push_array_no_zero(a, t, n) (t *)arena_alloc(a, .size = sizeof(t), .align = alignof(t), .count = n, .flags = ArenaAllocateFlags_NoZero)
#define push_struct(a, t)           push_array(a, t, 1)
#define push_struct_no_zero(a, t)   push_array_no_zero(a, t, 1)

function void *
arena_alloc_(Arena *a, ArenaAllocateInfo info)
{
	void *result = 0;
	if (a->beg) {
		u8 *start = arena_aligned_start(*a, info.align);
		iz available = a->end - start;
		assert((available >= 0 && info.count <= available / info.size));
		asan_unpoison_region(start, info.count * info.size);
		a->beg = start + info.count * info.size;
		result = start;
		if ((info.flags & ArenaAllocateFlags_NoZero) == 0)
			result = mem_clear(start, 0, info.count * info.size);
	}
	return result;
}

function Arena
sub_arena(Arena *a, iz size, uz align)
{
	Arena result = {.beg = arena_alloc(a, .size = size, .align = align, .flags = ArenaAllocateFlags_NoZero)};
	result.end   = result.beg + size;
	return result;
}

function Arena
sub_arena_end(Arena *a, iz len, uz align)
{
	Arena result;
	result.beg = (u8 *)((u64)(a->end - len) & ~(align - 1)),
	result.end = a->end,

	a->end = result.beg;
	assert(a->end >= a->beg);

	return result;
}

function TempArena
begin_temp_arena(Arena *a)
{
	TempArena result = {.arena = a, .original_arena = *a};
	return result;
}

function void
end_temp_arena(TempArena ta)
{
	Arena *a = ta.arena;
	if (a) {
		assert(a->beg >= ta.original_arena.beg);
		*a = ta.original_arena;
	}
}


enum { DA_INITIAL_CAP = 16 };

#define da_index(it, s) ((it) - (s)->data)
#define da_reserve(a, s, n) \
  (s)->data = da_reserve_((a), (s)->data, &(s)->capacity, (s)->count + n, \
                          _Alignof(typeof(*(s)->data)), sizeof(*(s)->data))

#define da_append_count(a, s, items, item_count) do { \
	da_reserve((a), (s), (item_count));                                             \
	mem_copy((s)->data + (s)->count, (items), sizeof(*(items)) * (uz)(item_count)); \
	(s)->count += (item_count);                                                     \
} while (0)

#define da_push(a, s) \
  ((s)->count == (s)->capacity  \
    ? da_reserve(a, s, 1),      \
      (s)->data + (s)->count++  \
    : (s)->data + (s)->count++)

function void *
da_reserve_(Arena *a, void *data, iz *capacity, iz needed, uz align, iz size)
{
	iz cap = *capacity;

	/* NOTE(rnp): handle both 0 initialized DAs and DAs that need to be moved (they started
	 * on the stack or someone allocated something in the middle of the arena during usage) */
	if (!data || a->beg != (u8 *)data + cap * size) {
		void *copy = arena_alloc(a, .size = size, .align = align, .count = cap);
		if (data) mem_copy(copy, data, (uz)(cap * size));
		data = copy;
	}

	if (!cap) cap = DA_INITIAL_CAP;
	while (cap < needed) cap *= 2;
	arena_alloc(a, .size = size, .align = align, .count = cap - *capacity);
	*capacity = cap;
	return data;
}

function u32
utf8_encode(u8 *out, u32 cp)
{
	u32 result = 1;
	if (cp <= 0x7F) {
		out[0] = cp & 0x7F;
	} else if (cp <= 0x7FF) {
		result = 2;
		out[0] = ((cp >>  6) & 0x1F) | 0xC0;
		out[1] = ((cp >>  0) & 0x3F) | 0x80;
	} else if (cp <= 0xFFFF) {
		result = 3;
		out[0] = ((cp >> 12) & 0x0F) | 0xE0;
		out[1] = ((cp >>  6) & 0x3F) | 0x80;
		out[2] = ((cp >>  0) & 0x3F) | 0x80;
	} else if (cp <= 0x10FFFF) {
		result = 4;
		out[0] = ((cp >> 18) & 0x07) | 0xF0;
		out[1] = ((cp >> 12) & 0x3F) | 0x80;
		out[2] = ((cp >>  6) & 0x3F) | 0x80;
		out[3] = ((cp >>  0) & 0x3F) | 0x80;
	} else {
		out[0] = '?';
	}
	return result;
}

function UnicodeDecode
utf16_decode(u16 *data, iz length)
{
	UnicodeDecode result = {.cp = U32_MAX};
	if (length) {
		result.consumed = 1;
		result.cp = data[0];
		if (length > 1 && BETWEEN(data[0], 0xD800u, 0xDBFFu)
		               && BETWEEN(data[1], 0xDC00u, 0xDFFFu))
		{
			result.consumed = 2;
			result.cp = ((data[0] - 0xD800u) << 10u) | ((data[1] - 0xDC00u) + 0x10000u);
		}
	}
	return result;
}

function u32
utf16_encode(u16 *out, u32 cp)
{
	u32 result = 1;
	if (cp == U32_MAX) {
		out[0] = '?';
	} else if (cp < 0x10000u) {
		out[0] = (u16)cp;
	} else {
		u32 value = cp - 0x10000u;
		out[0] = (u16)(0xD800u + (value >> 10u));
		out[1] = (u16)(0xDC00u + (value & 0x3FFu));
		result = 2;
	}
	return result;
}

function Stream
stream_from_buffer(u8 *buffer, u32 capacity)
{
	Stream result = {.data = buffer, .cap = (i32)capacity};
	return result;
}

function Stream
stream_alloc(Arena *a, i32 cap)
{
	Stream result = stream_from_buffer(arena_commit(a, cap), (u32)cap);
	return result;
}

function s8
stream_to_s8(Stream *s)
{
	s8 result = s8("");
	if (!s->errors) result = (s8){.len = s->widx, .data = s->data};
	return result;
}

function void
stream_reset(Stream *s, i32 index)
{
	s->errors = s->cap <= index;
	if (!s->errors)
		s->widx = index;
}

function void
stream_commit(Stream *s, i32 count)
{
	s->errors |= !BETWEEN(s->widx + count, 0, s->cap);
	if (!s->errors)
		s->widx += count;
}

function void
stream_append(Stream *s, void *data, iz count)
{
	s->errors |= (s->cap - s->widx) < count;
	if (!s->errors) {
		mem_copy(s->data + s->widx, data, (uz)count);
		s->widx += (i32)count;
	}
}

function void
stream_append_byte(Stream *s, u8 b)
{
	stream_append(s, &b, 1);
}

function void
stream_pad(Stream *s, u8 b, i32 n)
{
	while (n > 0) stream_append_byte(s, b), n--;
}

function void
stream_append_s8(Stream *s, s8 str)
{
	stream_append(s, str.data, str.len);
}

#define stream_append_s8s(s, ...) stream_append_s8s_(s, arg_list(s8, ##__VA_ARGS__))
function void
stream_append_s8s_(Stream *s, s8 *strs, iz count)
{
	for (iz i = 0; i < count; i++)
		stream_append(s, strs[i].data, strs[i].len);
}

function void
stream_append_u64_width(Stream *s, u64 n, u64 min_width)
{
	u8 tmp[64];
	u8 *end = tmp + sizeof(tmp);
	u8 *beg = end;
	min_width = MIN(sizeof(tmp), min_width);

	do { *--beg = (u8)('0' + (n % 10)); } while (n /= 10);
	while (end - beg > 0 && (uz)(end - beg) < min_width)
		*--beg = '0';

	stream_append(s, beg, end - beg);
}

function void
stream_append_u64(Stream *s, u64 n)
{
	stream_append_u64_width(s, n, 0);
}

function void
stream_append_hex_u64_width(Stream *s, u64 n, iz width)
{
	assert(width <= 16);
	if (!s->errors) {
		u8  buf[16];
		u8 *end = buf + sizeof(buf);
		u8 *beg = end;
		while (n) {
			*--beg = (u8)"0123456789abcdef"[n & 0x0F];
			n >>= 4;
		}
		while (end - beg < width)
			*--beg = '0';
		stream_append(s, beg, end - beg);
	}
}

function void
stream_append_hex_u64(Stream *s, u64 n)
{
	stream_append_hex_u64_width(s, n, 2);
}

function void
stream_append_i64(Stream *s, i64 n)
{
	if (n < 0) {
		stream_append_byte(s, '-');
		n *= -1;
	}
	stream_append_u64(s, (u64)n);
}

function void
stream_append_f64(Stream *s, f64 f, u64 prec)
{
	if (f < 0) {
		stream_append_byte(s, '-');
		f *= -1;
	}

	/* NOTE: round last digit */
	f += 0.5f / (f64)prec;

	if (f >= (f64)(-1UL >> 1)) {
		stream_append_s8(s, s8("inf"));
	} else {
		u64 integral = (u64)f;
		u64 fraction = (u64)((f - (f64)integral) * (f64)prec);
		stream_append_u64(s, integral);
		stream_append_byte(s, '.');
		for (u64 i = prec / 10; i > 1; i /= 10) {
			if (i > fraction)
				stream_append_byte(s, '0');
		}
		stream_append_u64(s, fraction);
	}
}

function void
stream_append_f64_e(Stream *s, f64 f)
{
	/* TODO: there should be a better way of doing this */
	#if 0
	/* NOTE: we ignore subnormal numbers for now */
	union { f64 f; u64 u; } u = {.f = f};
	i32 exponent = ((u.u >> 52) & 0x7ff) - 1023;
	f32 log_10_of_2 = 0.301f;
	i32 scale       = (exponent * log_10_of_2);
	/* NOTE: normalize f */
	for (i32 i = ABS(scale); i > 0; i--)
		f *= (scale > 0)? 0.1f : 10.0f;
	#else
	i32 scale = 0;
	if (f != 0) {
		while (f > 1) {
			f *= 0.1f;
			scale++;
		}
		while (f < 1) {
			f *= 10.0f;
			scale--;
		}
	}
	#endif

	u32 prec = 100;
	stream_append_f64(s, f, prec);
	stream_append_byte(s, 'e');
	stream_append_byte(s, scale >= 0? '+' : '-');
	for (u32 i = prec / 10; i > 1; i /= 10)
		stream_append_byte(s, '0');
	stream_append_u64(s, (u64)ABS(scale));
}

function void
stream_append_v2(Stream *s, v2 v)
{
	stream_append_byte(s, '{');
	stream_append_f64(s, v.x, 100);
	stream_append_s8(s, s8(", "));
	stream_append_f64(s, v.y, 100);
	stream_append_byte(s, '}');
}

function Stream
arena_stream(Arena a)
{
	Stream result = {0};
	result.data   = a.beg;
	result.cap    = (i32)(a.end - a.beg);

	/* TODO(rnp): no idea what to do here if we want to maintain the ergonomics */
	asan_unpoison_region(result.data, result.cap);

	return result;
}

function s8
arena_stream_commit(Arena *a, Stream *s)
{
	ASSERT(s->data == a->beg);
	s8 result = stream_to_s8(s);
	arena_commit(a, result.len);
	return result;
}

function s8
arena_stream_commit_zero(Arena *a, Stream *s)
{
	b32 error = s->errors || s->widx == s->cap;
	if (!error)
		s->data[s->widx] = 0;
	s8 result = stream_to_s8(s);
	arena_commit(a, result.len + 1);
	return result;
}

function s8
arena_stream_commit_and_reset(Arena *arena, Stream *s)
{
	s8 result = arena_stream_commit_zero(arena, s);
	*s = arena_stream(*arena);
	return result;
}

#if !defined(XXH_IMPLEMENTATION)
# define XXH_INLINE_ALL
# define XXH_IMPLEMENTATION
# define XXH_STATIC_LINKING_ONLY
# include "external/xxhash.h"
#endif

function u128
u128_hash_from_data(void *data, uz size)
{
	u128 result = {0};
	XXH128_hash_t hash = XXH3_128bits_withSeed(data, size, 4969);
	mem_copy(&result, &hash, sizeof(result));
	return result;
}

function u64
u64_hash_from_s8(s8 v)
{
	u64 result = XXH3_64bits_withSeed(v.data, (uz)v.len, 4969);
	return result;
}

function s8
c_str_to_s8(char *cstr)
{
	s8 result = {.data = (u8 *)cstr};
	if (cstr) { while (*cstr) { result.len++; cstr++; } }
	return result;
}

function b32
s8_equal(s8 a, s8 b)
{
	b32 result = a.len == b.len;
	for (iz i = 0; result && i < a.len; i++)
		result = a.data[i] == b.data[i];
	return result;
}

/* NOTE(rnp): returns < 0 if byte is not found */
function iz
s8_scan_backwards(s8 s, u8 byte)
{
	iz result = (u8 *)memory_scan_backwards(s.data, byte, s.len) - s.data;
	return result;
}

function s8
s8_cut_head(s8 s, iz cut)
{
	s8 result = s;
	if (cut > 0) {
		result.data += cut;
		result.len  -= cut;
	}
	return result;
}

function s8
s8_alloc(Arena *a, iz len)
{
	s8 result = {.data = push_array(a, u8, len), .len = len};
	return result;
}

function s8
s16_to_s8(Arena *a, s16 in)
{
	s8 result = s8("");
	if (in.len) {
		iz commit = in.len * 4;
		iz length = 0;
		u8 *data = arena_commit(a, commit + 1);
		u16 *beg = in.data;
		u16 *end = in.data + in.len;
		while (beg < end) {
			UnicodeDecode decode = utf16_decode(beg, end - beg);
			length += utf8_encode(data + length, decode.cp);
			beg    += decode.consumed;
		}
		data[length] = 0;
		result = (s8){.len = length, .data = data};
		arena_pop(a, commit - length);
	}
	return result;
}

function s16
s8_to_s16(Arena *a, s8 in)
{
	s16 result = {0};
	if (in.len) {
		iz required = 2 * in.len + 1;
		u16 *data   = push_array(a, u16, required);
		iz length   = 0;
		/* TODO(rnp): utf8_decode */
		for (iz i = 0; i < in.len; i++) {
			u32 cp  = in.data[i];
			length += utf16_encode(data + length, cp);
		}
		result = (s16){.len = length, .data = data};
		arena_pop(a, required - length);
	}
	return result;
}

#define push_s8_from_parts(a, j, ...) push_s8_from_parts_((a), (j), arg_list(s8, __VA_ARGS__))
function s8
push_s8_from_parts_(Arena *arena, s8 joiner, s8 *parts, iz count)
{
	iz length = joiner.len * (count - 1);
	for (iz i = 0; i < count; i++)
		length += parts[i].len;

	s8 result = {.len = length, .data = arena_commit(arena, length + 1)};

	iz offset = 0;
	for (iz i = 0; i < count; i++) {
		if (i != 0) {
			mem_copy(result.data + offset, joiner.data, (uz)joiner.len);
			offset += joiner.len;
		}
		mem_copy(result.data + offset, parts[i].data, (uz)parts[i].len);
		offset += parts[i].len;
	}
	result.data[result.len] = 0;

	return result;
}

function s8
push_s8(Arena *a, s8 str)
{
	s8 result   = s8_alloc(a, str.len + 1);
	result.len -= 1;
	mem_copy(result.data, str.data, (uz)result.len);
	return result;
}

/* NOTE(rnp): from Hacker's Delight */
function force_inline u64
round_down_power_of_two(u64 a)
{
	u64 result = 0x8000000000000000ULL >> clz_u64(a);
	return result;
}

function force_inline u64
round_up_power_of_two(u64 a)
{
	u64 result = 0x8000000000000000ULL >> (clz_u64(a - 1) - 1);
	return result;
}

function force_inline iz
round_up_to(iz value, iz multiple)
{
	iz result = value;
	if (value % multiple != 0)
		result += multiple - value % multiple;
	return result;
}

function void
split_rect_horizontal(Rect rect, f32 fraction, Rect *left, Rect *right)
{
	if (left) {
		left->pos    = rect.pos;
		left->size.h = rect.size.h;
		left->size.w = rect.size.w * fraction;
	}
	if (right) {
		right->pos    = rect.pos;
		right->pos.x += rect.size.w * fraction;
		right->size.h = rect.size.h;
		right->size.w = rect.size.w * (1.0f - fraction);
	}
}

function void
split_rect_vertical(Rect rect, f32 fraction, Rect *top, Rect *bot)
{
	if (top) {
		top->pos    = rect.pos;
		top->size.w = rect.size.w;
		top->size.h = rect.size.h * fraction;
	}
	if (bot) {
		bot->pos    = rect.pos;
		bot->pos.y += rect.size.h * fraction;
		bot->size.w = rect.size.w;
		bot->size.h = rect.size.h * (1.0f - fraction);
	}
}

function void
cut_rect_horizontal(Rect rect, f32 at, Rect *left, Rect *right)
{
	at = MIN(at, rect.size.w);
	if (left) {
		*left = rect;
		left->size.w = at;
	}
	if (right) {
		*right = rect;
		right->pos.x  += at;
		right->size.w -= at;
	}
}

function void
cut_rect_vertical(Rect rect, f32 at, Rect *top, Rect *bot)
{
	at = MIN(at, rect.size.h);
	if (top) {
		*top = rect;
		top->size.h = at;
	}
	if (bot) {
		*bot = rect;
		bot->pos.y  += at;
		bot->size.h -= at;
	}
}

function IntegerConversion
integer_from_s8(s8 raw)
{
	read_only local_persist alignas(64) i8 lut[64] = {
		 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
		-1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	};

	IntegerConversion result = {.unparsed = raw};

	iz  i     = 0;
	i64 scale = 1;
	if (raw.len > 0 && raw.data[0] == '-') {
		scale = -1;
		i     =  1;
	}

	b32 hex = 0;
	if (raw.len - i > 2 && raw.data[i] == '0' && (raw.data[1] == 'x' || raw.data[1] == 'X')) {
		hex = 1;
		i += 2;
	}

	#define integer_conversion_body(radix, clamp) do {\
		for (; i < raw.len; i++) {\
			i64 value = lut[Min((u8)(raw.data[i] - (u8)'0'), clamp)];\
			if (value >= 0) {\
				if (result.U64 > (U64_MAX - (u64)value) / radix) {\
					result.result = IntegerConversionResult_OutOfRange;\
					result.U64    = U64_MAX;\
					return result;\
				} else {\
					result.U64 = radix * result.U64 + (u64)value;\
				}\
			} else {\
				break;\
			}\
		}\
	} while (0)

	if (hex) integer_conversion_body(16u, 63u);
	else     integer_conversion_body(10u, 15u);

	#undef integer_conversion_body

	result.unparsed = (s8){.len = raw.len - i, .data = raw.data + i};
	result.result   = IntegerConversionResult_Success;
	if (scale < 0) result.U64 = 0 - result.U64;

	return result;
}

function f64
parse_f64(s8 s)
{
	IntegerConversion integral = integer_from_s8(s);

	s = integral.unparsed;
	if (*s.data == '.') { s.data++; s.len--; }
	while (s.len > 0 && s.data[s.len - 1] == '0') s.len--;

	IntegerConversion fractional = integer_from_s8(s);

	u64 power = (u64)(fractional.unparsed.data - s.data);
	f64 frac  = (f64)fractional.U64;
	while (power > 0) { frac /= 10.0; power--; }

	f64 result = (f64)integral.S64 + frac;
	return result;
}
