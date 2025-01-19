/* See LICENSE for license details. */
static i32 hadamard_12_12_transpose[] = {
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1, -1, -1,  1, -1, -1, -1,  1,  1,  1, -1,  1,
	1,  1, -1, -1,  1, -1, -1, -1,  1,  1,  1, -1,
	1, -1,  1, -1, -1,  1, -1, -1, -1,  1,  1,  1,
	1,  1, -1,  1, -1, -1,  1, -1, -1, -1,  1,  1,
	1,  1,  1, -1,  1, -1, -1,  1, -1, -1, -1,  1,
	1,  1,  1,  1, -1,  1, -1, -1,  1, -1, -1, -1,
	1, -1,  1,  1,  1, -1,  1, -1, -1,  1, -1, -1,
	1, -1, -1,  1,  1,  1, -1,  1, -1, -1,  1, -1,
	1, -1, -1, -1,  1,  1,  1, -1,  1, -1, -1,  1,
	1,  1, -1, -1, -1,  1,  1,  1, -1,  1, -1, -1,
	1, -1,  1, -1, -1, -1,  1,  1,  1, -1,  1, -1,
};

static void *
mem_clear(void *p_, u8 c, size len)
{
	u8 *p = p_;
	while (len) p[--len] = c;
	return p;
}

static void
mem_copy(void *restrict src, void *restrict dest, size n)
{
	ASSERT(n >= 0);
	u8 *s = src, *d = dest;
	for (; n; n--) *d++ = *s++;
}

static void
mem_move(u8 *src, u8 *dest, size n)
{
	if (dest < src) mem_copy(src, dest, n);
	else            while (n) { n--; dest[n] = src[n]; }
}

#define alloc(a, t, n)    (t *)alloc_(a, sizeof(t), _Alignof(t), n)
#define push_struct(a, t) (t *)alloc_(a, sizeof(t), _Alignof(t), 1)
static void *
alloc_(Arena *a, size len, size align, size count)
{
	/* NOTE: special case 0 arena */
	if (a->beg == 0)
		return 0;

	size padding   = -(uintptr_t)a->beg & (align - 1);
	size available = a->end - a->beg - padding;
	if (available < 0 || count > available / len)
		ASSERT(0 && "arena OOM\n");
	void *p = a->beg + padding;
	a->beg += padding + count * len;
	/* TODO: Performance? */
	return mem_clear(p, 0, count * len);
}

static Arena
sub_arena(Arena *a, size size)
{
	Arena result = {0};
	if ((a->end - a->beg) >= size) {
		result.beg  = a->beg;
		result.end  = a->beg + size;
		a->beg     += size;
	}
	return result;
}

static TempArena
begin_temp_arena(Arena *a)
{
	TempArena result = {.arena = a, .old_beg = a->beg};
	return result;
}

static void
end_temp_arena(TempArena ta)
{
	Arena *a = ta.arena;
	if (a) {
		ASSERT(a->beg >= ta.old_beg)
		a->beg = ta.old_beg;
	}
}

static Stream
arena_stream(Arena *a)
{
	Stream result = {0};
	result.data   = a->beg;
	result.cap    = a->end - a->beg;
	a->beg = a->end;
	return result;
}

static Stream
stream_alloc(Arena *a, size cap)
{
	Stream result = {.cap = cap};
	result.data = alloc(a, u8, cap);
	return result;
}

static s8
stream_to_s8(Stream *s)
{
	s8 result = {.len = s->widx, .data = s->data};
	return result;
}

static void
stream_append_byte(Stream *s, u8 b)
{
	s->errors |= s->widx + 1 > s->cap;
	if (!s->errors)
		s->data[s->widx++] = b;
}

static void
stream_append_s8(Stream *s, s8 str)
{
	s->errors |= (s->cap - s->widx) < str.len;
	if (!s->errors) {
		mem_copy(str.data, s->data + s->widx, str.len);
		s->widx += str.len;
	}
}

static void
stream_append_s8_array(Stream *s, s8 *strs, size count)
{
	while (count > 0) {
		stream_append_s8(s, *strs);
		strs++;
		count--;
	}
}

static void
stream_append_u64(Stream *s, u64 n)
{
	u8 tmp[64];
	u8 *end = tmp + sizeof(tmp);
	u8 *beg = end;
	do { *--beg = '0' + (n % 10); } while (n /= 10);
	stream_append_s8(s, (s8){.len = end - beg, .data = beg});
}

static void
stream_append_i64(Stream *s, i64 n)
{
	if (n < 0) {
		stream_append_byte(s, '-');
		n *= -1;
	}
	stream_append_u64(s, n);
}

static void
stream_append_f64(Stream *s, f64 f, i64 prec)
{
	if (f < 0) {
		stream_append_byte(s, '-');
		f *= -1;
	}

	/* NOTE: round last digit */
	f += 0.5f / prec;

	if (f >= (f64)(-1UL >> 1)) {
		stream_append_s8(s, s8("inf"));
	} else {
		u64 integral = f;
		u64 fraction = (f - integral) * prec;
		stream_append_u64(s, integral);
		stream_append_byte(s, '.');
		for (i64 i = prec / 10; i > 1; i /= 10) {
			if (i > fraction)
				stream_append_byte(s, '0');
		}
		stream_append_u64(s, fraction);
	}
}

static void
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

	i32 prec = 100;
	stream_append_f64(s, f, prec);
	stream_append_byte(s, 'e');
	stream_append_byte(s, scale >= 0? '+' : '-');
	for (i32 i = prec / 10; i > 1; i /= 10)
		stream_append_byte(s, '0');
	stream_append_u64(s, ABS(scale));
}

static void
stream_append_v2(Stream *s, v2 v)
{
	stream_append_byte(s, '{');
	stream_append_f64(s, v.x, 100);
	stream_append_s8(s, s8(", "));
	stream_append_f64(s, v.y, 100);
	stream_append_byte(s, '}');
}

static void
stream_append_variable(Stream *s, Variable *var)
{
	switch (var->type) {
	case VT_F32: {
		f32 *f32_val = var->store;
		stream_append_f64(s, *f32_val * var->display_scale, 100);
	} break;
	case VT_I32: {
		i32 *i32_val = var->store;
		stream_append_i64(s, *i32_val * var->display_scale);
	} break;
	default: INVALID_CODE_PATH;
	}
}

static s8
cstr_to_s8(char *cstr)
{
	s8 result = {.data = (u8 *)cstr};
	while (*cstr) { result.len++; cstr++; }
	return result;
}

static s8
s8_cut_head(s8 s, size cut)
{
	s8 result = s;
	if (cut > 0) {
		result.data += cut;
		result.len -= cut;
	}
	return result;
}

static s8
s8alloc(Arena *a, size len)
{
	return (s8){ .data = alloc(a, u8, len), .len = len };
}

static s8
push_s8(Arena *a, s8 str)
{
	s8 result = s8alloc(a, str.len);
	mem_copy(str.data, result.data, result.len);
	return result;
}

static b32
uv4_equal(uv4 a, uv4 b)
{
	return a.x == b.x && a.y == b.y && a.z == b.z && a.w == b.w;
}

static u32
round_down_power_of_2(u32 a)
{
	u32 result = 0x80000000UL >> clz_u32(a);
	return result;
}

static v2
clamp_v2_rect(v2 v, Rect r)
{
	v2 result = v;
	result.x = CLAMP(v.x, r.pos.x, r.pos.x + r.size.x);
	result.y = CLAMP(v.y, r.pos.y, r.pos.y + r.size.y);
	return result;
}

static v2
sub_v2(v2 a, v2 b)
{
	v2 result = {
		.x = a.x - b.x,
		.y = a.y - b.y,
	};
	return result;
}

static v2
mul_v2(v2 a, v2 b)
{
	v2 result = {
		.x = a.x * b.x,
		.y = a.y * b.y,
	};
	return result;
}


static f32
magnitude_v2(v2 a)
{
	f32 result = sqrt_f32(a.x * a.x + a.y * a.y);
	return result;
}

static v3
cross(v3 a, v3 b)
{
	v3 result = {
		.x = a.y * b.z - a.z * b.y,
		.y = a.z * b.x - a.x * b.z,
		.z = a.x * b.y - a.y * b.x,
	};
	return result;
}

static v3
sub_v3(v3 a, v3 b)
{
	v3 result = {
		.x = a.x - b.x,
		.y = a.y - b.y,
		.z = a.z - b.z,
	};
	return result;
}

static v3
add_v3(v3 a, v3 b)
{
	v3 result;
	result.x = a.x + b.x;
	result.y = a.y + b.y;
	result.z = a.z + b.z;
	return result;
}

static f32
dot_v3(v3 a, v3 b)
{
	f32 result = a.x * b.x + a.y * b.y + a.z * b.z;
	return result;
}

static f32
magnitude_squared_v3(v3 a)
{
	f32 result = dot_v3(a, a);
	return result;
}

static f32
magnitude_v3(v3 a)
{
	f32 result = sqrt_f32(magnitude_squared_v3(a));
	return result;
}

static v3
normalize_v3(v3 a)
{
	f32 length = magnitude_v3(a);
	v3 result;
	result.x = a.x / length;
	result.y = a.y / length;
	result.z = a.z / length;
	return result;
}

static v3
orthogonal_v3(v3 a)
{
	f32 x = ABS(a.x);
	f32 y = ABS(a.y);
	f32 z = ABS(a.z);
	v3 b = (x < y) ? (x < z ? (v3){.x = 1} : (v3){.z = 1}) : (y < z ? (v3){.y = 1} : (v3){.z = 1});

	v3 result = cross(a, b);

	return result;
}

static v4
sub_v4(v4 a, v4 b)
{
	v4 result;
	result.x = a.x - b.x;
	result.y = a.y - b.y;
	result.z = a.z - b.z;
	result.w = a.w - b.w;
	return result;
}

static v4
div_v4(v4 a, v4 b)
{
	v4 result;
	result.x = a.x / b.x;
	result.y = a.y / b.y;
	result.z = a.z / b.z;
	result.w = a.w / b.w;
	return result;
}

static v4
abs_v4(v4 a)
{
	v4 result;
	result.x = ABS(a.x);
	result.y = ABS(a.y);
	result.z = ABS(a.z);
	result.w = ABS(a.w);
	return result;
}

static f32
magnitude_v4(v4 a)
{
	f32 result = sqrt_f32(a.x * a.x + a.y * a.y + a.z * a.z + a.w * a.w);
	return result;
}

static v4
normalize_v4(v4 a)
{
	f32 length = magnitude_v4(a);
	v4 result;
	result.x = a.x / length;
	result.y = a.y / length;
	result.z = a.z / length;
	result.w = a.w / length;
	return result;
}

static f64
parse_f64(s8 s)
{
	f64 integral = 0, fractional = 0, sign = 1;

	if (s.len && *s.data == '-') {
		sign = -1;
		s.data++;
		s.len--;
	}

	while (s.len && *s.data != '.') {
		integral *= 10;
		integral += *s.data - '0';
		s.data++;
		s.len--;
	}

	if (*s.data == '.') { s.data++; s.len--; }

	while (s.len) {
		ASSERT(s.data[s.len - 1] != '.');
		fractional /= 10;
		fractional += (f64)(s.data[--s.len] - '0') / 10.0;
	}
	f64 result = sign * (integral + fractional);
	return result;
}

static void
fill_kronecker_sub_matrix(i32 *out, i32 out_stride, i32 scale, i32 *b, uv2 b_dim)
{
	f32x4 vscale = dup_f32x4(scale);
	for (u32 i = 0; i < b_dim.y; i++) {
		for (u32 j = 0; j < b_dim.x; j += 4, b += 4) {
			f32x4 vb = cvt_i32x4_f32x4(load_i32x4(b));
			store_i32x4(cvt_f32x4_i32x4(mul_f32x4(vscale, vb)), out + j);
		}
		out += out_stride;
	}
}

/* NOTE: this won't check for valid space/etc and assumes row major order */
static void
kronecker_product(i32 *out, i32 *a, uv2 a_dim, i32 *b, uv2 b_dim)
{
	uv2 out_dim = {.x = a_dim.x * b_dim.x, .y = a_dim.y * b_dim.y};
	ASSERT(out_dim.y % 4 == 0);
	for (u32 i = 0; i < a_dim.y; i++) {
		i32 *vout = out;
		for (u32 j = 0; j < a_dim.x; j++, a++) {
			fill_kronecker_sub_matrix(vout, out_dim.y, *a, b, b_dim);
			vout += b_dim.y;
		}
		out += out_dim.y * b_dim.x;
	}
}

/* NOTE/TODO: to support even more hadamard sizes use the Paley construction */
static void
fill_hadamard_transpose(i32 *out, i32 *tmp, u32 dim)
{
	ASSERT(dim);
	b32 power_of_2     = ISPOWEROF2(dim);
	b32 multiple_of_12 = dim % 12 == 0;

	if (!power_of_2 && !multiple_of_12)
		return;

	if (!power_of_2) {
		ASSERT(multiple_of_12);
		dim /= 12;
	}

	i32 *m;
	if (power_of_2) m = out;
	else            m = tmp;

	#define IND(i, j) ((i) * dim + (j))
	m[0] = 1;
	for (u32 k = 1; k < dim; k *= 2) {
		for (u32 i = 0; i < k; i++) {
			for (u32 j = 0; j < k; j++) {
				i32 val = m[IND(i, j)];
				m[IND(i + k, j)]     =  val;
				m[IND(i, j + k)]     =  val;
				m[IND(i + k, j + k)] = -val;
			}
		}
	}
	#undef IND

	if (!power_of_2)
		kronecker_product(out, tmp, (uv2){.x = dim, .y = dim}, hadamard_12_12_transpose,
		                  (uv2){.x = 12, .y = 12});
}
