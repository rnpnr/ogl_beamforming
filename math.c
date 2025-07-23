#include "external/cephes.c"

function void
fill_kronecker_sub_matrix(i32 *out, i32 out_stride, i32 scale, i32 *b, iv2 b_dim)
{
	f32x4 vscale = dup_f32x4((f32)scale);
	for (i32 i = 0; i < b_dim.y; i++) {
		for (i32 j = 0; j < b_dim.x; j += 4, b += 4) {
			f32x4 vb = cvt_i32x4_f32x4(load_i32x4(b));
			store_i32x4(out + j, cvt_f32x4_i32x4(mul_f32x4(vscale, vb)));
		}
		out += out_stride;
	}
}

/* NOTE: this won't check for valid space/etc and assumes row major order */
function void
kronecker_product(i32 *out, i32 *a, iv2 a_dim, i32 *b, iv2 b_dim)
{
	iv2 out_dim = {{a_dim.x * b_dim.x, a_dim.y * b_dim.y}};
	assert(out_dim.y % 4 == 0);
	for (i32 i = 0; i < a_dim.y; i++) {
		i32 *vout = out;
		for (i32 j = 0; j < a_dim.x; j++, a++) {
			fill_kronecker_sub_matrix(vout, out_dim.y, *a, b, b_dim);
			vout += b_dim.y;
		}
		out += out_dim.y * b_dim.x;
	}
}

/* NOTE/TODO: to support even more hadamard sizes use the Paley construction */
function i32 *
make_hadamard_transpose(Arena *a, i32 dim)
{
	read_only local_persist	i32 hadamard_12_12_transpose[] = {
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

	read_only local_persist i32 hadamard_20_20_transpose[] = {
		1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
		1, -1, -1,  1,  1, -1, -1, -1, -1,  1, -1,  1, -1,  1,  1,  1,  1, -1, -1,  1,
		1, -1,  1,  1, -1, -1, -1, -1,  1, -1,  1, -1,  1,  1,  1,  1, -1, -1,  1, -1,
		1,  1,  1, -1, -1, -1, -1,  1, -1,  1, -1,  1,  1,  1,  1, -1, -1,  1, -1, -1,
		1,  1, -1, -1, -1, -1,  1, -1,  1, -1,  1,  1,  1,  1, -1, -1,  1, -1, -1,  1,
		1, -1, -1, -1, -1,  1, -1,  1, -1,  1,  1,  1,  1, -1, -1,  1, -1, -1,  1,  1,
		1, -1, -1, -1,  1, -1,  1, -1,  1,  1,  1,  1, -1, -1,  1, -1, -1,  1,  1, -1,
		1, -1, -1,  1, -1,  1, -1,  1,  1,  1,  1, -1, -1,  1, -1, -1,  1,  1, -1, -1,
		1, -1,  1, -1,  1, -1,  1,  1,  1,  1, -1, -1,  1, -1, -1,  1,  1, -1, -1, -1,
		1,  1, -1,  1, -1,  1,  1,  1,  1, -1, -1,  1, -1, -1,  1,  1, -1, -1, -1, -1,
		1, -1,  1, -1,  1,  1,  1,  1, -1, -1,  1, -1, -1,  1,  1, -1, -1, -1, -1,  1,
		1,  1, -1,  1,  1,  1,  1, -1, -1,  1, -1, -1,  1,  1, -1, -1, -1, -1,  1, -1,
		1, -1,  1,  1,  1,  1, -1, -1,  1, -1, -1,  1,  1, -1, -1, -1, -1,  1, -1,  1,
		1,  1,  1,  1,  1, -1, -1,  1, -1, -1,  1,  1, -1, -1, -1, -1,  1, -1,  1, -1,
		1,  1,  1,  1, -1, -1,  1, -1, -1,  1,  1, -1, -1, -1, -1,  1, -1,  1, -1,  1,
		1,  1,  1, -1, -1,  1, -1, -1,  1,  1, -1, -1, -1, -1,  1, -1,  1, -1,  1,  1,
		1,  1, -1, -1,  1, -1, -1,  1,  1, -1, -1, -1, -1,  1, -1,  1, -1,  1,  1,  1,
		1, -1, -1,  1, -1, -1,  1,  1, -1, -1, -1, -1,  1, -1,  1, -1,  1,  1,  1,  1,
		1, -1,  1, -1, -1,  1,  1, -1, -1, -1, -1,  1, -1,  1, -1,  1,  1,  1,  1, -1,
		1,  1, -1, -1,  1,  1, -1, -1, -1, -1,  1, -1,  1, -1,  1,  1,  1,  1, -1, -1,
	};

	i32 *result = 0;

	b32 power_of_2     = ISPOWEROF2(dim);
	b32 multiple_of_12 = dim % 12 == 0;
	b32 multiple_of_20 = dim % 20 == 0;
	iz elements        = dim * dim;

	i32 base_dim = 0;
	if (power_of_2) {
		base_dim  = dim;
	} else if (multiple_of_20 && ISPOWEROF2(dim / 20)) {
		base_dim  = 20;
		dim      /= 20;
	} else if (multiple_of_12 && ISPOWEROF2(dim / 12)) {
		base_dim  = 12;
		dim      /= 12;
	}

	if (ISPOWEROF2(dim) && base_dim && arena_capacity(a, i32) >= elements * (1 + (dim != base_dim))) {
		result = push_array(a, i32, elements);

		Arena tmp = *a;
		i32 *m = dim == base_dim ? result : push_array(&tmp, i32, elements);

		#define IND(i, j) ((i) * dim + (j))
		m[0] = 1;
		for (i32 k = 1; k < dim; k *= 2) {
			for (i32 i = 0; i < k; i++) {
				for (i32 j = 0; j < k; j++) {
					i32 val = m[IND(i, j)];
					m[IND(i + k, j)]     =  val;
					m[IND(i, j + k)]     =  val;
					m[IND(i + k, j + k)] = -val;
				}
			}
		}
		#undef IND

		i32 *m2 = 0;
		iv2 m2_dim;
		switch (base_dim) {
		case 12:{ m2 = hadamard_12_12_transpose; m2_dim = (iv2){{12, 12}}; }break;
		case 20:{ m2 = hadamard_20_20_transpose; m2_dim = (iv2){{20, 20}}; }break;
		}
		if (m2) kronecker_product(result, m, (iv2){{dim, dim}}, m2, m2_dim);
	}

	return result;
}

/* NOTE(rnp): adapted from "Discrete Time Signal Processing" (Oppenheim) */
function f32 *
kaiser_low_pass_filter(Arena *arena, f32 cutoff_frequency, f32 sampling_frequency, f32 beta, i32 length)
{
	f32 *result = push_array(arena, f32, length);
	f32 wc      = 2 * PI * cutoff_frequency / sampling_frequency;
	f32 a       = (f32)length / 2.0f;
	f32 pi_i0_b = PI * (f32)cephes_i0(beta);

	for (i32 n = 0; n < length; n++) {
		f32 t       = (f32)n - a;
		f32 impulse = !f32_cmp(t, 0) ? sin_f32(wc * t) / t : 1;
		t           = t / a;
		f32 window  = (f32)cephes_i0(beta * sqrt_f32(1 - t * t)) / pi_i0_b;
		result[n]   = impulse * window;
	}

	return result;
}

function b32
iv2_equal(iv2 a, iv2 b)
{
	b32 result = a.x == b.x && a.y == b.y;
	return result;
}

function b32
iv3_equal(iv3 a, iv3 b)
{
	b32 result = a.x == b.x && a.y == b.y && a.z == b.z;
	return result;
}

function v2
clamp_v2_rect(v2 v, Rect r)
{
	v2 result = v;
	result.x = CLAMP(v.x, r.pos.x, r.pos.x + r.size.x);
	result.y = CLAMP(v.y, r.pos.y, r.pos.y + r.size.y);
	return result;
}

function v2
v2_scale(v2 a, f32 scale)
{
	v2 result;
	result.x = a.x * scale;
	result.y = a.y * scale;
	return result;
}

function v2
v2_add(v2 a, v2 b)
{
	v2 result;
	result.x = a.x + b.x;
	result.y = a.y + b.y;
	return result;
}

function v2
v2_sub(v2 a, v2 b)
{
	v2 result = v2_add(a, v2_scale(b, -1.0f));
	return result;
}

function v2
v2_mul(v2 a, v2 b)
{
	v2 result;
	result.x = a.x * b.x;
	result.y = a.y * b.y;
	return result;
}

function v2
v2_div(v2 a, v2 b)
{
	v2 result;
	result.x = a.x / b.x;
	result.y = a.y / b.y;
	return result;
}

function v2
v2_floor(v2 a)
{
	v2 result;
	result.x = (f32)((i32)a.x);
	result.y = (f32)((i32)a.y);
	return result;
}

function f32
v2_magnitude(v2 a)
{
	f32 result = sqrt_f32(a.x * a.x + a.y * a.y);
	return result;
}

function v3
cross(v3 a, v3 b)
{
	v3 result;
	result.x = a.y * b.z - a.z * b.y;
	result.y = a.z * b.x - a.x * b.z;
	result.z = a.x * b.y - a.y * b.x;
	return result;
}

function v3
v3_abs(v3 a)
{
	v3 result;
	result.x = ABS(a.x);
	result.y = ABS(a.y);
	result.z = ABS(a.z);
	return result;
}

function v3
v3_scale(v3 a, f32 scale)
{
	v3 result;
	result.x = scale * a.x;
	result.y = scale * a.y;
	result.z = scale * a.z;
	return result;
}

function v3
v3_add(v3 a, v3 b)
{
	v3 result;
	result.x = a.x + b.x;
	result.y = a.y + b.y;
	result.z = a.z + b.z;
	return result;
}

function v3
v3_sub(v3 a, v3 b)
{
	v3 result = v3_add(a, v3_scale(b, -1.0f));
	return result;
}

function v3
v3_div(v3 a, v3 b)
{
	v3 result;
	result.x = a.x / b.x;
	result.y = a.y / b.y;
	result.z = a.z / b.z;
	return result;
}

function f32
v3_dot(v3 a, v3 b)
{
	f32 result = a.x * b.x + a.y * b.y + a.z * b.z;
	return result;
}

function f32
v3_magnitude_squared(v3 a)
{
	f32 result = v3_dot(a, a);
	return result;
}

function f32
v3_magnitude(v3 a)
{
	f32 result = sqrt_f32(v3_dot(a, a));
	return result;
}

function v3
v3_normalize(v3 a)
{
	v3 result = v3_scale(a, 1.0f / v3_magnitude(a));
	return result;
}

function uv4
uv4_from_u32_array(u32 v[4])
{
	uv4 result;
	result.E[0] = v[0];
	result.E[1] = v[1];
	result.E[2] = v[2];
	result.E[3] = v[3];
	return result;
}

function b32
uv4_equal(uv4 a, uv4 b)
{
	return a.x == b.x && a.y == b.y && a.z == b.z && a.w == b.w;
}

function v4
v4_from_f32_array(f32 v[4])
{
	v4 result;
	result.E[0] = v[0];
	result.E[1] = v[1];
	result.E[2] = v[2];
	result.E[3] = v[3];
	return result;
}

function v4
v4_scale(v4 a, f32 scale)
{
	v4 result;
	result.x = scale * a.x;
	result.y = scale * a.y;
	result.z = scale * a.z;
	result.w = scale * a.w;
	return result;
}

function v4
v4_add(v4 a, v4 b)
{
	v4 result;
	result.x = a.x + b.x;
	result.y = a.y + b.y;
	result.z = a.z + b.z;
	result.w = a.w + b.w;
	return result;
}

function v4
v4_sub(v4 a, v4 b)
{
	v4 result = v4_add(a, v4_scale(b, -1));
	return result;
}

function f32
v4_dot(v4 a, v4 b)
{
	f32 result = a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w;
	return result;
}

function v4
v4_lerp(v4 a, v4 b, f32 t)
{
	v4 result = v4_add(a, v4_scale(v4_sub(b, a), t));
	return result;
}

function m4
m4_identity(void)
{
	m4 result;
	result.c[0] = (v4){{1, 0, 0, 0}};
	result.c[1] = (v4){{0, 1, 0, 0}};
	result.c[2] = (v4){{0, 0, 1, 0}};
	result.c[3] = (v4){{0, 0, 0, 1}};
	return result;
}

function v4
m4_row(m4 a, u32 row)
{
	v4 result;
	result.E[0] = a.c[0].E[row];
	result.E[1] = a.c[1].E[row];
	result.E[2] = a.c[2].E[row];
	result.E[3] = a.c[3].E[row];
	return result;
}

function m4
m4_mul(m4 a, m4 b)
{
	m4 result;
	for (u32 i = 0; i < 4; i++) {
		for (u32 j = 0; j < 4; j++) {
			result.c[i].E[j] = v4_dot(m4_row(a, j), b.c[i]);
		}
	}
	return result;
}

/* NOTE(rnp): based on:
 * https://web.archive.org/web/20131215123403/ftp://download.intel.com/design/PentiumIII/sml/24504301.pdf
 * TODO(rnp): redo with SIMD as given in the link (but need to rewrite for column-major)
 */
function m4
m4_inverse(m4 m)
{
	m4 result;
	result.E[ 0] =  m.E[5] * m.E[10] * m.E[15] - m.E[5] * m.E[11] * m.E[14] - m.E[9] * m.E[6] * m.E[15] + m.E[9] * m.E[7] * m.E[14] + m.E[13] * m.E[6] * m.E[11] - m.E[13] * m.E[7] * m.E[10];
	result.E[ 4] = -m.E[4] * m.E[10] * m.E[15] + m.E[4] * m.E[11] * m.E[14] + m.E[8] * m.E[6] * m.E[15] - m.E[8] * m.E[7] * m.E[14] - m.E[12] * m.E[6] * m.E[11] + m.E[12] * m.E[7] * m.E[10];
	result.E[ 8] =  m.E[4] * m.E[ 9] * m.E[15] - m.E[4] * m.E[11] * m.E[13] - m.E[8] * m.E[5] * m.E[15] + m.E[8] * m.E[7] * m.E[13] + m.E[12] * m.E[5] * m.E[11] - m.E[12] * m.E[7] * m.E[ 9];
	result.E[12] = -m.E[4] * m.E[ 9] * m.E[14] + m.E[4] * m.E[10] * m.E[13] + m.E[8] * m.E[5] * m.E[14] - m.E[8] * m.E[6] * m.E[13] - m.E[12] * m.E[5] * m.E[10] + m.E[12] * m.E[6] * m.E[ 9];
	result.E[ 1] = -m.E[1] * m.E[10] * m.E[15] + m.E[1] * m.E[11] * m.E[14] + m.E[9] * m.E[2] * m.E[15] - m.E[9] * m.E[3] * m.E[14] - m.E[13] * m.E[2] * m.E[11] + m.E[13] * m.E[3] * m.E[10];
	result.E[ 5] =  m.E[0] * m.E[10] * m.E[15] - m.E[0] * m.E[11] * m.E[14] - m.E[8] * m.E[2] * m.E[15] + m.E[8] * m.E[3] * m.E[14] + m.E[12] * m.E[2] * m.E[11] - m.E[12] * m.E[3] * m.E[10];
	result.E[ 9] = -m.E[0] * m.E[ 9] * m.E[15] + m.E[0] * m.E[11] * m.E[13] + m.E[8] * m.E[1] * m.E[15] - m.E[8] * m.E[3] * m.E[13] - m.E[12] * m.E[1] * m.E[11] + m.E[12] * m.E[3] * m.E[ 9];
	result.E[13] =  m.E[0] * m.E[ 9] * m.E[14] - m.E[0] * m.E[10] * m.E[13] - m.E[8] * m.E[1] * m.E[14] + m.E[8] * m.E[2] * m.E[13] + m.E[12] * m.E[1] * m.E[10] - m.E[12] * m.E[2] * m.E[ 9];
	result.E[ 2] =  m.E[1] * m.E[ 6] * m.E[15] - m.E[1] * m.E[ 7] * m.E[14] - m.E[5] * m.E[2] * m.E[15] + m.E[5] * m.E[3] * m.E[14] + m.E[13] * m.E[2] * m.E[ 7] - m.E[13] * m.E[3] * m.E[ 6];
	result.E[ 6] = -m.E[0] * m.E[ 6] * m.E[15] + m.E[0] * m.E[ 7] * m.E[14] + m.E[4] * m.E[2] * m.E[15] - m.E[4] * m.E[3] * m.E[14] - m.E[12] * m.E[2] * m.E[ 7] + m.E[12] * m.E[3] * m.E[ 6];
	result.E[10] =  m.E[0] * m.E[ 5] * m.E[15] - m.E[0] * m.E[ 7] * m.E[13] - m.E[4] * m.E[1] * m.E[15] + m.E[4] * m.E[3] * m.E[13] + m.E[12] * m.E[1] * m.E[ 7] - m.E[12] * m.E[3] * m.E[ 5];
	result.E[14] = -m.E[0] * m.E[ 5] * m.E[14] + m.E[0] * m.E[ 6] * m.E[13] + m.E[4] * m.E[1] * m.E[14] - m.E[4] * m.E[2] * m.E[13] - m.E[12] * m.E[1] * m.E[ 6] + m.E[12] * m.E[2] * m.E[ 5];
	result.E[ 3] = -m.E[1] * m.E[ 6] * m.E[11] + m.E[1] * m.E[ 7] * m.E[10] + m.E[5] * m.E[2] * m.E[11] - m.E[5] * m.E[3] * m.E[10] - m.E[ 9] * m.E[2] * m.E[ 7] + m.E[ 9] * m.E[3] * m.E[ 6];
	result.E[ 7] =  m.E[0] * m.E[ 6] * m.E[11] - m.E[0] * m.E[ 7] * m.E[10] - m.E[4] * m.E[2] * m.E[11] + m.E[4] * m.E[3] * m.E[10] + m.E[ 8] * m.E[2] * m.E[ 7] - m.E[ 8] * m.E[3] * m.E[ 6];
	result.E[11] = -m.E[0] * m.E[ 5] * m.E[11] + m.E[0] * m.E[ 7] * m.E[ 9] + m.E[4] * m.E[1] * m.E[11] - m.E[4] * m.E[3] * m.E[ 9] - m.E[ 8] * m.E[1] * m.E[ 7] + m.E[ 8] * m.E[3] * m.E[ 5];
	result.E[15] =  m.E[0] * m.E[ 5] * m.E[10] - m.E[0] * m.E[ 6] * m.E[ 9] - m.E[4] * m.E[1] * m.E[10] + m.E[4] * m.E[2] * m.E[ 9] + m.E[ 8] * m.E[1] * m.E[ 6] - m.E[ 8] * m.E[2] * m.E[ 5];

	f32 determinant = m.E[0] * result.E[0] + m.E[1] * result.E[4] + m.E[2] * result.E[8] + m.E[3] * result.E[12];
	determinant = 1.0f / determinant;
	for(i32 i = 0; i < 16; i++)
		result.E[i] *= determinant;
	return result;
}

function m4
m4_translation(v3 delta)
{
	m4 result;
	result.c[0] = (v4){{1, 0, 0, 0}};
	result.c[1] = (v4){{0, 1, 0, 0}};
	result.c[2] = (v4){{0, 0, 1, 0}};
	result.c[3] = (v4){{delta.x, delta.y, delta.z, 1}};
	return result;
}

function m4
m4_scale(v3 scale)
{
	m4 result;
	result.c[0] = (v4){{scale.x, 0,       0,       0}};
	result.c[1] = (v4){{0,       scale.y, 0,       0}};
	result.c[2] = (v4){{0,       0,       scale.z, 0}};
	result.c[3] = (v4){{0,       0,       0,       1}};
	return result;
}

function m4
m4_rotation_about_z(f32 turns)
{
	f32 sa = sin_f32(turns * 2 * PI);
	f32 ca = cos_f32(turns * 2 * PI);
	m4 result;
	result.c[0] = (v4){{ca, -sa, 0, 0}};
	result.c[1] = (v4){{sa,  ca, 0, 0}};
	result.c[2] = (v4){{0,    0, 1, 0}};
	result.c[3] = (v4){{0,    0, 0, 1}};
	return result;
}

function m4
m4_rotation_about_y(f32 turns)
{
	f32 sa = sin_f32(turns * 2 * PI);
	f32 ca = cos_f32(turns * 2 * PI);
	m4 result;
	result.c[0] = (v4){{ca, 0, -sa, 0}};
	result.c[1] = (v4){{0,  1,  0,  0}};
	result.c[2] = (v4){{sa, 0,  ca, 0}};
	result.c[3] = (v4){{0,  0,  0,  1}};
	return result;
}

function m4
y_aligned_volume_transform(v3 extent, v3 translation, f32 rotation_turns)
{
	m4 T = m4_translation(translation);
	m4 R = m4_rotation_about_y(rotation_turns);
	m4 S = m4_scale(extent);
	m4 result = m4_mul(T, m4_mul(R, S));
	return result;
}

function v4
m4_mul_v4(m4 a, v4 v)
{
	v4 result;
	result.x = v4_dot(m4_row(a, 0), v);
	result.y = v4_dot(m4_row(a, 1), v);
	result.z = v4_dot(m4_row(a, 2), v);
	result.w = v4_dot(m4_row(a, 3), v);
	return result;
}

function m4
orthographic_projection(f32 n, f32 f, f32 t, f32 r)
{
	m4 result;
	f32 a = -2 / (f - n);
	f32 b = - (f + n) / (f - n);
	result.c[0] = (v4){{1 / r, 0,     0,  0}};
	result.c[1] = (v4){{0,     1 / t, 0,  0}};
	result.c[2] = (v4){{0,     0,     a,  0}};
	result.c[3] = (v4){{0,     0,     b,  1}};
	return result;
}

function m4
perspective_projection(f32 n, f32 f, f32 fov, f32 aspect)
{
	m4 result;
	f32 t = tan_f32(fov / 2.0f);
	f32 r = t * aspect;
	f32 a = -(f + n) / (f - n);
	f32 b = -2 * f * n / (f - n);
	result.c[0] = (v4){{1 / r, 0,     0,  0}};
	result.c[1] = (v4){{0,     1 / t, 0,  0}};
	result.c[2] = (v4){{0,     0,     a, -1}};
	result.c[3] = (v4){{0,     0,     b,  0}};
	return result;
}

function m4
camera_look_at(v3 camera, v3 point)
{
	v3 orthogonal = {{0, 1.0f, 0}};
	v3 normal     = v3_normalize(v3_sub(camera, point));
	v3 right      = cross(orthogonal, normal);
	v3 up         = cross(normal,     right);

	v3 translate;
	camera      = v3_sub((v3){0}, camera);
	translate.x = v3_dot(camera, right);
	translate.y = v3_dot(camera, up);
	translate.z = v3_dot(camera, normal);

	m4 result;
	result.c[0] = (v4){{right.x,     up.x,        normal.x,    0}};
	result.c[1] = (v4){{right.y,     up.y,        normal.y,    0}};
	result.c[2] = (v4){{right.z,     up.z,        normal.z,    0}};
	result.c[3] = (v4){{translate.x, translate.y, translate.z, 1}};
	return result;
}

/* NOTE(rnp): adapted from "Essential Mathematics for Games and Interactive Applications" (Verth, Bishop) */
function f32
obb_raycast(m4 obb_orientation, v3 obb_size, v3 obb_center, ray r)
{
	v3 p = v3_sub(obb_center, r.origin);
	v3 X = obb_orientation.c[0].xyz;
	v3 Y = obb_orientation.c[1].xyz;
	v3 Z = obb_orientation.c[2].xyz;

	/* NOTE(rnp): projects direction vector onto OBB axis */
	v3 f;
	f.x = v3_dot(X, r.direction);
	f.y = v3_dot(Y, r.direction);
	f.z = v3_dot(Z, r.direction);

	/* NOTE(rnp): projects relative vector onto OBB axis */
	v3 e;
	e.x = v3_dot(X, p);
	e.y = v3_dot(Y, p);
	e.z = v3_dot(Z, p);

	f32 result = 0;
	f32 t[6] = {0};
	for (i32 i = 0; i < 3; i++) {
		if (f32_cmp(f.E[i], 0)) {
			if (-e.E[i] - obb_size.E[i] > 0 || -e.E[i] + obb_size.E[i] < 0)
				result = -1.0f;
			f.E[i] = F32_EPSILON;
		}
		t[i * 2 + 0] = (e.E[i] + obb_size.E[i]) / f.E[i];
		t[i * 2 + 1] = (e.E[i] - obb_size.E[i]) / f.E[i];
	}

	if (result != -1) {
		f32 tmin = MAX(MAX(MIN(t[0], t[1]), MIN(t[2], t[3])), MIN(t[4], t[5]));
		f32 tmax = MIN(MIN(MAX(t[0], t[1]), MAX(t[2], t[3])), MAX(t[4], t[5]));
		if (tmax >= 0 && tmin <= tmax) {
			result = tmin > 0 ? tmin : tmax;
		} else {
			result = -1;
		}
	}

	return result;
}

function v4
hsv_to_rgb(v4 hsv)
{
	/* f(k(n))   = V - V*S*max(0, min(k, min(4 - k, 1)))
	 * k(n)      = fmod((n + H * 6), 6)
	 * (R, G, B) = (f(n = 5), f(n = 3), f(n = 1))
	 */
	align_as(16) f32 nval[4] = {5.0f, 3.0f, 1.0f, 0.0f};
	f32x4 n   = load_f32x4(nval);
	f32x4 H   = dup_f32x4(hsv.x);
	f32x4 S   = dup_f32x4(hsv.y);
	f32x4 V   = dup_f32x4(hsv.z);
	f32x4 six = dup_f32x4(6);

	f32x4 t   = add_f32x4(n, mul_f32x4(six, H));
	f32x4 rem = floor_f32x4(div_f32x4(t, six));
	f32x4 k   = sub_f32x4(t, mul_f32x4(rem, six));

	t = min_f32x4(sub_f32x4(dup_f32x4(4), k), dup_f32x4(1));
	t = max_f32x4(dup_f32x4(0), min_f32x4(k, t));
	t = mul_f32x4(t, mul_f32x4(S, V));

	v4 rgba;
	store_f32x4(rgba.E, sub_f32x4(V, t));
	rgba.a = hsv.a;
	return rgba;
}

function f32
ease_cubic(f32 t)
{
	f32 result;
	if (t < 0.5f) {
		result = 4.0f * t * t * t;
	} else {
		f32 c  = -2.0f * t + 2.0f;
		result =  1.0f - c * c * c / 2.0f;
	}
	return result;
}
