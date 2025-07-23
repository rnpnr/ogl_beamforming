/* See LICENSE for license details. */
#include "compiler.h"

#if COMPILER_CLANG || COMPILER_GCC
  #define force_inline inline __attribute__((always_inline))
#elif COMPILER_MSVC
  #define force_inline __forceinline
#endif

#if COMPILER_MSVC || (COMPILER_CLANG && OS_WINDOWS)
  #pragma section(".rdata$", read)
  #define read_only __declspec(allocate(".rdata$"))
#elif COMPILER_CLANG
  #define read_only __attribute__((section(".rodata")))
#elif COMPILER_GCC
  /* TODO(rnp): how do we do this with gcc, putting it in rodata causes warnings and writing to
   * it doesn't cause a fault */
  #define read_only
#endif

#if COMPILER_MSVC
  #define align_as(n)    __declspec(align(n))
  #define pack_struct(s) __pragma(pack(push, 1)) s __pragma(pack(pop))
  #define no_return      __declspec(noreturn)

  #define debugbreak()  __debugbreak()
  #define unreachable() __assume(0)

  #define memory_write_barrier()       _WriteBarrier()

  #define atomic_store_u64(ptr, n)     *((volatile u64 *)(ptr)) = (n)
  #define atomic_store_u32(ptr, n)     *((volatile u32 *)(ptr)) = (n)
  #define atomic_load_u64(ptr)         *((volatile u64 *)(ptr))
  #define atomic_load_u32(ptr)         *((volatile u32 *)(ptr))
  #define atomic_add_u64(ptr, n)         _InterlockedExchangeAdd64((volatile u64 *)(ptr), (n))
  #define atomic_add_u32(ptr, n)         _InterlockedExchangeAdd((volatile u32 *)(ptr), (n))
  #define atomic_and_u64(ptr, n)         _InterlockedAnd64((volatile u64 *)(ptr), (n))
  #define atomic_and_u32(ptr, n)         _InterlockedAnd((volatile u32 *)(ptr), (n))
  #define atomic_cas_u64(ptr, cptr, n)  (_InterlockedCompareExchange64((volatile u64 *)(ptr), *(cptr), (n)) == *(cptr))
  #define atomic_cas_u32(ptr, cptr, n)  (_InterlockedCompareExchange((volatile u32 *)(ptr),   *(cptr), (n)) == *(cptr))
  #define atomic_or_u32(ptr, n)          _InterlockedOr((volatile u32 *)(ptr), (n))

  #define atan2_f32(y, x) atan2f(y, x)
  #define cos_f32(a)      cosf(a)
  #define sin_f32(a)      sinf(a)
  #define tan_f32(a)      tanf(a)
  #define ceil_f32(a)     ceilf(a)
  #define sqrt_f32(a)     sqrtf(a)

  #define exp_f64(a)      exp(a)
  #define sqrt_f64(a)     sqrt(a)

#else
  #define align_as(n)      __attribute__((aligned(n)))
  #define pack_struct(s) s __attribute__((packed))
  #define no_return        __attribute__((noreturn))

  #if ARCH_ARM64
    /* TODO? debuggers just loop here forever and need a manual PC increment (step over) */
    #define debugbreak() asm volatile ("brk 0xf000")
  #else
    #define debugbreak() asm volatile ("int3; nop")
  #endif
  #define unreachable() __builtin_unreachable()

  #define memory_write_barrier()        asm volatile ("" ::: "memory")

  #define atomic_store_u64(ptr, n)      __atomic_store_n(ptr,    n, __ATOMIC_RELEASE)
  #define atomic_load_u64(ptr)          __atomic_load_n(ptr,        __ATOMIC_ACQUIRE)
  #define atomic_add_u64(ptr, n)        __atomic_fetch_add(ptr,  n, __ATOMIC_ACQ_REL)
  #define atomic_and_u64(ptr, n)        __atomic_and_fetch(ptr,  n, __ATOMIC_RELEASE)
  #define atomic_cas_u64(ptr, cptr, n)  __atomic_compare_exchange_n(ptr, cptr, n, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)
  #define atomic_or_u32(ptr, n)         __atomic_or_fetch(ptr,   n, __ATOMIC_RELEASE)
  #define atomic_add_u32                atomic_add_u64
  #define atomic_and_u32                atomic_and_u64
  #define atomic_cas_u32                atomic_cas_u64
  #define atomic_load_u32               atomic_load_u64
  #define atomic_store_u32              atomic_store_u64

  #define atan2_f32(y, x) __builtin_atan2f(y, x)
  #define cos_f32(a)      __builtin_cosf(a)
  #define sin_f32(a)      __builtin_sinf(a)
  #define tan_f32(a)      __builtin_tanf(a)
  #define ceil_f32(a)     __builtin_ceilf(a)
  #define sqrt_f32(a)     __builtin_sqrtf(a)

  #define exp_f64(a)      __builtin_exp(a)
  #define sqrt_f64(a)     __builtin_sqrt(a)
#endif

#if COMPILER_MSVC

function force_inline u32
clz_u32(u32 a)
{
	u32 result = 32, index;
	if (a) {
		_BitScanReverse(&index, a);
		result = index;
	}
	return result;
}

function force_inline u32
ctz_u32(u32 a)
{
	u32 result = 32, index;
	if (a) {
		_BitScanForward(&index, a);
		result = index;
	}
	return result;
}

#else /* !COMPILER_MSVC */

function force_inline u32
clz_u32(u32 a)
{
	u32 result = 32;
	if (a) result = (u32)__builtin_clz(a);
	return result;
}

function force_inline u32
ctz_u32(u32 a)
{
	u32 result = 32;
	if (a) result = (u32)__builtin_ctz(a);
	return result;
}

#endif

#if ARCH_ARM64
/* NOTE(rnp): we are only doing a handful of f32x4 operations so we will just use NEON and do
 * the macro renaming thing. If you are implementing a serious wide vector operation you should
 * use SVE(2) instead. The semantics are different however and the code will be written for an
 * arbitrary vector bit width. In that case you will also need x86_64 code for determining
 * the supported vector width (ideally at runtime though that may not be possible).
 */
#include <arm_neon.h>
typedef float32x4_t f32x4;
typedef int32x4_t   i32x4;

#define add_f32x4(a, b)       vaddq_f32(a, b)
#define cvt_i32x4_f32x4(a)    vcvtq_f32_s32(a)
#define cvt_f32x4_i32x4(a)    vcvtq_s32_f32(a)
#define div_f32x4(a, b)       vdivq_f32(a, b)
#define dup_f32x4(f)          vdupq_n_f32(f)
#define floor_f32x4(a)        vrndmq_f32(a)
#define load_f32x4(a)         vld1q_f32(a)
#define load_i32x4(a)         vld1q_s32(a)
#define max_f32x4(a, b)       vmaxq_f32(a, b)
#define min_f32x4(a, b)       vminq_f32(a, b)
#define mul_f32x4(a, b)       vmulq_f32(a, b)
#define set_f32x4(a, b, c, d) vld1q_f32((f32 []){d, c, b, a})
#define sqrt_f32x4(a)         vsqrtq_f32(a)
#define store_f32x4(o, a)     vst1q_f32(o, a)
#define store_i32x4(o, a)     vst1q_s32(o, a)
#define sub_f32x4(a, b)       vsubq_f32(a, b)

#elif ARCH_X64
#include <immintrin.h>
typedef __m128  f32x4;
typedef __m128i i32x4;

#define add_f32x4(a, b)       _mm_add_ps(a, b)
#define cvt_i32x4_f32x4(a)    _mm_cvtepi32_ps(a)
#define cvt_f32x4_i32x4(a)    _mm_cvtps_epi32(a)
#define div_f32x4(a, b)       _mm_div_ps(a, b)
#define dup_f32x4(f)          _mm_set1_ps(f)
#define floor_f32x4(a)        _mm_floor_ps(a)
#define load_f32x4(a)         _mm_loadu_ps(a)
#define load_i32x4(a)         _mm_loadu_si128((i32x4 *)a)
#define max_f32x4(a, b)       _mm_max_ps(a, b)
#define min_f32x4(a, b)       _mm_min_ps(a, b)
#define mul_f32x4(a, b)       _mm_mul_ps(a, b)
#define set_f32x4(a, b, c, d) _mm_set_ps(a, b, c, d)
#define sqrt_f32x4(a)         _mm_sqrt_ps(a)
#define store_f32x4(o, a)     _mm_storeu_ps(o, a)
#define store_i32x4(o, a)     _mm_storeu_si128((i32x4 *)o, a)
#define sub_f32x4(a, b)       _mm_sub_ps(a, b)

#endif
