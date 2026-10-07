#ifndef __CX_LIBC_INTERNAL_BUILTINS_H
#define __CX_LIBC_INTERNAL_BUILTINS_H 1

/*
 * Compatibility macros consumed before system headers are preprocessed.
 *
 * These are intentionally small declaration/typechecking shims. Keep target
 * facts such as __GNUC__ and __linux__ in Rust, but put C-spelled compatibility
 * macros here so they can be hardened with normal preprocessor behavior.
 */

#define __USER_LABEL_PREFIX__

#if defined(_WIN64)
#define __SIZE_TYPE__ unsigned long long
#define __PTRDIFF_TYPE__ long long
#define __INTPTR_TYPE__ long long
#define __UINTPTR_TYPE__ unsigned long long
#define __SIZE_MAX__ 0xffffffffffffffffULL
#define __PTRDIFF_MAX__ 0x7fffffffffffffffLL
#define __SIZEOF_POINTER__ 8
#elif defined(__LP64__)
#define __SIZE_TYPE__ unsigned long
#define __PTRDIFF_TYPE__ long
#define __INTPTR_TYPE__ long
#define __UINTPTR_TYPE__ unsigned long
#define __SIZE_MAX__ 0xffffffffffffffffUL
#define __PTRDIFF_MAX__ 0x7fffffffffffffffL
#define __SIZEOF_POINTER__ 8
#else
#define __SIZE_TYPE__ unsigned int
#define __PTRDIFF_TYPE__ int
#define __INTPTR_TYPE__ int
#define __UINTPTR_TYPE__ unsigned int
#define __SIZE_MAX__ 0xffffffffU
#define __PTRDIFF_MAX__ 0x7fffffff
#define __SIZEOF_POINTER__ 4
#endif

#define __WCHAR_TYPE__ int
#define __WINT_TYPE__ unsigned int

#define __CHAR_BIT__ 8
#define __SCHAR_MAX__ 0x7f
#define __SHRT_MAX__ 0x7fff
#define __INT_MAX__ 0x7fffffff
#define __LONG_LONG_MAX__ 0x7fffffffffffffffLL
#define __SIZEOF_SHORT__ 2
#define __SIZEOF_INT__ 4
#define __SIZEOF_LONG_LONG__ 8
#define __SIZEOF_FLOAT__ 4
#define __SIZEOF_DOUBLE__ 8

#if defined(__LP64__)
#define __LONG_MAX__ 0x7fffffffffffffffL
#define __SIZEOF_LONG__ 8
#else
#define __LONG_MAX__ 0x7fffffffL
#define __SIZEOF_LONG__ 4
#endif

#define __FLT_RADIX__ 2
#define __FLT_EVAL_METHOD__ 0
#define __DECIMAL_DIG__ 17

#define __FLT_MANT_DIG__ 24
#define __FLT_DIG__ 6
#define __FLT_MIN_EXP__ (-125)
#define __FLT_MIN_10_EXP__ (-37)
#define __FLT_MAX_EXP__ 128
#define __FLT_MAX_10_EXP__ 38
#define __FLT_MAX__ 3.40282346638528859811704183484516925e+38F
#define __FLT_MIN__ 1.17549435082228750796873653722224568e-38F
#define __FLT_EPSILON__ 1.19209289550781250000000000000000000e-7F

#define __DBL_MANT_DIG__ 53
#define __DBL_DIG__ 15
#define __DBL_MIN_EXP__ (-1021)
#define __DBL_MIN_10_EXP__ (-307)
#define __DBL_MAX_EXP__ 1024
#define __DBL_MAX_10_EXP__ 308
#define __DBL_MAX__ 1.79769313486231570814527423731704357e+308
#define __DBL_MIN__ 2.22507385850720138309023271733240406e-308
#define __DBL_EPSILON__ 2.22044604925031308084726333618164062e-16

/* long double is lowered as double until MIR has an extended float type. */
#define __LDBL_MANT_DIG__ __DBL_MANT_DIG__
#define __LDBL_DIG__ __DBL_DIG__
#define __LDBL_MIN_EXP__ __DBL_MIN_EXP__
#define __LDBL_MIN_10_EXP__ __DBL_MIN_10_EXP__
#define __LDBL_MAX_EXP__ __DBL_MAX_EXP__
#define __LDBL_MAX_10_EXP__ __DBL_MAX_10_EXP__
#define __LDBL_MAX__ __DBL_MAX__
#define __LDBL_MIN__ __DBL_MIN__
#define __LDBL_EPSILON__ __DBL_EPSILON__

#define __extension__

#define __const const
#define __const__ const
#define __inline inline
#define __inline__ inline
#define __restrict
#define __restrict__
#define __volatile volatile
#define __volatile__ volatile

#define __builtin_expect(x, expected) (x)
#define __builtin_constant_p(x) 0
#define __builtin_object_size(ptr, type) -1

#define __builtin_inf() (1.0 / 0.0)
#define __builtin_inff() (1.0f / 0.0f)
#define __builtin_huge_val() (1.0 / 0.0)
#define __builtin_huge_valf() (1.0f / 0.0f)
#define __builtin_nan(tag) (0.0 / 0.0)
#define __builtin_nanf(tag) (0.0f / 0.0f)

#define __builtin_bswap16(x) (x)
#define __builtin_bswap32(x) (x)
#define __builtin_bswap64(x) (x)

#endif
