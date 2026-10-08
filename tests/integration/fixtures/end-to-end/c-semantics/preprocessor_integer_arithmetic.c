#include <stdio.h>

#define PP_LONG_MAX 0x7fffffffffffffffL
#define PP_ULONG_MAX (PP_LONG_MAX * 2UL + 1UL)

#if ((PP_ULONG_MAX >> 31) >> 31) != 3
#error incorrect unsigned limit arithmetic
#endif

#if !(-1 < 0) || !(-1 > 0U)
#error incorrect signed and unsigned comparisons
#endif

#if -1 < 0U || -1 != ~0U
#error incorrect mixed signed and unsigned conversions
#endif

#if 0xffffffff != 4294967295 || !(0xffffffffffffffff > 0) || !(01777777777777777777777 > 0)
#error incorrect integer literal types
#endif

#if (9223372036854775807 + 0U) * 2 != 18446744073709551614U
#error incorrect unsigned multiplication conversion
#endif

#if -7 / 3 != -2 || -7 % 3 != -1 || (-8 >> 1U) != -4
#error incorrect signed division and right shift
#endif

#if !((1 < 2U) - 2 < 0) || !((1 && 2U) - 2 < 0)
#error incorrect logical and comparison result types
#endif

#if (1 | 2U) != 3U || (7 & 3U) != 3U || (7 ^ 3U) != 4U
#error incorrect bitwise arithmetic
#endif

#if PP_ULONG_MAX + 1 != 0 || -1U != PP_ULONG_MAX
#error incorrect unsigned wrapping
#endif

#if 0U - 1 != PP_ULONG_MAX || PP_ULONG_MAX * 2 != 18446744073709551614U
#error incorrect unsigned subtraction and multiplication
#endif

#if (~0U >> 63) != 1 || !((1U << 63) > 0) || ((1U << 63) << 1) != 0
#error incorrect unsigned shifts
#endif

#if PP_ULONG_MAX / 3 != 6148914691236517205U || PP_ULONG_MAX % 2 != 1
#error incorrect unsigned division and remainder
#endif

#if (1 ? -1 : 0U) < 0 || (0 ? 0U / 0 : -1) < 0
#error incorrect conditional integer conversions
#endif

#if ((1 ? -1 : 0U) >> 63) != 1 || ((1 ? -1 : 0) >> 63) != -1
#error incorrect conditional result signedness
#endif

#if !((1 ? -1 : -1U / 0) > 0) || !((1 ? (0 ? 0U : -1) : 0) > 0)
#error incorrect nested conditional conversions
#endif

#if 0 && (1 / 0)
#error incorrect logical conjunction
#endif

#if !(1 || (1 / 0)) || !(1 ? 1 : 1 / 0) || !(0 ? 1 / 0 : 1)
#error incorrect short circuit evaluation
#endif

#if 0 && (9223372036854775807 + 1)
#error incorrect skipped signed overflow
#endif

#if !(1 || (-(-9223372036854775807 - 1)))
#error incorrect skipped negation overflow
#endif

#if !(1 || (1 << 64))
#error incorrect logical disjunction
#endif

#if (0 && (1 || (1 / 0))) || !(0 && (1 % 0) || 1)
#error incorrect nested logical evaluation
#endif

int main(void) {
    puts("preprocessor arithmetic ok");
    return 0;
}
