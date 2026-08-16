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

#define __extension__
#define __attribute__(x)
#define __attribute(x)

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

#define __builtin_bswap16(x) (x)
#define __builtin_bswap32(x) (x)
#define __builtin_bswap64(x) (x)

#endif
