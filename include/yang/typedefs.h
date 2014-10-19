//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_TYPEDEFS_H
#define YANG_INCLUDE_YANG_TYPEDEFS_H
/**
 * #preamble
 *
 * ``typedef``\s
 * =============
 */

#include <cmath>
#include <cstdint>
#include <type_traits>

/** #sumline ## */
namespace yang {
namespace internal {
  template<bool B, typename T = void>
  using enable_if = typename std::enable_if<B, T>::type;
  typedef void (*void_fp)();
}

/** #toplevel */
typedef int32_t int_t;
/** #toplevel */
typedef uint32_t uint_t;
/** #toplevel ## */
typedef double float_t;

/** #toplevel */
inline int_t euclidean_div(int_t n, int_t d);

/** #toplevel ## */
inline int_t euclidean_mod(int_t n, int_t d);

inline int_t euclidean_div(int_t n, int_t d)
{
  bool sign = (n < 0) == (d < 0);
  int_t t = (n < 0 ? -(1 + n) : n) / std::abs(d);
  return (d < 0) + (sign ? t : -(1 + t));
}

inline int_t euclidean_mod(int_t n, int_t d)
{
  int_t absn = std::abs(n);
  int_t absd = std::abs(d);
  return (n >= 0 ? n : n + (bool(absn % absd) + absn / absd) * absd) % absd;
}

/** #summary */
} // ::yang

#endif
