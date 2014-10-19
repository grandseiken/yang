//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_VEC_H
#define YANG_INCLUDE_YANG_VEC_H

#include <algorithm>
#include <ostream>
#include "typedefs.h"

/** #summary */
namespace yang {

template<std::size_t N>
struct element_accessor {
  static const element_accessor instance; 
};
/**
 * #class
 *
 * Vector type corresponding to Yang vector types.
 */
template<typename T, std::size_t N>
class vec {
/** #sumline */
public:

  /** #member ## */
  T elements[N];

  /** #member */
  vec();
  /** #member */
  template<typename... U,
           typename = internal::enable_if<N == sizeof...(U)>>
  vec(const U&... args);
  /** #member ## */
  template<typename U>
  explicit vec(const vec<U, N>& arg);

  /** #member */
  explicit operator bool() const;
  /** #member */
  T& operator[](std::size_t i);
  /** #member */
  const T& operator[](std::size_t i) const;
  /** #member */
  template<std::size_t M, typename = internal::enable_if<(M < N)>>
  T& operator[](const element_accessor<M>&);
  /** #member ## */
  template<std::size_t M, typename = internal::enable_if<(M < N)>>
  const T& operator[](const element_accessor<M>&) const;

  /** #member */
  vec<bool, N> operator&&(const vec& arg) const;
  /** #member */
  vec<bool, N> operator&&(const T& arg) const;
  /** #member */
  vec<bool, N> operator||(const vec& arg) const;
  /** #member ## */
  vec<bool, N> operator||(const T& arg) const;

  /** #member */
  vec<bool, N> operator==(const vec& arg) const;
  /** #member */
  vec<bool, N> operator==(const T& arg) const;
  /** #member */
  vec<bool, N> operator!=(const vec& arg) const;
  /** #member */
  vec<bool, N> operator!=(const T& arg) const;
  /** #member */
  vec<bool, N> operator>=(const vec& arg) const;
  /** #member */
  vec<bool, N> operator>=(const T& arg) const;
  /** #member */
  vec<bool, N> operator<=(const vec& arg) const;
  /** #member */
  vec<bool, N> operator<=(const T& arg) const;
  /** #member */
  vec<bool, N> operator>(const vec& arg) const;
  /** #member */
  vec<bool, N> operator>(const T& arg) const;
  /** #member */
  vec<bool, N> operator<(const vec& arg) const;
  /** #member ## */
  vec<bool, N> operator<(const T& arg) const;

  /** #member */
  vec operator+(const vec& arg) const;
  /** #member */
  vec operator+(const T& arg) const;
  /** #member */
  vec operator-(const vec& arg) const;
  /** #member */
  vec operator-(const T& arg) const;
  /** #member */
  vec operator*(const vec& arg) const;
  /** #member */
  vec operator*(const T& arg) const;
  /** #member */
  vec operator/(const vec& arg) const;
  /** #member */
  vec operator/(const T& arg) const;
  /** #member */
  vec operator%(const vec& arg) const;
  /** #member ##*/
  vec operator%(const T& arg) const;

  /** #member */
  vec operator&(const vec& arg) const;
  /** #member */
  vec operator&(const T& arg) const;
  /** #member */
  vec operator|(const vec& arg) const;
  /** #member */
  vec operator|(const T& arg) const;
  /** #member */
  vec operator^(const vec& arg) const;
  /** #member */
  vec operator^(const T& arg) const;
  /** #member */
  vec operator<<(const vec& arg) const;
  /** #member */
  vec operator<<(const T& arg) const;
  /** #member */
  vec operator>>(const vec& arg) const;
  /** #member ## */
  vec operator>>(const T& arg) const;

  /** #member */
  vec& operator+=(const vec& arg);
  /** #member */
  vec& operator+=(const T& arg);
  /** #member */
  vec& operator-=(const vec& arg);
  /** #member */
  vec& operator-=(const T& arg);
  /** #member */
  vec& operator*=(const vec& arg);
  /** #member */
  vec& operator*=(const T& arg);
  /** #member */
  vec& operator/=(const vec& arg);
  /** #member */
  vec& operator/=(const T& arg);
  /** #member */
  vec& operator%=(const vec& arg);
  /** #member ## */
  vec& operator%=(const T& arg);

  /** #member */
  vec& operator&=(const vec& arg);
  /** #member */
  vec& operator&=(const T& arg);
  /** #member */
  vec& operator|=(const vec& arg);
  /** #member */
  vec& operator|=(const T& arg);
  /** #member */
  vec& operator^=(const vec& arg);
  /** #member */
  vec& operator^=(const T& arg);
  /** #member */
  vec& operator<<=(const vec& arg);
  /** #member */
  vec& operator<<=(const T& arg);
  /** #member */
  vec& operator>>=(const vec& arg);
  /** #member ## */
  vec& operator>>=(const T& arg);

  /** #member */
  vec<bool, N> operator!() const;
  /** #member */
  vec operator+() const;
  /** #member */
  vec operator-() const;
  /** #member */
  vec operator~() const;
  /** #member */
  vec operator++(int);
  /** #member */
  vec operator--(int);
  /** #member */
  vec& operator++();
  /** #member */
  vec& operator--();

/** #sumline ## */
};

/** #toplevel */
template<typename T, std::size_t N>
vec<bool, N> operator&&(const T& t, const vec<T, N>& v);
/** #toplevel ## */
template<typename T, std::size_t N>
vec<bool, N> operator||(const T& t, const vec<T, N>& v);

/** #toplevel */
template<typename T, std::size_t N>
vec<bool, N> operator==(const T& t, const vec<T, N>& v);
/** #toplevel */
template<typename T, std::size_t N>
vec<bool, N> operator!=(const T& t, const vec<T, N>& v);
/** #toplevel */
template<typename T, std::size_t N>
vec<bool, N> operator>=(const T& t, const vec<T, N>& v);
/** #toplevel */
template<typename T, std::size_t N>
vec<bool, N> operator<=(const T& t, const vec<T, N>& v);
/** #toplevel */
template<typename T, std::size_t N>
vec<bool, N> operator>(const T& t, const vec<T, N>& v);
/** #toplevel ## */
template<typename T, std::size_t N>
vec<bool, N> operator<(const T& t, const vec<T, N>& v);

/** #toplevel */
template<typename T, std::size_t N>
vec<T, N> operator+(const T& t, const vec<T, N>& v);
/** #toplevel */
template<typename T, std::size_t N>
vec<T, N> operator-(const T& t, const vec<T, N>& v);
/** #toplevel */
template<typename T, std::size_t N>
vec<T, N> operator*(const T& t, const vec<T, N>& v);
/** #toplevel */
template<typename T, std::size_t N>
vec<T, N> operator/(const T& t, const vec<T, N>& v);
/** #toplevel ## */
template<typename T, std::size_t N>
vec<T, N> operator%(const T& t, const vec<T, N>& v);

/** #toplevel */
template<typename T, std::size_t N>
vec<T, N> operator&(const T& t, const vec<T, N>& v);
/** #toplevel */
template<typename T, std::size_t N>
vec<T, N> operator|(const T& t, const vec<T, N>& v);
/** #toplevel */
template<typename T, std::size_t N>
vec<T, N> operator^(const T& t, const vec<T, N>& v);
/** #toplevel */
template<typename T, std::size_t N>
vec<T, N> operator<<(const T& t, const vec<T, N>& v);
/** #toplevel ## */
template<typename T, std::size_t N>
vec<T, N> operator>>(const T& t, const vec<T, N>& v);

/** #toplevel */
template<typename T, std::size_t N>
vec<T, N> euclidean_div(const vec<T, N>& v, const vec<T, N>& u);
/** #toplevel */
template<typename T, std::size_t N>
vec<T, N> euclidean_div(const vec<T, N>& v, const T& t);
/** #toplevel ## */
template<typename T, std::size_t N>
vec<T, N> euclidean_div(const T& t, const vec<T, N>& v);

/** #toplevel */
template<typename T, std::size_t N>
vec<T, N> euclidean_mod(const vec<T, N>& v, const vec<T, N>& u);
/** #toplevel */
template<typename T, std::size_t N>
vec<T, N> euclidean_mod(const vec<T, N>& v, const T& t);
/** #toplevel ## */
template<typename T, std::size_t N>
vec<T, N> euclidean_mod(const T& t, const vec<T, N>& v);

/** #toplevel ## */
template<typename T, std::size_t N>
std::ostream& operator<<(std::ostream& stream, const vec<T, N>& v);

namespace internal {
  template<typename T, std::size_t N, typename F>
  vec<T, N> map_vec(const F& f)
  {
    vec<T, N> v;
    for (std::size_t i = 0; i < N; ++i) {
      v.elements[i] = f(i);
    }
    return v;
  }
} // ::internal
#define MAP_VEC(T, N, expr) \
    internal::map_vec<T, N>([&](std::size_t i){return (expr);})

template<typename T, std::size_t N>
vec<T, N>::vec()
  : elements{0}
{
}

template<typename T, std::size_t N>
template<typename... U, typename>
vec<T, N>::vec(const U&... args)
  : elements{T(args)...}
{
}

template<typename T, std::size_t N>
template<typename U>
vec<T, N>::vec(const vec<U, N>& arg)
  : elements{}
{
  *this = MAP_VEC(T, N, arg.elements[i]);
}

template<typename T, std::size_t N>
vec<T, N>::operator bool() const
{
  for (std::size_t i = 0; i < N; ++i) {
    if (!elements[i]) {
      return false;
    }
  }
  return true;
}

template<typename T, std::size_t N>
T& vec<T, N>::operator[](std::size_t i)
{
  return elements[i];
}

template<typename T, std::size_t N>
const T& vec<T, N>::operator[](std::size_t i) const
{
  return elements[i];
}

template<typename T, std::size_t N>
template<std::size_t M, typename>
T& vec<T, N>::operator[](const element_accessor<M>&)
{
  return elements[M];
}

template<typename T, std::size_t N>
template<std::size_t M, typename>
const T& vec<T, N>::operator[](const element_accessor<M>&) const
{
  return elements[M];
}

template<typename T, std::size_t N>
vec<bool, N> vec<T, N>::operator!() const
{
  return MAP_VEC(bool, N, !elements[i]);
}

template<typename T, std::size_t N>
vec<T, N> vec<T, N>::operator+() const
{
  return MAP_VEC(T, N, +elements[i]);
}

template<typename T, std::size_t N>
vec<T, N> vec<T, N>::operator-() const
{
  return MAP_VEC(T, N, -elements[i]);
}

template<typename T, std::size_t N>
vec<T, N> vec<T, N>::operator~() const
{
  return MAP_VEC(T, N, ~elements[i]);
}

template<typename T, std::size_t N>
vec<T, N> vec<T, N>::operator++(int)
{
  auto temp = *this;
  operator++();
  return temp;
}

template<typename T, std::size_t N>
vec<T, N> vec<T, N>::operator--(int)
{
  auto temp = *this;
  operator--();
  return temp;
}

template<typename T, std::size_t N>
vec<T, N>& vec<T, N>::operator++()
{
  return operator+=(T{1});
}

template<typename T, std::size_t N>
vec<T, N>& vec<T, N>::operator--()
{
  return operator-=(T{1});
}

#define DEFINE_VEC_OPS_NO_ASSIGN(R, OP)\
  template<typename T, std::size_t N>\
  vec<R, N> vec<T, N>::operator OP(const vec& arg) const\
  {\
    return MAP_VEC(R, N, elements[i] OP arg[i]);\
  }\
  template<typename T, std::size_t N>\
  vec<R, N> vec<T, N>::operator OP(const T& arg) const\
  {\
    return MAP_VEC(R, N, elements[i] OP arg);\
  }\
  template<typename T, std::size_t N>\
  vec<R, N> operator OP(const T& t, const vec<T, N>& v)\
  {\
    return MAP_VEC(R, N, t OP v[i]);\
  }
#define DEFINE_VEC_OPS(R, OP)\
  DEFINE_VEC_OPS_NO_ASSIGN(R, OP)\
  template<typename T, std::size_t N>\
  vec<R, N>& vec<T, N>::operator OP##=(const vec& arg)\
  {\
    return *this = operator OP(arg);\
  }\
  template<typename T, std::size_t N>\
  vec<R, N>& vec<T, N>::operator OP##=(const T& arg)\
  {\
    return *this = operator OP(arg);\
  }

DEFINE_VEC_OPS_NO_ASSIGN(bool, &&)
DEFINE_VEC_OPS_NO_ASSIGN(bool, ||)
DEFINE_VEC_OPS_NO_ASSIGN(bool, ==)
DEFINE_VEC_OPS_NO_ASSIGN(bool, !=)
DEFINE_VEC_OPS_NO_ASSIGN(bool, >=)
DEFINE_VEC_OPS_NO_ASSIGN(bool, <=)
DEFINE_VEC_OPS_NO_ASSIGN(bool, >)
DEFINE_VEC_OPS_NO_ASSIGN(bool, <)
DEFINE_VEC_OPS(T, +)
DEFINE_VEC_OPS(T, -)
DEFINE_VEC_OPS(T, *)
DEFINE_VEC_OPS(T, /)
DEFINE_VEC_OPS(T, %)
DEFINE_VEC_OPS(T, &)
DEFINE_VEC_OPS(T, |)
DEFINE_VEC_OPS(T, ^)
DEFINE_VEC_OPS(T, <<)
DEFINE_VEC_OPS(T, >>)
#undef DEFINE_VEC_OPS

template<typename T, std::size_t N>
vec<T, N> euclidean_div(const vec<T, N>& v, const vec<T, N>& u)
{
  return MAP_VEC(T, N, euclidean_div(v[i], u[i]));
}

template<typename T, std::size_t N>
vec<T, N> euclidean_mod(const vec<T, N>& v, const vec<T, N>& u)
{
  return MAP_VEC(T, N, euclidean_mod(v[i], u[i]));
}

template<typename T, std::size_t N>
vec<T, N> euclidean_div(const vec<T, N>& v, const T& t)
{
  return MAP_VEC(T, N, euclidean_div(v[i], t));
}

template<typename T, std::size_t N>
vec<T, N> euclidean_mod(const vec<T, N>& v, const T& t)
{
  return MAP_VEC(T, N, euclidean_mod(v[i], t));
}

template<typename T, std::size_t N>
vec<T, N> euclidean_div(const T& t, const vec<T, N>& v)
{
  return MAP_VEC(T, N, euclidean_div(t, v[i]));
}

template<typename T, std::size_t N>
vec<T, N> euclidean_mod(const T& t, const vec<T, N>& v)
{
  return MAP_VEC(T, N, euclidean_mod(t, v[i]));
}

template<typename T, std::size_t N>
std::ostream& operator<<(std::ostream& stream, const vec<T, N>& arg)
{
  stream << "(";
  for (std::size_t i = 0; i < N; ++i) {
    if (i) {
      stream << ", ";
    }
    stream << arg.elements[i];
  }
  return stream << ")";
}

#undef MAP_VEC
template<std::size_t N>
const element_accessor<N> element_accessor<N>::instance;

/** #summary */
namespace {
  static const auto& x = element_accessor<0>::instance;
  static const auto& y = element_accessor<1>::instance;
  static const auto& z = element_accessor<2>::instance;
  static const auto& w = element_accessor<3>::instance;
}

template<std::size_t N, typename = internal::enable_if<(N > 1)>>
using bvec_t = vec<bool, N>;
template<std::size_t N, typename = internal::enable_if<(N > 1)>>
using ivec_t = vec<int_t, N>;
template<std::size_t N, typename = internal::enable_if<(N > 1)>>
using fvec_t = vec<float_t, N>;

} // ::yang
/**/

#endif
