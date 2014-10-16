//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_VEC_H
#define YANG_INCLUDE_YANG_VEC_H

#include <algorithm>
#include <ostream>
#include "typedefs.h"

// TODO: make this class behave more like the vector types in Yang code, or
// come up with a justification for the discrepancies.
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
  bool operator==(const vec& arg) const;
  /** #member ## */
  bool operator!=(const vec& arg) const;

  /** #member */
  vec operator+(const vec& arg) const;
  /** #member */
  vec operator-(const vec& arg) const;
  /** #member */
  vec operator*(const vec& arg) const;
  /** #member */
  vec operator/(const vec& arg) const;
  /** #member */
  vec operator*(const T& arg) const;
  /** #member */
  vec operator/(const T& arg) const;
  /** #member ## */
  vec operator-() const;

  /** #member */
  vec& operator+=(const vec& arg);
  /** #member */
  vec& operator-=(const vec& arg);
  /** #member */
  vec& operator*=(const vec& arg);
  /** #member */
  vec& operator/=(const vec& arg);
  /** #member */
  vec& operator*=(const T& arg);
  /** #member ## */
  vec& operator/=(const T& arg);

/** #sumline ## */
};

/** #function */
template<typename T, std::size_t N>
vec<T, N> operator*(const T& t, const vec<T, N>& v);
/** #function ## */
template<typename T, std::size_t N>
vec<T, N> operator/(const T& t, const vec<T, N>& v);

/** #function */
template<typename T, std::size_t N>
vec<T, N> euclidean_div(const vec<T, N>& v, const vec<T, N>& u);
/** #function */
template<typename T, std::size_t N>
vec<T, N> euclidean_mod(const vec<T, N>& v, const vec<T, N>& u);
/** #function */
template<typename T, std::size_t N>
vec<T, N> euclidean_div(const vec<T, N>& v, const T& t);
/** #function */
template<typename T, std::size_t N>
vec<T, N> euclidean_mod(const vec<T, N>& v, const T& t);
/** #function */
template<typename T, std::size_t N>
vec<T, N> euclidean_div(const T& t, const vec<T, N>& v);
/** #function ## */
template<typename T, std::size_t N>
vec<T, N> euclidean_mod(const T& t, const vec<T, N>& v);

/** #function ## */
template<typename T, std::size_t N>
std::ostream& operator<<(std::ostream& stream, const vec<T, N>& v);

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
  for (std::size_t i = 0; i < N; ++i) {
    elements[i] = T(arg.elements[i]);
  }
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
bool vec<T, N>::operator==(const vec& arg) const
{
  for (std::size_t i = 0; i < N; ++i) {
    if (elements[i] != arg.elements[i]) {
      return false;
    }
  }
  return true;
}

template<typename T, std::size_t N>
bool vec<T, N>::operator!=(const vec& arg) const
{
  return !operator==(arg);
}

template<typename T, std::size_t N>
vec<T, N> vec<T, N>::operator+(const vec& arg) const
{
  vec v;
  for (std::size_t i = 0; i < N; ++i) {
    v[i] = elements[i] + arg[i];
  }
  return v;
}

template<typename T, std::size_t N>
vec<T, N> vec<T, N>::operator-(const vec& arg) const
{
  vec v;
  for (std::size_t i = 0; i < N; ++i) {
    v[i] = elements[i] - arg[i];
  }
  return v;
}

template<typename T, std::size_t N>
vec<T, N> vec<T, N>::operator*(const vec& arg) const
{
  vec v;
  for (std::size_t i = 0; i < N; ++i) {
    v[i] = elements[i] * arg[i];
  }
  return v;
}

template<typename T, std::size_t N>
vec<T, N> vec<T, N>::operator/(const vec& arg) const
{
  vec v;
  for (std::size_t i = 0; i < N; ++i) {
    v[i] = elements[i] / arg[i];
  }
  return v;
}

template<typename T, std::size_t N>
vec<T, N> vec<T, N>::operator*(const T& arg) const
{
  vec v;
  for (std::size_t i = 0; i < N; ++i) {
    v[i] = elements[i] * arg;
  }
  return v;
}

template<typename T, std::size_t N>
vec<T, N> vec<T, N>::operator/(const T& arg) const
{
  vec v;
  for (std::size_t i = 0; i < N; ++i) {
    v[i] = elements[i] / arg;
  }
  return v;
}

template<typename T, std::size_t N>
vec<T, N> vec<T, N>::operator-() const
{
  return vec().operator-(*this);
}

template<typename T, std::size_t N>
vec<T, N>& vec<T, N>::operator+=(const vec& arg)
{
  return *this = operator+(arg);
}

template<typename T, std::size_t N>
vec<T, N>& vec<T, N>::operator-=(const vec& arg)
{
  return *this = operator-(arg);
}

template<typename T, std::size_t N>
vec<T, N>& vec<T, N>::operator*=(const vec& arg)
{
  return *this = operator*(arg);
}

template<typename T, std::size_t N>
vec<T, N>& vec<T, N>::operator/=(const vec& arg)
{
  return *this = operator/(arg);
}

template<typename T, std::size_t N>
vec<T, N>& vec<T, N>::operator*=(const T& arg)
{
  return *this = operator*(arg);
}

template<typename T, std::size_t N>
vec<T, N>& vec<T, N>::operator/=(const T& arg)
{
  return *this = operator/(arg);
}

template<typename T, std::size_t N>
vec<T, N> operator*(const T& t, const vec<T, N>& v)
{
  vec<T, N> u;
  for (std::size_t i = 0; i < N; ++i) {
    u[i] = t * v[i];
  }
  return u;
}

template<typename T, std::size_t N>
vec<T, N> operator/(const T& t, const vec<T, N>& v)
{
  vec<T, N> u;
  for (std::size_t i = 0; i < N; ++i) {
    u[i] = t / v[i];
  }
  return u;
}

template<typename T, std::size_t N>
vec<T, N> euclidean_div(const vec<T, N>& v, const vec<T, N>& u)
{
  vec<T, N> r;
  for (std::size_t i = 0; i < N; ++i) {
    r[i] = euclidean_div(v[i], u[i]);
  }
  return r;
}

template<typename T, std::size_t N>
vec<T, N> euclidean_mod(const vec<T, N>& v, const vec<T, N>& u)
{
  vec<T, N> r;
  for (std::size_t i = 0; i < N; ++i) {
    r[i] = euclidean_mod(v[i], u[i]);
  }
  return r;
}

template<typename T, std::size_t N>
vec<T, N> euclidean_div(const vec<T, N>& v, const T& t)
{
  vec<T, N> r;
  for (std::size_t i = 0; i < N; ++i) {
    r[i] = euclidean_div(v[i], t);
  }
  return r;
}

template<typename T, std::size_t N>
vec<T, N> euclidean_mod(const vec<T, N>& v, const T& t)
{
  vec<T, N> r;
  for (std::size_t i = 0; i < N; ++i) {
    r[i] = euclidean_mod(v[i], t);
  }
  return r;
}

template<typename T, std::size_t N>
vec<T, N> euclidean_div(const T& t, const vec<T, N>& v)
{
  vec<T, N> r;
  for (std::size_t i = 0; i < N; ++i) {
    r[i] = euclidean_div(t, v[i]);
  }
  return r;
}

template<typename T, std::size_t N>
vec<T, N> euclidean_mod(const T& t, const vec<T, N>& v)
{
  vec<T, N> r;
  for (std::size_t i = 0; i < N; ++i) {
    r[i] = euclidean_mod(t, v[i]);
  }
  return r;
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
using ivec_t = vec<int_t, N>;
template<std::size_t N, typename = internal::enable_if<(N > 1)>>
using fvec_t = vec<float_t, N>;

} // ::yang
/**/

#endif
