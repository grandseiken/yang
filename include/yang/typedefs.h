//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_TYPEDEFS_H
#define YANG_INCLUDE_YANG_TYPEDEFS_H

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <ostream>

namespace yang {

template<bool B, typename T = void>
using enable_if = typename std::enable_if<B, T>::type;

typedef void void_t;
typedef int32_t int_t;
typedef uint32_t uint_t;
typedef double float_t;
typedef void (*void_fp)();

inline int_t euclidean_div(int_t n, int_t d)
{
  bool sign = (n < 0) == (d < 0);
  int_t t = (n < 0 ? -(1 + n) : n) / std::abs(d);
  return (d < 0) + (sign ? t : -(1 + t));
}

inline int_t euclidean_mod(int_t n, int_t d)
{
  int_t num = std::abs(n);
  int_t div = std::abs(d);
  return (num < 0 ? num + (bool(num % div) + num / div) * div : num) % div;
}

template<std::size_t N>
struct element_accessor {
  element_accessor() {}
};

// Vector type corresponding to Yang vector types.
template<typename T, std::size_t N>
class vec {
public:

  T elements[N];

  // Constructors.

  vec()
    : elements{0}
  {
  }

  template<typename... U,
           typename = enable_if<N == sizeof...(U)>>
  vec(const U&... args)
    : elements{args...}
  {
  }

  vec(const vec&) noexcept = default;
  vec(vec&&) noexcept = default;

  template<typename U>
  explicit vec(const vec<U, N>& arg)
    : elements{}
  {
    for (std::size_t i = 0; i < N; ++i) {
      elements[i] = T(arg.elements[i]);
    }
  }

  // Assignment.

  vec& operator=(const vec&) noexcept = default;
  vec& operator=(vec&&) noexcept = default;

  // Indexing.

  T& operator[](std::size_t i)
  {
    return elements[i];
  }

  const T& operator[](std::size_t i) const
  {
    return elements[i];
  }

  template<std::size_t M, typename = enable_if<(M < N)>>
  T& operator[](const element_accessor<M>&)
  {
    return elements[M];
  }

  template<std::size_t M, typename = enable_if<(M < N)>>
  const T& operator[](const element_accessor<M>&) const
  {
    return elements[M];
  }

  // Comparison operators. Relational operators return true iff all of the
  // element-wise comparisons return true.

  bool operator==(const vec& arg) const
  {
    for (std::size_t i = 0; i < N; ++i) {
      if (elements[i] != arg.elements[i]) {
        return false;
      }
    }
    return true;
  }

  bool operator!=(const vec& arg) const
  {
    return !operator==(arg);
  }

  bool operator<(const vec& arg) const
  {
    for (std::size_t i = 0; i < N; ++i) {
      if (elements[i] >= arg.elements[i]) {
        return false;
      }
    }
    return true;
  }

  bool operator>(const vec& arg) const
  {
    for (std::size_t i = 0; i < N; ++i) {
      if (elements[i] <= arg.elements[i]) {
        return false;
      }
    }
    return true;
  }

  bool operator<=(const vec& arg) const
  {
    for (std::size_t i = 0; i < N; ++i) {
      if (elements[i] > arg.elements[i]) {
        return false;
      }
    }
    return true;
  }

  bool operator>=(const vec& arg) const
  {
    for (std::size_t i = 0; i < N; ++i) {
      if (elements[i] < arg.elements[i]) {
        return false;
      }
    }
    return true;
  }

  // Binary operators.

  vec operator+(const vec& arg) const
  {
    vec v;
    for (std::size_t i = 0; i < N; ++i) {
      v[i] = elements[i] + arg[i];
    }
    return v;
  }

  vec operator-(const vec& arg) const
  {
    vec v;
    for (std::size_t i = 0; i < N; ++i) {
      v[i] = elements[i] - arg[i];
    }
    return v;
  }

  vec operator*(const vec& arg) const
  {
    vec v;
    for (std::size_t i = 0; i < N; ++i) {
      v[i] = elements[i] * arg[i];
    }
    return v;
  }

  vec operator/(const vec& arg) const
  {
    vec v;
    for (std::size_t i = 0; i < N; ++i) {
      v[i] = elements[i] / arg[i];
    }
    return v;
  }

  vec operator*(const T& arg) const
  {
    vec v;
    for (std::size_t i = 0; i < N; ++i) {
      v[i] = elements[i] * arg;
    }
    return v;
  }

  vec operator/(const T& arg) const
  {
    vec v;
    for (std::size_t i = 0; i < N; ++i) {
      v[i] = elements[i] / arg;
    }
    return v;
  }

  // Shortcut assignment operators.

  vec& operator+=(const vec& arg)
  {
    return operator=(operator+(arg));
  }

  vec& operator-=(const vec& arg)
  {
    return operator=(operator-(arg));
  }

  vec& operator*=(const vec& arg)
  {
    return operator=(operator*(arg));
  }

  vec& operator/=(const vec& arg)
  {
    return operator=(operator/(arg));
  }

  vec& operator*=(const T& arg)
  {
    return operator=(operator*(arg));
  }

  vec& operator/=(const T& arg)
  {
    return operator=(operator/(arg));
  }

  // Unary operators and mathematical functions.

  vec operator-() const
  {
    return vec().operator-(*this);
  }

  vec normalised() const
  {
    return operator/(length() ? length() : 1);
  }

  vec& normalise()
  {
    return operator=(normalised());
  }

  T length_squared() const
  {
    return dot(*this);
  }

  T length() const
  {
    return std::sqrt(length_squared());
  }

  // Dot product.

  T dot(const vec& arg) const
  {
    T t{0};
    for (std::size_t i = 0; i < N; ++i) {
      t += elements[i] * arg.elements[i];
    }
    return t;
  }

  // Cross products for two and three dimensions.

  template<bool B = true, typename = enable_if<N == 2 && B>>
  T cross(const vec& arg) const
  {
    return elements[0] * arg[1] - arg[0] * elements[1];
  }

  template<bool B = true, typename = enable_if<N == 3 && B>>
  vec cross(const vec& arg) const
  {
    return vec(elements[1] * arg[2] - elements[2] * arg[1],
               elements[2] * arg[0] - elements[0] * arg[2],
               elements[0] * arg[1] - elements[1] * arg[0]);
  }

  // Euclidean division and modulo operators.

  template<typename = std::enable_if<std::is_same<T, int_t>::value>>
  vec euclidean_div(const vec& arg) const
  {
    vec v;
    for (std::size_t i = 0; i < N; ++i) {
      v[i] = yang::euclidean_div(elements[i], arg[i]);
    }
    return v;
  }

  template<typename = std::enable_if<std::is_same<T, int_t>::value>>
  vec euclidean_mod(const vec& arg) const
  {
    vec v;
    for (std::size_t i = 0; i < N; ++i) {
      v[i] = yang::euclidean_mod(elements[i], arg[i]);
    }
    return v;
  }

  template<typename = std::enable_if<std::is_same<T, int_t>::value>>
  vec euclidean_div(const T& arg) const
  {
    vec v;
    for (std::size_t i = 0; i < N; ++i) {
      v[i] = yang::euclidean_div(elements[i], arg);
    }
    return v;
  }

  template<typename = std::enable_if<std::is_same<T, int_t>::value>>
  vec euclidean_mod(const T& arg) const
  {
    vec v;
    for (std::size_t i = 0; i < N; ++i) {
      v[i] = yang::euclidean_mod(elements[i], arg);
    }
    return v;
  }

};

// Binary operators with a single element on the left.

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

// Miscellaenous element-wise mathematical functions.

template<typename T, std::size_t N>
vec<T, N> abs(const vec<T, N>& v)
{
  vec<T, N> u;
  for (std::size_t i = 0; i < N; ++i) {
    u[i] = std::abs(v[i]);
  }
  return u;
}

template<typename T, std::size_t N>
vec<T, N> pow(const T& a, const vec<T, N>& b)
{
  vec<T, N> v;
  for (std::size_t i = 0; i < N; ++i) {
    v[i] = std::pow(a, b[i]);
  }
  return v;
}

template<typename T, std::size_t N>
vec<T, N> pow(const vec<T, N>& a, const T& b)
{
  vec<T, N> v;
  for (std::size_t i = 0; i < N; ++i) {
    v[i] = std::pow(a[i], b);
  }
  return v;
}

template<typename T, std::size_t N>
vec<T, N> pow(const vec<T, N>& a, const vec<T, N>& b)
{
  vec<T, N> v;
  for (std::size_t i = 0; i < N; ++i) {
    v[i] = std::pow(a[i], b[i]);
  }
  return v;
}

template<typename T, std::size_t N>
vec<T, N> min(const vec<T, N>& a, const vec<T, N>& b)
{
  vec<T, N> v;
  for (std::size_t i = 0; i < N; ++i) {
    v[i] = std::min(a[i], b[i]);
  }
  return v;
}

template<typename T, std::size_t N>
vec<T, N> max(const vec<T, N>& a, const vec<T, N>& b)
{
  vec<T, N> v;
  for (std::size_t i = 0; i < N; ++i) {
    v[i] = std::max(a[i], b[i]);
  }
  return v;
}

// Write string representation to stream.

template<typename T, std::size_t N>
std::ostream& operator<<(std::ostream& stream, const vec<T, N>& arg)
{
  for (std::size_t i = 0; i < N; ++i) {
    if (i) {
      stream << ", ";
    }
    stream << arg.elements[i];
  }
  return stream;
}

// Standard accessor types for 4-element vectors.

template<bool>
struct element_accessor_instance {
  static const element_accessor<0> x;
  static const element_accessor<1> y;
  static const element_accessor<2> z;
  static const element_accessor<3> w;
};

namespace {
  static const auto& x = element_accessor_instance<true>::x;
  static const auto& y = element_accessor_instance<true>::y;
  static const auto& z = element_accessor_instance<true>::z;
  static const auto& w = element_accessor_instance<true>::w;
}

template<bool B>
const element_accessor<0> element_accessor_instance<B>::x;
template<bool B>
const element_accessor<1> element_accessor_instance<B>::y;
template<bool B>
const element_accessor<2> element_accessor_instance<B>::z;
template<bool B>
const element_accessor<3> element_accessor_instance<B>::w;

template<std::size_t N, typename = enable_if<(N > 1)>>
using ivec_t = vec<int_t, N>;
template<std::size_t N, typename = enable_if<(N > 1)>>
using fvec_t = vec<float_t, N>;

// End namespace yang.
}

#endif
