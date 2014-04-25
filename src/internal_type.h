//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_INTERNAL_TYPE_H
#define YANG_SRC_INTERNAL_TYPE_H

#include <string>
#include <vector>
#include <yang/type.h>

namespace yang {
namespace internal {

// Type together with an error flag, for expressions containing an error
// where the type cannot be determined. Further errors involving a value
// of this type are suppressed (to avoid cascading error messages).
//
// It might be nice to name this something different to yang::Type.
class Type {
public:

  // Convert from external type.
  Type(const yang::Type& type);
  // Construct void or error type.
  explicit Type(bool error = false);
  // Convert to external type.
  const yang::Type& external() const;
  // Return a string representation of the type.
  std::string string() const;
  // Change constness.
  Type make_const(bool is_const) const;

  // Raw equality comparisons (ignoring ERROR). Don't use for type-checking.
  bool operator==(const Type& t) const;
  bool operator!=(const Type& t) const;

  // All of the following functions also return true if the error bit of any
  // type involved is set.
  bool is_error() const;
  bool is_void() const;
  bool not_void() const;
  bool primitive() const;
  bool is_vector() const;
  bool is_int() const;
  bool is_float() const;
  bool function() const;
  bool user_type() const;
  bool element_size(std::size_t num_elements) const;
  bool element_is(std::size_t index, const Type& type) const;
  // True if the vector element-counts of these types allow for interaction;
  // that is, either the element-counts are the same (and they can interact
  // point-wise), or either element-count is 1 (and the value can be implicitly
  // vectorised).
  bool is_binary_match(const Type& t) const;
  // Unify returns t if is(t), and otherwise an error type.
  bool is(const Type& t) const;
  Type unify(const Type& t) const;

private:

  friend struct std::hash<Type>;
  yang::Type _wrap;
  bool _error;

};

// End namespace yang::internal.
}
}

namespace std {
  template<>
  struct hash<yang::internal::Type> {
    std::size_t operator()(const yang::internal::Type& t) const
    {
      return std::hash<yang::Type>()(t._wrap) ^ t._error;
    }
  };
}

#endif
