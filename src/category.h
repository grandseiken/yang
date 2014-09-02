//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_CATEGORY_H
#define YANG_SRC_CATEGORY_H

#include <string>
#include <vector>
#include <yang/type.h>

namespace yang {
namespace internal {
struct ContextInternals;
struct Symbol;

// Type together with some extra data which is useful during static checking.
// Further errors involving an error category are suppressed to avoid cascading
// error messages.
class Category {
public:

  // Create from external type or create error category.
  Category(const Type& type = Type::void_t());
  static Category error();

  // Retrieve external type and tags.
  const Type& type() const;
  const std::vector<void*>& tags() const;

  // Return a new category that's identical except for the given change.
  Category make_const(bool is_const) const;
  Category make_lvalue(bool is_lvalue) const;
  Category add_tag(void* tag) const;
  Category add_tags(const Category& category) const;

  // Unify returns t if is(t), and otherwise an error category; it also merges
  // tags, lvalue-ness and const-ness.
  Category unify(const Category& c) const;

  // All of the following functions also return true if the error bit of any
  // category involved is set.
  bool is(const Category& c) const;

  bool is_error() const;
  bool is_lvalue() const;
  bool not_const() const;

  bool is_void() const;
  bool not_void() const;
  bool is_vector() const;
  bool is_int() const;
  bool is_float() const;
  bool function() const;
  bool user_type() const;
  bool element_size(std::size_t num_elements) const;
  bool element_is(std::size_t index, const Category& category) const;

  // True if the vector element-counts of these categories allow for
  // interaction; that is, either the element-counts are the same (and they can
  // interact point-wise), or either element-count is 1 (and the value can be
  // implicitly vectorised if necessary).
  bool is_binary_match(const Category& c) const;
  bool is_assign_binary_match(const Category& c) const;

private:

  friend struct std::hash<Category>;
  Type _type;
  bool _error;
  bool _lvalue;
  bool _const;
  // Stored pointers to symbols so that they can be propagated for warning
  // purposes.
  std::vector<void*> _tags;

};

// End namespace yang::internal.
}
}

namespace std {
  template<>
  struct hash<yang::internal::Category> {
    std::size_t operator()(const yang::internal::Category& c) const
    {
      return std::hash<yang::Type>()(c._type) ^ c._error ^ c._lvalue;
    }
  };
}

#endif
