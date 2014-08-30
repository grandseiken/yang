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

// Type together with an error flag, for expressions containing an error
// where the type cannot be determined. Further errors involving a value
// of this type are suppressed (to avoid cascading error messages).
//
// TODO: it might be nice to clean this up, especially now that it stores
// lvalue-ness and tags as well: remove const-ness from external types (store a
// bool with globals instead), since it's useless in every other situation and
// putting it here is nicer. Similarly remove exported from externals.
class Category {
public:

  // Convert from external type.
  Category(const Type& type);
  // Construct void or error type.
  explicit Category(bool error = false);
  // Convert to external type.
  const Type& type() const;
  // Return a string representation of the type.
  std::string string(const ContextInternals& context, bool quote = true) const;
  // Change constness.
  Category make_const(bool is_const) const;
  // Change lvalueness.
  Category make_lvalue(bool is_lvalue) const;
  // Change tags.
  Category add_tag(void* tag) const;
  Category add_tags(const Category& category) const;
  const std::vector<void*>& tags() const;

  // All of the following functions also return true if the error bit of any
  // type involved is set.
  bool is_error() const;
  bool is_lvalue() const;
  bool is_void() const;
  bool not_void() const;
  bool primitive() const;
  bool is_vector() const;
  bool is_int() const;
  bool is_float() const;
  bool function() const;
  bool user_type() const;
  bool element_size(std::size_t num_elements) const;
  bool element_is(std::size_t index, const Category& category) const;
  // True if the vector element-counts of these types allow for interaction;
  // that is, either the element-counts are the same (and they can interact
  // point-wise), or either element-count is 1 (and the value can be implicitly
  // vectorised).
  bool is_binary_match(const Category& c) const;
  bool is_assign_binary_match(const Category& c) const;
  // Unify returns t if is(t), and otherwise an error type.
  bool is(const Category& c) const;
  Category unify(const Category& c) const;

  // Raw equality comparisons (ignoring ERROR). Don't use for type-checking.
  bool operator==(const Category& c) const;
  bool operator!=(const Category& c) const;

private:

  friend struct std::hash<Category>;
  Type _type;
  bool _error;
  bool _lvalue;
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
