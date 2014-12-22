//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_TYPE_H
#define YANG_INCLUDE_YANG_TYPE_H

#include <string>
#include <vector>
#include <utility>

/** #sumline ## */
namespace yang {
class Context;
class Type;

namespace internal {
class ContextInternals;
class StaticChecker;
template<typename>
Type type_of();

// Static member is guaranteed to have a different address per template
// instantiation.
template<typename>
struct NativeTypeId {
  static const void* id;
};
template<typename T>
const void* NativeTypeId<T>::id = nullptr;
// Maps native types to unique identifiers. Probably, type_uid<const T>
// (and volatile, etc) should be aliased to type_uid<T>.
template<typename T>
const void* type_uid()
{
  return &NativeTypeId<T>::id;
}

} // ::internal

/** #class */
class Type {
/** #sumline */
public:

  /**
   * #member ##
   *
   * Return a string representation of the type in the given `Context`.
   */
  std::string string(const Context& context) const;

  /** #member */
  bool is_void() const;
  /** #member */
  bool is_int() const;
  /** #member ## */
  bool is_float() const;

  /** #member */
  bool is_vector() const;
  /** #member */
  bool is_ivec() const;
  /** #member */
  bool is_fvec() const;
  /** #member ## */
  std::size_t vector_size() const;

  /** #member */
  bool is_function() const;
  /** #member */
  const std::vector<Type>& function_args() const;
  /** #member ## */
  const Type& function_return() const;

  /** #member */
  bool is_user_type() const;
  /** #member */
  bool is_raw_user_type() const;
  /** #member ## */
  bool is_managed_user_type() const;

  /** #member */
  bool is_interface() const;
  /** #member ## */
  const std::vector<std::pair<std::string, Type>>& interface_members() const;

  /** #member */
  bool operator==(const Type& t) const;
  /** #member ## */
  bool operator!=(const Type& t) const;

  /** #member */
  static Type void_t();
  /** #member */
  static Type int_t();
  /** #member */
  static Type float_t();
  /** #member */
  static Type ivec_t(std::size_t size);
  /** #member */
  static Type fvec_t(std::size_t size);
  /** #member */
  static Type function_t(const Type& return_t, const std::vector<Type>& args);
  /** #member */
  template<typename>
  static Type raw_user_t();
  /** #member */
  static Type raw_user_t(const Type& user_type);
  /** #member */
  template<typename>
  static Type managed_user_t();
  /** #member */
  static Type managed_user_t(const Type& user_type);
  /** #member ## */
  static Type interface_t(
      const std::vector<std::pair<std::string, Type>>& members);

  /**
   * #member
   *
   * Constructs the `Type` object corresponding to the C++ type given as the
   * template argument.
   */
  template<typename>
  static Type of();
  /**
   * #member
   *
   * Constructs a new type with all user types erased (replaced by a single
   * placeholder raw or managed user type).
   */
  static Type erased_t(const Type& type);

private:

  friend struct std::hash<Type>;
  // For ContextInternals& string function.
  friend class Instance;
  friend class internal::StaticChecker;

  Type();
  std::string string(const internal::ContextInternals& context) const;

  enum type_base {
    VOID,
    INT,
    FLOAT,
    FUNCTION,
    RAW_USER_TYPE,
    MANAGED_USER_TYPE,
    INTERFACE,
  };

  type_base _base;
  std::size_t _count;
  std::vector<Type> _return;
  std::vector<Type> _args;
  const void* _user_type_uid;
  std::vector<std::pair<std::string, Type>> _members;

/** #sumline ## */
};

template<typename T>
Type Type::of()
{
  return internal::type_of<T>();
}

template<typename T>
Type Type::raw_user_t()
{
  Type t;
  t._base = RAW_USER_TYPE;
  t._user_type_uid = internal::type_uid<T>();
  return t;
}

template<typename T>
Type Type::managed_user_t()
{
  Type t;
  t._base = MANAGED_USER_TYPE;
  t._user_type_uid = internal::type_uid<T>();
  return t;
}

/** #summary */
} // ::yang

namespace std {
  template<>
  struct hash<yang::Type> {
    std::size_t operator()(const yang::Type& type) const;
  };
}
/**/

#endif
