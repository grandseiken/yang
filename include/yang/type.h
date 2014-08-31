//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_TYPE_H
#define YANG_INCLUDE_YANG_TYPE_H

#include <string>
#include <unordered_map>
#include <vector>

namespace yang {
class Context;

namespace internal {
class ContextInternals;
class StaticChecker;

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

// End namespace internal.
}

class Type {
public:

  // Return a string representation of the type.
  std::string string(const Context& context) const;

  // Singular types.
  bool is_void() const;
  bool is_int() const;
  bool is_float() const;

  // Int and float vector types.
  bool is_vector() const;
  bool is_ivec() const;
  bool is_fvec() const;
  std::size_t vector_size() const;

  // Function types.
  bool is_function() const;
  std::size_t function_num_args() const;
  const Type& function_return() const;
  const Type& function_arg(std::size_t index) const;

  // User types.
  bool is_user_type() const;
  bool is_raw_user_type() const;
  bool is_managed_user_type() const;

  bool operator==(const Type& t) const;
  bool operator!=(const Type& t) const;

  // Static constructors.
  static Type void_t();
  static Type int_t();
  static Type float_t();
  static Type ivec_t(std::size_t size);
  static Type fvec_t(std::size_t size);
  static Type function_t(
      const Type& return_t, const std::vector<Type>& args);
  template<typename>
  static Type raw_user_t();
  static Type raw_user_t(const Type& user_type);
  template<typename>
  static Type managed_user_t();
  static Type managed_user_t(const Type& user_type);
  // Constructs a new type with all user types erased (replaced by a single
  // placeholder raw or managed user type).
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
  };

  type_base _base;
  std::size_t _count;
  std::vector<Type> _elements;
  const void* _user_type_uid;

};

struct Global {
  Global(const Type& type, bool is_const, bool is_exported);
  Type type;
  bool is_const;
  bool is_exported;
};
typedef std::unordered_map<std::string, Global> global_table;
typedef std::unordered_map<std::string, Type> type_table;
typedef type_table function_table;

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

// End namespace yang.
}

namespace std {
  template<>
  struct hash<yang::Type> {
    std::size_t operator()(const yang::Type& type) const;
  };
}

#endif
