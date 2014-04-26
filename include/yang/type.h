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
namespace internal {
class Type;

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
// TODO: this function existing at all is a super big temporary hack and should
// be got rid of. Printing out random pointers needs to be replaced with proper
// typedef lookup.
inline std::string type_uidstr(const void* uid)
{
  return std::to_string((std::intptr_t)uid);
}
template<typename T>
std::string type_uidstr()
{
  return type_uidstr(type_uid<T>());
}

// End namespace internal.
}

class Type {
public:

  // Return a string representation of the type.
  std::string string() const;

  // Type qualifiers.
  bool is_exported() const;
  bool is_const() const;

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
  bool is_managed_user_type() const;
  std::string user_type_name() const;

  bool operator==(const Type& t) const;
  bool operator!=(const Type& t) const;

  // Construct types. By default neither the exported bit nor the const bit is
  // set on created Types.
  static Type void_t();
  static Type int_t();
  static Type float_t();
  static Type ivec_t(std::size_t size);
  static Type fvec_t(std::size_t size);
  static Type function_t(
      const Type& return_t, const std::vector<Type>& args);
  template<typename>
  static Type user_t(bool managed);

  // Return a new type that's identical except exported.
  Type make_exported(bool exported = true) const;
  // Return a new type that's identical except const.
  Type make_const(bool is_const = true) const;
  // Return a new type with user types erased; that is, all user types replaced
  // by a single placeholder managed or unmanaged user type.
  Type erase_user_types() const;

private:

  friend struct std::hash<Type>;
  Type();

  enum type_base {
    VOID,
    INT,
    FLOAT,
    FUNCTION,
    USER_TYPE,
  };

  bool _exported;
  bool _const;
  type_base _base;
  std::size_t _count;
  std::vector<Type> _elements;
  const void* _user_type_uid;
  bool _managed_user_type;
  static Type void_type;

};

typedef std::unordered_map<std::string, yang::Type> symbol_table;

template<typename T>
Type Type::user_t(bool managed)
{
  Type t;
  t._base = USER_TYPE;
  t._user_type_uid = internal::type_uid<T>();
  t._managed_user_type = managed;
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
