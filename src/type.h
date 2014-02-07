//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_TYPE_H
#define YANG_SRC_TYPE_H

#include <string>
#include <vector>

namespace yang {

namespace internal {
  class IrCommon;
  class Type;
  template<typename>
  struct TypeInfo;
  struct GenericNativeFunction;
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
  bool is_int_vector() const;
  bool is_float_vector() const;
  std::size_t get_vector_size() const;

  // Function types.
  bool is_function() const;
  std::size_t get_function_num_args() const;
  const Type& get_function_return_type() const;
  const Type& get_function_arg_type(std::size_t index) const;

  // User types.
  bool is_user_type() const;
  const std::string& get_user_type_name() const;
  // Return a new type with user types erased; that is, all user types replaced
  // by a single placeholder user type with no name.
  Type erase_user_types() const;

  bool operator==(const Type& t) const;
  bool operator!=(const Type& t) const;

private:

  friend struct std::hash<Type>;
  friend class internal::IrCommon;
  friend class internal::Type;
  template<typename>
  friend struct internal::TypeInfo;
  friend struct internal::GenericNativeFunction;
  friend class Program;
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
  std::string _user_type_name;
  static Type void_type;
  static std::string erased_type_name;

};

// End namespace yang.
}

namespace std {
  template<>
  struct hash<yang::Type> {
    std::size_t operator()(const yang::Type& type) const;
  };
}

#endif
