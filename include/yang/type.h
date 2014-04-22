//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_TYPE_H
#define YANG_INCLUDE_YANG_TYPE_H

#include <string>
#include <vector>

namespace yang {

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
  bool is_managed_user_type() const;
  const std::string& get_user_type_name() const;

  bool operator==(const Type& t) const;
  bool operator!=(const Type& t) const;

  // Construct types. By default neither the exported bit nor the const bit is
  // set on created Types.
  static Type void_t();
  static Type int_t();
  static Type float_t();
  static Type int_vector_t(std::size_t size);
  static Type float_vector_t(std::size_t size);
  static Type function_t(
      const Type& return_t, const std::vector<Type>& args);
  static Type user_t(const std::string& name, bool managed);

  // Return a new type that's identical except exported.
  Type make_exported(bool exported = true) const;
  // Return a new type that's identical except const.
  Type make_const(bool is_const = true) const;
  // Return a new type with user types erased; that is, all user types replaced
  // by a single placeholder user type with no name.
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
  std::string _user_type_name;
  bool _managed_user_type;
  static Type void_type;

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
