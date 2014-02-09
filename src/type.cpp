//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/type.h>

namespace yang {

std::string Type::string() const
{
  std::string s;
  if (_base == USER_TYPE) {
    s = _user_type_name + "*";
  }
  else if (_base == FUNCTION) {
    s += _elements[0].string() + "(";
    for (std::size_t i = 1; i < _elements.size(); ++i) {
      if (i > 1) {
        s += ", ";
      }
      s += _elements[i].string();
    }
    s += ")";
  }
  else {
    s += _base == VOID ? "void" :
         _base == INT ? "int" :
         _base == FLOAT ? "float" : "error";

    if (_count > 1) {
      s += std::to_string(_count);
    }
  }
  return (_exported ? "export " : "") + s + (_const ? " const" : "");
}

bool Type::is_exported() const
{
  return _exported;
}

bool Type::is_const() const
{
  return _const;
}

bool Type::is_void() const
{
  return _base == VOID;
}

bool Type::is_int() const
{
  return _base == INT && _count == 1;
}

bool Type::is_float() const
{
  return _base == FLOAT && _count == 1;
}

bool Type::is_vector() const
{
  return is_int_vector() || is_float_vector();
}

bool Type::is_int_vector() const
{
  return _base == INT && _count > 1;
}

bool Type::is_float_vector() const
{
  return _base == FLOAT && _count > 1;
}

std::size_t Type::get_vector_size() const
{
  return _count;
}

bool Type::is_function() const
{
  return _base == FUNCTION;
}

std::size_t Type::get_function_num_args() const
{
  return _elements.empty() ? 0 : _elements.size() - 1;
}

const Type& Type::get_function_return_type() const
{
  return _elements.empty() ? void_type : _elements[0];
}

const Type& Type::get_function_arg_type(std::size_t index) const
{
  return 1 + index >= _elements.size() ? void_type : _elements[1 + index];
}

bool Type::is_user_type() const
{
  return _base == USER_TYPE;
}

const std::string& Type::get_user_type_name() const
{
  if (_user_type_name.empty()) {
    return erased_type_name;
  }
  return _user_type_name;
}

Type Type::erase_user_types() const
{
  Type t = *this;
  t._user_type_name.clear();
  for (auto& u : t._elements) {
    u = u.erase_user_types();
  }
  return t;
}

bool Type::operator==(const Type& t) const
{
  if (_elements.size() != t._elements.size()) {
    return false;
  }
  for (std::size_t i = 0; i < _elements.size(); ++i) {
    if (_elements[i] != t._elements[i]) {
      return false;
    }
  }
  return _user_type_name == t._user_type_name &&
      _base == t._base && _count == t._count;
}

bool Type::operator!=(const Type& t) const
{
  return !operator==(t);
}

Type::Type()
  : _exported(false)
  , _const(false)
  , _base(VOID)
  , _count(1)
{
}

Type Type::void_type;
std::string Type::erased_type_name = "[Erased]";

// End namespace yang.
}

namespace {
  // Taken from boost.
  void hash_combine(std::size_t& seed, std::size_t v)
  {
    seed ^= v + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  }
}

namespace std {
  std::size_t hash<yang::Type>::operator()(const yang::Type& type) const
  {
    std::size_t seed = 0;
    hash_combine(seed, type._base);
    hash_combine(seed, type._count);
    for (const auto& t : type._elements) {
      hash_combine(seed, operator()(t));
    }
    return seed;
  }
}
