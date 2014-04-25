//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "internal_type.h"

namespace yang {
namespace internal {

Type::Type(const yang::Type& type)
  : Type(VOID)
{
  _base =
      type.is_int() || type.is_int_vector() ? INT :
      type.is_float() || type.is_float_vector() ? FLOAT :
      type.is_function() ? FUNCTION :
      type.is_user_type() ? USER_TYPE :
      VOID;
  _const = type.is_const();
  _count = type.get_vector_size();
  _user_type_uid = type._user_type_uid;
  _managed_user_type = type.is_managed_user_type();
  if (type.is_function()) {
    _elements.push_back(type.get_function_return_type());
    for (std::size_t i = 0; i < type.get_function_num_args(); ++i) {
      _elements.push_back(type.get_function_arg_type(i));
    }
  }
}

Type::Type(type_base base, std::size_t count)
  : _base(base)
  , _count(count)
  , _const(false)
  , _user_type_uid(nullptr)
  , _managed_user_type(false)
{
  if (base == FUNCTION || count == 0 ||
      (count != 1 && base != INT && base != FLOAT && base != ERROR)) {
    _base = ERROR;
    _count = 1;
  }
}

Type::Type(type_base base, const Type& return_type)
  : _base(FUNCTION)
  , _count(1)
  , _const(false)
  , _user_type_uid(nullptr)
  , _managed_user_type(false)
{
  if (base != FUNCTION) {
    _base = ERROR;
    return;
  }
  _elements.push_back(return_type);
}

Type::Type(type_base base, const void* user_type_uid, bool managed)
  : _base(USER_TYPE)
  , _count(1)
  , _const(false)
  , _user_type_uid(user_type_uid)
  , _managed_user_type(managed)
{
  if (base != USER_TYPE) {
    _base = ERROR;
  }
}

void Type::set_const(bool is_const)
{
  _const = is_const;
}

Type::type_base Type::base() const
{
  return _base;
}

std::size_t Type::count() const
{
  return _count;
}

std::string Type::user_type_name() const
{
  return type_uidstr(_user_type_uid);
}

bool Type::managed() const
{
  return _managed_user_type;
}

bool Type::is_const() const
{
  return _const;
}

std::string Type::string() const
{
  return "`" + string_internal() + "`";
}

bool Type::is_error() const
{
  return _base == ERROR;
}

bool Type::is_void() const
{
  return is_error() || _base == VOID;
}

bool Type::not_void() const
{
  return _base != VOID;
}

bool Type::primitive() const
{
  return is_error() ||
      (_count == 1 && (is_int() || is_float()));
}

bool Type::is_vector() const
{
  return is_error() ||
      (_count > 1 && (is_int() || is_float()));
}

bool Type::is_int() const
{
  return is_error() || _base == INT;
}

bool Type::is_float() const
{
  return is_error() || _base == FLOAT;
}

bool Type::function() const
{
  return is_error() || _base == FUNCTION;
}

bool Type::user_type() const
{
  return is_error() || _base == USER_TYPE;
}

const Type& Type::elements(std::size_t index) const
{
  return is_error() ? *this : _elements[std::min(index, _elements.size() - 1)];
}

void Type::add_element(const Type& type)
{
  _elements.push_back(type);
}

std::size_t Type::element_size() const
{
  return _elements.size();
}

bool Type::element_size(std::size_t num_elements) const
{
  return is_error() || _elements.size() == num_elements;
}

bool Type::element_is(std::size_t index, const Type& type) const
{
  return is_error() || _elements[index].is(type);
}

bool Type::count_binary_match(const Type& t) const
{
  return is_error() || t.is_error() ||
      count() == t.count() || count() == 1 || t.count() == 1;
}

bool Type::is(const Type& t) const
{
  return *this == t || is_error() || t.is_error();
}

Type Type::unify(const Type& t) const
{
  return *this != t ? ERROR : *this;
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
  return _base == t._base && _count == t._count &&
      _user_type_uid == t._user_type_uid &&
      _managed_user_type == t._managed_user_type;
}

bool Type::operator!=(const Type& t) const
{
  return !(*this == t);
}

yang::Type Type::external(bool exported) const
{
  yang::Type t =
      _base == INT ? yang::Type::int_t() :
      _base == FLOAT ? yang::Type::float_t() :
      yang::Type::void_t();
  if (_count > 1) {
    t = _base == INT ? yang::Type::int_vector_t(_count) :
        _base == FLOAT ? yang::Type::float_vector_t(_count) :
        yang::Type::void_t();
  }
  if (_base == FUNCTION) {
    std::vector<yang::Type> args;
    auto it = _elements.begin();
    for (++it; it != _elements.end(); ++it) {
      args.push_back(it->external(false));
    }
    t = yang::Type::function_t(_elements[0].external(false), args);
  }
  if (_base == USER_TYPE) {
    t = yang::Type::user_t<void>(_managed_user_type);
    t._user_type_uid = _user_type_uid;
  }
  return t.make_exported(exported).make_const(_const);
}

std::string Type::string_internal() const
{
  if (_base == USER_TYPE) {
    return user_type_name() +
        (_managed_user_type ? "&" : "*") + (_const ? " const" : "");
  }

  if (_base == FUNCTION) {
    std::string s = _elements[0].string_internal() + "(";
    for (std::size_t i = 1; i < _elements.size(); ++i) {
      if (i > 1) {
        s += ", ";
      }
      s += _elements[i].string_internal();
    }
    return s + ")" + (_const ? " const" : "");
  }

  std::string s =
      _base == VOID ? "void" :
      _base == INT ? "int" :
      _base == FLOAT ? "float" : "error";

  if (_count > 1) {
    s += std::to_string(_count);
  }
  return s + (_const ? " const" : "");
}

// End namespace yang::internal.
}
}
