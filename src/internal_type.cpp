#include "internal_type.h"

namespace yang {
namespace internal {

Type::Type(const yang::Type& type)
  : Type(VOID)
{
  _base =
      type._base == yang::Type::INT ? INT :
      type._base == yang::Type::WORLD ? WORLD :
      type._base == yang::Type::FUNCTION ? FUNCTION :
      type._base == yang::Type::USER_TYPE ? USER_TYPE :
      VOID;
  _const = type._const;
  _count = type._count;
  _user_type_name = type._user_type_name;
  for (const yang::Type& u : type._elements) {
    _elements.push_back(u);
  }
}

Type::Type(type_base base, std::size_t count)
  : _base(base)
  , _count(count)
  , _const(false)
{
  if (base == FUNCTION || count == 0 ||
      (count != 1 && base != INT && base != WORLD)) {
    _base = ERROR;
    _count = 1;
  }
}

Type::Type(type_base base, const Type& return_type)
  : _base(FUNCTION)
  , _count(1)
  , _const(false)
{
  if (base != FUNCTION) {
    _base = ERROR;
    return;
  }
  _elements.push_back(return_type);
}

Type::Type(type_base base, const std::string& user_type_name)
  : _base(USER_TYPE)
  , _count(1)
  , _const(false)
  , _user_type_name(user_type_name)
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

const std::string& Type::user_type_name() const
{
  return _user_type_name;
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
      (_count == 1 && (is_int() || is_world()));
}

bool Type::is_vector() const
{
  return is_error() ||
      (_count > 1 && (is_int() || is_world()));
}

bool Type::is_int() const
{
  return is_error() || _base == INT;
}

bool Type::is_world() const
{
  return is_error() || _base == WORLD;
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
      _user_type_name == t._user_type_name;
}

bool Type::operator!=(const Type& t) const
{
  return !(*this == t);
}

yang::Type Type::external(bool exported) const
{
  yang::Type t;
  t._exported = exported;
  t._const = _const;
  t._base =
      _base == INT ? yang::Type::INT :
      _base == WORLD ? yang::Type::WORLD :
      _base == FUNCTION ? yang::Type::FUNCTION :
      _base == USER_TYPE ? yang::Type::USER_TYPE :
      yang::Type::VOID;
  t._count = _count;
  t._user_type_name = _user_type_name;
  for (const Type& u : _elements) {
    t._elements.push_back(u.external(false));
  }
  return t;
}

std::string Type::string_internal() const
{
  if (_base == USER_TYPE) {
    return _user_type_name + "*";
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
      _base == WORLD ? "world" : "error";

  if (_count > 1) {
    s += std::to_string(_count);
  }
  return s + (_const ? " const" : "");
}

// End namespace yang::internal.
}
}
