//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "internal_type.h"

namespace yang {
namespace internal {

Type::Type(const yang::Type& type)
  : _wrap(type)
  , _error(false)
{
}

Type::Type(bool error)
  : _wrap(yang::Type::void_t())
  , _error(error)
{
}

const yang::Type& Type::external() const
{
  return _wrap;
}

std::string Type::string() const
{
  return _error ? "<error>" : "`" + _wrap.string() + "`";
}

Type Type::make_const(bool is_const) const
{
  return _error ? *this : external().make_const(is_const);
}

bool Type::is_error() const
{
  return _error;
}

bool Type::is_void() const
{
  return is_error() || _wrap.is_void();
}

bool Type::not_void() const
{
  return is_error() || !_wrap.is_void();
}

bool Type::primitive() const
{
  return is_error() || _wrap.is_int() || _wrap.is_float();
}

bool Type::is_vector() const
{
  return is_error() || _wrap.is_ivec() || _wrap.is_fvec();
}

bool Type::is_int() const
{
  return is_error() || _wrap.is_int() || _wrap.is_ivec();
}

bool Type::is_float() const
{
  return is_error() || _wrap.is_float() || _wrap.is_fvec();
}

bool Type::function() const
{
  return is_error() || _wrap.is_function();
}

bool Type::user_type() const
{
  return is_error() || _wrap.is_user_type();
}

bool Type::element_size(std::size_t num_elements) const
{
  return is_error() ||
      (!_wrap.is_function() && num_elements == 0) ||
      (_wrap.is_function() && 1 + _wrap.function_num_args() == num_elements);
}

bool Type::element_is(std::size_t index, const Type& type) const
{
  return is_error() || type.is_error() ||
      (!index && Type(_wrap.function_return()) == type) ||
      (index && Type(_wrap.function_arg(index - 1)) == type);
}

bool Type::is_binary_match(const Type& t) const
{
  return is_error() || t.is_error() ||
      _wrap.vector_size() == t._wrap.vector_size() ||
      _wrap.vector_size() == 1 || t._wrap.vector_size() == 1;
}

bool Type::is(const Type& t) const
{
  return *this == t || is_error() || t.is_error();
}

Type Type::unify(const Type& t) const
{
  return *this != t ? Type(true) : *this;
}

bool Type::operator==(const Type& t) const
{
  if (_error != t._error) {
    return false;
  }
  if (_error) {
    return true;
  }
  return _wrap == t._wrap;
}

bool Type::operator!=(const Type& t) const
{
  return !(*this == t);
}

// End namespace yang::internal.
}
}
