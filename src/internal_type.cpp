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
  , _lvalue(false)
{
}

Type::Type(bool error)
  : _wrap(yang::Type::void_t())
  , _error(error)
  , _lvalue(false)
{
}

const yang::Type& Type::external() const
{
  return _wrap;
}

std::string Type::string(const ContextInternals& context, bool quote) const
{
  return _error ? "<error>" :
      quote ? "`" + _wrap.string(context) + "`" : _wrap.string(context);
}

Type Type::make_const(bool is_const) const
{
  Type t = *this;
  t._wrap = external().make_const(is_const);
  return _error ? *this : t;
}

Type Type::make_lvalue(bool is_lvalue) const
{
  Type t = *this;
  t._lvalue = is_lvalue;
  return _error ? *this : t;
}

Type Type::add_tag(void* tag) const
{
  Type t = *this;
  t._tags.push_back(tag);
  return t;
}

Type Type::add_tags(const Type& type) const
{
  Type t = *this;
  for (void* tag : type.tags()) {
    t._tags.push_back(tag);
  }
  return t;
}

const std::vector<void*>& Type::tags() const
{
  return _tags;
}

bool Type::is_error() const
{
  return _error;
}

bool Type::is_lvalue() const
{
  return is_error() || _lvalue;
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

bool Type::is_assign_binary_match(const Type& t) const
{
  return is_error() || t.is_error() ||
      _wrap.vector_size() == t._wrap.vector_size() ||
      t._wrap.vector_size() == 1;
}

bool Type::is(const Type& t) const
{
  return is_error() || t.is_error() ||
      _wrap.make_const(false) == t._wrap.make_const(false);
}

Type Type::unify(const Type& t) const
{
  auto r = Type(true).add_tags(*this);
  if (!is_error() && !t.is_error() && is(t)) {
    r = make_const(external().is_const() || t.external().is_const());
    r = r.make_lvalue(is_lvalue() && t.is_lvalue());
  }
  return r.add_tags(t);
}

bool Type::operator==(const Type& t) const
{
  if (_error != t._error || _lvalue != t._lvalue) {
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
