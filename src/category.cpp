//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "category.h"

namespace yang {
namespace internal {

Category::Category(const Type& type)
  : _type(type)
  , _error(false)
  , _lvalue(false)
  , _const(false)
{
}

Category::Category(bool error)
  : _type(Type::void_t())
  , _error(error)
  , _lvalue(false)
  , _const(false)
{
}

const Type& Category::type() const
{
  return _type;
}

std::string Category::string(const ContextInternals& context, bool quote) const
{
  return _error ? "<error>" :
      quote ? "`" + _type.string(context) + "`" : _type.string(context);
}

Category Category::make_const(bool is_const) const
{
  Category c = *this;
  c._const = is_const;
  return _error ? *this : c;
}

Category Category::make_lvalue(bool is_lvalue) const
{
  Category c = *this;
  c._lvalue = is_lvalue;
  return _error ? *this : c;
}

Category Category::add_tag(void* tag) const
{
  Category c = *this;
  c._tags.push_back(tag);
  return c;
}

Category Category::add_tags(const Category& category) const
{
  Category c = *this;
  for (void* tag : category.tags()) {
    c._tags.push_back(tag);
  }
  return c;
}

const std::vector<void*>& Category::tags() const
{
  return _tags;
}

bool Category::is_error() const
{
  return _error;
}

bool Category::is_lvalue() const
{
  return is_error() || _lvalue;
}

bool Category::not_const() const
{
  return is_error() || !_const;
}

bool Category::is_void() const
{
  return is_error() || _type.is_void();
}

bool Category::not_void() const
{
  return is_error() || !_type.is_void();
}

bool Category::primitive() const
{
  return is_error() || _type.is_int() || _type.is_float();
}

bool Category::is_vector() const
{
  return is_error() || _type.is_ivec() || _type.is_fvec();
}

bool Category::is_int() const
{
  return is_error() || _type.is_int() || _type.is_ivec();
}

bool Category::is_float() const
{
  return is_error() || _type.is_float() || _type.is_fvec();
}

bool Category::function() const
{
  return is_error() || _type.is_function();
}

bool Category::user_type() const
{
  return is_error() || _type.is_user_type();
}

bool Category::element_size(std::size_t num_elements) const
{
  return is_error() ||
      (!_type.is_function() && num_elements == 0) ||
      (_type.is_function() && 1 + _type.function_num_args() == num_elements);
}

bool Category::element_is(std::size_t index, const Category& category) const
{
  return is_error() || category.is_error() ||
      (!index && Category(_type.function_return()) == category) ||
      (index && Category(_type.function_arg(index - 1)) == category);
}

bool Category::is_binary_match(const Category& c) const
{
  return is_error() || c.is_error() ||
      _type.vector_size() == c._type.vector_size() ||
      _type.vector_size() == 1 || c._type.vector_size() == 1;
}

bool Category::is_assign_binary_match(const Category& c) const
{
  return is_error() || c.is_error() ||
      _type.vector_size() == c._type.vector_size() ||
      c._type.vector_size() == 1;
}

bool Category::is(const Category& c) const
{
  return is_error() || c.is_error() || _type == c._type;
}

Category Category::unify(const Category& c) const
{
  auto r = Category(true).add_tags(*this);
  if (!is_error() && !c.is_error() && is(c)) {
    r = make_const(_const || c._const);
    r = r.make_lvalue(is_lvalue() && c.is_lvalue());
  }
  return r.add_tags(c);
}

bool Category::operator==(const Category& c) const
{
  if (_error != c._error || _lvalue != c._lvalue) {
    return false;
  }
  return _error || _type == c._type;
}

bool Category::operator!=(const Category& c) const
{
  return !(*this == c);
}

// End namespace yang::internal.
}
}
