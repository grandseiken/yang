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

Category Category::error()
{
  Category c;
  c._error = true;
  return c;
}

const Type& Category::type() const
{
  return _type;
}

const std::vector<void*>& Category::tags() const
{
  return _tags;
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

Category Category::vector_element(std::size_t size) const
{
  if (is_error()) {
    return *this;
  }
  Category c = *this;
  if (is_int()) {
    c._type = size == 1 ? Type::int_t() : Type::ivec_t(size);
    return c;
  }
  if (is_float()) {
    c._type = size == 1 ? Type::float_t() : Type::fvec_t(size);
    return c;
  }
  return error();
}

Category Category::interface_member(const std::string& member) const
{
  if (is_error()) {
    return *this;
  }
  Category c = make_lvalue(false).make_const(false);
  for (const auto& pair : _type.interface_members()) {
    if (pair.first == member) {
      c._type = pair.second;
      return c;
    }
  }
  return Type::void_t();
}

Category Category::unify(const Category& c) const
{
  auto r = error().add_tags(*this);
  if (!is_error() && !c.is_error() && is(c)) {
    r = make_const(_const || c._const);
    r = r.make_lvalue(is_lvalue() && c.is_lvalue());
  }
  return r.add_tags(c);
}

bool Category::is(const Category& c) const
{
  return is_error() || c.is_error() || _type == c._type;
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

bool Category::interface() const
{
  return is_error() || _type.is_interface();
}

bool Category::arg_size(std::size_t num_args) const
{
  return is_error() ||
      (_type.is_function() && _type.function_args().size() == num_args);
}

bool Category::arg_is(std::size_t index, const Category& category) const
{
  return is_error() || category.is(_type.function_args()[index]);
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

}} // ::yang::internal
