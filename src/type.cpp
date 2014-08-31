//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/type.h>
#include <yang/context.h>
#include <yang/error.h>

namespace yang {

std::string Type::string(const Context& context) const
{
  return string(*context._internals);
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
  return is_ivec() || is_fvec();
}

bool Type::is_ivec() const
{
  return _base == INT && _count > 1;
}

bool Type::is_fvec() const
{
  return _base == FLOAT && _count > 1;
}

std::size_t Type::vector_size() const
{
  return _count;
}

bool Type::is_function() const
{
  return _base == FUNCTION;
}

std::size_t Type::function_num_args() const
{
  return _elements.empty() ? 0 : _elements.size() - 1;
}

const Type& Type::function_return() const
{
  return _elements.empty() ? void_type : _elements[0];
}

const Type& Type::function_arg(std::size_t index) const
{
  return 1 + index >= _elements.size() ? void_type : _elements[1 + index];
}

bool Type::is_user_type() const
{
  return _base == USER_TYPE;
}

bool Type::is_managed_user_type() const
{
  return _managed_user_type;
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
  return _user_type_uid == t._user_type_uid &&
      _managed_user_type == t._managed_user_type &&
      _base == t._base && _count == t._count;
}

bool Type::operator!=(const Type& t) const
{
  return !operator==(t);
}

Type Type::void_t()
{
  return {};
}

Type Type::int_t()
{
  Type t;
  t._base = INT;
  return t;
}

Type Type::float_t()
{
  Type t;
  t._base = FLOAT;
  return t;
}

Type Type::ivec_t(std::size_t size)
{
  if (size <= 1) {
    throw runtime_error(
        "vector type created with size " + std::to_string(size));
  }
  Type t = int_t();
  t._count = size;
  return t;
}

Type Type::fvec_t(std::size_t size)
{
  if (size <= 1) {
    throw runtime_error(
        "vector type created with size " + std::to_string(size));
  }
  Type t = float_t();
  t._count = size;
  return t;
}

Type Type::function_t(const Type& return_t, const std::vector<Type>& args)
{
  Type t;
  t._base = FUNCTION;
  t._elements.push_back(return_t);
  for (const Type& u : args) {
    t._elements.push_back(u);
  }
  return t;
}

Type Type::make_managed(bool managed) const
{
  Type t = *this;
  t._managed_user_type = t._base == USER_TYPE && managed;
  return t;
}

Type Type::erase_user_types() const
{
  Type t = *this;
  t._user_type_uid = nullptr;
  for (auto& u : t._elements) {
    u = u.erase_user_types();
  }
  return t;
}

Type::Type()
  : _base(VOID)
  , _count(1)
  , _user_type_uid(nullptr)
  , _managed_user_type(false)
{
}

std::string Type::string(const internal::ContextInternals& context) const
{
  std::string s;
  if (_base == USER_TYPE) {
    // Find the name(s) of this type with the fewest namespace prefixes.
    bool first = true;
    std::size_t scope_min = 0;
    std::vector<std::string> names;
    for (const auto& pair : context.types) {
      if (*this != pair.second) {
        continue;
      }
      std::size_t scope = 0;
      for (std::size_t i = 0; i < pair.first.length(); ++i) {
        scope += pair.first[i] == ':';
      }
      if (scope < scope_min || first) {
        scope_min = scope;
        names.clear();
      }
      if (scope == scope_min) {
        names.push_back(pair.first);
      }
      first = false;
    }

    if (names.empty()) {
      s = "anon." + std::to_string((std::intptr_t)_user_type_uid);
    }
    else if (names.size() == 1) {
      s = names.back();
    }
    else {
      for (std::size_t i = 0; i < names.size(); ++i) {
        if (i) {
          s += ",";
        }
        s += names[i];
      }
      s = "{" + s + "}";
    }
    s += (_managed_user_type ? "&" : "*");
  }
  else if (_base == FUNCTION) {
    s += _elements[0].string(context) + "(";
    for (std::size_t i = 1; i < _elements.size(); ++i) {
      if (i > 1) {
        s += ", ";
      }
      s += _elements[i].string(context);
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
  return s;
}

Type Type::void_type;

Global::Global(const Type& type, bool is_const, bool is_exported)
  : type(type)
  , is_const(is_const)
  , is_exported(is_exported)
{
}

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
    hash_combine(seed, (std::intptr_t)type._user_type_uid);
    hash_combine(seed, type._managed_user_type);
    return seed;
  }
}
