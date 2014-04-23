//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/context.h>

namespace {

yang::Type prefix_type(const std::string& name, const yang::Type& type)
{
  if (type.is_function()) {
    std::vector<yang::Type> args;
    for (std::size_t i = 0; i < type.get_function_num_args(); ++i) {
      args.push_back(prefix_type(name, type.get_function_arg_type(i)));
    }
    yang::Type t = yang::Type::function_t(
        prefix_type(name, type.get_function_return_type()), args);
    return t.make_exported(type.is_exported()).make_const(type.is_const());
  }
  else if (type.is_user_type()) {
    yang::Type t = yang::Type::user_t(name + type.get_user_type_name(),
                                      type.is_managed_user_type());
    return t.make_exported(type.is_exported()).make_const(type.is_const());
  }
  return type;
}

// End anonymous namespace.
}

namespace yang {

Context::Context()
  : _internals{new internal::ContextInternals{{}, {}, {}, false}}
{
}

void Context::register_namespace(const std::string& name,
                                 const Context& context)
{
  check_namespace(name);
  if (&*_internals == &*context._internals) {
    throw runtime_error("context registered as a namespace in itself");
  }
  for (const auto pair : context._internals->types) {
    for (const auto qair : _internals->types) {
      if (pair.second.native.obj->is(*qair.second.native.obj)) {
        throw runtime_error(
            "type `" + pair.first + "` in context registered as namespace `"
            + name + "` duplicates type `" + qair.first + "`");
      }
    }
  }
  _internals->namespaces.insert(name);
  copy_internals();

  for (const auto& pair : context._internals->types) {
    _internals->types.emplace(name + "::" + pair.first, pair.second);
  }
  for (const auto& pair : context._internals->functions) {
    std::string s = name + "::" + pair.first;
    _internals->functions.emplace(s, pair.second);
    _internals->functions[s].type = prefix_type(
        name + "::", _internals->functions[s].type);
  }
}

void Context::check_namespace(const std::string& name) const
{
  check_identifier(name);
  auto jt = _internals->namespaces.find(name);
  if (jt != _internals->namespaces.end()) {
    throw runtime_error(
        "duplicate namespace `" + name + "` registered in context");
  }
  auto it = _internals->types.find(name);
  if (it != _internals->types.end()) {
    throw runtime_error("namespace `" + name + "` conflicts with typename");
  }
}

void Context::check_identifier(const std::string& ident) const
{
  for (char c : ident) {
    if (!(c >= 'a' && c <= 'z') && !(c >= 'A' && c <= 'Z') &&
        !(c >= '0' && c <= '9') && c != '_') {
      throw runtime_error("invalid identifier `" + ident + "`");
    }
  }
}

void Context::copy_internals()
{
  // Lazily duplicate the internal state so that modified Contexts don't affect
  // objects which depend on their internals already.
  if (_internals->immutable) {
    _internals->immutable = false;
    _internals.reset(new internal::ContextInternals(*_internals));
  }
}

// End namespace yang.
}
