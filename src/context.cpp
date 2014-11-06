//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/context.h>
#include <yang/instance.h>
#include <yang/runtime_error.h>

namespace yang {
namespace internal {

const Type& ContextInternals::type_lookup(const std::string& name) const
{
  static Type none = Type::void_t();
  auto it = types.find(name);
  return it == types.end() ? none : it->second;
}

const ContextInternals::Constructor& ContextInternals::constructor_lookup(
    const std::string& name) const
{
  static Constructor none;
  auto it = constructors.find(name);
  return it == constructors.end() ? none : it->second;
}

const ErasedFunction& ContextInternals::function_lookup(
    const std::string& name) const
{
  static ErasedFunction none;
  auto it = functions.find(name);
  return it == functions.end() ? member_lookup(name) : it->second;
}

const ErasedFunction& ContextInternals::member_lookup(
    const std::string& name) const
{
  static ErasedFunction none;
  std::size_t index = name.find_last_of(':');
  if (index == std::string::npos) {
    return none;
  }
  std::string first = name.substr(0, index - 1);
  std::string last = name.substr(index + 1);
  return member_lookup(type_lookup(first), last);
}

const ErasedFunction& ContextInternals::member_lookup(
    const Type& type, const std::string& name) const
{
  static ErasedFunction none;
  auto member_it = members.find(type);
  if (member_it == members.end()) {
    return none;
  }
  auto member_jt = member_it->second.find(name);
  return member_jt == member_it->second.end() ? none : member_jt->second;
}

} // ::internal

Context::Context()
  : _internals{new internal::ContextInternals{{}, {}, {}, {}, {}, false}}
{
}

void Context::register_namespace(const std::string& name,
                                 const Context& context)
{
  check_namespace(name);
  if (&*_internals == &*context._internals) {
    throw RuntimeError("context registered as a namespace in itself");
  }
  for (const auto& pair : context._internals->members) {
    for (const auto& qair : pair.second) {
      // If types have duplicated member functions, we can merge only if we're
      // sure they're absolutely identical.
      const auto& map = _internals->members[pair.first];
      auto it = map.find(qair.first);
      if (it != map.end() && it->second != qair.second) {
        throw RuntimeError(
            "member function `" + pair.first.string(context) + "::" +
            qair.first + "` in context registered as namespace conflicts with "
            "existing member of `" + pair.first.string(*this) + "`");
      }
    }
  }
  copy_internals();

  _internals->namespaces.insert(name);
  for (const auto& pair : context._internals->types) {
    _internals->types.emplace(name + "::" + pair.first, pair.second);
  }
  for (const auto& pair : context._internals->constructors) {
    _internals->constructors.emplace(name + "::" + pair.first, pair.second);
  }
  for (const auto& pair : context._internals->functions) {
    _internals->functions.emplace(name + "::" + pair.first, pair.second);
  }
  for (const auto& pair : context._internals->members) {
    for (const auto& qair : pair.second) {
      _internals->members[pair.first].emplace(qair.first, qair.second);
    }
  }
}

void Context::register_namespace(const std::string& name,
                                 const Instance& instance)
{
  // Since this function makes the Context depend on some Instance, it would
  // seem that an uncollectable cycle of shared_ptrs could be constructed by
  // creating an Instance using some Context, and then registering that Instance
  // back into the same Context as a namespace.
  //
  // However, this (and similar) issues are handily sidestepped by the lazy
  // duplication of context internals. The second Context will in fact have a
  // different internals than the original, and the cycle will be broken.
  check_namespace(name);
  Context context;
  for (const auto& pair : instance.get_functions()) {
    // This is kind of awkward. It depends on otherwise-unnecessary friendship
    // from Instance. A nice integration of yang::Value might make it a lot
    // better.
    auto& f = context._internals->functions[pair.first];
    f.type = pair.second;
    f.env_ref = instance._global_data.get();
    f.yang_function = instance.get_native_fp(pair.first);
  }
  // Conservative decision: no type used by the instance is automatically
  // exposed. Clearly, member functions shouldn't be added, so it'd be weird
  // to expose types but not their members.
  // This could be revisited: it might be okay to expose the namespace-prefixed
  // shortest name of each type that actually appears in the exported API, for
  // example.
  register_namespace(name, context);
}

void Context::check_namespace(const std::string& name) const
{
  check_identifier(name);
  if (_internals->types.find(name) != _internals->types.end()) {
    throw RuntimeError(
        "namespace `" + name + "` conflicts with existing registered typename");
  }
  if (_internals->namespaces.find(name) != _internals->namespaces.end()) {
    throw RuntimeError(
        "duplicate namespace `" + name + "` registered in context");
  }
}

void Context::check_type(const std::string& name) const
{
  check_identifier(name);
  if (_internals->namespaces.find(name) != _internals->namespaces.end()) {
    throw RuntimeError(
        "typename `" + name + "` conflicts with existing registered namespace");
  }
  if (_internals->types.find(name) != _internals->types.end()) {
    throw RuntimeError(
        "duplicate typename `" + name + "` registered in context");
  }
}

void Context::check_function(const std::string& name) const
{
  check_identifier(name);
  if (_internals->constructors.find(name) != _internals->constructors.end()) {
    throw RuntimeError(
        "function `" + name +
        "` conflicts with existing registered constructor");
  }
  if (_internals->functions.find(name) != _internals->functions.end()) {
    throw RuntimeError(
        "duplicate function `" + name + "` registered in context");
  }
}

void Context::check_constructor(const std::string& name) const
{
  check_identifier(name);
  if (_internals->functions.find(name) != _internals->functions.end()) {
    throw RuntimeError(
        "constructor `" + name +
        "` conflicts with existing registered function");
  }
  if (_internals->constructors.find(name) != _internals->constructors.end()) {
    throw RuntimeError(
        "duplicate constructor `" + name + "` registered in context");
  }
}

void Context::check_member_function(const Type& type,
                                    const std::string& name) const
{
  check_identifier(name);
  if (_internals->members[type].find(name) !=
      _internals->members[type].end()) {
    throw RuntimeError(
        "duplicate member function `" + type.string(*this) + "::" +
        name + "` registered in context");
  }
}

void Context::check_identifier(const std::string& ident) const
{
  for (char c : ident) {
    if (!(c >= 'a' && c <= 'z') && !(c >= 'A' && c <= 'Z') &&
        !(c >= '0' && c <= '9') && c != '_') {
      throw RuntimeError("invalid identifier `" + ident + "`");
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

} // ::yang
