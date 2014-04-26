//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/context.h>
#include <yang/instance.h>

namespace yang {
namespace internal {

ContextInternals::type_def::type_def(const yang::Type& type)
  : type(type)
{
}

const ContextInternals::type_def& ContextInternals::type_lookup(
    const std::string& name) const
{
  static type_def none(yang::Type::void_t());
  auto it = types.find(name);
  return it == types.end() ? none : it->second;
}

const GenericFunction& ContextInternals::function_lookup(
    const std::string& name) const
{
  static GenericFunction none;
  auto it = functions.find(name);
  return it == functions.end() ? member_lookup(name) : it->second;
}

const GenericFunction& ContextInternals::member_lookup(
    const std::string& name) const
{
  static GenericFunction none;
  std::size_t index = name.find_last_of(':');
  if (index == std::string::npos) {
    return none;
  }
  std::string first = name.substr(0, index - 1);
  std::string last = name.substr(index + 1);
  return member_lookup(type_lookup(first).type, last);
}

const GenericFunction& ContextInternals::member_lookup(
    const yang::Type& type, const std::string& name) const
{
  static GenericFunction none;
  auto member_it = members.find(type);
  if (member_it == members.end()) {
    return none;
  }
  auto member_jt = member_it->second.find(name);
  return member_jt == member_it->second.end() ? none : member_jt->second;
}

// End namespace internal.
}

Context::Context()
  : _internals{new internal::ContextInternals{{}, {}, {}, {}, false}}
{
}

void Context::register_namespace(const std::string& name,
                                 const Context& context)
{
  check_namespace(name);
  if (&*_internals == &*context._internals) {
    throw runtime_error("context registered as a namespace in itself");
  }
  for (const auto& pair : context._internals->members) {
    for (const auto& qair : pair.second) {
      // TODO: is there any way around this? We could perhaps merge if the member
      // functions are identical... is there any use-case for that?
      const auto& map = _internals->members[pair.first];
      if (map.find(qair.first) != map.end()) {
        throw runtime_error(
            "context registered as namespace with conflicting member "
            "function `" + pair.first.string() + "::" + qair.first + "`");
      }
    }
  }
  _internals->namespaces.insert(name);
  copy_internals();

  for (const auto& pair : context._internals->types) {
    _internals->types.emplace(name + "::" + pair.first, pair.second);
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
    // TODO: finish implementing this. With tests: using internal types with
    // no names; using external types with many namespaced names.
  }
  register_namespace(name, context);
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
