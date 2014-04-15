//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_CONTEXT_H
#define YANG_INCLUDE_YANG_CONTEXT_H

#include <string>
#include <unordered_map>

#include "error.h"
#include "native.h"
#include "refcounting.h"
#include "type.h"
#include "type_info.h"

// Big features:
// TODO: interfaces. For example, it should be possible to define somehow an
// interface type like:
//
//   interface T {
//     int() foo;
//     void(int x) bar;
//   };
//
// The following things are then convertible to a value of type T:
//
// - pointers to a user-type having member functions matching the interface;
// - Instances of Yang programs having functions matching the interface.
//
// Mostly, these can only be passed from C++; perhaps a special "self" keyword
// can exist as well to get the current program instance as an interface value.
//
// Misc stuff:
// TODO: context combination and scopes.
// TODO: as alternative to textual include, allow code-sharing by way of
// treating a Program as a Context (using LLVM modules to avoid the need for
// complicated trampolining back and forth).
// TODO: better assignment with "references". E.g. a[i] and ++i should both be
// assignable-to. Vectorised assignment? Pattern-matching assignment?
// TODO: code hot-swapping. Careful with pointer values (e.g. functions) in
// global data struct which probably need to be left as default values.
// TODO: possibly allow ref-counted user types. Set up some template that
// mirrors the mechanics of Function.
// TODO: make sure everything is (possibly optionally?) thread-safe.
// Things that aren't thread-safe (and need locking):
// - YangTrampolineGlobals
// - possibly cpp_trampoline_lookup_map
// - static refcount deletion lists; these need to be per-something somehow
//   so that objects added temporarily don't get deallocated!
// - global data / closure data structure access. Both for concurrent invocation
//   of functions from the same Instance, and concurrent invocation of functions
//   from different Instances where one happens to hold a reference to a
//   function from the other.
// The last is the trickiest bit and should probably be optional.
// TODO: add a layer of abstraction so the LLVM backend can be swapped out
// easily (e.g. for a bytecode backend).
//
// Further off (helpful stuff that can be emulated without needing to be built-
// -in right away):
// TODO: a standard library (as a Context).
// TODO: add a generic value class for easy generic behaviour on C++ side.
// TODO: add some kind of built-in data structures, including at least a generic
// map<K, V> type. Decide on semantics (e.g. garbage collection).
// TODO: make sure the exposed APIs have sensible and useful interfaces; e.g.,
// should they have accessors to retrieve all possible useful data; do they
// return strings or output to streams; etc.
namespace yang {
namespace internal {

// Data preserved while it's needed (by Context objects or Programs that depend
// on them). It might make more sense for Contexts to be completely immutable
// with a builder mechanism, but since they can only be *added* to currently,
// everything should work even if they're modified after they've been used to
// instantiate a program.
struct ContextInternals {
  typedef std::unordered_map<std::string, internal::GenericNativeType> type_map;
  typedef std::unordered_map<
      std::string, internal::GenericFunction> function_map;

  type_map types;
  function_map functions;
};

// End namespace internal.
}

class Context {
public:

  Context()
    : _internals(new internal::ContextInternals) {};

  // Add a user type. Types must be registered before registering functions that
  // make use of them.
  template<typename T>
  void register_type(const std::string& name);

  // Add a member function to a user type.
  template<typename T, typename R, typename... Args>
  void register_member_function(
      const std::string& name, const Function<R(T*, Args...)>& f);

  // Add a globally-available function to the context.
  template<typename R, typename... Args>
  void register_function(
      const std::string& name, const Function<R(Args...)>& f);

  // Information about registered types.
  template<typename T>
  bool has_type() const;
  template<typename T>
  std::string get_type_name() const;

private:

  friend class Program;
  friend const internal::ContextInternals&
      internal::context_internals(const Context&);
  std::shared_ptr<internal::ContextInternals> _internals;

};

// Implementation of TypeInfo for user types, which has to see the definition of
// Context.
namespace internal {

template<typename T>
bool context_has_type_impl(const ContextInternals& internals)
{
  for (const auto& pair : internals.types) {
    if (pair.second.obj->is<T*>()) {
      return true;
    }
  }
  return false;
}

template<typename T>
std::string context_get_type_name_impl(const ContextInternals& internals)
{
  for (const auto& pair : internals.types) {
    if (pair.second.obj->is<T*>()) {
      return pair.first;
    }
  }
  throw runtime_error("use of unregistered user type");
} 

template<typename T>
struct TypeInfo<T*> {
  yang::Type operator()(const ContextInternals& context) const
  {
    if (!context_has_type_impl<T>(context)) {
      throw runtime_error("use of unregistered user type");
    }
    return yang::Type::user_t(context_get_type_name_impl<T>(context));
  }

  // Without passing a context, we just construct erased user-types.
  yang::Type operator()() const
  {
    return yang::Type::user_t();
  }
};

// End namespace internal.
}

template<typename T>
void Context::register_type(const std::string& name)
{
  auto it = _internals->types.find(name);
  if (it != _internals->types.end()) {
    throw runtime_error("duplicate type `" + name + "` registered in context");
    return;
  }
  // Could also store a reverse-map from internal pointer type-id; probably
  // not a big deal.
  for (const auto& pair : _internals->types) {
    if (pair.second.obj->is<T*>()) {
      throw runtime_error("duplicate types `" + name + "` and `" + pair.first +
                          "` registered in context");
      return;
    }
  }

  internal::GenericNativeType& symbol = _internals->types[name];
  symbol.obj = std::unique_ptr<internal::NativeType<T*>>(
      new internal::NativeType<T*>());
}

template<typename T, typename R, typename... Args>
void Context::register_member_function(
    const std::string& name, const Function<R(T*, Args...)>& f)
{
  if (!has_type<T>()) {
    throw runtime_error(
        "member `" + name + "` registered on unregistered type");
    return;
  }
  std::string type_name = get_type_name<T>();
  register_function(type_name + "::" + name, f);
}

template<typename R, typename... Args>
void Context::register_function(
    const std::string& name, const Function<R(Args...)>& f)
{
  auto it = _internals->functions.find(name);
  if (it != _internals->functions.end()) {
    throw runtime_error(
        "duplicate function `" + name + "` registered in context");
    return;
  }

  internal::TypeInfo<Function<R(Args...)>> info;
  internal::GenericFunction& symbol = _internals->functions[name];
  symbol.type = info(*_internals);
  symbol.ptr =
      std::unique_ptr<internal::FunctionBase>(new Function<R(Args...)>(f));
  internal::GenerateReverseTrampolineLookupTable<Function<R(Args...)>>()();
}

template<typename T>
bool Context::has_type() const
{
  return internal::context_has_type_impl<T>(*_internals);
}

template<typename T>
std::string Context::get_type_name() const
{
  return internal::context_get_type_name_impl<T>(*_internals);
}

// End namepsace yang.
}

#endif
