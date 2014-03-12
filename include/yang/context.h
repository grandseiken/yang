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
// TODO: contexts should be immutable and store their data in a shared_ptr which
// is shared by client programs.
// TODO: as alternative to textual include, allow code-sharing by way of
// treating a Program as a Context (using LLVM modules to avoid the need for
// complicated trampolining back and forth).
// TODO: vectorised assignment, or pattern-matching assignment? Also, indexed
// assignment.
// TODO: code hot-swapping. Careful with pointer values (e.g. functions) in
// global data struct which probably need to be left as default values.
// TODO: possibly allow ref-counted user types. Set up some template that
// mirrors the mechanics of Function.
// TODO: make sure everything is (possibly optionally?) thread-safe.
// Things that aren't thread-safe (and need locking):
// - YangTrampolineGlobals
// - possibly cpp_trampoline_lookup_map
// - static refcount deletion lists
// - global data / closure data structure access. Both for concurrent invocation
//   of functions from the same Instance, and concurrent invocation of functions
//   from different Instances where one happens to hold a reference to a
//   function from the other.
// The last is the trickiest bit and should probably be optional.
//
// Further off (helpful stuff that can be emulated without needing to be built-
// -in right away):
// TODO: a standard library (as a Context).
// TODO: add a LuaValue-like generic value class.
// TODO: add some kind of built-in data structures, including at least a generic
// map<K, V> type. May require garbage-collection, unless we place tight
// restrictions on their usage (e.g. only global variables).
// TODO: make sure the exposed APIs have sensible and useful interfaces; e.g.,
// should they have accessors to retrieve all possible useful data; do they
// return strings or output to streams; etc.
namespace yang {
namespace internal {
  class StaticChecker;
  class IrGenerator;
}

class Context {
public:

  Context() {}

  // Noncopyable.
  Context(const Context&) = delete;
  Context& operator=(const Context&) = delete;

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

  typedef std::unordered_map<std::string, internal::GenericNativeType> type_map;
  typedef std::unordered_map<
      std::string, internal::GenericFunction> function_map;

  friend class internal::StaticChecker;
  friend class internal::IrGenerator;
  const type_map& get_types() const;
  const function_map& get_functions() const;

  type_map _types;
  function_map _functions;

};

// Implementation of TypeInfo for user types, which has to see the definition of
// Context.
namespace internal {
  template<typename T>
  struct TypeInfo<T*> {
    yang::Type operator()(const Context& context) const
    {
      if (!context.has_type<T>()) {
        throw runtime_error("use of unregistered user type");
      }
      return yang::Type::user_t(context.get_type_name<T>());
    }

    // Without passing a context, we just construct erased user-types.
    yang::Type operator()() const
    {
      return yang::Type::user_t();
    }
  };
}

template<typename T>
void Context::register_type(const std::string& name)
{
  auto it = _types.find(name);
  if (it != _types.end()) {
    throw runtime_error("duplicate type `" + name + "` registered in context");
    return;
  }
  // Could also store a reverse-map from internal pointer type-id; probably
  // not a big deal.
  for (const auto& pair : _types) {
    if (pair.second.obj->is<T*>()) {
      throw runtime_error("duplicate types `" + name + "` and `" + pair.first +
                          "` registered in context");
      return;
    }
  }

  internal::GenericNativeType& symbol = _types[name];
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
  auto it = _functions.find(name);
  if (it != _functions.end()) {
    throw runtime_error(
        "duplicate function `" + name + "` registered in context");
    return;
  }

  internal::TypeInfo<Function<R(Args...)>> info;
  internal::GenericFunction& symbol = _functions[name];
  symbol.type = info(*this);
  symbol.ptr =
      std::unique_ptr<internal::FunctionBase>(new Function<R(Args...)>(f));
  internal::GenerateReverseTrampolineLookupTable<Function<R(Args...)>>()();
}

template<typename T>
bool Context::has_type() const
{
  for (const auto& pair : _types) {
    if (pair.second.obj->is<T*>()) {
      return true;
    }
  }
  return false;
}

template<typename T>
std::string Context::get_type_name() const
{
  for (const auto& pair : _types) {
    if (pair.second.obj->is<T*>()) {
      return pair.first;
    }
  }
  throw runtime_error("use of unregistered user type");
}

// End namepsace yang.
}

#endif
