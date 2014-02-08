//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_CONTEXT_H
#define YANG_SRC_CONTEXT_H

#include <functional>
#include <string>
#include <unordered_map>

#include "error.h"
#include "native.h"
#include "type.h"
#include "type_info.h"

// Big features:
// TODO: closures and generally more useful functions. To make this work,
// function values will need to be represented in IR as (vectors of) three
// pointers:
//
// - first: to the actual C++ or Yang compiled function.
// - second: to the reverse trampoline (for C++ functions); for Yang functions,
//   it will be null.
// - third: to a structure containing the closed-over environment. For C++
//   functions, this will be null.
//
// Any variables in a function referenced by an inner function will be allocated
// in a new structure instead of on the stack. Inner function-expressions (ones
// that reference an enclosing variable, anyway) will then create a value with
// the third pointer pointing to that structure.
//
// Nested closures will need to store a pointer to the parent closure in the
// structure. For example, consider:
//
// export f = int()()()
// {
//   var v = 0;
//   return int()()
//   {
//     ++v;
//     var u = 0;
//     return int()
//     {
//       ++v;
//       ++u;
//       return v + u;
//     };
//   };
// };
//
// Each call f() creates a closure with a slot for v. However, if we take a
// single invocation g = f(), then each call g() creates a closure with a slot
// for u and a pointer to the same closure containing v.
//
// This will almost certainly necessitate some kind of garbage-collection or
// reference-counting; particular care must be taken if the values can be passed
// or returned to C++ and stored.
//
// There are many advantages over the current setup. It essentially unifies all
// the various kinds of function so that there aren't any special rules:
//
// - Rather than std::functions being part of the context only, they'll be able
//   to be passed in to any Yang function.
// - User-type member functions can be implemented in a much more simple and
//   useful way; member access creates an implicit inner function which closes
//   over the value on the left. These can then be invoked or passed around like
//   any other value.
// - C++ yang::Function objects, rather than explicitly storing a pointer to the
//   program instance, can be implemented as closures over the global data
//   pointer. This means Yang functions can be passed to *other scripts* while
//   still targeting the script they were retrieved from.
//
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
// TODO: vectorised assignment, or pattern-matching assignment? Also, indexed
// assignment.
// TODO: warnings: for example, unused variables.
// TODO: code hot-swapping. Careful with pointer values (e.g. functions) in
// global data struct which probably need to be left as default values.
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

  // TODO: make this class take yang::Functions instead of std::functions.
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
      const std::string& name, const std::function<R(T*, Args...)>& f);

  // Add a globally-available function to the context.
  template<typename R, typename... Args>
  void register_function(
      const std::string& name, const std::function<R(Args...)>& f);

  // Information about registered types.
  template<typename T>
  bool has_type() const;
  template<typename T>
  std::string get_type_name() const;

private:

  typedef std::unordered_map<std::string, internal::GenericNativeType> type_map;
  typedef std::unordered_map<
      std::string, internal::GenericNativeFunction> function_map;

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
        throw runtime_error("using unregistered user type");
      }
      yang::Type t;
      t._base = yang::Type::USER_TYPE;
      t._user_type_name = context.get_type_name<T>();
      return t;
    }

    // Without passing a context, we just construct erased user-types.
    yang::Type operator()() const
    {
      yang::Type t;
      t._base = yang::Type::USER_TYPE;
      return t;
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
    const std::string& name, const std::function<R(T*, Args...)>& f)
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
    const std::string& name, const std::function<R(Args...)>& f)
{
  auto it = _functions.find(name);
  if (it != _functions.end()) {
    throw runtime_error(
        "duplicate function `" + name + "` registered in context");
    return;
  }

  internal::TypeInfo<Function<R(Args...)>> info;
  internal::GenericNativeFunction& symbol = _functions[name];
  symbol.type = info(*this);
  symbol.ptr = std::unique_ptr<internal::NativeFunction<R(Args...)>>(
      new internal::NativeFunction<R(Args...)>(f));
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
  return "";
}

// End namepsace yang.
}

#endif
