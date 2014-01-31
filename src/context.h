#ifndef YANG_SRC_CONTEXT_H
#define YANG_SRC_CONTEXT_H

#include <functional>
#include <string>
#include <unordered_map>

#include "log.h"
#include "native.h"
#include "type.h"
#include "type_info.h"

// General directions:
// TODO: Context composition; the standard libraries should be kinds of Context;
// and user types should be possible. As alternative to textual include, allow
// code-sharing by way of treating a Program as a Context (using LLVM modules to
// avoid the need for complicated trampolining back and forth).
// TODO: there should be a type corresponding to Instance, with built-in
// functionality. Possible some sort of duck-typing interface for Instances. The
// same typing mechanism should apply to user types.
// TODO: add some kind of built-in data structures, including at least a generic
// map<K, V> type. May require garbage-collection, unless we place tight
// restrictions on their usage (e.g. only global variables).
// TODO: add a LuaValue-like generic value class.
// TODO: possibly implement closures, if it seems feasible.
// TODO: vectorised assignment, or pattern-matching assignment? Also, indexed
// assignment.
// TODO: warnings: for example, unused variables.
// TODO: many calls to log_err should probably actually be thrown exceptions.
// TODO: code hot-swapping. Careful with pointer values (e.g. functions) in
// global data struct which probably need to be left as default values.
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

  template<typename R, typename... Args>
  void register_function_raw(
      function_map& map,
      const std::string& name, const std::function<R(Args...)>& f) const;

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
        log_err("using unregistered user type");
        return {};
      }
      yang::Type t;
      t._base = yang::Type::USER_TYPE;
      t._user_type_name = context.get_type_name<T>();
      return t;
    }
  };
}

template<typename T>
void Context::register_type(const std::string& name)
{
  auto it = _types.find(name);
  if (it != _types.end()) {
    log_err("duplicate type `", name, "` registered in context");
    return;
  }
  // Could also store a reverse-map from internal pointer type-id; probably
  // not a big deal.
  for (const auto& pair : _types) {
    if (pair.second.obj->is<T*>()) {
      log_err("duplicate types `", name, "` and `", pair.first,
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
    log_err("member `", name, "` registered on unregistered type");
    return;
  }
  std::string type_name = get_type_name<T>();
  internal::GenericNativeType& type = _types[type_name];

  auto it = type.members.find(name);
  if (it != type.members.end()) {
    log_err("duplicate member `", name,
            "` registered on type `", type_name, "`");
  }
  register_function_raw(type.members, name, f);
}

template<typename R, typename... Args>
void Context::register_function(
    const std::string& name, const std::function<R(Args...)>& f)
{
  auto it = _functions.find(name);
  if (it != _functions.end()) {
    log_err("duplicate function `", name, "` registered in context");
    return;
  }
  register_function_raw(_functions, name, f);
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

template<typename R, typename... Args>
void Context::register_function_raw(
    function_map& map,
    const std::string& name, const std::function<R(Args...)>& f) const
{
  internal::TypeInfo<Function<R(Args...)>> info;
  internal::GenericNativeFunction& symbol = map[name];
  symbol.type = info(*this);
  symbol.ptr = std::unique_ptr<internal::NativeFunction<R(Args...)>>(
      new internal::NativeFunction<R(Args...)>(f));

  symbol.trampoline_ptr = (yang::void_fp)&internal::ReverseTrampolineCall<
      R(Args...),
      typename internal::TrampolineReturn<R>::type,
      typename internal::TrampolineArgs<Args...>::type>::call;
}

// End namepsace yang.
}

#endif
