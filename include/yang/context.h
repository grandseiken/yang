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
// TODO: as alternative to textual include, allow code-sharing by way of
// treating a Program as a Context (using LLVM modules to avoid the need for
// complicated trampolining back and forth). Also, treat an Instance as a
// Context for sharing global state between programs?
// TODO: better assignment with "references". E.g. a[i] and ++i should both be
// assignable-to. Vectorised assignment? Pattern-matching assignment?
// TODO: code hot-swapping. Careful with pointer values (e.g. functions) in
// global data struct which probably need to be left as default values.
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
//
// Further off (helpful stuff that can be emulated without needing to be built-
// -in right away):
// TODO: allow namespaces directly in Yang code?
// TODO: a standard library (as a Context?), possibly including some built-in
// data structures (like a generic map<K, V> type).
// TODO: add a layer of abstraction so the LLVM backend can be swapped out
// easily (e.g. for a bytecode backend).
namespace yang {
namespace internal {

// Data preserved while it's needed (by Context objects or Programs that depend
// on them).
struct ContextInternals {
  struct user_type_info {
    internal::GenericNativeType native;
    bool is_managed;
  };
  typedef std::unordered_map<std::string, user_type_info> type_map;
  typedef std::unordered_map<
      std::string, internal::GenericFunction> function_map;
  typedef std::unordered_set<std::string> namespace_set;

  namespace_set namespaces;
  type_map types;
  function_map functions;
  mutable bool immutable;
};

// End namespace internal.
}

class Context {
public:

  Context();

  // Add the contents of another context into a namespace.
  void register_namespace(const std::string& name, const Context& context);

  // Add a user type. Types must be registered before registering functions that
  // make use of them.
  template<typename T>
  void register_type(const std::string& name);

  // Add a managed user type.
  template<typename T, typename... Args>
  void register_managed_type(const std::string& name,
                             const Function<T*(Args...)>& constructor,
                             const Function<void(T*)>& destructor);

  // Add a member function to a user type.
  template<typename T, typename R, typename... Args>
  void register_member_function(
      const std::string& name, const Function<R(T*, Args...)>& f);
  template<typename T, typename R, typename... Args>
  void register_member_function(
      const std::string& name, const Function<R(Ref<T>, Args...)>& f);

  // Add a globally-available function to the context.
  // TODO: perhaps functions should conflict with typenames and namespaces in
  // general, rather than just constructors?
  template<typename R, typename... Args>
  void register_function(
      const std::string& name, const Function<R(Args...)>& f);

  // Information about registered types.
  template<typename T>
  bool has_type() const;
  template<typename T>
  bool is_managed() const;
  template<typename T>
  std::string get_type_name() const;

private:

  template<typename T>
  void check_type(const std::string& name) const;
  void check_namespace(const std::string& name) const;
  void check_identifier(const std::string& ident) const;
  void copy_internals();

  template<typename R, typename... Args>
  void register_function_internal(
      const std::string& name, const Function<R(Args...)>& f,
      bool ignore_managed_mismatch = false);

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
    if (pair.second.native.obj->is<T*>()) {
      return true;
    }
  }
  return false;
}

template<typename T>
bool context_is_managed_impl(const ContextInternals& internals)
{
  for (const auto& pair : internals.types) {
    if (pair.second.native.obj->is<T*>()) {
      return pair.second.is_managed;
    }
  }
  return false;
}

template<typename T>
std::string context_get_type_name_impl(const ContextInternals& internals)
{
  for (const auto& pair : internals.types) {
    if (pair.second.native.obj->is<T*>()) {
      return pair.first;
    }
  }
  throw runtime_error("use of unregistered user type");
}

template<typename T>
struct TypeInfoImpl<T*> {
  yang::Type operator()(const ContextInternals& context,
                        bool ignore_managed_mismatch = false) const
  {
    if (!context_has_type_impl<T>(context)) {
      throw runtime_error("use of unregistered user type");
    }
    if (!ignore_managed_mismatch && context_is_managed_impl<T>(context)) {
      throw runtime_error("use of `" + context_get_type_name_impl<T>(context) +
                          "` as an unmanaged user type");
    }
    return yang::Type::user_t(context_get_type_name_impl<T>(context), false);
  }

  // Without passing a context, we just construct erased user-types.
  yang::Type operator()() const
  {
    return yang::Type::user_t("", false);
  }
};

template<typename T>
struct TypeInfoImpl<Ref<T>> {
  yang::Type operator()(const ContextInternals& context,
                        bool ignore_managed_mismatch = false) const
  {
    if (!context_has_type_impl<T>(context)) {
      throw runtime_error("use of unregistered user type");
    }
    if (!ignore_managed_mismatch && !context_is_managed_impl<T>(context)) {
      throw runtime_error("use of `" + context_get_type_name_impl<T>(context) +
                          "` as a managed user type");
    }
    return yang::Type::user_t(context_get_type_name_impl<T>(context), true);
  }

  // Without passing a context, we just construct erased user-types.
  yang::Type operator()() const
  {
    return yang::Type::user_t("", true);
  }
};

// End namespace internal.
}

template<typename T>
void Context::register_type(const std::string& name)
{
  check_type<T>(name);
  copy_internals();
  auto& symbol = _internals->types[name];
  symbol.native.obj = std::unique_ptr<internal::NativeType<T*>>(
      new internal::NativeType<T*>());
  symbol.is_managed = false;
}

template<typename T, typename... Args>
void Context::register_managed_type(const std::string& name,
                                    const Function<T*(Args...)>& constructor,
                                    const Function<void(T*)>& destructor)
{
  check_type<T>(name);
  if (_internals->functions.find(name) != _internals->functions.end()) {
    throw runtime_error(
        "managed type `" + name + "` conflicts with registered function");
  }
  copy_internals();
  auto& symbol = _internals->types[name];
  symbol.native.obj = std::unique_ptr<internal::NativeType<T*>>(
      new internal::NativeType<T*>());
  symbol.is_managed = true;
  // Constructor/destructor work with T* rather than Ref<T> so we need to ignore
  // the usual verification.
  register_function_internal(name + "::!", constructor, true);
  register_function_internal(name + "::~", destructor, true);
}

template<typename T, typename R, typename... Args>
void Context::register_member_function(
    const std::string& name, const Function<R(T*, Args...)>& f)
{
  if (!has_type<T>()) {
    throw runtime_error(
        "member `" + name + "` registered on unregistered type");
  }
  check_identifier(name);

  copy_internals();
  std::string type_name = get_type_name<T>();
  register_function_internal(type_name + "::" + name, f);
}

template<typename T, typename R, typename... Args>
void Context::register_member_function(
    const std::string& name, const Function<R(Ref<T>, Args...)>& f)
{
  if (!has_type<T>()) {
    throw runtime_error(
        "member `" + name + "` registered on unregistered type");
  }
  check_identifier(name);

  copy_internals();
  std::string type_name = get_type_name<T>();
  register_function_internal(type_name + "::" + name, f);
}

template<typename R, typename... Args>
void Context::register_function(
    const std::string& name, const Function<R(Args...)>& f)
{
  check_identifier(name);
  std::string cname = name + "::!";
  if (_internals->functions.find(cname) != _internals->functions.end()) {
    throw runtime_error(
        "function `" + name + "` conflicts with registered managed type");
  }

  copy_internals();
  register_function_internal(name, f);
}

template<typename T>
bool Context::has_type() const
{
  return internal::context_has_type_impl<T>(*_internals);
}

template<typename T>
bool Context::is_managed() const
{
  return internal::context_is_managed_impl<T>(*_internals);
}

template<typename T>
std::string Context::get_type_name() const
{
  return internal::context_get_type_name_impl<T>(*_internals);
}

template<typename T>
void Context::check_type(const std::string& name) const
{
  check_identifier(name);
  auto it = _internals->types.find(name);
  if (it != _internals->types.end()) {
    throw runtime_error(
        "duplicate typename `" + name + "` registered in context");
  }
  auto jt = _internals->namespaces.find(name);
  if (jt != _internals->namespaces.end()) {
    throw runtime_error("typename `" + name + "` conflicts with namespace");
  }
  // Could also store a reverse-map from internal pointer type-id; probably
  // not a big deal.
  for (const auto& pair : _internals->types) {
    if (pair.second.native.obj->is<T*>()) {
      throw runtime_error("duplicate types `" + name + "` and `" + pair.first +
                          "` registered in context");
    }
  }
}

template<typename R, typename... Args>
void Context::register_function_internal(
    const std::string& name, const Function<R(Args...)>& f,
    bool ignore_managed_mismatch)
{
  auto it = _internals->functions.find(name);
  if (it != _internals->functions.end()) {
    throw runtime_error(
        "duplicate function `" + name + "` registered in context");
  }

  // Info lookup might throw; make sure not to enter anything in the function
  // table until after that.
  yang::Type t = internal::TypeInfo<Function<R(Args...)>>()(
      *_internals, ignore_managed_mismatch);
  internal::GenericFunction& symbol = _internals->functions[name];
  symbol.type = t;
  symbol.ptr =
      std::unique_ptr<internal::FunctionBase>(new Function<R(Args...)>(f));
  internal::GenerateReverseTrampolineLookupTable<Function<R(Args...)>>()();
}

// End namepsace yang.
}

#endif
