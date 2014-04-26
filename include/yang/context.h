//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_CONTEXT_H
#define YANG_INCLUDE_YANG_CONTEXT_H

#include <string>
#include <unordered_map>

#include "error.h"
#include "function.h"
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
class Instance;

namespace internal {

// Data preserved while it's needed (by Context objects or Programs that depend
// on them).
struct ContextInternals {
  struct type_def {
    type_def(const yang::Type& type);

    yang::Type type;
    // TODO: this is a little odd: constructor/destructor pairs are associated
    // with typedefs rather than native types themselves. It actually kind of
    // makes sense, since it allows multiple constructors. But it'd be redundant
    // with function overloading.
    GenericFunction constructor;
    GenericFunction destructor;
  };

  const type_def& type_lookup(const std::string& name) const;
  const GenericFunction& function_lookup(const std::string& name) const;
  const GenericFunction& member_lookup(const std::string& name) const;
  const GenericFunction& member_lookup(const yang::Type& t,
                                       const std::string& name) const;

  // Using some real heirarchical data structures for namespaces rather than
  // this still somewhat string-based mess might be a little bit nicer.
  typedef std::unordered_set<std::string> namespace_set;
  typedef std::unordered_map<std::string, type_def> type_map;
  typedef std::unordered_map<std::string, GenericFunction> function_map;
  typedef std::unordered_map<yang::Type, function_map> member_map;

  namespace_set namespaces;
  type_map types;
  function_map functions;
  member_map members;
  mutable bool immutable;
};

// End namespace internal.
}

class Context {
public:

  Context();

  // Dump the contents of another Context into a namespace in this one.
  void register_namespace(const std::string& name, const Context& context);
  // Add an Instance as a namespace. Everything compiled with this Context will
  // share the state of the single given Instance.
  void register_namespace(const std::string& name, const Instance& instance);

  // A slightly different kind of code-sharing between scripts would be to add
  // a Program as a namespace, i.e. every Instance created from that Context
  // having its own copy of the given Program's state.
  // This can currently be hacked in by either using a separate Context with its
  // own Instance namespace and recompiling every time, or plain old textual
  // includes. Direct support might be nice.

  // Add an (unmanaged) user type. Types must be registered before registering
  // functions that make use of them.
  // TODO: consider requiring the first overload of register_type to take a
  // template argument T*, and the second to take a Ref<T>; it makes common
  // mistakes less confusing.
  template<typename T>
  void register_type(const std::string& name);

  // Add a managed user type.
  template<typename T, typename... Args>
  void register_type(const std::string& name,
                     const Function<T*(Args...)>& constructor,
                     const Function<void(T*)>& destructor);

  // Add a member function to a user type. Unmanaged types must use the first
  // overload, and managed types must use the second.
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

private:

  template<typename T>
  void check_type(const std::string& name) const;
  void check_namespace(const std::string& name) const;
  void check_identifier(const std::string& ident) const;
  void copy_internals();

  template<typename R, typename... Args>
  void make_function_internal(const Function<R(Args...)>& f);

  friend class Program;
  std::shared_ptr<internal::ContextInternals> _internals;

};

template<typename T>
void Context::register_type(const std::string& name)
{
  check_type<T>(name);
  copy_internals();
  _internals->types.emplace(name, Type::user_t<T>(false));
}

template<typename T, typename... Args>
void Context::register_type(const std::string& name,
                            const Function<T*(Args...)>& constructor,
                            const Function<void(T*)>& destructor)
{
  check_type<T>(name);
  if (_internals->functions.find(name) != _internals->functions.end()) {
    throw runtime_error(
        "managed type `" + name + "` conflicts with registered function");
  }
  copy_internals();
  auto it = _internals->types.emplace(name, Type::user_t<T>(true));
  it.first->second.constructor = make_generic(constructor);
  it.first->second.destructor = make_generic(destructor);
}

template<typename T, typename R, typename... Args>
void Context::register_member_function(
    const std::string& name, const Function<R(T*, Args...)>& f)
{
  check_identifier(name);
  Type t = Type::user_t<T>(false);
  if (_internals->members[t].find(name) != _internals->members[t].end()) {
    throw runtime_error(
        "duplicate member function `" + internal::type_uidstr<T>() + "::" +
        name + "` registered in context");
  }
  copy_internals();
  _internals->members[t][name] = make_generic(f);
}

template<typename T, typename R, typename... Args>
void Context::register_member_function(
    const std::string& name, const Function<R(Ref<T>, Args...)>& f)
{
  check_identifier(name);
  Type t = Type::user_t<T>(true);
  if (_internals->members[t].find(name) != _internals->members[t].end()) {
    throw runtime_error(
        "duplicate member function `" + internal::type_uidstr<T>() + "::" +
        name + "` registered in context");
  }
  copy_internals();
  _internals->members[t][name] = make_generic(f);
}

template<typename R, typename... Args>
void Context::register_function(
    const std::string& name, const Function<R(Args...)>& f)
{
  check_identifier(name);
  auto type_it = _internals->types.find(name);
  if (type_it != _internals->types.end() &&
      type_it->second.type.is_managed_user_type()) {
    throw runtime_error(
        "function `" + name + "` conflicts with registered managed type");
  }
  auto it = _internals->functions.find(name);
  if (it != _internals->functions.end()) {
    throw runtime_error(
        "duplicate function `" + name + "` registered in context");
  }

  copy_internals();
  _internals->functions[name] = make_generic(f);
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
}

template<typename R, typename... Args>
internal::GenericFunction make_generic(const Function<R(Args...)>& f)
{
  internal::GenericFunction g;
  g.type = type_of<Function<R(Args...)>>();
  g.ptr = std::make_shared<Function<R(Args...)>>(f);
  internal::GenerateReverseTrampolineLookupTable<Function<R(Args...)>>()();
  return g;
}

// End namepsace yang.
}

#endif
