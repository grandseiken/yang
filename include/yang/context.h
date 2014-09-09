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
// TODO: code hot-swapping. Careful with pointer values (e.g. functions) in
// global data struct which probably need to be left as default values.
// TODO: make sure everything is (possibly optionally?) thread-safe.
// Things that aren't thread-safe and need locking or instancing:
// - AST orphans list
// - YangTrampolineGlobals
// - possibly cpp_trampoline_lookup_map
// - static refcount deletion lists; these need to be per-something somehow
//   so that objects added temporarily don't get deallocated!
// - global data / closure data structure access. Both for concurrent invocation
//   of functions from the same Instance, and concurrent invocation of functions
//   from different Instances where one happens to hold a reference to a
//   function from the other.
// The last is the trickiest bit and should probably be optional.
// TODO: vector swizzling.
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
  struct constructor {
    // This is a little odd: constructor/destructor pairs are named separately
    // from the type concerned. It makes sense, since it allows multiple
    // constructors; function overloading would make it unnecessary.
    GenericFunction ctor;
    GenericFunction dtor;
  };

  const Type& type_lookup(const std::string& name) const;
  const constructor& constructor_lookup(const std::string& name) const;
  const GenericFunction& function_lookup(const std::string& name) const;
  const GenericFunction& member_lookup(const std::string& name) const;
  const GenericFunction& member_lookup(const Type& t,
                                       const std::string& name) const;

  // Using some real heirarchical data structures for namespaces rather than
  // this still somewhat string-based mess might be a little bit nicer.
  typedef std::unordered_set<std::string> namespace_set;
  typedef std::unordered_map<std::string, Type> type_map;
  typedef std::unordered_map<std::string, constructor> constructor_map;
  typedef std::unordered_map<std::string, GenericFunction> function_map;
  typedef std::unordered_map<Type, function_map> member_map;

  namespace_set namespaces;
  type_map types;
  constructor_map constructors;
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

  // Add a user type. Note that the template argument should be T rather than T*
  // or Ref<T>.
  // TODO: should that be changed? It's less error-prone to require explicit
  // types, but then somewhat inconsistent with the other register_* template
  // arguments.
  template<typename T>
  void register_type(const std::string& name, bool managed = false);

  // Add a managed user type constructor/destructor pair.
  template<typename T, typename... Args>
  void register_constructor(const std::string& name,
                            const Function<T*(Args...)>& constructor,
                            const Function<void(T*)>& destructor);

  // Shorthand for registering a managed type with constructor named the same.
  template<typename T, typename... Args>
  void register_type(const std::string& name,
                     const Function<T*(Args...)>& constructor,
                     const Function<void(T*)>& destructor);

  // Add a member function to a user type. Raw types must use the first
  // overload, and managed types must use the second.
  template<typename T, typename R, typename... Args>
  void register_member_function(
      const std::string& name, const Function<R(T*, Args...)>& f);
  template<typename T, typename R, typename... Args>
  void register_member_function(
      const std::string& name, const Function<R(Ref<T>, Args...)>& f);

  // Add a free function to the context.
  template<typename R, typename... Args>
  void register_function(
      const std::string& name, const Function<R(Args...)>& f);

private:

  void check_namespace(const std::string& name) const;
  void check_type(const std::string& name) const;
  void check_function(const std::string& name) const;
  void check_constructor(const std::string& name) const;
  void check_member_function(const Type& type,
                             const std::string& name) const;
  void check_identifier(const std::string& ident) const;
  void copy_internals();

  template<typename R, typename... Args>
  internal::GenericFunction make_generic(const Function<R(Args...)>& f);

  friend class Program;
  friend class Type;
  std::shared_ptr<internal::ContextInternals> _internals;

};

template<typename T>
void Context::register_type(const std::string& name, bool managed)
{
  check_type(name);
  copy_internals();
  _internals->types.emplace(
      name, managed ? Type::managed_user_t<T>() : Type::raw_user_t<T>());
}

template<typename T, typename... Args>
void Context::register_constructor(const std::string& name,
                                   const Function<T*(Args...)>& constructor,
                                   const Function<void(T*)>& destructor)
{
  check_constructor(name);
  copy_internals();
  auto it = _internals->constructors.insert(
      std::make_pair(name, internal::ContextInternals::constructor()));
  it.first->second.ctor = make_generic(constructor);
  it.first->second.dtor = make_generic(destructor);
}

template<typename T, typename... Args>
void Context::register_type(const std::string& name,
                            const Function<T*(Args...)>& constructor,
                            const Function<void(T*)>& destructor)
{
  check_type(name);
  check_constructor(name);
  register_type<T>(name, true);
  register_constructor(name, constructor, destructor);
}

template<typename T, typename R, typename... Args>
void Context::register_member_function(
    const std::string& name, const Function<R(T*, Args...)>& f)
{
  Type t = Type::raw_user_t<T>();
  check_member_function(t, name);
  copy_internals();
  _internals->members[t][name] = make_generic(f);
}

template<typename T, typename R, typename... Args>
void Context::register_member_function(
    const std::string& name, const Function<R(Ref<T>, Args...)>& f)
{
  Type t = Type::managed_user_t<T>();
  check_member_function(t, name);
  copy_internals();
  _internals->members[t][name] = make_generic(f);
}

template<typename R, typename... Args>
void Context::register_function(
    const std::string& name, const Function<R(Args...)>& f)
{
  check_function(name);
  copy_internals();
  _internals->functions[name] = make_generic(f);
}

template<typename R, typename... Args>
internal::GenericFunction Context::make_generic(const Function<R(Args...)>& f)
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
