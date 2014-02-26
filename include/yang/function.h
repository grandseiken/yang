//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_FUNCTION_H
#define YANG_INCLUDE_YANG_FUNCTION_H

#include "native.h"
#include "type.h"
#include "typedefs.h"

namespace yang {

class Context;
class Instance;

namespace internal {

template<typename>
struct FunctionConstruct;
template<typename>
struct TypeInfo;

template<typename...>
struct TrampolineCallArgs;
template<typename>
struct TrampolineCallReturn;
template<typename, typename...>
struct TrampolineCall;

template<typename, typename>
struct ReverseTrampolineCallArgs;
template<typename, typename...>
struct ReverseTrampolineCallReturn;

template<typename>
struct GenerateReverseTrampolineLookupTable;

// End namespace internal.
}

// Opaque Yang function object.
template<typename T>
class Function {
  static_assert(sizeof(T) != sizeof(T), "use of non-function type");
};

// Common base class for dynamic storage.
namespace internal {

class FunctionBase {
public:

  virtual ~FunctionBase() {}

private:

  friend class IrCommon;
  virtual void get_representation(void** function, void** env) = 0;

};

// End namespace internal.
}

// Some general notes about how functions work in Yang:
//
// The yang::Function type is conceptually a superset of the std::function type.
// It can hold any std::function, and it can also hold any function retrieved
// from Yang code (including closures, member functions with bound object, etc).
//
// Similarly, function types in Yang can hold a reference to any C++
// std::function or any Yang function. This makes all functions completely
// interchangable: any invokable object from either language can be passed back
// and forth between the languages, stored, and invoked at will.
//
// To support this, the internal representation of functions in both the C++
// yang::Function type and inside Yang code consists of two pointers:
//
// - first: a pointer to the actual Yang function or C++ std::function (in fact,
//   in the latter case, the pointer is to a type-erased wrapper object that can
//   store an std::function with any template arguments, and supports automatic
//   reference counting).
// - second: to the environment. For C++ functions, this will be the null
//   pointer (and that is how we identify C++ functions). For Yang functions,
//   this will be a pointer either to the global data structure associated with
//   the program Instance, or to some node of the tree of closed environment
//   structures rooted at that Instance. (In this way, a Yang function
//   implicitly closes over the Instance it was retrieved from.) These closure
//   structures are also reference-counted.
//
// Some notes on the implementation:
//
// - the practice of allocating a structure for each lexical closure, with the
//   structures for inner closures having pointers to the parent, is typical.
//   These closure structures being a tree rooted at the structure storing the
//   global variables for the program instantiation is less common.
// - reference-counting is clearly needed for the closure data structures
//   themselves. More subtly, it's also needed for the global data structure
//   of each program instantiation (for, a function obtained from that instance
//   may be stored and invoked after the corresponding C++ Instance object has
//   been destroyed; and it's needed for the std::functions held within
//   yang::Functions: they too must be copied to the heap, since the function
//   might for example be passed to Yang code, stored, and invoked long after
//   the original std::function has gone out of scope.
// - reference-counting is a perfectly reasonable garbage-collection strategy
//   for Yang. Its stable performance characteristics are ideal for the uses
//   Yang was designed for, and it isn't possible to create cyclic structures
//   in Yang code.
// - there is also a large amount of template machinery to ensure we can
//   transparently call Yang functions from C++ and vice-versa, even though
//   they use different calling conventions.
template<typename R, typename... Args>
class Function<R(Args...)> : public internal::FunctionBase {
public:

  // Construct a generic Yang Function object from a generic C++ function
  // object.
  typedef std::function<R(Args...)> cpp_type;
  Function(const cpp_type& cpp_function);
  ~Function() override {}

  // Get the type corresponding to this function type as a Yang Type object.
  static Type get_type(const Context& context);

  // Invoke the function.
  R operator()(const Args&... args) const;

private:

  template<typename...>
  friend struct internal::TrampolineCallArgs;
  template<typename>
  friend struct internal::TrampolineCallReturn;
  template<typename, typename...>
  friend struct internal::TrampolineCall;

  template<typename, typename>
  friend struct internal::ReverseTrampolineCallArgs;
  template<typename, typename...>
  friend struct internal::ReverseTrampolineCallReturn;

  template<typename>
  friend struct internal::FunctionConstruct;

  // Invariant: Function objects returned to client code must never be null.
  // They must reference a genuine Yang function or C++ function, so that they
  // can be invoked or passed to Yang code. Library code that returns Functions
  // to client code must throw rather than returning something unusable.
  Function();

  void get_representation(void** function, void** env) override;

  // Reference-counted C++ function.
  internal::RefCountedNativeFunction<R(Args...)> _native_ref;

  // Bare variables (equivalent to the Yang representation).
  void* _function;
  void* _env;

};

namespace internal {

// Template deduction for make_fn.
template<typename T> struct RemoveClass {};
template<typename C, typename R, typename... Args>
struct RemoveClass<R(C::*)(Args...)> {
  using type = R(Args...);
};
template<typename C, typename R, typename... Args>
struct RemoveClass<R(C::*)(Args...) const> {
  using type = R(Args...);
};
template<typename C, typename R, typename... Args>
struct RemoveClass<R(C::*)(Args...) volatile> {
  using type = R(Args...);
};
template<typename C, typename R, typename... Args>
struct RemoveClass<R(C::*)(Args...) const volatile> {
  using type = R(Args...);
};

template<typename T>
struct GetSignature {
  using type = typename RemoveClass<
      decltype(&std::remove_reference<T>::type::operator())>::type;
};
template<typename R, typename... Args>
struct GetSignature<R(Args...)> {
  using type = R(Args...);
};
template<typename R, typename... Args>
struct GetSignature<R(&)(Args...)> {
  using type = R(Args...);
};
template<typename R, typename... Args>
struct GetSignature<R(*)(Args...)> {
  using type = R(Args...);
};

template<typename T>
using make_fn_type = yang::Function<typename GetSignature<T>::type>;

// Dynamic storage of an abitrary Function.
struct GenericFunction {
  GenericFunction()
    : ptr(nullptr) {}

  yang::Type type;
  std::unique_ptr<FunctionBase> ptr;
};

// Avoid including unnecessary files in this header.
yang::void_fp get_global_trampoline_function(const yang::Type& type);

// Call a Yang function via global trampolines.
template<typename R, typename... Args>
R call_via_trampoline(yang::void_fp target, void* env, const Args&... args);

// End namespace internal.
}

// Convenient template-deduction constructor function. Creates a yang::Function
// of the correct type from an unambiguous callable or lambda.
template<typename T>
internal::make_fn_type<T> make_fn(T&& t)
{
  return internal::make_fn_type<T>(std::forward<T>(t));
}

template<typename R, typename... Args>
Function<R(Args...)>::Function(const cpp_type& function)
  : _native_ref(function)
  , _function(&_native_ref.get())
  , _env(nullptr)
{
  // Make sure the reverse trampoline is generated, since the global Yang
  // trampolines will compile a reference it to immediately, even if the
  // Function is never used.
  internal::GenerateReverseTrampolineLookupTable<Function<R(Args...)>>()();
}

template<typename R, typename... Args>
R Function<R(Args...)>::operator()(const Args&... args) const
{
  // For C++ functions, just call it directly.
  if (!_env) {
    auto native = (internal::NativeFunction<void>*)_function;
    return native->get<R, Args...>()(args...);
  }

  // For yang functions, call via the trampoline machinery.
  return internal::call_via_trampoline<R>(
      (yang::void_fp)(std::intptr_t)_function, _env, args...);
}

template<typename R, typename... Args>
Function<R(Args...)>::Function()
  : _function(nullptr)
  , _env(nullptr)
{
}

template<typename R, typename... Args>
void Function<R(Args...)>::get_representation(void** function, void** env)
{
  *function = _function;
  *env = _env;
}

// End namespace yang.
}

// Due to an awkward set of template dependencies, these includes need to come
// all the way down here so that get_type and call_via_trampoline can be defined
// without recursively including function.h again.
#include "trampoline.h"

namespace yang {

template<typename R, typename... Args>
Type Function<R(Args...)>::get_type(const Context& context)
{
  return internal::TypeInfo<Function<R(Args...)>>()(context);
}

namespace internal {

template<typename T>
struct FunctionConstruct {
  static_assert(sizeof(T) != sizeof(T), "use of non-function type");
  T operator()(void*, void*) const
  {
    return {};
  }
};
template<typename R, typename... Args>
struct FunctionConstruct<Function<R(Args...)>> {
  Function<R(Args...)> operator()(void* function, void* env) const
  {
    Function<R(Args...)> f;
    f._function = function;
    f._env = env;
    return f;
  }
};

// Call a Yang function via global trampolines.
template<typename R, typename... Args>
R call_via_trampoline(yang::void_fp target, void* env, const Args&... args)
{
  yang::Type type = TypeInfo<Function<R(Args...)>>()();
  yang::void_fp trampoline = get_global_trampoline_function(type);
  // Generate the C++ side of the trampoline at compile-time.
  internal::GenerateForwardTrampolineLookupTable<Function<R(Args...)>>()();

  typedef internal::TrampolineCall<R, Args..., void*, yang::void_fp> call_type;
  auto trampoline_expanded = (typename call_type::fp_type)trampoline;
  return call_type()(trampoline_expanded, args..., env, target);
}

// End namespace yang::internal.
}
}

#endif
