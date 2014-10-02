//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_FUNCTION_H
#define YANG_INCLUDE_YANG_FUNCTION_H

#include "native.h"
#include "refcounting.h"
#include "trampoline.h"
#include "type.h"

namespace yang {

// Common base class for dynamic storage.
namespace internal {

class FunctionBase {
public:

  virtual ~FunctionBase() {}

private:

  friend class Builder;
  friend struct GenericFunction;
  virtual std::pair<void*, void*> get_yang_representation() const = 0;

};

// End namespace internal.
}

template<typename T>
class Function {
  static_assert(sizeof(T) != sizeof(T), "use of non-function type");
};

// The yang::Function type is a superset of the std::function type in that it
// can hold any std::function, and also any function retrieved from Yang code
// (including closures, member functions with bound object, etc).
//
// Similarly, function types in Yang can take the same set of values, and are
// thus interchangable. Invokable objects from either language can be passed
// back and forth between them, stored, and called at will.
//
// This is implemented by representing function types are a pair of pointers:
//
// (1) a pointer to the actual Yang function or C++ function (actually, a
//     type-erased wrapper in a reference-counted structure on the heap).
// (2) a pointer to the environment. The environment pointer is null if and only
//     if the function is a C++ function. For Yang functions, this points either
//     to the global data structure associated with the program Instance, or to
//     some node of the tree of closure structures rooted there.
//
// There is also a large amount of template machinery to ensure we can
// transparently call Yang functions from C++ and vice-versa, even though they
// use different calling conventions.
template<typename R, typename... Args>
class Function<R(Args...)> : public internal::FunctionBase {
public:

  // Construct a generic Yang Function object from a generic C++ function
  // object.
  typedef std::function<R(Args...)> cpp_type;
  Function(const cpp_type& cpp_function);
  ~Function() override {};

  // Invoke the function.
  R operator()(const Args&... args) const;

private:

  friend struct internal::Raw<Function>;

  // There is no null function, so library code that returns Functions to client
  // code must throw rather than returning something unusable.
  Function(void* function, void* env);
  std::pair<void*, void*> get_yang_representation() const override;

  // Either _native_ref is non-null, and _env_ref and _yang_function are null,
  // or vice-versa.
  internal::RefcountHook<internal::NativeFunctionInternals> _native_ref;
  internal::RefcountHook<internal::Prefix> _env_ref;
  void* _yang_function;

};

namespace internal {

template<typename>
struct IsFunction {
  enum {value = false};
};
template<typename R, typename... Args>
struct IsFunction<Function<R(Args...)>> {
  enum {value = true};
};

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
    : type(Type::void_t())
    , ptr(nullptr)
  {}

  Type type;
  std::shared_ptr<FunctionBase> ptr;

  bool operator==(const GenericFunction& other) const;
  bool operator!=(const GenericFunction& other) const;
};

// Avoid including unnecessary files in this header.
void_fp get_global_trampoline_function(const Type& type);

// Call a Yang function via global trampolines.
template<typename R, typename... Args>
R call_via_trampoline(void_fp target, void* env, const Args&... args)
{
  Type type = Type::erased_t(type_of<Function<R(Args...)>>());
  void_fp trampoline = get_global_trampoline_function(type);
  // Generate the C++ side of the trampoline at compile-time.
  internal::GenerateForwardTrampolineLookupTable<Function<R(Args...)>>()();

  typedef internal::TrampolineCall<R, Args..., void*, void_fp> call_type;
  auto trampoline_expanded = (typename call_type::fp_type)trampoline;
  return call_type()(trampoline_expanded, args..., env, target);
}

// End namespace internal.
}

// Convenient template-deduction constructor function. Creates a yang::Function
// of the correct type from an unambiguous callable or lambda.
template<typename T>
internal::make_fn_type<T> make_fn(T&& t)
{
  return internal::make_fn_type<T>(std::forward<T>(t));
}
// Avoid double-wrapping when the input is already a yang::Function.
template<typename R, typename... Args>
Function<R(Args...)> make_fn(const Function<R(Args...)>& f)
{
  return f;
}
template<typename R, typename... Args>
Function<R(Args...)> make_fn(Function<R(Args...)>&& f)
{
  return f;
}

template<typename R, typename... Args>
Function<R(Args...)>::Function(const cpp_type& function)
  : _env_ref(nullptr)
  , _yang_function(nullptr)
{
  _native_ref->erased_function.reset(
      new internal::ErasedFunction<R(Args...)>(function));
  // Make sure the reverse trampoline is generated, since the global Yang
  // trampolines will compile a reference it to immediately, even if the
  // Function is never used.
  internal::GenerateReverseTrampolineLookupTable<Function<R(Args...)>>()();
}

template<typename R, typename... Args>
R Function<R(Args...)>::operator()(const Args&... args) const
{
  // For C++ functions, just call it directly.
  if (!_env_ref.get()) {
    return _native_ref.get()->get<R, Args...>()(args...);
  }

  // For yang functions, call via the trampoline machinery.
  return internal::call_via_trampoline<R>(
      (void_fp)(std::intptr_t)_yang_function, _env_ref.get(), args...);
}

template<typename R, typename... Args>
Function<R(Args...)>::Function(void* function, void* env)
  : _native_ref(env ? nullptr : (internal::NativeFunctionInternals*)function)
  , _env_ref((internal::Prefix*)env)
  , _yang_function(env ? function : nullptr)
{
}

template<typename R, typename... Args>
std::pair<void*, void*> Function<R(Args...)>::get_yang_representation() const
{
  return {_env_ref.get() ? _yang_function : _native_ref.get(), _env_ref.get()};
}

// End namespace yang.
}

#endif
