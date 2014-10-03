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
namespace internal {

struct ErasedFunction {
  ErasedFunction();
  bool operator==(const ErasedFunction& other) const;
  bool operator!=(const ErasedFunction& other) const;

  Type type;
  // Either _native_ref is non-null, and _env_ref and _yang_function are null,
  // or vice-versa.
  internal::RefcountHook<internal::NativeFunctionInternals> native_ref;
  internal::RefcountHook<internal::Prefix> env_ref;
  void* yang_function;
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
// On the C++ side, we store the Yang and C++ parts of (1) separately since
// only the C++ part should be refcounted.
//
// There is also a large amount of template machinery to ensure we can
// transparently call Yang functions from C++ and vice-versa, even though they
// use different calling conventions.
template<typename R, typename... Args>
class Function<R(Args...)> {
public:

  // Construct a generic Yang Function object from a generic C++ function
  // object.
  typedef std::function<R(Args...)> cpp_type;
  Function(const cpp_type& cpp_function);

  // Invoke the function.
  R operator()(const Args&... args) const;

private:

  friend struct internal::Raw<Function>;
  friend class Context;
  // There is no null function, so library code that returns Functions to client
  // code must throw rather than returning something unusable.
  Function(const internal::RawFunction& raw);
  internal::RawFunction get_raw_representation() const;
  const internal::ErasedFunction& get_erased_representation() const;
  internal::ErasedFunction _data;

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
{
  _data.type = type_of<Function<R(Args...)>>();
  _data.native_ref =
      internal::RefcountHook<internal::NativeFunctionInternals>();
  _data.native_ref->erased_function.reset(
      new internal::ErasedNativeFunction<R(Args...)>(function));
  // Make sure the reverse trampoline is generated, since the global Yang
  // trampolines will compile a reference it to immediately, even if the
  // Function is never used.
  internal::GenerateReverseTrampolineLookupTable<Function<R(Args...)>>()();
}

template<typename R, typename... Args>
R Function<R(Args...)>::operator()(const Args&... args) const
{
  // For C++ functions, just call it directly.
  if (!_data.env_ref.get()) {
    return _data.native_ref.get()->get<R, Args...>()(args...);
  }

  // For yang functions, call via the trampoline machinery.
  return internal::call_via_trampoline<R>(
      (void_fp)(std::intptr_t)_data.yang_function,
      _data.env_ref.get(), args...);
}

template<typename R, typename... Args>
Function<R(Args...)>::Function(const internal::RawFunction& raw)
{
  _data.type = type_of<Function<R(Args...)>>();
  _data.native_ref = raw.environment_ptr ? nullptr :
      (internal::NativeFunctionInternals*)raw.function_ptr;
  _data.env_ref = raw.environment_ptr;
  _data.yang_function = raw.environment_ptr ? raw.function_ptr : nullptr;
}

template<typename R, typename... Args>
internal::RawFunction Function<R(Args...)>::get_raw_representation() const
{
  return internal::RawFunction{
      _data.env_ref.get() ? _data.yang_function : _data.native_ref.get(),
      _data.env_ref.get()};
}

template<typename R, typename... Args>
const internal::ErasedFunction& Function<R(Args...)>::get_erased_representation() const
{
  internal::GenerateReverseTrampolineLookupTable<Function<R(Args...)>>()();
  return _data;
}

// End namespace yang.
}

#endif
