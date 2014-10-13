//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_MAKE_FN_H
#define YANG_INCLUDE_YANG_MAKE_FN_H

#include <type_traits>

namespace yang {
template<typename>
class Function;

namespace internal {

// Template deduction.
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
using make_fn_type = Function<typename GetSignature<T>::type>;

} // ::internal

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

} // ::yang

#endif
