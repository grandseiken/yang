//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_NATIVE_H
#define YANG_INCLUDE_YANG_NATIVE_H

#include <functional>
#include <memory>
#include "typedefs.h"

namespace yang {
namespace internal {

template<typename>
struct ErasedNativeFunction;
template<>
struct ErasedNativeFunction<void> {
  virtual ~ErasedNativeFunction() {}
};
template<typename R, typename... Args>
struct ErasedNativeFunction<R(Args...)> : ErasedNativeFunction<void> {
  ErasedNativeFunction(const std::function<R(Args...)>& f)
    : f(f) {}
  ~ErasedNativeFunction() override {}

  std::function<R(Args...)> f;
};

struct Prefix;
struct Vtable;
struct NativeFunctionInternals {
  ~NativeFunctionInternals() {}

  // This must match the Prefix struct declared in src/refcounting.h.
  Prefix* parent;
  int_t refcount;
  Vtable* vtable;

  // Type-erased std::function.
  std::unique_ptr<ErasedNativeFunction<void>> erased_function;
  template<typename R, typename... Args>
  const std::function<R(Args...)>& get() const
  {
    return ((ErasedNativeFunction<R(Args...)>*)erased_function.get())->f;
  }
};

}} // ::yang::internal

#endif
