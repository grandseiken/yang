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
struct ErasedFunction;
template<>
struct ErasedFunction<void> {
  virtual ~ErasedFunction() {}
};
template<typename R, typename... Args>
struct ErasedFunction<R(Args...)> : ErasedFunction<void> {
  ErasedFunction(const std::function<R(Args...)>& f)
    : f(f) {}
  ~ErasedFunction() override {}

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
  std::unique_ptr<ErasedFunction<void>> erased_function;
  template<typename R, typename... Args>
  const std::function<R(Args...)>& get() const
  {
    return ((ErasedFunction<R(Args...)>*)erased_function.get())->f;
  }
};

// End namespace yang::internal.
}
}

#endif
