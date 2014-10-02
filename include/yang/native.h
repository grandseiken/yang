//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_NATIVE_H
#define YANG_INCLUDE_YANG_NATIVE_H

#include <functional>
#include <memory>

#include "refcounting.h"
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

// TODO: try to extract the refcounting logic from Program / Instance /
// NativeFunction into an RAII object with copy/move operators, etc?
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
NativeFunctionInternals* allocate_native_function_internals();

// Hooks a native function into the reference-counting runtime.
template<typename>
class NativeFunction;

template<typename R, typename... Args>
class NativeFunction<R(Args...)> {
public:

  NativeFunction();
  NativeFunction(const std::function<R(Args...)>& function);
  ~NativeFunction();

  NativeFunction(const NativeFunction& function);
  NativeFunction& operator=(const NativeFunction& function);
  NativeFunctionInternals* get();

private:

  NativeFunctionInternals* _chunk;

};

template<typename R, typename... Args>
NativeFunction<R(Args...)>::NativeFunction()
  : _chunk(nullptr)
{
}

template<typename R, typename... Args>
NativeFunction<R(Args...)>::NativeFunction(
    const std::function<R(Args...)>& function)
  : _chunk(nullptr)
{
  cleanup_structures();
  _chunk = allocate_native_function_internals();
  _chunk->erased_function.reset(new ErasedFunction<R(Args...)>{function});
  update_structure_refcount((Prefix*)_chunk, 1);
}

template<typename R, typename... Args>
NativeFunction<R(Args...)>::~NativeFunction()
{
  if (_chunk) {
    update_structure_refcount((Prefix*)_chunk, -1);
  }
}

template<typename R, typename... Args>
NativeFunction<R(Args...)>::NativeFunction(const NativeFunction& function)
  : _chunk(function._chunk)
{
  if (_chunk) {
    update_structure_refcount((Prefix*)_chunk, 1);
  }
}

template<typename R, typename... Args>
auto NativeFunction<R(Args...)>::operator=(
    const NativeFunction& function) -> NativeFunction&
{
  if (this == &function) {
    return *this;
  }
  if (_chunk) {
    update_structure_refcount((Prefix*)_chunk, -1);
  }
  if (_chunk = function._chunk) {
    update_structure_refcount((Prefix*)_chunk, 1);
  }
  return *this;
}

template<typename R, typename... Args>
NativeFunctionInternals* NativeFunction<R(Args...)>::get()
{
  return _chunk;
}

// End namespace yang::internal.
}
}

#endif
