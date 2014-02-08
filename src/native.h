//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_NATIVE_H
#define YANG_SRC_NATIVE_H

#include <functional>
#include <memory>
#include <type_traits>
#include "typedefs.h"
#include "type.h"

namespace yang {
namespace internal {

template<typename T>
class NativeFunction {
  static_assert(sizeof(T) != sizeof(T),
                "incorrect native function type argument used");
};

// Class for dynamic storage of an arbitrary C++ function. Includes a reference-
// -counting mechanism, as Yang code may hold references to these objects for
// arbitrary lengths of time.
template<>
class NativeFunction<void> {
public:

  inline NativeFunction();
  virtual ~NativeFunction() {}
  // Convert this to a particular NativeFunction of a given type and return the
  // contained function. It is the caller's responsibility to ensure the
  // template arguments are correct; i.e. that the dynamic type of this object
  // really is a NativeFunction<R(Args...)>.
  template<typename R, typename... Args>
  const std::function<R(Args...)>& get() const;

  // Increment the reference count. The count starts at zero; this function
  // should immediately after construction if reference-counting is to be used.
  // The object must have been allocated with new!
  inline NativeFunction* take_reference();
  // Decrement the reference count and return pointer to this object. If it is
  // then zero, deletes itself and returns null.
  inline NativeFunction* release_reference();

private:

  std::size_t _reference_count;

};

template<typename R, typename... Args>
class NativeFunction<R(Args...)> : public NativeFunction<void> {
public:

  typedef std::function<R(Args...)> function_type;
  NativeFunction(const function_type& function);
  ~NativeFunction() override {}
  const function_type& get_function() const;

  // Helpful overrides with the correct return type.
  NativeFunction* take_reference();
  NativeFunction* release_reference();

private:

  function_type _function;

};

// Automatic reference-counting for a NativeFunction.
template<typename T>
class RefCountedNativeFunction {
  static_assert(sizeof(T) != sizeof(T),
                "incorrect native function type argument used");
};

template<typename R, typename... Args>
class RefCountedNativeFunction<R(Args...)> {
public:

  RefCountedNativeFunction();
  RefCountedNativeFunction(const std::function<R(Args...)>& function);
  ~RefCountedNativeFunction();

  RefCountedNativeFunction(const RefCountedNativeFunction& function);
  RefCountedNativeFunction& operator=(const RefCountedNativeFunction& function);

  RefCountedNativeFunction(RefCountedNativeFunction&& function);
  RefCountedNativeFunction& operator=(RefCountedNativeFunction&& function);

  explicit operator bool() const;
  typedef NativeFunction<R(Args...)> internal;
  const internal& get() const;
  /***/ internal& get();

private:

  internal* _internal;

};

// Class for dynamic storage of an aribtrary user type.
template<typename T>
class NativeType {
  static_assert(sizeof(T) != sizeof(T),
                "incorrect native type argument used");
};

template<>
class NativeType<void> {
public:

  virtual ~NativeType() {}
  // Check whether this is a NativeType instantiated on a particular type.
  template<typename T>
  bool is() const;

protected:

  virtual const void** id() const = 0;

};

struct GenericNativeType {
  GenericNativeType()
    : obj(nullptr) {}

  std::unique_ptr<NativeType<void>> obj;
};

template<typename T>
class NativeType<T*> : public NativeType<void> {
public:

  ~NativeType() override {};

protected:

  virtual const void** id() const;

private:

  // Guaranteed to have a different address per template instantiation.
  static const void* _id;

};

NativeFunction<void>::NativeFunction()
  : _reference_count(0)
{
}

template<typename R, typename... Args>
const std::function<R(Args...)>& NativeFunction<void>::get() const
{
  return ((NativeFunction<R(Args...)>*)this)->get_function();
}

NativeFunction<void>* NativeFunction<void>::take_reference()
{
  ++_reference_count;
  return this;
}

NativeFunction<void>* NativeFunction<void>::release_reference()
{
  if (--_reference_count) {
    return this;
  }
  delete this;
  return nullptr;
}

template<typename R, typename... Args>
NativeFunction<R(Args...)>::NativeFunction(const function_type& function)
  : _function(function)
{
}

template<typename R, typename... Args>
auto NativeFunction<R(Args...)>::get_function() const -> const function_type&
{
  return _function;
}

template<typename R, typename... Args>
auto NativeFunction<R(Args...)>::take_reference() -> NativeFunction*
{
  return (NativeFunction*)NativeFunction<void>::take_reference();
}

template<typename R, typename... Args>
auto NativeFunction<R(Args...)>::release_reference() -> NativeFunction*
{
  return (NativeFunction*)NativeFunction<void>::release_reference();
}

template<typename R, typename... Args>
RefCountedNativeFunction<R(Args...)>::RefCountedNativeFunction()
  : _internal(nullptr)
{
}

template<typename R, typename... Args>
RefCountedNativeFunction<R(Args...)>::RefCountedNativeFunction(
    const std::function<R(Args...)>& function)
  : _internal((new internal(function))->take_reference())
{
}

template<typename R, typename... Args>
RefCountedNativeFunction<R(Args...)>::~RefCountedNativeFunction()
{
  if (_internal) {
    _internal->release_reference();
  }
}

template<typename R, typename... Args>
RefCountedNativeFunction<R(Args...)>::RefCountedNativeFunction(
    const RefCountedNativeFunction& function)
  : _internal(
      function._internal ? function._internal->take_reference() : nullptr)
{
}

template<typename R, typename... Args>
auto RefCountedNativeFunction<R(Args...)>::operator=(
    const RefCountedNativeFunction& function) -> RefCountedNativeFunction&
{
  if (this == &function) {
    return *this;
  }
  if (_internal) {
    _internal->release_reference();
  }
  _internal = function._internal ?
      function._internal->take_reference() : nullptr;
  return *this;
}

template<typename R, typename... Args>
RefCountedNativeFunction<R(Args...)>::RefCountedNativeFunction(
    RefCountedNativeFunction&& function)
  : _internal(nullptr)
{
  std::swap(_internal, function._internal);
}

template<typename R, typename... Args>
auto RefCountedNativeFunction<R(Args...)>::operator=(
    RefCountedNativeFunction&& function) -> RefCountedNativeFunction&
{
  std::swap(_internal, function._internal);
  return *this;
}

template<typename R, typename... Args>
RefCountedNativeFunction<R(Args...)>::operator bool() const
{
  return _internal;
}

template<typename R, typename... Args>
auto RefCountedNativeFunction<R(Args...)>::get() const -> const internal&
{
  return *_internal;
}

template<typename R, typename... Args>
auto RefCountedNativeFunction<R(Args...)>::get() -> internal&
{
  return *_internal;
}

template<typename T>
bool NativeType<void>::is() const
{
  NativeType<T> t;
  return id() == ((NativeType<void>*)&t)->id();
}

template<typename T>
const void** NativeType<T*>::id() const
{
  return &_id;
}

template<typename T>
const void* NativeType<T*>::_id = nullptr;

// End namespace yang::internal.
}
}

#endif
