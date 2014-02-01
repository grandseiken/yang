#ifndef YANG_SRC_NATIVE_H
#define YANG_SRC_NATIVE_H

#include <functional>
#include <memory>
#include "typedefs.h"
#include "type.h"

namespace yang {
namespace internal {

// Class for dynamic storage of an arbitrary function.
template<typename T>
class NativeFunction {
  static_assert(sizeof(T) != sizeof(T),
                "incorrect native function type argument used");
};

template<>
class NativeFunction<void> {
public:

  virtual ~NativeFunction() {}
  // Convert this to a particular NativeFunction of a given type and return the
  // contained function. It is the caller's responsibility to ensure the
  // template arguments are correct; i.e. that the dynamic type of this object
  // really is a NativeFunction<R(Args...)>.
  template<typename R, typename... Args>
  const std::function<R(Args...)>& get() const;

};

// Structure containing information about an arbitrary native function to be
// made available via a Context.
struct GenericNativeFunction {
  GenericNativeFunction()
    : ptr(nullptr) {}

  yang::Type type;
  std::unique_ptr<NativeFunction<void>> ptr;
  yang::void_fp trampoline_ptr;
};

template<typename R, typename... Args>
class NativeFunction<R(Args...)> : public NativeFunction<void> {
public:

  typedef std::function<R(Args...)> function_type;
  NativeFunction(const function_type& function);
  ~NativeFunction() override {}

private:

  friend NativeFunction<void>;
  function_type _function;

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

template<typename R, typename... Args>
NativeFunction<R(Args...)>::NativeFunction(const function_type& function)
  : _function(function)
{
}

template<typename R, typename... Args>
const std::function<R(Args...)>& NativeFunction<void>::get() const
{
  return ((NativeFunction<R(Args...)>*)this)->_function;
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
