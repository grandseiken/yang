//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_FUNCTION_H
#define YANG_SRC_FUNCTION_H

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

// End namespace internal.
}

// Opaque Yang function object.
template<typename T>
class Function {
  static_assert(sizeof(T) != sizeof(T), "use of non-function type");
};

template<typename R, typename... Args>
class Function<R(Args...)> {
public:

  // Construct a generic Yang Function object from a generic C++ function
  // object.
  typedef std::function<R(Args...)> cpp_type;
  Function(const cpp_type& cpp_function);

  // Get the type corresponding to this function type as a Yang Type object.
  static Type get_type(const Context& context);

  // Invoke the function.
  R operator()(const Args&... args) const;

private:

  // If this is a Yang function, get the program instance it references.
  // Otherwise, return a null pointer.
  Instance* get_instance() const;

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
  // can be invoked or passed to Yang code. Library code that return Functions
  // to client code must throw rather than returning something unusable.
  Function();

  // Bare variables (equivalent to the Yang representation).
  yang::void_fp _function;
  void* _env;
  yang::void_fp _target;

};

template<typename R, typename... Args>
Function<R(Args...)>::Function()
  : _function(nullptr)
  , _env(nullptr)
  , _target(nullptr)
{
}

template<typename R, typename... Args>
Type Function<R(Args...)>::get_type(const Context& context)
{
  internal::TypeInfo<Function<R(Args...)>> info;
  return info(context);
}

template<typename R, typename... Args>
Instance* Function<R(Args...)>::get_instance() const
{
  // Standard guarantees that pointer to structure points to its first member,
  // and the pointer to the program instance is always the first element of
  // the global data structure; so, we can just cast it to an instance
  // pointer.
  //
  // This will change when the environment pointer can also point to a closure
  // structure.
  if (!_env) {
    return nullptr;
  }
  return *(Instance**)_env;
}

namespace internal {

template<typename T>
struct FunctionConstruct {
  static_assert(sizeof(T) != sizeof(T), "use of non-function type");
  T operator()(yang::void_fp, void*) const
  {
    return {};
  }
};
template<typename R, typename... Args>
struct FunctionConstruct<Function<R(Args...)>> {
  Function<R(Args...)> operator()(yang::void_fp function, void* env,
                                  yang::void_fp target = nullptr) const
  {
    Function<R(Args...)> f;
    f._function = function;
    f._env = env;
    f._target = target;
    return f;
  }
};

// End namespace internal.
}

// End namespace yang.
}

#endif
