//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_INSTANCE_H
#define YANG_INCLUDE_YANG_INSTANCE_H

#include <string>

#include "internals.h"
#include "type.h"
#include "type_info.h"
#include "typedefs.h"

namespace llvm {
  class Function;
}

namespace yang {
class Program;

class Instance {
public:

  Instance(const Program& program);
  ~Instance();

  // Since Instance is just a handle, copying the object doesn't copy the
  // global variables and so on, it's still the same Instance. Hopefully that
  // isn't too confusing.
  Instance(const Instance&);
  Instance& operator=(const Instance&);

  template<typename T>
  T get_global(const std::string& name) const;
  template<typename T>
  void set_global(const std::string& name, const T& value);

  template<typename R, typename... Args>
  R call(const std::string& name, const Args&... args);
  template<typename T>
  T get_function(const std::string& name);

private:

  template<typename>
  friend class Function;

  void* get_native_fp(const std::string& name) const;
  void* get_native_fp(llvm::Function* ir_fp) const;

  // Runtime check that global exists and has the correct type and, if it is to
  // be modified, that it is both exported and non-const. Otherwise, throws.
  // Makes sure that we never return allow an invalid object (e.g. a null
  // function) to leak out into client code.
  void check_global(const std::string& name, const Type& type,
                    bool for_modification) const;
  // Similarly for functions.
  void check_function(const std::string& name, const Type& type) const;

  internal::InstanceInternals* _internals;
  void* _global_data;

};

template<typename T>
T Instance::get_global(const std::string& name) const
{
  // TypeInfo will fail at compile-time for completely unsupported types; will
  // at runtime for pointers to unregistered user types.
  internal::TypeInfo<T> info;
  check_global(name, info(*_internals->program->context), false);

  auto fp = (yang::void_fp)
      (std::intptr_t)get_native_fp("!global_get_" + name);
  return internal::call_via_trampoline<T>(fp, _global_data);
}

template<typename T>
void Instance::set_global(const std::string& name, const T& value)
{
  internal::TypeInfo<T> info;
  check_global(name, info(*_internals->program->context), true);

  auto fp = (yang::void_fp)
      (std::intptr_t)get_native_fp("!global_set_" + name);
  internal::call_via_trampoline<void>(fp, _global_data, value);
}

template<typename T>
T Instance::get_function(const std::string& name)
{
  internal::TypeInfo<T> info;
  check_function(name, info(*_internals->program->context));
  internal::FunctionConstruct<T> construct;
  return construct(get_native_fp(name), _global_data);
}

template<typename R, typename... Args>
R Instance::call(const std::string& name, const Args&... args)
{
  return get_function<Function<R(Args...)>>(name)(args...);
}

// End namespace yang.
}

#endif
