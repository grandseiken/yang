//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_INSTANCE_H
#define YANG_INCLUDE_YANG_INSTANCE_H

#include <string>
#include "function.h"
#include "type.h"
#include "type_info.h"

namespace llvm {
  class Function;
}

namespace yang {
class Program;
namespace internal {
  struct ProgramInternals;
}

class Instance {
public:

  Instance(const Program& program);

  template<typename T>
  T get_global(const std::string& name) const;
  template<typename T>
  void set_global(const std::string& name, const T& value);

  template<typename R, typename... Args>
  R call(const std::string& name, const Args&... args);
  template<typename T>
  T get_function(const std::string& name);

  // Convenience functions (equivalent to calling the same on the Program the
  // Instance was compiled from).
  const function_table& get_functions() const;
  const global_table& get_globals() const;

private:

  template<typename>
  friend class Function;
  friend class Context;

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

  internal::RefcountHook<internal::Prefix> _global_data;
  internal::ProgramInternals* _program;

};

template<typename T>
T Instance::get_global(const std::string& name) const
{
  // type_of will fail at compile-time for completely unsupported types; will
  // fail at runtime for incorrect (but plausible) types.
  check_global(name, type_of<T>(), false);
  auto fp = (void_fp)
      (std::intptr_t)get_native_fp("!global_get_" + name);
  return internal::call_via_trampoline<T>(fp, _global_data.get());
}

template<typename T>
void Instance::set_global(const std::string& name, const T& value)
{
  check_global(name, type_of<T>(), true);
  auto fp = (void_fp)
      (std::intptr_t)get_native_fp("!global_set_" + name);
  internal::call_via_trampoline<void>(fp, _global_data.get(), value);
}

template<typename T>
T Instance::get_function(const std::string& name)
{
  static_assert(internal::IsFunction<T>::value, "use of non-function type");
  check_function(name, type_of<T>());
  internal::RawFunction raw{get_native_fp(name), _global_data.get()};
  return internal::Raw<T>().from(raw);
}

template<typename R, typename... Args>
R Instance::call(const std::string& name, const Args&... args)
{
  return get_function<Function<R(Args...)>>(name)(args...);
}

// End namespace yang.
}

#endif
