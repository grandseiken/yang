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

/** #sumline ## */
namespace yang {
class Program;
namespace internal {
  struct ProgramInternals;
}

/** #class */
class Instance {
/** #sumline */
public:

  /** #member ## */
  Instance(const Program& program);

  /** #member ## */
  template<typename T>
  T get_global(const std::string& name) const;
  /** #member ## */
  template<typename T>
  void set_global(const std::string& name, const T& value);

  /**
   * #member ##
   *
   *   This is convenient shorthand for a call to ``get_function`` followed by
   *   an invocation of the resulting ``yang::Function`` object with some
   *   arguments. The template argument ``R``, which must be supplied,
   *   corresponds to the result type of the function called ``name``;
   *   ``Args...`` corresponds to its argument types.
   *
   *   For example, the following two approaches are equivalent::
   *
   *     // Using a yang::Function object.
   *     typedef yang::Function<yang::int_t(yang::int_t)> function_t;
   *     function_t foo = inst.get_function<function_t>("foo");
   *     yang::int_t result_a = foo(42);
   *
   *     // Using the "call" method.
   *     yang::int_t result_b = inst.call<yang::int_t>("foo", yang::int_t{42});
   */
  template<typename R, typename... Args>
  R call(const std::string& name, const Args&... args);
  /** #member ## */
  template<typename T>
  T get_function(const std::string& name);

  /**
   * #member
   *
   *   This convenience function is equivalent to calling ``get_functions()``
   *   on the ``Program`` this ``Instance`` was compiled from.
   */
  const function_table& get_functions() const;
  /**
   * #member
   *
   *   This convenience function is equivalent to calling ``get_globals()``
   *   on the ``Program`` this ``Instance`` was compiled from.
   */
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

/** #sumline ## */
};

template<typename T>
T Instance::get_global(const std::string& name) const
{
  // type_of will fail at compile-time for completely unsupported types; will
  // fail at runtime for incorrect (but plausible) types.
  check_global(name, type_of<T>(), false);
  auto fp = (internal::void_fp)
      (std::intptr_t)get_native_fp("!global_get_" + name);
  return internal::call_via_trampoline<T>(fp, _global_data.get());
}

template<typename T>
void Instance::set_global(const std::string& name, const T& value)
{
  check_global(name, type_of<T>(), true);
  auto fp = (internal::void_fp)
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
/** #sumline */
}

#endif
