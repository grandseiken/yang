//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_PIPELINE_H
#define YANG_INCLUDE_YANG_PIPELINE_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "error.h"
#include "type.h"
#include "type_info.h"
#include "typedefs.h"

namespace llvm {
  class ExecutionEngine;
  class Function;
  class LLVMContext;
  class Module;
}

namespace yang {

typedef std::unordered_map<std::string, Type> symbol_table;
class Context;

namespace internal {
  struct Node;

  // Data for a Program that is preserved as long as an Instance or some closure
  // structure needs it.
  struct ProgramInternals {
    std::string name;
    const Context& context;
    symbol_table functions;
    symbol_table globals;

    std::unique_ptr<llvm::LLVMContext> llvm_context;
    llvm::Module* module;
    std::unique_ptr<llvm::ExecutionEngine> engine;
  };
  // Similarly for an Instance.
  struct InstanceInternals {
    std::shared_ptr<const internal::ProgramInternals> ptr;
  };
}

class Program {
public:

  // Errors and warnings will be appended to the diagnostic_output string if
  // the pointer is non-null; otherwise, they will go to stderr.
  Program(const Context& context, const std::string& name,
          const std::string& contents, bool optimise = true,
          std::string* diagnostic_output = nullptr);
  ~Program();

  // Noncopyable.
  Program(Program&) = delete;
  Program& operator=(Program&) = delete;

  // By default, errors and warnings will be printed to standard error. By
  // passing diagnostic_output to the Program constructor, this behaviour can be
  // overridden, and something else can be done with the diagnostic information.
  //
  // For even finer-grained control, these data structures can be accessed for
  // raw error message strings and detailed information about the position of
  // errors in the source text. See error.h for more information.
  typedef std::vector<ErrorInfo> error_list;
  const error_list& get_errors() const;
  const error_list& get_warnings() const;

  const Context& get_context() const;
  const std::string& get_name() const;

  // Returns true if the contents parsed and checked successfully (i.e., if
  // get_errors().size() is zero). Otherwise, none of the following functions
  // will do anything useful.
  bool success() const;
  std::string print_ast() const;
  std::string print_ir() const;

  const symbol_table& get_functions() const;
  const symbol_table& get_globals() const;

private:

  void generate_ir(bool optimise);

  std::unique_ptr<internal::Node> _ast;
  error_list _errors;
  error_list _warnings;

  friend class Instance;
  std::shared_ptr<internal::ProgramInternals> _internals;

};

class Instance {
public:

  Instance(const Program& program);
  ~Instance();

  // Noncopyable.
  Instance(const Instance&) = delete;
  Instance& operator=(const Instance&) = delete;

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
  check_global(name, info(_internals->ptr->context), false);

  auto fp = (yang::void_fp)
      (std::intptr_t)get_native_fp("!global_get_" + name);
  return internal::call_via_trampoline<T>(fp, _global_data);
}

template<typename T>
void Instance::set_global(const std::string& name, const T& value)
{
  internal::TypeInfo<T> info;
  check_global(name, info(_internals->ptr->context), true);

  auto fp = (yang::void_fp)
      (std::intptr_t)get_native_fp("!global_set_" + name);
  internal::call_via_trampoline<void>(fp, _global_data, value);
}

template<typename T>
T Instance::get_function(const std::string& name)
{
  internal::TypeInfo<T> info;
  check_function(name, info(_internals->ptr->context));
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
