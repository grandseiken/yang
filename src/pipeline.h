//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_PIPELINE_H
#define YANG_SRC_PIPELINE_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "log.h"
#include "type.h"
#include "type_info.h"
#include "typedefs.h"

namespace llvm {
  class ExecutionEngine;
  class Function;
  class Module;
  class Type;
}

namespace yang {

class Context;
namespace internal {
  struct Node;
}

class Program {
public:

  Program(const Context& context, const std::string& name,
          const std::string& contents, bool optimise = true);
  ~Program();

  // Noncopyable.
  Program(Program&) = delete;
  Program& operator=(Program&) = delete;

  const Context& get_context() const;
  const std::string& get_name() const;

  // Returns true if the contents parsed and checked successfully. Otherwise,
  // none of the following functions will do anything useful.
  bool success() const;
  std::string print_ast() const;
  std::string print_ir() const;

  typedef std::unordered_map<std::string, Type> symbol_table;
  const symbol_table& get_functions() const;
  const symbol_table& get_globals() const;

private:

  friend class Instance;
  void generate_ir();
  void optimise_ir();

  const Context& _context;
  std::string _name;
  std::unique_ptr<internal::Node> _ast;

  symbol_table _functions;
  symbol_table _globals;

  llvm::Module* _module;
  std::unique_ptr<llvm::ExecutionEngine> _engine;
  std::unordered_map<Type, llvm::Function*> _trampoline_map;

};

class Instance {
public:

  Instance(const Program& program);
  ~Instance();

  // Noncopyable.
  Instance(const Instance&) = delete;
  Instance& operator=(const Instance&) = delete;

  const Program& get_program() const;

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

  yang::void_fp get_native_fp(const std::string& name) const;
  yang::void_fp get_native_fp(llvm::Function* ir_fp) const;

  template<typename R, typename... Args>
  R call_via_trampoline(const std::string& name, const Args&... args) const;
  template<typename R, typename... Args>
  R call_via_trampoline(yang::void_fp target, const Args&... args) const;

  // Runtime check that global exists and has the correct type and, if it is to
  // be modified, that it is both exported and non-const. Otherwise, throws.
  // Makes sure that we never return allow an invalid object (e.g. a null
  // function) to leak out into client code.
  void check_global(const std::string& name, const Type& type,
                    bool for_modification) const;
  // Similarly for functions.
  void check_function(const std::string& name, const Type& type) const;

  const Program& _program;
  void* _global_data;

};

// Implementation of Function::operator(), which has to see the defintion of
// Instance.
template<typename R, typename... Args>
R Function<R(Args...)>::operator()(const Args&... args) const
{
  // TODO: not clear how to call non-Yang functions (with no instance) yet.
  Instance* instance = get_instance();
  return instance->call_via_trampoline<R>(_function, args...);
}

template<typename T>
T Instance::get_global(const std::string& name) const
{
  // TypeInfo will fail at compile-time for completely unsupported types; will
  // at runtime for pointers to unregistered user types.
  internal::TypeInfo<T> info;
  check_global(name, info(_program.get_context()), false);
  return call_via_trampoline<T>("!global_get_" + name);
}

template<typename T>
void Instance::set_global(const std::string& name, const T& value)
{
  internal::TypeInfo<T> info;
  check_global(name, info(_program.get_context()), true);
  call_via_trampoline<void>("!global_set_" + name, value);
}

template<typename T>
T Instance::get_function(const std::string& name)
{
  internal::TypeInfo<T> info;
  check_function(name, info(_program.get_context()));
  internal::FunctionConstruct<T> construct;
  return construct(get_native_fp(name), _global_data);
}

template<typename R, typename... Args>
R Instance::call(const std::string& name, const Args&... args)
{
  internal::TypeInfo<Function<R(Args...)>> info;
  check_function(name, info(_program.get_context()));
  return call_via_trampoline<R>(name, args...);
}

template<typename R, typename... Args>
R Instance::call_via_trampoline(
    const std::string& name, const Args&... args) const
{
  return call_via_trampoline<R>(get_native_fp(name), args...);
}

template<typename R, typename... Args>
R Instance::call_via_trampoline(yang::void_fp target, const Args&... args) const
{
  Type type = Function<R(Args...)>::get_type(_program.get_context());
  // Since we can only obtain a valid Function object referencing a function
  // type for which a trampoline has been generated, there should always be
  // an entry in the trampoline map (provided we're careful to erase the user
  // types, since they're all the same for the purposes of calling convention.
  auto it = _program._trampoline_map.find(type.erase_user_types());
  yang::void_fp trampoline = get_native_fp(it->second);
  internal::GenerateForwardTrampolineLookupTable<Function<R(Args...)>>()();

  typedef internal::TrampolineCall<R, Args..., void*, yang::void_fp> call_type;
  auto trampoline_expanded = (typename call_type::fp_type)trampoline;
  return call_type()(trampoline_expanded, args..., _global_data, target);
}

// End namespace yang.
}

#endif
