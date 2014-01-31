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
  template<typename...>
  struct InstanceCheck;
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
  // be modified, that it is both exported and non-const.
  bool check_global(const std::string& name, const Type& type,
                    bool for_modification) const;
  // Similarly for functions.
  bool check_function(const std::string& name, const Type& type) const;

  const Program& _program;
  void* _global_data;

};

// Implementation of Function::operator(), which has to see the defintion of
// Instance.
template<typename R, typename... Args>
R Function<R(Args...)>::operator()(const Args&... args) const
{
  // Instance should always be non-null.
  internal::ValueConstruct<R> construct;
  if (!*this) {
    log_err(_instance->get_program().get_name(),
            ": called null function object");
    return construct(*_instance);
  }
  return _instance->call_via_trampoline<R>(_function, args...);
}

template<typename T>
T Instance::get_global(const std::string& name) const
{
  // TypeInfo will fail at compile-time for completely unsupported types; will
  // at runtime for pointers to unregistered user types.
  internal::TypeInfo<T> info;
  if (!check_global(name, info(_program.get_context()), false)) {
    return T();
  }
  return call_via_trampoline<T>("!global_get_" + name);
}

template<typename T>
void Instance::set_global(const std::string& name, const T& value)
{
  internal::TypeInfo<T> info;
  if (!check_global(name, info(_program.get_context()), true)) {
    return;
  }
  call_via_trampoline<void>("!global_set_" + name, value);
}

template<typename T>
T Instance::get_function(const std::string& name)
{
  internal::TypeInfo<T> info;
  internal::ValueConstruct<T> construct;
  T result = construct(*this);
  if (!check_function(name, info(_program.get_context()))) {
    return result;
  }
  construct.set_void_fp(result, get_native_fp(name));
  return result;
}

template<typename R, typename... Args>
R Instance::call(const std::string& name, const Args&... args)
{
  internal::TypeInfo<Function<R(Args...)>> info;
  internal::ValueConstruct<R> construct;
  if (!check_function(name, info(_program.get_context()))) {
    return construct(*this);
  }
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
  // Make sure only functions referencing this instance are passed in.
  internal::InstanceCheck<Args...> instance_check;
  internal::ValueConstruct<R> construct;
  if (!instance_check(*this, args...)) {
    return construct(const_cast<Instance&>(*this));
  }

  Type type = Function<R(Args...)>::get_type(_program.get_context());
  // Since we can only obtain a valid Function object referencing a function
  // type for which a trampoline has been generated, there should always be
  // an entry in the trampoline map (provided we're careful to erase the user
  // types, since they're all the same for the purposes of calling convention.
  auto it = _program._trampoline_map.find(type.erase_user_types());
  yang::void_fp trampoline = get_native_fp(it->second);

  typedef internal::TrampolineCall<R, void*, Args..., yang::void_fp> call_type;
  typename call_type::fp_type trampoline_expanded =
      (typename call_type::fp_type)trampoline;
  return call_type()(const_cast<Instance&>(*this),
                     trampoline_expanded, _global_data, args..., target);
}

// End namespace yang.
}

#endif
