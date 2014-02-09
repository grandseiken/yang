//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_IRCOMMON_H
#define YANG_SRC_IRCOMMON_H

#include <unordered_map>
#include <llvm/IR/IRBuilder.h>
#include <yang/type.h>
#include <yang/typedefs.h>

namespace llvm {
  class Constant;
  class ExecutionEngine;
  class Function;
  class Module;
  class PassManager;
  class Type;
  class Value;
}

namespace yang {
namespace internal {

class IrCommon {
public:

  IrCommon(llvm::Module& module, llvm::ExecutionEngine& engine);
  // Optimise the IR code. If function is null, does interprocedural
  // optimisation on the whole module.
  void optimise_ir(llvm::Function* function = nullptr) const;

  // Generate trampoline functions for converting between calling conventions.
  // If the trampoline for the given type doesn't already exist, a new function
  // will be generated and the insert point will need to be reset.
  llvm::Function* get_trampoline_function(const yang::Type& function_type);
  llvm::Function* get_reverse_trampoline_function(
      const yang::Type& function_type);

  typedef std::unordered_map<yang::Type, llvm::Function*> trampoline_map;
  const trampoline_map& get_trampoline_map() const;
  const trampoline_map& get_reverse_trampoline_map() const;

protected:

  const llvm::IRBuilder<>& b() const;
  /***/ llvm::IRBuilder<>& b();

  // Types.
  llvm::PointerType* void_ptr_type() const;
  llvm::Type* void_type() const;
  llvm::Type* int_type() const;
  llvm::Type* float_type() const;
  llvm::Type* vector_type(llvm::Type* type, std::size_t n) const;

  // Constants.
  llvm::Constant* constant_int(yang::int_t value) const;
  llvm::Constant* constant_float(yang::float_t value) const;
  llvm::Constant* constant_vector(
      const std::vector<llvm::Constant*>& values) const;
  llvm::Constant* constant_vector(llvm::Constant* value, std::size_t n) const;
  llvm::Value* constant_ptr(void* ptr);

  // Function types and values.
  llvm::Type* generic_function_type(llvm::Type* function_type) const;
  llvm::Type* generic_function_type(
      llvm::Type* return_type, const std::vector<llvm::Type*>& arg_types) const;
  llvm::FunctionType* function_type_from_generic(
      llvm::Type* generic_function_type) const;
  llvm::Value* generic_function_value(
      llvm::Value* function_ptr, llvm::Value* env_ptr,
      llvm::Value* target_ptr = nullptr);
  llvm::Value* generic_function_value(const GenericFunction& function);
  // Return a function type with extra parameter for the target function when
  // calling a trampoline.
  llvm::FunctionType* get_function_type_with_target(
      llvm::Type* function_type) const;

  // Convert back and forth between equivalent Yang and LLVM types.
  llvm::Type* get_llvm_type(const yang::Type& t) const;
  yang::Type get_yang_type(llvm::Type* t) const;

  // Get an LLVM function pointer to a native function.
  llvm::Function* get_native_function(
      const std::string& name, yang::void_fp native_fp,
      llvm::FunctionType* type) const;

private:

  // Get the trampoline type used either way.
  llvm::FunctionType* get_trampoline_type(
      llvm::FunctionType* function_type, bool reverse) const;
  std::size_t get_trampoline_num_return_args(llvm::Type* return_type) const;

  llvm::Module& _module;
  llvm::ExecutionEngine& _engine;
  llvm::IRBuilder<> _builder;

  // Generated trampolines (map from type of function to corresponding
  // trampoline function).
  trampoline_map _trampoline_map;
  trampoline_map _reverse_trampoline_map;

};

// Each program module generates its own reverse trampolines so it can inline
// them easily. We also need a global set of reverse trampolines so that
// Yang Function objects can be constructed from C++ functions with an
// appropriate trampoline.
class YangTrampolineGlobals {
public:

  static yang::void_fp get_trampoline_function(const yang::Type& function_type);
  static yang::void_fp get_reverse_trampoline_function(
      const yang::Type& function_type);

private:

  YangTrampolineGlobals();
  ~YangTrampolineGlobals();

  static YangTrampolineGlobals& get_instance();
  llvm::Module* create_module() const;

  std::string _error;
  llvm::Module* _module;
  std::unique_ptr<llvm::ExecutionEngine> _engine;
  IrCommon _common;

};

// End namespace yang::internal.
}
}

#endif
