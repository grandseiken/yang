//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_IRCOMMON_H
#define YANG_SRC_IRCOMMON_H

#include <unordered_map>
#include "irval.h"

namespace llvm {
  class Constant;
  class PassManager;
}

namespace yang {
namespace internal {

// This class contains all the codegen functions necessary to generate Yang-side
// trampolines.
class IrCommon {
public:

  IrCommon(llvm::Module& module, llvm::ExecutionEngine& engine,
           StaticData& static_data);
  // Optimise the IR code. If function is null, does interprocedural
  // optimisation on the whole module.
  void optimise_ir(llvm::Function* function = nullptr) const;

  // Generate trampoline functions for converting between calling conventions.
  // If the trampoline for the given type doesn't already exist, a new function
  // will be generated and the insert point will need to be reset.
  //
  // These functions are a bit subtle: they walk the function type tree,
  // flipping back and forth (as described in the source file comments), but
  // only actually generate one type on trampoline (since only one type is ever
  // needed in each context: forward globally, and reverse in programs).
  //
  // If the forward argument doesn't match the function called, the return value
  // can be null.
  llvm::Function* get_trampoline_function(
      const yang::Type& function_type, bool forward);
  llvm::Function* get_reverse_trampoline_function(
      const yang::Type& function_type, bool forward);

  typedef std::unordered_map<yang::Type, llvm::Function*> trampoline_map;
  const trampoline_map& get_trampoline_map() const;
  const trampoline_map& get_reverse_trampoline_map() const;

protected:

  // Return a function type with extra parameter for the target function when
  // calling a trampoline.
  llvm::FunctionType* get_function_type_with_target(
      llvm::FunctionType* function_type) const;

private:

  // Get the trampoline type used either way.
  llvm::FunctionType* get_trampoline_type(
      llvm::FunctionType* function_type, bool reverse) const;

  // Generated trampolines (map from type of function to corresponding
  // trampoline function).
  trampoline_map _trampoline_map;
  trampoline_map _reverse_trampoline_map;

protected:

  Builder _b;

};

// Each program module generates its own reverse trampolines so it can inline
// them easily. We also need a global set of reverse trampolines so that
// Yang Function objects can be constructed from C++ functions with an
// appropriate trampoline.
class YangTrampolineGlobals {
public:

  static yang::void_fp get_trampoline_function(const yang::Type& function_type);

private:

  YangTrampolineGlobals();
  ~YangTrampolineGlobals();

  static YangTrampolineGlobals& get_instance();
  llvm::Module* create_module();

  std::string _error;
  llvm::LLVMContext _context;
  llvm::Module* _module;
  std::unique_ptr<llvm::ExecutionEngine> _engine;

  StaticData _static_data;
  IrCommon _common;
  IrCommon::trampoline_map _trampoline_map;

};

// End namespace yang::internal.
}
}

#endif
