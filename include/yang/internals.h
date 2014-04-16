//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_INSTERNALS_H
#define YANG_INCLUDE_YANG_INSTERNALS_H

#include <memory>
#include <unordered_map>
#include "type.h"

namespace llvm {
  class ExecutionEngine;
  class LLVMContext;
  class Module;
}

namespace yang {
namespace internal {

struct ContextInternals;
typedef std::unordered_map<std::string, yang::Type> symbol_table;

// Data for a Program that is preserved as long as an Instance or some closure
// structure needs it.
struct ProgramInternals {
  // As well as looking up things in the Context, programs need to ensure that
  // RefCountedNativeFunctions they depend on are kept alive.
  std::shared_ptr<const ContextInternals> context;
  std::string name;
  symbol_table functions;
  symbol_table globals;

  std::unique_ptr<llvm::LLVMContext> llvm_context;
  llvm::Module* module;
  std::unique_ptr<llvm::ExecutionEngine> engine;
};

// Same for instance.
struct InstanceInternals {
  std::shared_ptr<const ProgramInternals> program;
};

// End namespace yang::internal.
}
}

#endif
