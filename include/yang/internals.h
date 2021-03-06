//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_INTERNALS_H
#define YANG_INCLUDE_YANG_INTERNALS_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>
#include "error_info.h"
#include "global.h"
#include "refcounting.h"
#include "typedefs.h"
#include "type.h"

namespace llvm {
  class ExecutionEngine;
  class LLVMContext;
  class Module;
}

namespace yang {
namespace internal {
struct ContextInternals;
struct Node;
typedef std::vector<std::unique_ptr<StaticDataEntry>> StaticData;

// Data for a Program that is preserved as long as an Instance or some closure
// structure needs it.
struct ProgramInternals {
  ~ProgramInternals();

  // This must match the Prefix struct declared in src/refcounting.h.
  Prefix* parent;
  int_t refcount;
  Vtable* vtable;

  // As well as looking up things in the Context, programs need to ensure that
  // the native functions they depend on are kept alive.
  std::shared_ptr<const ContextInternals> context;
  std::string name;

  StaticData static_data;
  std::unordered_map<std::string, Type> types;
  std::unordered_map<std::string, Type> functions;
  std::unordered_map<std::string, Global> globals;

  typedef std::vector<ErrorInfo> error_list;
  error_list errors;
  error_list warnings;

  std::unique_ptr<Node> ast;
  std::unique_ptr<llvm::LLVMContext> llvm_context;
  std::unique_ptr<llvm::ExecutionEngine> engine;
  llvm::Module* module;
};

}} // ::yang::internal

#endif
