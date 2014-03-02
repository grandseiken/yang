//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_REFCOUNTING_H
#define YANG_INCLUDE_YANG_REFCOUNTING_H

#include <memory>
#include <string>
#include <unordered_set>
#include <unordered_map>
#include "typedefs.h"
#include "type.h"

namespace llvm {
  class ExecutionEngine;
  class LLVMContext;
  class Module;
}

namespace yang {
class Context;
typedef std::unordered_map<std::string, Type> symbol_table;

namespace internal {
template<typename T>
class NativeFunction;

// Prefix structure of all global data and closure structures.
struct Prefix {
  Prefix* parent;

  // Current reference count.
  int_t refcount;
  // Pointer to destructor.
  void (*free)(Prefix*);

  // Number of outbound references.
  int_t refouts;
  // Pointer to outbound reference query function.
  void (*query)(Prefix*, Prefix**);
};

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

// Get list of instance internals which are to be cleaned up.
std::unordered_set<InstanceInternals*>& get_instance_cleanup_list();
// Get list of structures which are to be cleaned up.
std::unordered_set<Prefix*>& get_structure_cleanup_list();
// Get list of structures which may be involved in a cycle.
std::unordered_set<Prefix*>& get_structure_possible_cycle_list();
// Do structure refcounting.
void update_structure_refcount(Prefix* structure, int_t change);
// Do function refcounting.
void update_function_refcount(NativeFunction<void>* target, int_t change);
// Expensive cleaning up of cyclic structures that are no longer referenced.
void cleanup_cyclic_structures();
// Clean up structures that are no longer referenced.
void cleanup_structures();
// Clean up program internals.
void destroy_internals(InstanceInternals* global_parent);

// End namespace yang::internal.
}
}

#endif