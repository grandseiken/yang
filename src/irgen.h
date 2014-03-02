//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_IRGEN_H
#define YANG_SRC_IRGEN_H

#include <functional>
#include <unordered_map>
#include <string>

#include <yang/native.h>
#include <yang/type.h>
#include <yang/typedefs.h>
#include "ircommon.h"
#include "table.h"
#include "walker.h"

namespace llvm {
  class ExecutionEngine;
  class Function;
  class Module;
  class Type;
  class Value;
}

namespace yang {
class Context;

namespace internal {

struct IrGeneratorUnion {
  IrGeneratorUnion(llvm::Type* type);
  IrGeneratorUnion(llvm::Value* value);

  operator llvm::Type*() const;
  operator llvm::Value*() const;

  llvm::Type* type;
  llvm::Value* value;
};

class IrGenerator : public IrCommon, public ConstAstWalker<IrGeneratorUnion> {
public:

  typedef std::unordered_map<std::string, yang::Type> symbol_frame;
  IrGenerator(llvm::Module& module, llvm::ExecutionEngine& engine,
              symbol_frame& globals, const Context& context);
  ~IrGenerator();

  // Emit functions for allocating, freeing, reading and writing to instances
  // of the global structure. This should be called after the tree has been
  // walked!
  void emit_global_functions();

protected:

  void preorder(const Node& node) override;
  void infix(const Node& node, const result_list& results) override;
  IrGeneratorUnion visit(const Node& node, const result_list& results) override;

private:

  typedef std::unordered_map<std::string, std::size_t> structure_numbering;
  void init_structure_type(
      llvm::Type*& output_type, structure_numbering& output_numbering,
      const symbol_frame& symbols, const std::string& name);
  llvm::Value* allocate_structure_value(
      llvm::Type* type, const structure_numbering& numbering);
  llvm::Value* allocate_closure_struct(
      const symbol_frame& symbols, llvm::Value* parent_ptr);
  llvm::Value* get_parent_struct(std::size_t parent_steps, llvm::Value* v);
  llvm::Value* get_variable_ptr(const std::string& name);

  void create_function(
      const Node& node, llvm::FunctionType* function_type);

  llvm::Value* i2b(llvm::Value* v);
  llvm::Value* b2i(llvm::Value* v);
  llvm::Value* i2w(llvm::Value* v);
  llvm::Value* w2i(llvm::Value* v);

  // Indexing global and closure data structures.
  llvm::Value* structure_ptr(llvm::Value* ptr, std::size_t index);
  llvm::Value* global_ptr(const std::string& name);
  llvm::Value* global_ptr();

  // Storing to some structure (global data or closure) with refcounting.
  llvm::Value* memory_load(llvm::Value* ptr);
  void memory_init(llvm::IRBuilder<>& pos, llvm::Value* ptr);
  void memory_store(llvm::Value* value, llvm::Value* ptr);
  // Raw reference-counting.
  void update_reference_count(llvm::Value* value, int_t change);
  // Emit code to decrement reference count of locals in topmost scope, or
  // all scopes (for returns).
  void dereference_scoped_locals();
  void dereference_scoped_locals(std::size_t first_scope);

  // Power implementation.
  llvm::Value* pow(llvm::Value* v, llvm::Value* u);
  // Euclidean mod and div implementations.
  llvm::Value* mod(llvm::Value* v, llvm::Value* u);
  llvm::Value* div(llvm::Value* v, llvm::Value* u);

  llvm::Value* binary(
      llvm::Value* left, llvm::Value* right,
      std::function<llvm::Value*(llvm::Value*, llvm::Value*)> op);
  llvm::Value* fold(
      llvm::Value* value,
      std::function<llvm::Value*(llvm::Value*, llvm::Value*)> op,
      bool to_bool = false, bool with_ands = false, bool right_assoc = false);

  // Metadata symbols.
  enum metadata {
    ENVIRONMENT_PTR,
    CLOSURE_PTR,
    GLOBAL_INIT_FUNCTION,
    FUNCTION,
    PARENT_BLOCK,
    TYPE_EXPR_CONTEXT,

    IF_THEN_BLOCK,
    IF_ELSE_BLOCK,

    LOOP_COND_BLOCK,
    LOOP_BODY_BLOCK,
    LOOP_AFTER_BLOCK,

    LOOP_BREAK_LABEL,
    LOOP_CONTINUE_LABEL,

    LOGICAL_OP_SOURCE_BLOCK,
    LOGICAL_OP_RHS_BLOCK,

    MERGE_BLOCK,
  };

  // Create block and insert in the metadata table.
  llvm::BasicBlock* create_block(metadata meta, const std::string& name);

  llvm::Module& _module;
  const Context& _context;

  // List of static initialisation functions.
  std::vector<llvm::Function*> _global_inits;
  // Map from global name to index in the global structure.
  structure_numbering _global_numbering;
  // Type of the global structure.
  llvm::Type* _global_data;

  // We keep a second symbol table for special metadata entries that don't
  // correspond to actual source code symbols; this way we can add scopes
  // that automatically pop metadata without interfering with scope lookup.
  friend std::hash<metadata>;
  SymbolTable<std::string, llvm::Value*> _symbol_table;
  SymbolTable<metadata, llvm::Value*> _metadata;
  // Metadata that isn't an llvm::Value.
  std::string _immediate_left_assign;
  // Maps general scope indices to function scope indices, so that variable
  // accesses know how many global pointer dereferences to do.
  std::map<std::size_t, std::size_t> _scope_to_function_map;
  std::size_t _function_scope;
  // To look up a value in a closure, the flow is:
  // [std::string identifier] through _symbol_table to
  // [llvm::Value* value (in defining function)] through
  // _value_to_unique_name_map to [std::string unique_identifier].
  //
  // The scope index of the identifier then gives us the closure scope index via
  // _scope_to_function_map. This lets us look up the correct closure structure
  // via the environment pointer and index it using scope_closures[closure index]
  // and unique_identifier.
  //
  // TODO: clean up these data structures if possible, it's kind of awkward.
  struct closure_t {
    llvm::Type* type;
    structure_numbering numbering;
  };
  std::vector<closure_t> _scope_closures;
  std::unordered_map<llvm::Value*, std::string> _value_to_unique_name_map;

  // List of local variables in scope, for refcounting.
  std::vector<std::vector<std::vector<llvm::Value*>>> _refcount_locals;
  std::vector<std::size_t> _refcount_loop_indices;
  // Functions for refcounting on various types.
  llvm::Function* _refcount_function;
  llvm::Function* _refcount_structure;
  llvm::Function* _cleanup_structures;
  llvm::Function* _destroy_internals;
  // Map from structure type to destructor function.
  std::unordered_map<llvm::Type*, llvm::Function*> _destructors;

};

// End namespace yang::internal.
}
}

#endif
