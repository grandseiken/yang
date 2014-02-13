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

  void create_function(
      const Node& node, llvm::FunctionType* function_type);

  llvm::Value* i2b(llvm::Value* v);
  llvm::Value* b2i(llvm::Value* v);
  llvm::Value* i2w(llvm::Value* v);
  llvm::Value* w2i(llvm::Value* v);

  // Indexing the global data structure.
  llvm::Value* global_ptr(llvm::Value* ptr, std::size_t index);
  llvm::Value* global_ptr(const std::string& name);

  // Storing to some structure (global data or closure) with refcounting.
  llvm::Value* memory_load(llvm::Value* ptr);
  void memory_store(llvm::Value* value, llvm::Value* ptr,
                    bool first_initialisation = false);
  // Raw reference-counting.
  void update_reference_count(llvm::Value* value, int_t change);
  // Emit code to decrement reference count of locals in topmost scope, or
  // all scopes (for returns).
  void dereference_scoped_locals(bool all_scopes);

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
  std::unordered_map<std::string, std::size_t> _global_numbering;
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

  // List of local variables in scope, for refcounting.
  std::vector<std::vector<std::vector<llvm::Value*>>> _refcount_locals;
  // Refcount function.
  llvm::Function* _refcount_function;

};

// End namespace yang::internal.
}
}

#endif
