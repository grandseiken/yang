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
}

namespace yang {
class Context;

namespace internal {

class IrGenerator : public IrCommon, public ConstAstWalker<Value> {
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
  Value visit(const Node& node, const result_list& results) override;

private:

  struct structure_entry {
    structure_entry(const yang::Type& type = yang::Type::void_t(),
                    std::size_t index = 0)
      : type(type)
      , index(index) {}

    yang::Type type;
    std::size_t index;
  };
  struct structure_t {
    structure_t()
      : type(nullptr)
      , destructor(nullptr)
      , refout_query(nullptr)
      , refout_count(0) {}

    llvm::Type* type;
    std::unordered_map<std::string, structure_entry> table;

    llvm::Function* destructor;
    llvm::Function* refout_query;
    std::size_t refout_count;
  };

  structure_t init_structure_type(const symbol_frame& symbols,
                                  const std::string& name);
  llvm::Value* allocate_structure_value(const structure_t& st);
  llvm::Value* allocate_closure_struct(
      const symbol_frame& symbols, llvm::Value* parent_ptr);
  llvm::Value* get_parent_struct(std::size_t parent_steps, llvm::Value* v);
  llvm::Value* get_variable_ptr(const std::string& name);

  void create_function(const Node& node, const yang::Type& function_type);

  Value i2b(const Value& v);
  Value b2i(const Value& v);
  Value i2f(const Value& v);
  Value f2i(const Value& v);

  // Indexing global and closure data structures.
  llvm::Value* structure_ptr(llvm::Value* ptr, std::size_t index);
  llvm::Value* global_ptr(const std::string& name);
  llvm::Value* global_ptr();

  // Storing to some structure (global data or closure) with refcounting.
  Value memory_load(const yang::Type& type, llvm::Value* ptr);
  void memory_init(llvm::IRBuilder<>& pos, llvm::Value* ptr);
  void memory_store(const Value& value, llvm::Value* ptr);
  // Raw reference-counting.
  void update_reference_count(const Value& value, int_t change);
  void update_reference_count(llvm::Value* fptr,
                              llvm::Value* eptr, int_t change);
  // Emit code to decrement reference count of locals in topmost scope, or
  // all scopes (for returns).
  void dereference_scoped_locals();
  void dereference_scoped_locals(std::size_t first_scope);

  Value raw_binary(const Node& node, const Value& v, const Value& u);
  // Power implementation.
  llvm::Value* pow(const Value& v, const Value& u);
  // Euclidean mod and div implementations.
  llvm::Value* mod(const Value& v, const Value& u);
  llvm::Value* div(const Value& v, const Value& u);

  Value binary(const Node& node, const Value& left, const Value& right);
  Value fold(
      const Node& node, const Value& value,
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
  // Globals.
  structure_t _global_data;

  // We keep a second symbol table for special metadata entries that don't
  // correspond to actual source code symbols; this way we can add scopes
  // that automatically pop metadata without interfering with scope lookup.
  friend std::hash<metadata>;
  SymbolTable<std::string, Value> _symbol_table;
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
  // via the environment pointer and index it using
  // scope_closures[closure index] and unique_identifier.
  //
  // TODO: clean up these data structures if possible, it's kind of awkward.
  std::vector<structure_t> _scope_closures;
  std::unordered_map<llvm::Value*, std::string> _value_to_unique_name_map;

  // List of local variables in scope, for refcounting.
  std::vector<std::vector<std::vector<Value>>> _refcount_locals;
  std::vector<std::size_t> _refcount_loop_indices;
  // Functions for refcounting on various types.
  llvm::Function* _update_refcount;
  llvm::Function* _cleanup_structures;
  llvm::Function* _destroy_internals;

};

// End namespace yang::internal.
}
}

#endif
