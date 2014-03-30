//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_IRVAL_H
#define YANG_SRC_IRVAL_H

#include <yang/typedefs.h>
#include <yang/type.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include "table.h"

namespace llvm {
  class ExecutionEngine;
}

namespace yang {
namespace internal {
typedef std::unordered_map<std::string, yang::Type> symbol_frame;

struct Structure {
  struct entry {
    entry(const yang::Type& type = yang::Type::void_t(), std::size_t index = 0);

    yang::Type type;
    std::size_t index;
  };

  Structure();

  llvm::Type* type;
  std::unordered_map<std::string, entry> table;

  llvm::Function* destructor;
  llvm::Function* refout_query;
  std::size_t refout_count;
};

// A Yang value wraps an LLVM IR value and associates it with a Yang type,
// instead of a raw LLVM type.
struct Value {
  Value();
  Value(const yang::Type& type);
  Value(const yang::Type& type, llvm::Value* irval);
  operator llvm::Value*() const;

  yang::Type type;
  llvm::Value* irval;
};

// Wraps an LLVM IR builder with some convenience functions.
struct Builder {
  // LLVM type construction.
  llvm::PointerType* void_ptr_type() const;
  llvm::Type* void_type() const;
  llvm::Type* int_type() const;
  llvm::Type* float_type() const;
  llvm::Type* int_vector_type(std::size_t n) const;
  llvm::Type* float_vector_type(std::size_t n) const;

  // Functions.
  llvm::FunctionType* raw_function_type(const yang::Type& type) const;
  llvm::StructType* gen_function_type() const;

  // Value construction.
  llvm::Constant* constant_ptr(void* ptr) const;
  Value constant_int(yang::int_t value) const;
  Value constant_float(yang::float_t value) const;
  Value constant_int_vector(yang::int_t value, std::size_t n) const;
  Value constant_float_vector(yang::float_t value, std::size_t n) const;

  // Functions.
  Value function_value_null(const yang::Type& function_type) const;
  Value function_value(const yang::Type& function_type,
                       llvm::Value* fptr, llvm::Value* ptr);
  Value function_value(const GenericFunction& function);

  // Default value for a given type.
  Value default_for_type(const yang::Type& type, int_t fill = 0) const;

  // Convert from Yang type to LLVM type.
  llvm::Type* get_llvm_type(const yang::Type& t) const;

  // Get an LLVM function pointer to a native function.
  llvm::Function* get_native_function(
      const std::string& name, yang::void_fp native_fp,
      llvm::FunctionType* type) const;

  // Builder functions.
  llvm::IRBuilder<> b;
  llvm::Module& module;
  llvm::ExecutionEngine& engine;
};

class LexScope {
public:

  // Metadata symbols.
  enum metadata_t {
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

  LexScope(Builder& builder, bool create_functions = true);
  LexScope next_lex_scope() const;

  void push_scope(bool loop_scope = false);
  void pop_scope(bool loop_scope = false);

  // Structure functions.
  void init_structure_type(const symbol_frame& symbols,
                           bool global_data, bool has_parent = true);
  llvm::Value* allocate_structure_value();
  llvm::Value* allocate_closure_struct(llvm::Value* parent_ptr);
  // TODO: instead of exposed "structure" and structure_ptr we should just
  // have structure_ptr(ptr, field_name).
  llvm::Value* structure_ptr(llvm::Value* ptr, std::size_t index);

  // Create block and insert in the metadata table.
  llvm::BasicBlock* create_block(metadata_t meta, const std::string& name);
  llvm::BasicBlock* get_block(metadata_t meta);

  // Storing to some structure (global data or closure) with refcounting.
  Value memory_load(const yang::Type& type, llvm::Value* ptr);
  void memory_init(llvm::IRBuilder<>& pos, llvm::Value* ptr);
  void memory_store(const Value& value, llvm::Value* ptr);
  void refcount_init(const Value& value);

  // Raw reference-counting.
  void update_reference_count(const Value& value, int_t change);
  void update_reference_count(llvm::Value* fptr,
                              llvm::Value* eptr, int_t change);

  // Emit code to decrement reference count of locals in topmost scope, or
  // all scopes (for returns).
  void dereference_scoped_locals();
  void dereference_scoped_locals(std::size_t first_scope);
  void dereference_loop_locals();

  // We keep a second symbol table for special metadata entries that don't
  // correspond to actual source code symbols; this way we can add scopes
  // that automatically pop metadata without interfering with scope lookup.
  SymbolTable<std::string, Value> symbol_table;
  SymbolTable<metadata_t, llvm::Value*> metadata;

  // Global data or closure structure.
  Structure structure;
  // Closure lookup helper.
  // TODO: clean up these data structures if possible; still kind of awkward.
  std::unordered_map<llvm::Value*, std::string> value_to_unique_name_map;

private:

  // List of local variables in scope, for refcounting.
  std::vector<std::vector<Value>> _rc_locals;
  // Used for knowing what to refcount on BREAK and CONTINUE.
  std::vector<std::size_t> _rc_loop_indices;

  Builder& _b;

  // Hooks out to the refcount runtime.
  llvm::Function* _cleanup_structures;
  llvm::Function* _destroy_internals;
  llvm::Function* _update_refcount;

};

// End namespace yang::internal.
}
}

namespace std {
  template<>
  struct hash<yang::internal::LexScope::metadata_t> {
    std::size_t operator()(yang::internal::LexScope::metadata_t v) const;
  };
}

#endif
