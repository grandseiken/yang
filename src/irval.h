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
  class Type;
  class Value;
}

namespace yang {
namespace internal {

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

  llvm::Type* llvm_type() const;
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
  llvm::Constant* constant_ptr(void* ptr);
  Value constant_int(yang::int_t value) const;
  Value constant_float(yang::float_t value) const;
  Value constant_int_vector(yang::int_t value, std::size_t n) const;
  Value constant_float_vector(yang::float_t value, std::size_t n) const;

  // Functions.
  Value function_value_null(const yang::Type& function_type) const;
  Value function_value(const yang::Type& function_type,
                       llvm::Value* fptr, llvm::Value* ptr);
  Value function_value(const GenericFunction& function);

  // Convert from Yang type to LLVM type.
  llvm::Type* get_llvm_type(const yang::Type& t) const;

  // Builder functions.
  llvm::IRBuilder<> b;
};

// TODO: refactor everything now that it's moved in here.
struct FnScope {
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

  FnScope(Builder& builder, llvm::Function* update_refcount);

  // Create block and insert in the metadata table.
  llvm::BasicBlock* create_block(metadata_t meta, const std::string& name);
  llvm::BasicBlock* get_block(metadata_t meta);

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

  // We keep a second symbol table for special metadata entries that don't
  // correspond to actual source code symbols; this way we can add scopes
  // that automatically pop metadata without interfering with scope lookup.
  friend std::hash<metadata_t>;
  SymbolTable<std::string, Value> symbol_table;
  SymbolTable<metadata_t, llvm::Value*> metadata;

  // List of local variables in scope, for refcounting.
  std::vector<std::vector<std::vector<Value>>> rc_locals;
  std::vector<std::size_t> rc_loop_indices;

  // Maps general scope indices to function scope indices, so that variable
  // accesses know how many global pointer dereferences to do.
  std::map<std::size_t, std::size_t> scope_to_function_map;
  std::size_t function_scope;
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
  std::vector<Structure> scope_closures;
  std::unordered_map<llvm::Value*, std::string> value_to_unique_name_map;

  Builder& b;
  llvm::Function* update_refcount;
};

// End namespace yang::internal.
}
}

namespace std {
  template<>
  struct hash<yang::internal::FnScope::metadata_t> {
    std::size_t operator()(yang::internal::FnScope::metadata_t v) const;
  };
}

#endif
