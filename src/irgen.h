#ifndef YANG_SRC_IRGEN_H
#define YANG_SRC_IRGEN_H

#include <functional>
#include <unordered_map>
#include <string>

#include "typedefs.h"
#include "native.h"
#include "table.h"
#include "type.h"
#include "walker.h"
#include <llvm/IR/IRBuilder.h>

namespace llvm {
  class Constant;
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

class IrGenerator : public ConstAstWalker<IrGeneratorUnion> {
public:

  typedef std::unordered_map<std::string, yang::Type> symbol_frame;
  typedef std::unordered_map<std::string, GenericNativeFunction> context_frame;

  IrGenerator(llvm::Module& module, llvm::ExecutionEngine& engine,
              symbol_frame& globals, const Context& context);
  ~IrGenerator();

  // Emit functions for allocating, freeing, reading and writing to instances
  // of the global structure. This should be called after the tree has been
  // walked!
  void emit_global_functions();

  typedef std::unordered_map<yang::Type, llvm::Function*> trampoline_map;
  const trampoline_map& get_trampoline_map() const;

protected:

  void preorder(const Node& node) override;
  void infix(const Node& node, const result_list& results) override;
  IrGeneratorUnion visit(const Node& node, const result_list& results) override;

private:

  // Get an LLVM function pointer to a native function.
  llvm::Function* get_native_function(
      const std::string& name, yang::void_fp native_fp,
      llvm::FunctionType* type) const;

  // Tools for functions and calling conventions.
  void create_function(
      const Node& node, llvm::FunctionType* function_type);
  // Generate trampoline functions for converting between calling conventions.
  llvm::Function* create_trampoline_function(llvm::FunctionType* function_type);
  llvm::Function* create_reverse_trampoline_function(
      const yang::Type& function_type);
  // Get the trampoline type used either way.
  llvm::FunctionType* get_trampoline_type(
      llvm::FunctionType* function_type, bool reverse) const;
  std::size_t get_trampoline_num_return_args(llvm::Type* return_type) const;

  // General helper functions.
  llvm::Type* void_ptr_type() const;
  llvm::Type* void_type() const;
  llvm::Type* int_type() const;
  llvm::Type* float_type() const;
  llvm::Type* vector_type(llvm::Type* type, std::size_t n) const;

  llvm::Constant* constant_int(yang::int_t value) const;
  llvm::Constant* constant_float(yang::float_t value) const;
  llvm::Constant* constant_vector(
      const std::vector<llvm::Constant*>& values) const;
  llvm::Constant* constant_vector(llvm::Constant* value, std::size_t n) const;
  llvm::Value* constant_ptr(void* ptr);

  llvm::Type* generic_function_type(llvm::Type* function_type) const;
  llvm::Type* generic_function_type(
      llvm::Type* return_type, const std::vector<llvm::Type*>& arg_types) const;
  llvm::FunctionType* function_type_from_generic(
      llvm::Type* generic_function_type) const;
  llvm::Value* generic_function_value(
      llvm::Value* function_ptr, llvm::Value* env_ptr = nullptr,
      llvm::Value* target_ptr = nullptr);
  llvm::Value* generic_function_value(const GenericNativeFunction& function);

  llvm::Value* i2b(llvm::Value* v);
  llvm::Value* b2i(llvm::Value* v);
  llvm::Value* i2w(llvm::Value* v);
  llvm::Value* w2i(llvm::Value* v);

  llvm::Value* global_ptr(llvm::Value* ptr, std::size_t index);
  llvm::Value* global_ptr(const std::string& name);

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

  // Convert back and forth between equivalent Yang and LLVM types.
  // bare_functions = false wraps functions in pointers for passing around.
  llvm::Type* get_llvm_type(const yang::Type& t) const;
  yang::Type get_yang_type(llvm::Type* t) const;

  // Metadata symbols.
  enum metadata {
    GLOBAL_DATA_PTR,
    GLOBAL_INIT_FUNCTION,
    FUNCTION,
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

  const Context& _context;

  // List of static initialisation functions.
  std::vector<llvm::Function*> _global_inits;
  // Map from global name to index in the global structure.
  std::unordered_map<std::string, std::size_t> _global_numbering;
  // Type of the global structure.
  llvm::Type* _global_data;

  // Generated trampolines (map from type of function to corresponding
  // trampoline function).
  trampoline_map _trampoline_map;
  trampoline_map _reverse_trampoline_map;

  llvm::Module& _module;
  llvm::ExecutionEngine& _engine;
  llvm::IRBuilder<> _builder;

  // We keep a second symbol table for special metadata entries that don't
  // correspond to actual source code symbols; this way we can add scopes
  // that automatically pop metadata without interfering with scope lookup.
  friend std::hash<metadata>;
  SymbolTable<std::string, llvm::Value*> _symbol_table;
  SymbolTable<metadata, llvm::Value*> _metadata;
  // Metadata that isn't an llvm::Value.
  std::string _immediate_left_assign;

};

// End namespace yang::internal.
}
}

#endif
