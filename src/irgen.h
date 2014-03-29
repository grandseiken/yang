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

  // Emit functions for allocating, freeing, reading and writing to instances
  // of the global structure. This should be called after the tree has been
  // walked!
  void emit_global_functions();

protected:

  void preorder(const Node& node) override;
  void infix(const Node& node, const result_list& results) override;
  Value visit(const Node& node, const result_list& results) override;

private:

  Structure init_structure_type(const symbol_frame& symbols, bool global_data);
  llvm::Value* allocate_structure_value(const Structure& st);
  llvm::Value* allocate_closure_struct(
      const symbol_frame& symbols, llvm::Value* parent_ptr);
  llvm::Value* get_parent_struct(std::size_t parent_steps, llvm::Value* v);
  Value get_variable_ptr(const std::string& name);

  void create_function(const Node& node, const yang::Type& function_type);

  Value i2b(const Value& v);
  Value b2i(const Value& v);
  Value i2f(const Value& v);
  Value f2i(const Value& v);

  // Indexing global and closure data structures.
  llvm::Value* structure_ptr(llvm::Value* ptr, std::size_t index);

  Value raw_binary(const Node& node, const Value& v, const Value& u);
  llvm::Value* vectorise(
      const Value& v, const Value& u, llvm::Function* f, bool to_float = false);
  // Lsh/rsh implementation.
  llvm::Value* lsh(const Value& v, const Value& u);
  llvm::Value* rsh(const Value& v, const Value& u);
  // Power implementation.
  llvm::Value* pow(const Value& v, const Value& u);
  // Euclidean mod and div implementations.
  llvm::Value* mod(const Value& v, const Value& u);
  llvm::Value* div(const Value& v, const Value& u);

  Value binary(const Node& node, const Value& left, const Value& right);
  Value fold(
      const Node& node, const Value& value,
      bool to_bool = false, bool with_ands = false, bool right_assoc = false);

  llvm::Module& _module;
  const Context& _context;

  // List of static initialisation functions.
  std::vector<llvm::Function*> _global_inits;
  // Current function stack.
  std::vector<LexScope> _scopes;

  // Daft hack.
  std::string _immediate_left_assign;

  // Hooks out to the refcount runtime.
  llvm::Function* _cleanup_structures;
  llvm::Function* _destroy_internals;

};

// End namespace yang::internal.
}
}

#endif
