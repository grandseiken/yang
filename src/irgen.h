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
namespace internal {
struct ContextInternals;

class IrGenerator : public IrCommon, public ConstAstWalker<Value> {
public:

  IrGenerator(llvm::Module& module, llvm::ExecutionEngine& engine,
              StaticData& static_data, const symbol_frame& globals,
              const ContextInternals& context);

  // Emit functions for allocating, freeing, reading and writing to instances
  // of the global structure. This should be called after the tree has been
  // walked!
  void emit_global_functions();
  // Obtain function pointers for generated functions and store them in vtables.
  // This must be called after optimisation (if any) has taken place.
  void obtain_function_pointers();

protected:

  void before(const Node& node) override;
  Value after(const Node& node, const result_list& results) override;

private:

  llvm::Value* get_parent_struct(std::size_t parent_steps, llvm::Value* v);
  llvm::Value* get_global_struct();
  Value get_variable_ptr(const std::string& name);
  void create_function(const Node& node, const yang::Type& function_type);
  Value get_member_function(const yang::Type& type, const std::string& name);
  Value get_constructor(const std::string& type);
  Value create_call(const Value& f, const std::vector<Value>& args);

  Value i2b(const Value& v);
  Value b2i(const Value& v);
  Value i2f(const Value& v);
  Value f2i(const Value& v);

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

  const ContextInternals& _context;
  // List of static initialisation/destruction functions.
  std::vector<llvm::Function*> _global_inits;
  std::vector<llvm::Function*> _global_destructors;
  // Current function stack.
  std::vector<LexScope> _scopes;
  // Structure used for member function closure indirection and managed user
  // types.
  LexScope _chunk;

  typedef std::unordered_map<std::string, Value> value_map;
  std::unordered_map<yang::Type, value_map> _member_functions;
  value_map _constructors;

  // String literal uniquing.
  std::unordered_map<std::string, std::size_t> _string_literals;
  // Daft hack.
  std::string _immediate_left_assign;

};

// End namespace yang::internal.
}
}

#endif
