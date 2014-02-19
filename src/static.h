//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_STATIC_H
#define YANG_SRC_STATIC_H

#include <string>
#include <unordered_map>

#include <yang/type.h>
#include "internal_type.h"
#include "table.h"
#include "walker.h"

namespace yang {
class Context;

namespace internal {
struct ParseData;

class StaticChecker : public ConstAstWalker<Type> {
public:

  typedef std::unordered_map<std::string, yang::Type> symbol_frame;
  StaticChecker(const yang::Context& context, ParseData& data,
                symbol_frame& functions_output, symbol_frame& globals_output);
  ~StaticChecker();

  // True if any errors were detected during the checking. Otherwise, assuming
  // there are no bugs in the compiler, we can generate the IR without worrying
  // about malformed AST.
  bool errors() const;

protected:

  void preorder(const Node& node) override;
  void infix(const Node& node, const result_list& results) override;
  Type visit(const Node& node, const result_list& results) override;

private:

  void enter_function(const Type& return_type);
  const Type& current_return_type() const;
  bool inside_function() const;

  bool is_type_expression(const Node& node) const;
  bool valid_all_contexts(const Node& node) const;
  bool inside_type_context() const;

  // Symbol table management that also tracks unreferenced symbols for warnings.
  void push_symbol_tables();
  void pop_symbol_tables();
  void add_symbol(const Node& node, const std::string& name, std::size_t index,
                  const Type& type, bool unreferenced_warning = true);
  void add_symbol(const Node& node, const std::string& name, const Type& type,
                  bool unreferenced_warning = true);

  bool use_function_immediate_assign_hack(const Node& node) const;
  void add_symbol_checking_collision(
      const Node& node, const std::string& name, const Type& type,
      bool unreferenced_warning = true);
  void add_symbol_checking_collision(
      const Node& node, const std::string& name,
      std::size_t index, const Type& type, bool unreferenced_warning = true);
  void error(const Node& node, const std::string& message, bool error = true);

  struct function {
    Type return_type;
    std::string name;
  };

  bool _errors;
  std::string _current_function;
  std::string _immediate_left_assign;

  enum metadata {
    EXPORT_GLOBAL,
    LOOP_BODY,
    RETURN_TYPE,
    TYPE_EXPR_CONTEXT,
    ERR_EXPR_CONTEXT,
    CALLEE_CONTEXT,
    ASSIGN_LHS_CONTEXT,
  };
  friend std::hash<metadata>;

  SymbolTable<metadata, Type> _metadata;
  SymbolTable<std::string, Type> _symbol_table;

  const yang::Context& _context;
  ParseData& _data;
  symbol_frame& _functions_output;
  symbol_frame& _globals_output;

  // We can store a bit indicating whether a symbol has been referenced
  // since it was defined here, for the purposes of warning about unused
  // constructs.
  //
  // It has to be a pair, rather than removing from the table on finding a
  // reference, or else a reference might remove a warning about an unrelated
  // symbol from an outer scope.
  typedef std::pair<const Node*, bool> unreferenced_pair;
  SymbolTable<std::string, unreferenced_pair> _unreferenced_warnings;

};

// End namespace yang::internal.
}
}

#endif
