#ifndef YANG_SRC_STATIC_H
#define YANG_SRC_STATIC_H

#include <string>
#include <unordered_map>

#include "internal_type.h"
#include "table.h"
#include "type.h"
#include "walker.h"

namespace yang {
class Context;

namespace internal {

class StaticChecker : public ConstAstWalker<Type> {
public:

  typedef std::unordered_map<std::string, yang::Type> symbol_frame;
  StaticChecker(const yang::Context& context,
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
  bool inside_type_context() const;

  bool use_function_immediate_assign_hack(const Node& node) const;
  void add_symbol_checking_collision(
      const Node& node, const std::string& name, const Type& type);
  void add_symbol_checking_collision(
      const Node& node, const std::string& name,
      std::size_t index, const Type& type);
  void error(const Node& node, const std::string& message);

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
    CALLEE_CONTEXT,
  };
  friend std::hash<metadata>;

  SymbolTable<metadata, Type> _metadata;
  SymbolTable<std::string, Type> _symbol_table;

  const yang::Context& _context;
  symbol_frame& _functions_output;
  symbol_frame& _globals_output;

};

// End namespace yang::internal.
}
}

#endif
