//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_STATIC_H
#define YANG_SRC_STATIC_H

#include <map>
#include <string>
#include <unordered_map>

#include <yang/type.h>
#include "internal_type.h"
#include "table.h"
#include "walker.h"

namespace yang {
namespace internal {
struct ContextInternals;
struct ParseData;

class StaticChecker : public ConstAstWalker<Type> {
public:

  typedef std::unordered_map<std::string, yang::Type> symbol_frame;
  StaticChecker(const ContextInternals& context, ParseData& data,
                symbol_frame& functions_output, symbol_frame& globals_output);
  ~StaticChecker();

protected:

  void before(const Node& node) override;
  void infix(const Node& node, const result_list& results) override {}
  Type after(const Node& node, const result_list& results) override;

private:

  void enter_function(const Type& return_type);
  const Type& current_return_type() const;
  bool inside_function() const;
  bool inside_type_context() const;

  // Symbol table management that also tracks unreferenced symbols for warnings.
  void push_symbol_tables();
  void pop_symbol_tables();
  void add_symbol(const Node& node, const std::string& name, const Type& type,
                  bool global, bool unreferenced_warning = true);
  void add_symbol_checking_collision(
      const Node& node, const std::string& name,
      const Type& type, bool global, bool unreferenced_warning = true);

  void error(const Node& node, const std::string& message, bool error = true);
  std::string str(const Node& node) const;
  std::string str(const Type& type) const;

  std::string _immediate_left_assign;
  bool _immediate_left_assign_warn_reads;

  enum metadata_t {
    EXPORT_GLOBAL,
    GLOBAL_DESTRUCTOR,
    LOOP_BODY,
    RETURN_TYPE,
    TYPE_EXPR_CONTEXT,
    ERR_EXPR_CONTEXT,
  };
  friend std::hash<metadata_t>;
  
  // Along with the type, we need to store the subscope number for
  // disambiguating identically-named closed variables in different subscopes.
  struct symbol_t {
    symbol_t();
    Type type;
    bool closed;
    std::size_t scope_number;

    // We also store a few bits indicating whether a symbol has been referenced
    // since it was defined, for the purposes of warning about unused variables.
    const Node* declaration;
    bool warn_writes;
    bool warn_reads;
    bool warn_closed;
  };

  struct lex_scope_t {
    lex_scope_t(const Node& node, const std::string& name);
    const Node& function;
    std::string name;

    SymbolTable<metadata_t, Type> metadata;
    SymbolTable<std::string, symbol_t> symbol_table;

    // For each function, we also need to number its scopes uniquely, so that we
    // can distinguish identically-named variables in the closure structure.
    std::vector<std::size_t> scope_numbering;
    std::size_t scope_numbering_next;
  };

  std::vector<lex_scope_t> _scopes;

  const ContextInternals& _context;
  ParseData& _data;
  symbol_frame& _functions_output;
  symbol_frame& _globals_output;

};

// End namespace yang::internal.
}
}

#endif
