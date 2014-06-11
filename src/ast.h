//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_AST_H
#define YANG_SRC_AST_H

#include <memory>
#include <string>
#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <yang/error.h>
#include <yang/typedefs.h>
#include <yang/type.h>

typedef void* scan_t;

namespace yang {
namespace internal {

struct Node {
  enum modifier {
    MODIFIER_NONE = 0,
    MODIFIER_EXPORT = 1,
    MODIFIER_NEGATION = 2,
    MODIFIER_CLOSED = 4,
  };

  enum node_type {
    ERROR,

    // Types.
    TYPE_VOID,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_FUNCTION,

    // Top-level elements.
    PROGRAM,
    GLOBAL,
    GLOBAL_ASSIGN,
    FUNCTION,
    NAMED_EXPRESSION,

    // Statements.
    BLOCK,
    LOOP_AFTER_BLOCK,
    EXPR_STMT,
    RETURN_STMT,
    IF_STMT,
    FOR_STMT,
    WHILE_STMT,
    DO_WHILE_STMT,
    BREAK_STMT,
    CONTINUE_STMT,

    // Expressions.
    EMPTY_EXPR,
    MEMBER_SELECTION,
    IDENTIFIER,
    INT_LITERAL,
    FLOAT_LITERAL,
    TERNARY,
    CALL,
    LOGICAL_OR,
    LOGICAL_AND,
    BITWISE_OR,
    BITWISE_AND,
    BITWISE_XOR,
    BITWISE_LSHIFT,
    BITWISE_RSHIFT,
    POW,
    MOD,
    ADD,
    SUB,
    MUL,
    DIV,
    EQ,
    NE,
    GE,
    LE,
    GT,
    LT,
    FOLD_LOGICAL_OR,
    FOLD_LOGICAL_AND,
    FOLD_BITWISE_OR,
    FOLD_BITWISE_AND,
    FOLD_BITWISE_XOR,
    FOLD_BITWISE_LSHIFT,
    FOLD_BITWISE_RSHIFT,
    FOLD_POW,
    FOLD_MOD,
    FOLD_ADD,
    FOLD_SUB,
    FOLD_MUL,
    FOLD_DIV,
    FOLD_EQ,
    FOLD_NE,
    FOLD_GE,
    FOLD_LE,
    FOLD_GT,
    FOLD_LT,
    ASSIGN_LOGICAL_OR,
    ASSIGN_LOGICAL_AND,
    ASSIGN_BITWISE_OR,
    ASSIGN_BITWISE_AND,
    ASSIGN_BITWISE_XOR,
    ASSIGN_BITWISE_LSHIFT,
    ASSIGN_BITWISE_RSHIFT,
    ASSIGN_POW,
    ASSIGN_MOD,
    ASSIGN_ADD,
    ASSIGN_SUB,
    ASSIGN_MUL,
    ASSIGN_DIV,
    LOGICAL_NEGATION,
    BITWISE_NEGATION,
    ARITHMETIC_NEGATION,
    INCREMENT,
    DECREMENT,
    ASSIGN,
    ASSIGN_VAR,
    ASSIGN_CONST,
    INT_CAST,
    FLOAT_CAST,
    VECTOR_CONSTRUCT,
    VECTOR_INDEX,
  };

  // Child nodes passed to constructors or add transfer ownership, and are
  // destroyed when the parent is destroyed.

  // Construct nodes with source indices based on an inner node.
  Node(scan_t scan, const Node* inner, node_type type);
  Node(scan_t scan, const Node* inner, node_type type, Node* a);
  Node(scan_t scan, const Node* inner, node_type type, Node* a, Node* b);
  Node(scan_t scan, const Node* inner,
       node_type type, Node* a, Node* b, Node* c);

  // Construct nodes with source indices based on current scan position.
  Node(scan_t scan, node_type type);
  Node(scan_t scan, node_type type, yang::int_t value);
  Node(scan_t scan, node_type type, yang::float_t value);
  Node(scan_t scan, node_type type, const std::string& value);

  ~Node();

  void add_front(Node* node);
  void add(Node* node);
  void set_inner_bounds(const Node* node);
  void extend_inner_bounds(const Node* node);
  void extend_bounds(const Node* node);

  // Pointer to the Flex scanner data structure.
  scan_t scan;

  // Information about the location of this Node in the source text, for
  // diagnostic purposes.
  std::size_t left_index;
  std::size_t right_index;
  // Position of entire substree, rather than just the token.
  std::size_t left_tree_index;
  std::size_t right_tree_index;

  // Type and children.
  node_type type;
  std::vector<Node*> children;

  // Literal values.
  yang::int_t int_value;
  yang::float_t float_value;
  std::string string_value;

  // We mostly don't need to pass any type information between the static
  // checker and the IR generator, except for closure scope information.
  typedef std::unordered_map<std::string, yang::Type> symbol_frame;
  struct mutable_struct {
    std::size_t scope_number = 0;
    symbol_frame closed_environment;
  };
  mutable mutable_struct static_info;
};

// Get the human-readable text of an operator.
std::string node_op_string(Node::node_type t);

struct ParseData {
  ParseData(const std::string& name, const std::string& contents);
  // Format a nice error or warning string for printing.
  yang::ErrorInfo format_error(
      std::size_t left_index, std::size_t right_index,
      std::size_t left_tree_index, std::size_t right_tree_index,
      const std::string& message, bool error = true) const;

  // User-supplied name of the program we're parsing.
  std::string name;
  // Current character index in lexer.
  std::size_t character;

  // Precomputed lookup data for error-printing. Respectively, we have: a map
  // from character index to line number; a map from character index to column
  // number; and a vector of split lines.
  const std::string& source_text;
  std::vector<std::size_t> char_to_line;
  std::vector<std::size_t> char_to_column;
  std::vector<std::string> lines;

  // Parse output.
  Node* parser_output;
  // Structured error reporting.
  std::vector<yang::ErrorInfo> errors;
  std::vector<yang::ErrorInfo> warnings;

  // If parsing aborts half-way due to a syntax error, Nodes allocated by the
  // parser will leak. To avoid this, we keep a set of Nodes which have been
  // allocated but not inserted as children so they can be freed later.
  std::unordered_set<Node*> orphans;
};

// End namespace yang::internal.
}
}

#endif
