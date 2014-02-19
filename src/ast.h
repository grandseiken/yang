//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_AST_H
#define YANG_SRC_AST_H

#include <memory>
#include <string>
#include <unordered_set>
#include <vector>
#include <yang/typedefs.h>

typedef void* scan_t;

namespace yang {
namespace internal {

struct Node {
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
    EMPTY_STMT,
    EXPR_STMT,
    RETURN_VOID_STMT,
    RETURN_STMT,
    IF_STMT,
    FOR_STMT,
    DO_WHILE_STMT,
    BREAK_STMT,
    CONTINUE_STMT,

    // Expressions.
    SCOPE_RESOLUTION,
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
    LOGICAL_NEGATION,
    BITWISE_NEGATION,
    ARITHMETIC_NEGATION,
    ASSIGN,
    ASSIGN_VAR,
    ASSIGN_CONST,
    INT_CAST,
    FLOAT_CAST,
    VECTOR_CONSTRUCT,
    VECTOR_INDEX,
  };

  // Child nodes passed to constructors or add transfer ownership, and are
  // destroyed when the parent is destroyed. These would take explicit
  // unique_ptr<Node> parameters but for sake of brevity in the parser.

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

  // Clone an entire tree.
  Node* clone(bool clone_children = true) const;

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
  std::vector<std::unique_ptr<Node>> children;

  // Literal values.
  yang::int_t int_value;
  yang::float_t float_value;
  std::string string_value;

  // We almost always don't need to pass any type information between the type
  // checker and the IR generator, except when dealing with user types (whose
  // names are erased in LLVM). We can store them here when necessary.
  mutable std::string user_type_name;
};

// Get the human-readable text of an operator.
std::string node_op_string(Node::node_type t);

struct ParseData {
  ParseData(const std::string& name, const std::string& contents);
  // Format a nice error or warning string for printing.
  std::string format_error(
      std::size_t left_index, std::size_t right_index,
      std::size_t left_tree_index, std::size_t right_tree_index,
      const std::string& message, bool error = true);

  // User-supplied name of the program we're parsing.
  std::string name;
  // Current character count in lexer.
  std::size_t column;

  // Precomputed lookup data for error-printing. Respectively, we have: a map
  // from column number to line number; a map from column number to position
  // within that line; and a vector of split lines.
  std::vector<std::size_t> char_to_line;
  std::vector<std::size_t> char_to_line_position;
  std::vector<std::string> lines;

  // Parse output.
  Node* parser_output;
  std::vector<std::string> errors;
  std::vector<std::string> warnings;

  // If parsing aborts half-way due to a syntax error, Nodes allocated by the
  // parser will leak. To avoid this, we keep a set of Nodes which have been
  // allocated but not inserted as children so they can be freed later.
  std::unordered_set<Node*> orphans;
};

// End namespace yang::internal.
}
}

#endif
