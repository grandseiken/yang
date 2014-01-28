#ifndef YANG_SRC_AST_H
#define YANG_SRC_AST_H

#include <memory>
#include <string>
#include <unordered_set>
#include <vector>
#include "typedefs.h"

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

    // Statements.
    BLOCK,
    EMPTY_STMT,
    EXPR_STMT,
    RETURN_STMT,
    IF_STMT,
    FOR_STMT,
    DO_WHILE_STMT,
    BREAK_STMT,
    CONTINUE_STMT,

    // Expressions.
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
  Node(node_type type);
  Node(node_type type, Node* a);
  Node(node_type type, Node* a, Node* b);
  Node(node_type type, Node* a, Node* b, Node* c);

  Node(node_type type, yang::int_t value);
  Node(node_type type, yang::float_t value);
  Node(node_type type, const std::string& value);

  void add_front(Node* node);
  void add(Node* node);

  // Information about the location of this Node in the source text, for
  // diagnostic purposes.
  std::size_t line;
  std::string text;

  // Type and children.
  node_type type;
  std::vector<std::unique_ptr<Node>> children;

  // Literal values.
  yang::int_t int_value;
  yang::float_t float_value;
  std::string string_value;

  // Get the human-readable text of an operator.
  static std::string op_string(node_type t);

  // If parsing aborts half-way due to a syntax error, Nodes allocated by the
  // parser will leak. To avoid this, we keep a set of Nodes which have been
  // allocated but not inserted as children so they can be freed later.
  static std::unordered_set<Node*> orphans;
};

struct ParseGlobals {
  static const std::string* lexer_input_contents;
  static std::size_t lexer_input_offset;

  static Node* parser_output;
  static std::vector<std::string> errors;
  static std::string error(
      std::size_t line, const std::string& token, const std::string& message);
};

// End namespace yang::internal.
}
}

#endif
