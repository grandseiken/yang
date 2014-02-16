//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "print.h"

namespace yang {
namespace internal {

AstPrinter::AstPrinter()
  : _indent(0)
{
}

void AstPrinter::preorder(const Node& node)
{
  switch (node.type) {
    case Node::BLOCK:
      ++_indent;
      break;

    default: {}
  }
}

void AstPrinter::infix(const Node&, const result_list&)
{
}

std::string AstPrinter::visit(const Node& node, const result_list& results)
{
  std::string s = node_op_string(node.type);
  std::string type_id =
      node.string_value.length() ? " " + node.string_value : "";

  switch (node.type) {
    case Node::TYPE_VOID:
      return "void" + type_id;
    case Node::TYPE_INT:
      return "int" +
          (node.int_value > 1 ? std::to_string(node.int_value) : "") + type_id;
    case Node::TYPE_FLOAT:
      return "float" +
          (node.int_value > 1 ? std::to_string(node.int_value) : "") + type_id;
    case Node::TYPE_FUNCTION:
    {
      std::string r = results[0] + "(";
      for (std::size_t i = 1; i < results.size(); ++i) {
        if (i > 1) {
          r += ", ";
        }
        r += results[i];
      }
      return r + ")" + type_id;
    }

    case Node::PROGRAM:
    {
      std::string s = "";
      for (const std::string r : results) {
        s += r + '\n';
      }
      return s;
    }
    case Node::GLOBAL:
      return (node.int_value ? "export " : "") +
          std::string("global\n") + results[0];
    case Node::GLOBAL_ASSIGN:
      return (node.int_value ? "export " : "") +
          results[0] + " = " + results[1];
    case Node::FUNCTION:
      return results[0] + "\n" + results[1];
    case Node::NAMED_EXPRESSION:
      return results[0] + " " + node.string_value;

    case Node::BLOCK:
    {
      --_indent;
      std::string s = "";
      for (std::size_t i = 0; i < results.size(); ++i) {
        s += results[i];
      }
      return indent() + "{\n" + s + indent() + "}";
    }
    case Node::EMPTY_STMT:
      return indent() + ";\n";
    case Node::EXPR_STMT:
      return indent() + results[0] + ";\n";
    case Node::RETURN_VOID_STMT:
      return indent() + "return;\n";
    case Node::RETURN_STMT:
      return indent() + "return " + results[0] + ";\n";
    case Node::IF_STMT:
    {
      std::string s =
          indent() + "if (" + results[0] + ")\n" + results[1] + "\n";
      if (results.size() > 2) {
        s += indent() + "else\n" + results[2] + "\n";
      }
      return s;
    }
    case Node::FOR_STMT:
      return indent() +
          "for (" + results[0] + "; " + results[1] + "; " + results[2] + ")\n" +
          results[3] + "\n";
    case Node::DO_WHILE_STMT:
      return indent() + "do\n" + results[0] + "\n" +
          indent() + "while (" + results[1] + ");\n";
    case Node::BREAK_STMT:
      return indent() + "break;\n";
    case Node::CONTINUE_STMT:
      return indent() + "continue;\n";

    case Node::SCOPE_RESOLUTION:
      return "(" + results[0] + "::" + node.string_value + ")";
    case Node::MEMBER_SELECTION:
      return "(" + results[0] + "." + node.string_value + ")";
    case Node::IDENTIFIER:
      return node.string_value;
    case Node::INT_LITERAL:
      return std::to_string(node.int_value);
    case Node::FLOAT_LITERAL:
      return std::to_string(node.float_value);

    case Node::TERNARY:
      return "(" + results[0] + " ? " + results[1] + " : " + results[2] + ")";
    case Node::CALL:
    {
      std::string s = "(" + results[0] + "(";
      for (std::size_t i = 1; i < results.size(); ++i) {
        if (i > 1) {
          s += ", ";
        }
        s += results[i];
      }
      return s + "))";
    }

    case Node::LOGICAL_OR:
    case Node::LOGICAL_AND:
    case Node::BITWISE_OR:
    case Node::BITWISE_AND:
    case Node::BITWISE_XOR:
    case Node::BITWISE_LSHIFT:
    case Node::BITWISE_RSHIFT:
    case Node::POW:
    case Node::MOD:
    case Node::ADD:
    case Node::SUB:
    case Node::MUL:
    case Node::DIV:
    case Node::EQ:
    case Node::NE:
    case Node::GE:
    case Node::LE:
    case Node::GT:
    case Node::LT:
      return "(" + results[0] + " " + s + " " + results[1] + ")";

    case Node::FOLD_LOGICAL_OR:
    case Node::FOLD_LOGICAL_AND:
    case Node::FOLD_BITWISE_OR:
    case Node::FOLD_BITWISE_AND:
    case Node::FOLD_BITWISE_XOR:
    case Node::FOLD_BITWISE_LSHIFT:
    case Node::FOLD_BITWISE_RSHIFT:
    case Node::FOLD_POW:
    case Node::FOLD_MOD:
    case Node::FOLD_ADD:
    case Node::FOLD_SUB:
    case Node::FOLD_MUL:
    case Node::FOLD_DIV:
    case Node::FOLD_EQ:
    case Node::FOLD_NE:
    case Node::FOLD_GE:
    case Node::FOLD_LE:
    case Node::FOLD_GT:
    case Node::FOLD_LT:
      return "($" + results[0] + s + ")";

    case Node::LOGICAL_NEGATION:
    case Node::BITWISE_NEGATION:
    case Node::ARITHMETIC_NEGATION:
      return "(" + s + results[0] + ")";

    case Node::ASSIGN:
      return "(" + results[0] + " = " + results[1] + ")";
    case Node::ASSIGN_VAR:
      return "(var " + results[0] + " = " + results[1] + ")";
    case Node::ASSIGN_CONST:
      return "(const " + results[0] + " = " + results[1] + ")";

    case Node::INT_CAST:
      return "[" + results[0] + "]";
    case Node::FLOAT_CAST:
      return "(" + results[0] + ".)";

    case Node::VECTOR_CONSTRUCT:
    {
      std::string output = "";
      for (const std::string& s : results) {
        if (output.length()) {
          output += ", ";
        }
        output += s;
      }
      return "(" + output + ")";
    }
    case Node::VECTOR_INDEX:
      return "(" + results[0] + "[" + results[1] + "])";

    default:
      return "";
  }
}

std::string AstPrinter::indent() const
{
  return std::string(2 * _indent, ' ');
}

// End namespace yang::internal.
}
}
