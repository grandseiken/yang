//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "ast.h"

#include <sstream>
#include "../gen/yang.l.h"

namespace yang {
namespace internal {

Node::Node(node_type type)
  : line(yang_lineno)
  , text(yang_text ? yang_text : "")
  , type(type)
  , int_value(0)
  , float_value(0)
{
  orphans.insert(this);
}

Node::Node(node_type type, Node* a)
  : Node(type)
{
  add(a);
}

Node::Node(node_type type, Node* a, Node* b)
  : Node(type)
{
  add(a);
  add(b);
}

Node::Node(node_type type, Node* a, Node* b, Node* c)
  : Node(type)
{
  add(a);
  add(b);
  add(c);
}

Node::Node(node_type type, yang::int_t value)
  : line(yang_lineno)
  , text(yang_text ? yang_text : "")
  , type(type)
  , int_value(value)
  , float_value(0)
{
  orphans.insert(this);
}

Node::Node(node_type type, yang::float_t value)
  : line(yang_lineno)
  , text(yang_text ? yang_text : "")
  , type(type)
  , int_value(0)
  , float_value(value)
{
  orphans.insert(this);
}

Node::Node(node_type type, const std::string& value)
  : line(yang_lineno)
  , text(yang_text ? yang_text : "")
  , type(type)
  , int_value(0)
  , float_value(0)
  , string_value(value)
{
  orphans.insert(this);
}

Node* Node::clone(bool clone_children) const
{
  Node* node = new Node(type);
  node->line = line;
  node->text = text;
  node->int_value = int_value;
  node->float_value = float_value;
  node->string_value = string_value;
  node->user_type_name = user_type_name;
  if (!clone_children) {
    return node;
  }

  // Paranoid: avoid blowing the stack on deep AST trees.
  std::vector<std::pair<const Node*, Node*>> stack;
  stack.emplace(stack.end(), this, node);
  while (!stack.empty()) {
    auto pair = stack.back();
    stack.pop_back();

    for (const auto& child : pair.first->children) {
      Node* n = child->clone(false);
      pair.second->add(n);
      stack.emplace(stack.end(), child.get(), n);
    }
  }
  return node;
}

void Node::add_front(Node* node)
{
  orphans.erase(node);
  children.insert(children.begin(), std::unique_ptr<Node>(node));
}

void Node::add(Node* node)
{
  orphans.erase(node);
  children.push_back(std::unique_ptr<Node>(node));
}

std::string Node::op_string(node_type t)
{
  return
      t == Node::TERNARY ? "?:" :
      t == Node::CALL ? "()" :
      t == Node::LOGICAL_OR ? "||" :
      t == Node::LOGICAL_AND ? "&&" :
      t == Node::BITWISE_OR ? "|" :
      t == Node::BITWISE_AND ? "&" :
      t == Node::BITWISE_XOR ? "^" :
      t == Node::BITWISE_LSHIFT ? "<<" :
      t == Node::BITWISE_RSHIFT ? ">>" :
      t == Node::POW ? "**" :
      t == Node::MOD ? "%" :
      t == Node::ADD ? "+" :
      t == Node::SUB ? "-" :
      t == Node::MUL ? "*" :
      t == Node::DIV ? "/" :
      t == Node::EQ ? "==" :
      t == Node::NE ? "!=" :
      t == Node::GE ? ">=" :
      t == Node::LE ? "<=" :
      t == Node::GT ? ">" :
      t == Node::LT ? "<" :
      t == Node::FOLD_LOGICAL_OR ? "||" :
      t == Node::FOLD_LOGICAL_AND ? "&&" :
      t == Node::FOLD_BITWISE_OR ? "|" :
      t == Node::FOLD_BITWISE_AND ? "&" :
      t == Node::FOLD_BITWISE_XOR ? "^" :
      t == Node::FOLD_BITWISE_LSHIFT ? "<<" :
      t == Node::FOLD_BITWISE_RSHIFT ? ">>" :
      t == Node::FOLD_POW ? "**" :
      t == Node::FOLD_MOD ? "%" :
      t == Node::FOLD_ADD ? "+" :
      t == Node::FOLD_SUB ? "-" :
      t == Node::FOLD_MUL ? "*" :
      t == Node::FOLD_DIV ? "/" :
      t == Node::FOLD_EQ ? "==" :
      t == Node::FOLD_NE ? "!=" :
      t == Node::FOLD_GE ? ">=" :
      t == Node::FOLD_LE ? "<=" :
      t == Node::FOLD_GT ? ">" :
      t == Node::FOLD_LT ? "<" :
      t == Node::LOGICAL_NEGATION ? "!" :
      t == Node::BITWISE_NEGATION ? "~" :
      t == Node::ARITHMETIC_NEGATION ? "-" :
      t == Node::INT_CAST ? "[]" :
      t == Node::FLOAT_CAST ? "." :
      t == Node::VECTOR_CONSTRUCT ? "()" :
      t == Node::VECTOR_INDEX ? "[]" :
      t == Node::MEMBER_SELECTION ? "." :
      t == Node::SCOPE_RESOLUTION ? "::" :
      "unknown operator";
}

std::unordered_set<Node*> Node::orphans;

const std::string* ParseGlobals::lexer_input_contents = nullptr;
std::size_t ParseGlobals::lexer_input_offset = 0;

Node* ParseGlobals::parser_output = nullptr;
std::vector<std::string> ParseGlobals::errors;

std::string ParseGlobals::error(
    std::size_t line, const std::string& token, const std::string& message)
{
  bool replace = false;
  std::string t = token;
  std::size_t it;
  while ((it = t.find('\n')) != std::string::npos) {
    t.replace(it, 1 + it, "");
    replace = true;
  }
  while ((it = t.find('\r')) != std::string::npos) {
    t.replace(it, 1 + it, "");
    replace = true;
  }
  while ((it = t.find('\t')) != std::string::npos) {
    t.replace(it, 1 + it, " ");
  }
  if (replace) {
    --line;
  }

  std::stringstream ss;
  ss << "Error at line " << line <<
      ", near `" << t << "`:\n\t" << message;
  return ss.str();
}

// End namespace yang::internal.
}
}
