//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "ast.h"

#include <sstream>
#include "../gen/yang.y.h"
#include "../gen/yang.l.h"

namespace yang {
namespace internal {

namespace {
  ParseData& data(scan_t scan)
  {
    return *(ParseData*)yang_get_extra(scan);
  }
}

Node::Node(scan_t scan, const Node* inner, node_type type)
  : scan(scan)
  , left_index(inner->left_tree_index)
  , right_index(inner->right_tree_index)
  , left_tree_index(left_index)
  , right_tree_index(right_index)
  , type(type)
  , int_value(0)
  , float_value(0)
{
  data(scan).orphans.insert(this);
}

Node::Node(scan_t scan, const Node* inner, node_type type, Node* a)
  : Node(scan, inner, type)
{
  add(a);
}

Node::Node(scan_t scan, const Node* inner, node_type type, Node* a, Node* b)
  : Node(scan, inner, type)
{
  add(a);
  add(b);
}

Node::Node(scan_t scan, const Node* inner,
           node_type type, Node* a, Node* b, Node* c)
  : Node(scan, inner, type)
{
  add(a);
  add(b);
  add(c);
}

Node::Node(scan_t scan, node_type type)
  : scan(scan)
  , left_index(data(scan).column - yang_get_leng(scan))
  , right_index(data(scan).column)
  , left_tree_index(left_index)
  , right_tree_index(right_index)
  , type(type)
  , int_value(0)
  , float_value(0)
{
  data(scan).orphans.insert(this);
}

Node::Node(scan_t scan, node_type type, yang::int_t value)
  : scan(scan)
  , left_index(data(scan).column - yang_get_leng(scan))
  , right_index(data(scan).column)
  , left_tree_index(left_index)
  , right_tree_index(right_index)
  , type(type)
  , int_value(value)
  , float_value(0)
{
  data(scan).orphans.insert(this);
}

Node::Node(scan_t scan, node_type type, yang::float_t value)
  : scan(scan)
  , left_index(data(scan).column - yang_get_leng(scan))
  , right_index(data(scan).column)
  , left_tree_index(left_index)
  , right_tree_index(right_index)
  , type(type)
  , int_value(0)
  , float_value(value)
{
  data(scan).orphans.insert(this);
}

Node::Node(scan_t scan, node_type type, const std::string& value)
  : scan(scan)
  , left_index(data(scan).column - yang_get_leng(scan))
  , right_index(data(scan).column)
  , left_tree_index(left_index)
  , right_tree_index(right_index)
  , type(type)
  , int_value(0)
  , float_value(0)
  , string_value(value)
{
  data(scan).orphans.insert(this);
}

Node* Node::clone(bool clone_children) const
{
  Node* node = new Node(scan, type);
  node->left_index = left_index;
  node->right_index = right_index;
  node->left_tree_index = left_tree_index;
  node->right_tree_index = right_tree_index;
  node->int_value = int_value;
  node->float_value = float_value;
  node->string_value = string_value;
  node->user_type_name = user_type_name;
  if (!clone_children) {
    return node;
  }

  // Paranoid: avoid blowing the stack on deep AST trees.
  std::vector<std::pair<const Node*, Node*>> stack;
  stack.emplace_back(this, node);
  while (!stack.empty()) {
    auto pair = stack.back();
    stack.pop_back();

    for (const auto& child : pair.first->children) {
      Node* n = child->clone(false);
      pair.second->add(n);
      stack.emplace_back(child.get(), n);
    }
  }
  return node;
}

void Node::add_front(Node* node)
{
  ((ParseData*)yang_get_extra(scan))->orphans.erase(node);
  children.insert(children.begin(), std::unique_ptr<Node>(node));
  left_tree_index = std::min(left_tree_index, node->left_tree_index);
  right_tree_index = std::max(right_tree_index, node->right_tree_index);
}

void Node::add(Node* node)
{
  ((ParseData*)yang_get_extra(scan))->orphans.erase(node);
  children.push_back(std::unique_ptr<Node>(node));
  left_tree_index = std::min(left_tree_index, node->left_tree_index);
  right_tree_index = std::max(right_tree_index, node->right_tree_index);
}

void Node::set_inner_bounds(const Node* node)
{
  left_index = node->left_tree_index;
  right_index = node->right_tree_index;
  left_tree_index = std::min(left_tree_index, left_index);
  right_tree_index = std::max(right_tree_index, right_index);
}

void Node::extend_inner_bounds(const Node* node)
{
  left_index = std::min(left_tree_index, node->left_tree_index);
  right_index = std::max(right_tree_index, node->right_tree_index);
  left_tree_index = std::min(left_tree_index, node->left_tree_index);
  right_tree_index = std::max(right_tree_index, node->right_tree_index);
}

void Node::extend_bounds(const Node* node)
{
  left_tree_index = std::min(left_tree_index, node->left_tree_index);
  right_tree_index = std::max(right_tree_index, node->right_tree_index);
}

std::string node_op_string(Node::node_type t)
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

ParseData::ParseData(const std::string& name, const std::string& contents)
  : name(name)
  , column(0)
  , parser_output(nullptr)
{
  lines.emplace_back();
  std::size_t line_position = 0;

  for (std::size_t i = 0; i < contents.length(); ++i) {
    char c = contents[i];
    char_to_line.push_back(lines.size() - 1);
    char_to_line_position.push_back(line_position);
    ++line_position;

    if (c == '\n') {
      lines.emplace_back();
      line_position = 0;
      continue;
    }
    if (c == '\t') {
      lines.back() += " ";
      continue;
    }
    lines.back() += c;
  }
  char_to_line.push_back(lines.size() - 1);
  char_to_line_position.push_back(line_position);
}

void ParseData::add_error(
    std::size_t left_index, std::size_t right_index,
    std::size_t left_tree_index, std::size_t right_tree_index,
    const std::string& message)
{
  auto left_tree_line = char_to_line[left_tree_index];
  auto right_tree_line = char_to_line[right_tree_index - 1];
  auto left_line = char_to_line[left_index];
  auto right_line = char_to_line[right_index - 1];
  auto left = char_to_line_position[left_index];
  auto right = char_to_line_position[right_index - 1];
  auto left_tree = char_to_line_position[left_tree_index];
  auto right_tree = char_to_line_position[right_tree_index - 1];

  std::stringstream ss;

  ss << "error in program `" + name + "`, at ";
  if (left_tree_line == right_tree_line) {
    ss << "line " << (1 + left_tree_line);
  }
  else {
    ss << "lines " << (1 + left_tree_line) << "-" << (1 + right_tree_line);
  }

  ss << ":\n\t" << message << "\n";
  std::size_t length = 0;
  for (std::size_t i = left_tree_line; i <= right_tree_line; ++i) {
    ss << lines[i] << "\n";
    length = std::max(length, lines[i].length());
  }

  for (std::size_t i = 0; i < length; ++i) {
    bool err = right_line - left_line > 1;
    bool err_tree = right_tree_line - left_tree_line > 1;

    if (right_line - left_line == 1) {
      err = i >= left || i <= right;
    }
    if (right_tree_line - left_tree_line == 1) {
      err_tree = i >= left_tree || i <= right_tree;
    }

    if (right_line == left_line) {
      err = i >= left && i <= right;
    }
    if (right_tree_line == left_tree_line) {
      err_tree = i >= left_tree && i <= right_tree;
    }

    ss << (err ? "^" : err_tree ? "~" : " ");
  }
  ss << "\n";
  errors.push_back(ss.str());
}

// End namespace yang::internal.
}
}
