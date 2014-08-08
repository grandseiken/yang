//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_WALKER_H
#define YANG_SRC_WALKER_H

#include <vector>
#include "ast.h"

namespace yang {
namespace internal {

template<bool>
struct AstWalkerNodeType {};
template<>
struct AstWalkerNodeType<false> {
  typedef Node type;
};
template<>
struct AstWalkerNodeType<true> {
  typedef const Node type;
};

template<typename T, bool Const>
class AstWalkerBase {
public:

  typedef typename AstWalkerNodeType<Const>::type N;
  typedef std::vector<T> result_list;

  // Implements an algorithm such that after() will be called for each node in
  // the AST, with the passed result_list containing the results of the calls
  // to after() for each of the node's children.
  // Furthermore, for each node, preorder() will be called before any children
  // are visited, and infix() will be called in-between visiting each child,
  // with a partial result list.
  T walk(N& node);

protected:

  virtual void before(N& node) = 0;
  virtual void infix(N& node, const result_list& results) = 0;
  virtual T after(N& node, const result_list& results) = 0;

  void call_after(N& node, const std::function<void(const result_list&)>& cb);
  void call_after(N& node, const std::function<void()>& cb);
  void result(N& node, const std::function<T(const result_list&)>& cb);
  void result(N& node, const std::function<T()>& cb);
  void call_after_result(N& node, const std::function<void(const T&)>& cb);

private:

  struct callback {
    std::function<void(const result_list&)> call_after_args;
    std::function<void()> call_after;
    std::function<T(const result_list&)> result_args;
    std::function<T()> result;
    std::function<void(const T&)> call_after_result;
  };
  std::unordered_map<N*, std::vector<callback>> _callbacks;
  T handle_after(N& node, const result_list& results);

};

template<typename T, bool Const>
T AstWalkerBase<T, Const>::walk(N& node)
{
  struct stack_elem {
    N* n;
    decltype(node.children.begin()) it;
    result_list results;
  };
  std::vector<stack_elem> stack;

  result_list root_output;
  stack.push_back({&node, node.children.begin(), result_list()});
  while (true) {
    stack_elem& elem = stack.back();
    // Correctly handle calling before() on zero-length nodes, but not
    // duplicating the last before() and after() on others.
    if (elem.it == elem.n->children.begin()) {
      before(*elem.n);
    }
    else if (elem.it != elem.n->children.end()) {
      infix(*elem.n, elem.results);
    }

    if (elem.it == elem.n->children.end()) {
      if (stack.size() == 1) {
        return handle_after(*elem.n, elem.results);
      }
      (++stack.rbegin())->results.push_back(
          handle_after(*elem.n, elem.results));
      stack.pop_back();
      continue;
    }

    N& next = **elem.it++;
    stack.push_back({&next, next.children.begin(), result_list()});
  }
}

template<typename T, bool Const>
void AstWalkerBase<T, Const>::call_after(
    N& node, const std::function<void(const result_list&)>& cb)
{
  _callbacks[&node].emplace_back();
  _callbacks[&node].back().call_after_args = cb;
}

template<typename T, bool Const>
void AstWalkerBase<T, Const>::call_after(
    N& node, const std::function<void()>& cb)
{
  _callbacks[&node].emplace_back();
  _callbacks[&node].back().call_after = cb;
}

template<typename T, bool Const>
void AstWalkerBase<T, Const>::result(
    N& node, const std::function<T(const result_list&)>& cb)
{
  _callbacks[&node].emplace_back();
  _callbacks[&node].back().result_args = cb;
}

template<typename T, bool Const>
void AstWalkerBase<T, Const>::result(
    N& node, const std::function<T()>& cb)
{
  _callbacks[&node].emplace_back();
  _callbacks[&node].back().result = cb;
}

template<typename T, bool Const>
void AstWalkerBase<T, Const>::call_after_result(
    N& node, const std::function<void(const T&)>& cb)
{
  _callbacks[&node].emplace_back();
  _callbacks[&node].back().call_after_result = cb;
}

template<typename T, bool Const>
T AstWalkerBase<T, Const>::handle_after(N& node, const result_list& results)
{
  T result = {};
  bool have_result = false;
  auto copy = _callbacks[&node];
  for (auto it = copy.rbegin(); it != copy.rend(); ++it) {
    if (!have_result && it->result_args) {
      result = it->result_args(results);
      have_result = true;
    }
    if (!have_result && it->result) {
      result = it->result();
      have_result = true;
    }

    if (it->call_after_args) {
      it->call_after_args(results);
    }
    if (it->call_after) {
      it->call_after();
    }
  }

  _callbacks.erase(&node);
  if (!have_result) {
    result = after(node, results);
  }
  for (auto it = copy.rbegin(); it != copy.rend(); ++it) {
    if (it->call_after_result) {
      it->call_after_result(result);
    }
  }
  return result;
}

template<typename T>
using AstWalker = AstWalkerBase<T, false>;
template<typename T>
using ConstAstWalker = AstWalkerBase<T, true>;

// End namespace yang::internal.
}
}

#endif
