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

  // Implements an algorithm such that visit() will be called for each node in
  // the AST, with the passed result_list containing the results of the calls
  // to visit() for each of the node's children.
  // Furthermore, for each node, preorder() will be called before any children
  // are visited, and infix() will be called in-between visiting each child,
  // with a partial result list.
  T walk(N& node);

protected:

  virtual void preorder(N& node) = 0;
  virtual void infix(N& node, const result_list& results) = 0;
  virtual T visit(N& node, const result_list& results) = 0;

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
    stack_elem& elem = *stack.rbegin();
    // Correctly handle calling preorder on zero-length nodes, but not
    // duplicating the last preorder and visit on other nodes.
    if (elem.it == elem.n->children.begin()) {
      preorder(*elem.n);
    }
    else if (elem.it != elem.n->children.end()) {
      infix(*elem.n, elem.results);
    }

    if (elem.it == elem.n->children.end()) {
      if (stack.size() == 1) {
        return visit(*elem.n, elem.results);
      }
      (++stack.rbegin())->results.push_back(visit(*elem.n, elem.results));
      stack.pop_back();
      continue;
    }

    N& next = **elem.it++;
    stack.push_back({&next, next.children.begin(), result_list()});
  }
}

template<typename T>
using AstWalker = AstWalkerBase<T, false>;
template<typename T>
using ConstAstWalker = AstWalkerBase<T, true>;

// End namespace yang::internal.
}
}

#endif
