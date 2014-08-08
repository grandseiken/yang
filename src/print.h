//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_PRINT_H
#define YANG_SRC_PRINT_H

#include <string>
#include "walker.h"

namespace yang {
namespace internal {

class AstPrinter : public ConstAstWalker<std::string> {
public:

  AstPrinter();

protected:

  void before(const Node& node) override;
  void infix(const Node& node, const result_list& results) override;
  std::string after(const Node& node, const result_list& results) override;

private:

  std::string indent() const;
  std::size_t _indent;

};

// End namespace yang::internal.
}
}

#endif
