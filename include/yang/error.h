//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_ERROR_H
#define YANG_INCLUDE_YANG_ERROR_H

#include <stdexcept>
#include <string>

namespace yang {

class runtime_error : public std::runtime_error {
public:
  runtime_error(const std::string& what)
    : std::runtime_error(what) {}
};

// Information about the position of an error or warning in the source text.
// Start-end ranges are 0-indexed and inclusive.
struct ErrorPositionInfo {
  // Line and column the error starts on.
  std::size_t start_line;
  std::size_t start_column;

  // Line and column the error ends on.
  std::size_t end_line;
  std::size_t end_column;

  // Alternatively, these fields provide the raw start and end character index
  // of the error,without regard to line breaks.
  std::size_t start_index;
  std::size_t end_index;

  // For convienience, this string is set to the text contained between the
  // start and end.
  std::string text;
};

// Structure for error and warning reporting.
struct ErrorInfo {
  // Descriptive message about what's wrong.
  std::string raw_message;

  // Nicely-formatted error message, including highlighted excerpts from the
  // source text.
  std::string formatted_message;

  // Each error is associated with two chunks of the source text. The first
  // ("node") points to the exact node in the source which caused the error;
  // the second ("tree") contains the whole subtree associated with it.
  //
  // For example, if a progam contains the expression
  //
  //   (1.0 + 2 + 3)
  //
  // then an error will be reported about adding an int to a float. The "node"
  // will point at the first "+", and the "tree" will point at "1.0 + 2".
  //
  // The node is always contained inside the tree, but they can be identical.
  ErrorPositionInfo node;
  ErrorPositionInfo tree;
};

} // ::yang

#endif
