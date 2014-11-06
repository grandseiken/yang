//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_SOURCE_INFO_H
#define YANG_INCLUDE_YANG_SOURCE_INFO_H

#include <cstddef>
#include <string>

/** #sumline ## */
namespace yang {

/**
 * #class
 *
 * Provides information about the exact location of a fragment of Yang code in
 * the source text. Used by `ErrorInfo` to report the locations of errors and
 * warnings detected when compiling Yang code.
 *
 * Note that the line and column fields are 1-indexed and *inclusive*, while the
 * raw index field is 0-indexed.
 */
struct SourceInfo {
  /**
   * #member
   *
   * The 1-indexed line of the source text on which the first character of the
   * code fragment falls.
   */
  std::size_t start_line;
  /**
   * #member ##
   *
   * The 1-indexed column of the line indicated by `SourceInfo::start_line` on
   * which the first character of the code fragment falls.
   */
  std::size_t start_column;

  /**
   * #member
   *
   * The 1-indexed line of the source text on which the final character of the
   * code fragment falls.
   */
  std::size_t end_line;
  /**
   * #member ##
   *
   * The 1-indexed column of the line indicated by `SourceInfo::end_line` on
   * which the final character of the code fragment falls.
   */
  std::size_t end_column;

  /**
   * #member
   *
   * The raw 0-indexed position of the first character of the code fragment
   * within the entire source text (including newlines).
   */
  std::size_t index;
  /**
   * #member
   *
   * The raw length of the code fragment (including newlines).
   */
  std::size_t length;
  /**
   * #member
   *
   * For convenience, the code fragment itself. This is equal to
   * ``s.substr(index, length)``, where ``s`` is the source text (i.e. the
   * ``std::string`` passed to `Program::Program`).
   */
  std::string text;
/** #summary */
};

} // ::yang
/**/

#endif
