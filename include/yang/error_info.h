//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_ERROR_INFO_H
#define YANG_INCLUDE_YANG_ERROR_INFO_H

#include <string>
#include "source_info.h"

/** #sumline ## */
namespace yang {

/**
 * #class
 *
 * Provides structured error and warning reporting.
 *
 * When reporting programming errors in Yang code to users, simply displaying
 * the `ErrorInfo::formatted_message` string should usually suffice. In some
 * cases (e.g. for an interactive IDE) it would be useful to apply custom
 * formatting to the error message or obtain the location in the source code
 * where the error occurred.
 *
 * Each diagnostic is associated with two chunks of the source text. The first
 * (`ErrorInfo::node`) points to the exact node in the parse tree which caused
 * the error; the second (`ErrorInfo::tree`) points to the entire dependent
 * subtree.
 *
 * For example, a progam containing the expression ``(1.0 + 2 + 3)`` will report
 * an error about adding an ``int`` and a ``float``. The `ErrorInfo::node` of
 * this error will point at the first plus character and the `ErrorInfo::tree`
 * will point at the subtree ``1.0 + 2``.
 *
 * The `ErrorInfo::node` is always contained inside the `ErrorInfo::tree`, but
 * they can be identical.
 */
struct ErrorInfo {
  /**
   * #member
   *
   * An unformatted string describing the sort of problem that was detected,
   * without reference to the source text.
   */
  std::string raw_message;
  /**
   * #member ##
   * 
   * A nicely-formatted error or warning message. This consists of the
   * `ErrorInfo::raw_message` in addition to line numbers and highlighted
   * excerpts from the source text.
   */
  std::string formatted_message;

  /**
   * #member
   *
   * A `SourceInfo` object describing the location of the exact node in the
   * source text that triggered the error or warning.
   */
  SourceInfo node;
  /**
   * #member
   *
   * A `SourceInfo` object describing the location of the entire subtree in
   * the source text that caused the error or warning.
   */
  SourceInfo tree;
/** #summary */
};

} // ::yang
/**/

#endif
