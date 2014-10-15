//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_ERROR_INFO_H
#define YANG_INCLUDE_YANG_ERROR_INFO_H

#include <string>
#include "error_position_info.h"

/** #sumline ## */
namespace yang {

/**
 * #class
 *
 * Provides structured error and warning reporting.
 *
 * Each diagnostic is associated with two chunks of the source text. The first
 * (``node``) points to the exact node in the parse tree which caused the error;
 * the second (``tree``) points to the whole dependent subtree.
 *
 * For example, a progam containing the expression ``(1.0 + 2 + 3)`` will report
 * an error about adding an ``int`` and a ``float``. The ``node`` of this error
 * will point at the first plus character and the ``tree`` will point at the
 * subtree ``1.0 + 2``.
 *
 * The ``node`` is always contained inside the ``tree``, but they can be
 * identical.
 */
struct ErrorInfo {
  /**
   * #member
   *
   * Descriptive message about what's wrong.
   */
  std::string raw_message;
  /**
   * #member ##
   * 
   * Nicely-formatted error message, including highlighted excerpts from the
   * source text.
   */
  std::string formatted_message;

  /**
   * #member
   */
  ErrorPositionInfo node;
  /**
   * #member
   */
  ErrorPositionInfo tree;
/** #summary */
};

} // ::yang
/**/

#endif
