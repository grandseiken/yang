//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_RUNTIME_ERROR_H
#define YANG_INCLUDE_YANG_RUNTIME_ERROR_H

#include <stdexcept>

/** #sumline ## */
namespace yang {

/**
 * #class
 *
 * All exceptions thrown by the Yang library are instances of the
 * `runtime_error` class.
 *
 * In general, these errors are thrown when the library usage depends upon some
 * property of runtime-compiled Yang code which turns out not to hold -- for
 * example, once a Yang program is compiled, this error will be thrown on trying
 * to access a function from it that doesn't exist, or calling a function as if
 * it had a different signature than it really does.
 */
class runtime_error : public std::runtime_error {
/** #sumline */
public:
  /**
   * #member
   *
   * Construct the error with some descriptive message. Use the ``what()``
   * method inherited from ``std::runtime_error`` to retrieve the message.
   */
  runtime_error(const std::string& what)
    : std::runtime_error(what) {}
/** #summary */
};

}
/**/

#endif
