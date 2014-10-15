//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_ERROR_POSITION_INFO_H
#define YANG_INCLUDE_YANG_ERROR_POSITION_INFO_H

#include <cstddef>
#include <string>

/** #sumline ## */
namespace yang {

/**
 * #class
 *
 * Provides information about the position of an error or warning in the source
 * text. Start to end ranges are 0-indexed and inclusive.
 */
struct ErrorPositionInfo {
  /**
   * #member
   *
   * Line on which the error starts.
   */
  std::size_t start_line;
  /**
   * #member ##
   *
   * Column on which the error starts.
   */
  std::size_t start_column;

  /**
   * #member
   *
   * Line on which the error ends.
   */
  std::size_t end_line;
  /**
   * #member ##
   *
   * Column on which the error ends.
   */
  std::size_t end_column;

  /**
   * #member
   *
   * Raw index of the character on which the error starts (treating the source
   * text as a single string including newline characters).
   */
  std::size_t start_index;
  /**
   * #member ##
   *
   * Raw index of the character on which the error ends (treating the source
   * text as a single string including newline characters).
   */
  std::size_t end_index;

  /**
   * #member
   *
   * For convenience, contains the entire string in which the error occurs from
   * start to end.
   */
  std::string text;
/** #summary */
};

} // ::yang
/**/

#endif
