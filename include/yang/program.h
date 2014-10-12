//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_PROGRAM_H
#define YANG_INCLUDE_YANG_PROGRAM_H

#include <string>
#include <vector>
#include "error.h"
#include "internals.h"

/** #sumline ## */
namespace yang {
class Context;

/** #class */
class Program {
/** #sumline */
public:

  /**
   * #member ##
   *
   *   By default, errors and warnings will be printed to standard error. If a
   *   non-null pointer is given as the ``diagnostic_output`` parameter, errors
   *   and warnings will be appended to the target string instead.
   *
   *   For even finer-grained control, the error data structures can be accessed
   *   for raw error message strings and detailed information about the position
   *   of errors in the source text. See ``ErrorInfo`` for more information.
   */
  Program(const Context& context, const std::string& name,
          const std::string& contents, bool optimise = true,
          std::string* diagnostic_output = nullptr);
  /** #member */
  const std::vector<ErrorInfo>& get_errors() const;
  /** #member */
  const std::vector<ErrorInfo>& get_warnings() const;
  /**
   * #member ##
   *
   *   Returns the ``name`` parameter that was passed in to this ``Program``'s
   *   constructor.
   */
  const std::string& get_name() const;

  /**
   * #member ##
   *
   *   Returns ``true`` if the contents parsed and checked successfully (i.e.,
   *   if ``get_errors().size()`` is zero). Otherwise, none of the following
   *   functions will do anything useful.
   */
  bool success() const;
  /** #member */
  std::string print_ast() const;
  /** #member ## */
  std::string print_ir() const;

  /** #member */
  const function_table& get_functions() const;
  /** #member */
  const global_table& get_globals() const;

private:

  void generate_ir(bool optimise, const global_table& nonexported_globals);

  friend class Instance;
  internal::RefcountHook<internal::ProgramInternals> _internals;

/** #sumline ## */
};

// End namespace yang.
/** #sumline */
}

#endif
