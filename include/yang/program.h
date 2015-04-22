//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_PROGRAM_H
#define YANG_INCLUDE_YANG_PROGRAM_H

#include <string>
#include <unordered_map>
#include <vector>
#include "error_info.h"
#include "global.h"
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
   * Compiles the given source text in the given `Context`.
   *
   * By default, errors and warnings will be printed to standard error. If a
   * non-null pointer is given as the ``diagnostic_output`` parameter, errors
   * and warnings will be appended to the target string instead.
   *
   * For even finer-grained control, the error data structures can be accessed
   * for raw error message strings and detailed information about the position
   * of errors in the source text. See `ErrorInfo` for more information.
   */
  Program(const Context& context, const std::string& name,
          const std::string& source_text, bool optimise = true,
          std::string* diagnostic_output = nullptr);
  /** #member */
  const std::vector<ErrorInfo>& get_errors() const;
  /** #member */
  const std::vector<ErrorInfo>& get_warnings() const;
  /**
   * #member ##
   *
   * Returns the ``name`` parameter given to this `Program` on construction.
   */
  const std::string& get_name() const;

  /**
   * #member
   *
   * Returns ``true`` if the source text parsed and checked successfully (i.e.
   * if `get_errors().size() == 0 <Program::get_errors>`). Otherwise, returns
   * ``false``.
   *
   * If this function returns ``false``, then `Program::get_functions` and
   * `Program::get_globals` will yield empty maps, and attempting to construct
   * an `Instance` using this object will throw a `RuntimeError`.
   */
  bool success() const;
  /** #member */
  std::string print_ir() const;
  /** #member */
  const std::unordered_map<std::string, Type>& get_functions() const;
  /** #member */
  const std::unordered_map<std::string, Global>& get_globals() const;

private:

  void generate_ir(
      bool optimise,
      const std::unordered_map<std::string, Global>& nonexported_globals);

  friend class Instance;
  internal::RefcountHook<internal::ProgramInternals> _internals;

/** #summary */
};

} // ::yang
/**/

#endif
