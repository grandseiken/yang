//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_PIPELINE_H
#define YANG_INCLUDE_YANG_PIPELINE_H

#include <memory>
#include <string>
#include <vector>

#include "error.h"
#include "internals.h"

namespace yang {
class Context;

class Program {
public:

  // Errors and warnings will be appended to the diagnostic_output string if
  // the pointer is non-null; otherwise, they will go to stderr.
  Program(const Context& context, const std::string& name,
          const std::string& contents, bool optimise = true,
          std::string* diagnostic_output = nullptr);
  ~Program();

  // By default, errors and warnings will be printed to standard error. By
  // passing diagnostic_output to the Program constructor, this behaviour can be
  // overridden, and something else can be done with the diagnostic information.
  //
  // For even finer-grained control, these data structures can be accessed for
  // raw error message strings and detailed information about the position of
  // errors in the source text. See error.h for more information.
  typedef std::vector<ErrorInfo> error_list;
  const error_list& get_errors() const;
  const error_list& get_warnings() const;
  const std::string& get_name() const;

  // Returns true if the contents parsed and checked successfully (i.e., if
  // get_errors().size() is zero). Otherwise, none of the following functions
  // will do anything useful.
  bool success() const;
  std::string print_ast() const;
  std::string print_ir() const;

  const symbol_table& get_functions() const;
  const symbol_table& get_globals() const;

private:

  void generate_ir(bool optimise);

  friend class Instance;
  std::shared_ptr<internal::ProgramInternals> _internals;

};

// End namespace yang.
}

#endif
