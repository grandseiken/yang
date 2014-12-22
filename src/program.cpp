//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/program.h>

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>

#include <yang/context.h>
#include <yang/runtime_error.h>
#include "ast.h"
#include "irgen.h"
#include "log.h"
#include "print.h"
#include "static.h"
#include "../gen/yang.y.h"
#include "../gen/yang.l.h"

int yang_parse(yyscan_t scan);

namespace yang {
namespace internal {

ProgramInternals::~ProgramInternals()
{
}

} // ::internal

Program::Program(const Context& context, const std::string& name,
                 const std::string& contents, bool optimise,
                 std::string* diagnostic_output)
{
  _internals->context = context._internals;
  _internals->name = name;
  _internals->module = nullptr;

  internal::ParseData data(name, contents);
  yyscan_t scan = nullptr;

  auto log_errors = [&]
  {
    auto print = [&](const yang::ErrorInfo& error)
    {
      if (diagnostic_output) {
        *diagnostic_output += error.formatted_message + '\n';
      }
      else {
        log_err(error.formatted_message);
      }
    };

    for (const auto& error : data.errors) {
      print(error);
      _internals->errors.push_back(error);
    }
    // Warnings are still printed, but they don't cause anything to fail.
    for (const auto& warning : data.warnings) {
      print(warning);
      _internals->warnings.push_back(warning);
    }
    data.warnings.clear();
    return bool(data.errors.size());
  };

  yang_lex_init(&scan);
  yang_set_extra(&data, scan);
  auto buffer = yang__scan_string(contents.c_str(), scan);
  yang_parse(scan);
  yang__delete_buffer(buffer, scan);
  yang_lex_destroy(scan);

  std::unique_ptr<internal::Node> output(data.parser_output);
  data.orphans.erase(output.get());
  for (internal::Node* node : data.orphans) {
    std::unique_ptr<internal::Node>{node};
  }
  if (log_errors()) {
    return;
  }

  std::unordered_map<std::string, Global> nonexported_globals;
  internal::StaticChecker checker(
      *context._internals, data, _internals->functions,
      _internals->globals, nonexported_globals);

  checker.walk(*output);
  if (log_errors()) {
    _internals->functions.clear();
    _internals->globals.clear();
    return;
  }

  // Now there's no errors so we can generate IR without worrying about
  // malformed input.
  _internals->ast = std::move(output);
  _internals->llvm_context.reset(new llvm::LLVMContext);
  context._internals->immutable = true;
  generate_ir(optimise, nonexported_globals);
}

const std::vector<ErrorInfo>& Program::get_errors() const
{
  return _internals->errors;
}

const std::vector<ErrorInfo>& Program::get_warnings() const
{
  return _internals->warnings;
}

const std::string& Program::get_name() const
{
  return _internals->name;
}

bool Program::success() const
{
  return bool(_internals->ast) && bool(_internals->module);
}

std::string Program::print_ast() const
{
  if (!success()) {
    throw RuntimeError(_internals->name +
                       ": program did not compile successfully");
  }
  internal::AstPrinter printer;
  return printer.walk(*_internals->ast) + '\n';
}

std::string Program::print_ir() const
{
  if (!success()) {
    throw RuntimeError(_internals->name +
                       ": program did not compile successfully");
  }
  std::string output;
  llvm::raw_string_ostream os(output);
  _internals->module->print(os, nullptr);
  return output;
}

const std::unordered_map<std::string, Type>& Program::get_functions() const
{
  return _internals->functions;
}

const std::unordered_map<std::string, Global>& Program::get_globals() const
{
  return _internals->globals;
}

void Program::generate_ir(
    bool optimise,
    const std::unordered_map<std::string, Global>& nonexported_globals)
{
  std::string error;
  llvm::InitializeNativeTarget();

  // We need to use a different LLVMContext for each program, to avoid using
  // unbounded memory for all the LLVM structure types we create.
  _internals->module = new llvm::Module(
      _internals->name, *_internals->llvm_context);
  // The ExecutionEngine takes ownership of the LLVM module (and by extension
  // most other things we create during codegen). So the engine alone must be
  // uniqued and deleted.
  _internals->engine = std::unique_ptr<llvm::ExecutionEngine>(
      llvm::EngineBuilder(_internals->module).setErrorStr(&error).create());
  if (!_internals->engine) {
    delete _internals->module;
    throw RuntimeError(
        _internals->name + ": couldn't create execution engine: " + error);
  }
  // Disable implicit searching so we don't accidentally resolve linked-in
  // functions.
  _internals->engine->DisableSymbolSearching();

  std::unordered_map<std::string, Global> all_globals = nonexported_globals;
  for (const auto& pair : _internals->globals) {
    all_globals.emplace(pair);
  }

  internal::IrGenerator irgen(*_internals, all_globals);
  irgen.walk(*_internals->ast);
  irgen.emit_global_functions();

  llvm::raw_string_ostream eos(error);
  if (llvm::verifyModule(*_internals->module, &eos)) {
    // Shouldn't be possible and indicates severe bug, so log the entire IR.
    log_err(print_ir());
    throw RuntimeError(_internals->name +
                       ": couldn't verify module: " + error);
  }
  if (optimise) {
    irgen.optimise_ir();
  }
  irgen.resolve_function_pointers();
}

} // ::yang
