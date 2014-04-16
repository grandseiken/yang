//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/program.h>

#include <llvm/Analysis/Verifier.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>

#include <yang/context.h>
#include "ast.h"
#include "irgen.h"
#include "log.h"
#include "print.h"
#include "static.h"
#include "../gen/yang.y.h"
#include "../gen/yang.l.h"

int yang_parse(yyscan_t scan);

namespace yang {

Program::Program(const Context& context, const std::string& name,
                 const std::string& contents, bool optimise,
                 std::string* diagnostic_output)
  : _ast(nullptr)
  , _internals(new internal::ProgramInternals{
      context._internals, name, {}, {},
      std::unique_ptr<llvm::LLVMContext>(new llvm::LLVMContext),
      nullptr, nullptr})
{
  internal::ParseData data(name, contents);
  yyscan_t scan = nullptr;

  auto log_errors = [&]()
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
      _errors.push_back(error);
    }
    // Warnings are still printed, but they don't cause anything to fail.
    for (const auto& warning : data.warnings) {
      print(warning);
      _warnings.push_back(warning);
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
  // Now there's no errors so we can generate IR without worrying about
  // malformed input.

  internal::StaticChecker checker(
      *_internals->context, data, _internals->functions, _internals->globals);
  checker.walk(*output);
  if (log_errors()) {
    _internals->functions.clear();
    _internals->globals.clear();
    return;
  }
  _ast = std::move(output);

  generate_ir(optimise);
}

Program::~Program()
{
}

const Program::error_list& Program::get_errors() const
{
  return _errors;
}

const Program::error_list& Program::get_warnings() const
{
  return _warnings;
}

const std::string& Program::get_name() const
{
  return _internals->name;
}

bool Program::success() const
{
  return bool(_ast) && bool(_internals->module);
}

std::string Program::print_ast() const
{
  if (!success()) {
    throw runtime_error(_internals->name +
                        ": program did not compile successfully");
  }
  internal::AstPrinter printer;
  return printer.walk(*_ast) + '\n';
}

std::string Program::print_ir() const
{
  if (!success()) {
    throw runtime_error(_internals->name +
                        ": program did not compile successfully");
  }
  std::string output;
  llvm::raw_string_ostream os(output);
  _internals->module->print(os, nullptr);
  return output;
}

const Program::symbol_table& Program::get_functions() const
{
  return _internals->functions;
}

const Program::symbol_table& Program::get_globals() const
{
  return _internals->globals;
}

void Program::generate_ir(bool optimise)
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
    throw runtime_error(
        _internals->name + ": couldn't create execution engine: " + error);
  }
  // Disable implicit searching so we don't accidentally resolve linked-in
  // functions.
  _internals->engine->DisableSymbolSearching();

  internal::IrGenerator irgen(
      *_internals->module, *_internals->engine,
      _internals->globals, *_internals->context);
  irgen.walk(*_ast);
  irgen.emit_global_functions();

  if (llvm::verifyModule(*_internals->module,
                         llvm::ReturnStatusAction, &error)) {
    // Shouldn't be possible and indicates severe bug, so log the entire IR.
    log_err(print_ir());
    throw runtime_error(_internals->name +
                        ": couldn't verify module: " + error);
  }
  if (optimise) {
    irgen.optimise_ir();
  }
}

// End namespace yang.
}
