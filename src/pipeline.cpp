//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/pipeline.h>

#include <llvm/Analysis/Verifier.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>

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
  : _context(context)
  , _name(name)
  , _ast(nullptr)
  , _llvm_context(new llvm::LLVMContext)
  , _module(nullptr)
  , _engine(nullptr)
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

  internal::StaticChecker checker(
      _context, data, _functions, _globals);
  checker.walk(*output);
  if (log_errors()) {
    _functions.clear();
    _globals.clear();
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

const Context& Program::get_context() const
{
  return _context;
}

const std::string& Program::get_name() const
{
  return _name;
}

bool Program::success() const
{
  return bool(_ast) && bool(_module);
}

std::string Program::print_ast() const
{
  if (!success()) {
    throw runtime_error(_name + ": program did not compile successfully");
  }
  internal::AstPrinter printer;
  return printer.walk(*_ast) + '\n';
}

std::string Program::print_ir() const
{
  if (!success()) {
    throw runtime_error(_name + ": program did not compile successfully");
  }
  std::string output;
  llvm::raw_string_ostream os(output);
  _module->print(os, nullptr);
  return output;
}

const Program::symbol_table& Program::get_functions() const
{
  return _functions;
}

const Program::symbol_table& Program::get_globals() const
{
  return _globals;
}

void Program::generate_ir(bool optimise)
{
  std::string error;
  llvm::InitializeNativeTarget();

  // We need to use a different LLVMContext for each program, to avoid using
  // unbounded memory for all the LLVM structure types we create.
  _module = new llvm::Module(_name, *_llvm_context);
  // The ExecutionEngine takes ownership of the LLVM module (and by extension
  // most other things we create during codegen). So the engine alone must be
  // uniqued and deleted.
  _engine = std::unique_ptr<llvm::ExecutionEngine>(
      llvm::EngineBuilder(_module).setErrorStr(&error).create());
  if (!_engine) {
    delete _module;
    throw runtime_error(_name + ": couldn't create execution engine: " + error);
  }
  // Disable implicit searching so we don't accidentally resolve linked-in
  // functions.
  _engine->DisableSymbolSearching();

  internal::IrGenerator irgen(*_module, *_engine, _globals, _context);
  irgen.walk(*_ast);
  irgen.emit_global_functions();

  if (llvm::verifyModule(*_module, llvm::ReturnStatusAction, &error)) {
    // Shouldn't be possible and indicates severe bug, so log the entire IR.
    log_err(print_ir());
    throw runtime_error(_name + ": couldn't verify module: " + error);
  }
  if (optimise) {
    irgen.optimise_ir();
  }
  _trampoline_map = irgen.get_trampoline_map();
}

Instance::Instance(const Program& program)
  : _program(program)
  , _global_data(nullptr)
{
  if (!_program.success()) {
    throw runtime_error(
        _program._name +
        ": instantiating program which did not compile successfully");
  }
  void* global_alloc = get_native_fp("!global_alloc");
  typedef void* (*alloc_fp)();
  _global_data = ((alloc_fp)(std::intptr_t)global_alloc)();
}

Instance::~Instance()
{
  void* global_free = get_native_fp("!global_free");
  typedef void (*free_fp)(void*);
  ((free_fp)(std::intptr_t)global_free)(_global_data);
}

const Program& Instance::get_program() const
{
  return _program;
}

void* Instance::get_native_fp(const std::string& name) const
{
  return get_native_fp(_program._module->getFunction(name));
}

void* Instance::get_native_fp(llvm::Function* ir_fp) const
{
  // ISO C++ forbids casting between pointer-to-function and pointer-to-object!
  // Most users of this function will eventually cast to a function pointer
  // type and invoke it at some point.
  //
  // Unfortunately (due to dependence on dlsym?), there doesn't seem to be any
  // way around this (technically) defined behaviour. I guess it should work
  // in practice since the whole native codegen thing is inherently machine-
  // -dependent anyway.
  return _program._engine->getPointerToFunction(ir_fp);
}

void Instance::check_global(const std::string& name, const Type& type,
                            bool for_modification) const
{
  auto it = _program._globals.find(name);
  if (it == _program._globals.end()) {
    throw runtime_error(
        _program._name + ": requested global `" + name + "` does not exist");
  }
  if (type != it->second) {
    throw runtime_error(
        _program._name + ": global `" + it->second.string() + " " + name +
        "` accessed via incompatible type `" + type.string() + "`");
  }
  if (for_modification &&
      (it->second.is_const() || !it->second.is_exported())) {
    throw runtime_error(
        _program._name + ": global `" + it->second.string() + " " + name +
        "` cannot be modified externally");
  }
}

void Instance::check_function(const std::string& name, const Type& type) const
{
  auto it = _program._functions.find(name);
  if (it == _program._functions.end()) {
    throw runtime_error(
        _program._name + ": requested function `" + name + "` does not exist");
  }
  if (type != it->second) {
    throw runtime_error(
        _program._name + ": function `" + it->second.string() +
        " " + name + "` accessed via incompatible type `" +
        type.string() + "`");
  }
}

// End namespace yang.
}
