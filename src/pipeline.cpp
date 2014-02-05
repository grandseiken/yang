#include "pipeline.h"

#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/PassManager.h>

#include "ast.h"
#include "error.h"
#include "irgen.h"
#include "log.h"
#include "print.h"
#include "static.h"
#include "../gen/yang.y.h"

int yang_parse();

namespace yang {

Program::Program(const Context& context, const std::string& name,
                 const std::string& contents, bool optimise)
  : _context(context)
  , _name(name)
  , _ast(nullptr)
  , _module(nullptr)
  , _engine(nullptr)
{
  internal::ParseGlobals::lexer_input_contents = &contents;
  internal::ParseGlobals::lexer_input_offset = 0;
  internal::ParseGlobals::parser_output = nullptr;
  internal::ParseGlobals::errors.clear();

  yang_parse();
  std::unique_ptr<internal::Node> output(internal::ParseGlobals::parser_output);
  internal::Node::orphans.erase(output.get());
  for (internal::Node* node : internal::Node::orphans) {
    std::unique_ptr<internal::Node>{node};
  }
  internal::Node::orphans.clear();

  for (const std::string& err : internal::ParseGlobals::errors) {
    log_err(err);
  }
  if (internal::ParseGlobals::errors.size()) {
    return;
  }

  internal::StaticChecker checker(_context, _functions, _globals);
  checker.walk(*output);
  if (checker.errors()) {
    _functions.clear();
    _globals.clear();
    return;
  }
  _ast = std::move(output);

  generate_ir();
  if (optimise) {
    optimise_ir();
  }
}

Program::~Program()
{
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

void Program::generate_ir()
{
  std::string error;
  llvm::InitializeNativeTarget();

  // The ExecutionEngine takes ownership of the LLVM module (and by extension
  // everything else we created during codegen). So the engine alone must be
  // uniqued and deleted.
  _module = new llvm::Module(_name, llvm::getGlobalContext());
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
    throw runtime_error(_name + ": couldn't verify module: " + error);
  }
  _trampoline_map = irgen.get_trampoline_map();
}

void Program::optimise_ir()
{
  llvm::PassManager optimiser;
  optimiser.add(new llvm::DataLayout(*_engine->getDataLayout()));

  // Basic alias analysis and register promotion.
  optimiser.add(llvm::createBasicAliasAnalysisPass());
  optimiser.add(llvm::createPromoteMemoryToRegisterPass());

  // Optimise instructions, and reassociate for constant propagation.
  optimiser.add(llvm::createInstructionCombiningPass());
  optimiser.add(llvm::createReassociatePass());
  optimiser.add(llvm::createGVNPass());

  // Simplify the control-flow graph before tackling loops.
  optimiser.add(llvm::createCFGSimplificationPass());

  // Handle loops.
  optimiser.add(llvm::createIndVarSimplifyPass());
  optimiser.add(llvm::createLoopIdiomPass());
  optimiser.add(llvm::createLoopRotatePass());
  optimiser.add(llvm::createLoopUnrollPass());
  optimiser.add(llvm::createLoopUnswitchPass());
  optimiser.add(llvm::createLoopDeletionPass());

  // Simplify again and delete all the dead code.
  optimiser.add(llvm::createCFGSimplificationPass());
  optimiser.add(llvm::createAggressiveDCEPass());

  // Interprocedural optimisations.
  optimiser.add(llvm::createFunctionInliningPass());
  optimiser.add(llvm::createIPConstantPropagationPass());
  optimiser.add(llvm::createGlobalOptimizerPass());
  optimiser.add(llvm::createDeadArgEliminationPass());
  optimiser.add(llvm::createGlobalDCEPass());
  optimiser.add(llvm::createTailCallEliminationPass());

  // After function inlining run a few passes again.
  optimiser.add(llvm::createInstructionCombiningPass());
  optimiser.add(llvm::createReassociatePass());
  optimiser.add(llvm::createGVNPass());

  // Run the optimisation passes.
  // TODO: work out if there's others we should run, or in a different order.
  optimiser.run(*_module);
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
  yang::void_fp global_alloc = get_native_fp("!global_alloc");
  typedef void* (*alloc_fp)(void*);
  _global_data = ((alloc_fp)global_alloc)(this);
}

Instance::~Instance()
{
  yang::void_fp global_free = get_native_fp("!global_free");
  typedef void (*free_fp)(void*);
  ((free_fp)global_free)(_global_data);
}

const Program& Instance::get_program() const
{
  return _program;
}

yang::void_fp Instance::get_native_fp(const std::string& name) const
{
  return get_native_fp(_program._module->getFunction(name));
}

yang::void_fp Instance::get_native_fp(llvm::Function* ir_fp) const
{
  void* void_p = _program._engine->getPointerToFunction(ir_fp);
  // ISO C++ forbids casting between pointer-to-function and pointer-to-object!
  // Unfortunately (due to dependence on dlsym?), there doesn't seem to be any
  // way around this (technically) defined behaviour. I guess it should work
  // in practice since the whole native codegen thing is inherently machine-
  // -depend anyway. Also occurs in irgen.cpp.
  return (yang::void_fp)(std::intptr_t)void_p;
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
