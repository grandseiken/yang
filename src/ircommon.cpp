//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "ircommon.h"

#include <llvm/Analysis/Passes.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/IR/Module.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/PassManager.h>
#include <llvm/Support/TargetSelect.h>
#include <yang/runtime_error.h>
#include <yang/trampoline.h>

namespace yang {
namespace internal {

IrCommon::IrCommon(llvm::Module& module, llvm::ExecutionEngine& engine,
                   StaticData& static_data)
  : _b{llvm::IRBuilder<>(module.getContext()), module, engine, static_data, {}}
{
}

void IrCommon::optimise_ir(llvm::Function* function) const
{
  // I'm not sure exactly what sequence of optimisations e.g. opt -O3 uses.
  // This sequence may be over the top, but compares favourably.
  std::size_t passes = 2;
  llvm::PassManager module_optimiser;
  llvm::FunctionPassManager function_optimiser(&_b.module);

  auto optimiser = function ?
      (llvm::PassManager*)&function_optimiser : &module_optimiser;
  optimiser->add(new llvm::DataLayoutPass(*_b.engine.getDataLayout()));

  // This should come first, so that inlined code can take advantage of e.g.
  // unreachable hints before they are deleted.
  if (!function) {
    optimiser->add(llvm::createFunctionInliningPass());
  }

  // Basic alias analysis and register promotion.
  optimiser->add(llvm::createBasicAliasAnalysisPass());
  optimiser->add(llvm::createPromoteMemoryToRegisterPass());

  // Optimise instructions, and reassociate for constant propagation.
  optimiser->add(llvm::createInstructionCombiningPass());
  optimiser->add(llvm::createReassociatePass());
  optimiser->add(llvm::createGVNPass());

  // Simplify the control-flow graph before tackling loops.
  optimiser->add(llvm::createCFGSimplificationPass());

  // Handle loops.
  optimiser->add(llvm::createIndVarSimplifyPass());
  optimiser->add(llvm::createLoopIdiomPass());
  optimiser->add(llvm::createLoopRotatePass());
  optimiser->add(llvm::createLoopUnrollPass());
  optimiser->add(llvm::createLoopUnswitchPass());
  optimiser->add(llvm::createLoopDeletionPass());

  // Simplify again and delete all the dead code.
  optimiser->add(llvm::createCFGSimplificationPass());
  optimiser->add(llvm::createAggressiveDCEPass());

  if (function) {
    for (std::size_t i = 0; i < passes; ++i) {
      function_optimiser.run(*function);
    }
    return;
  }

  optimiser->add(llvm::createIPConstantPropagationPass());
  optimiser->add(llvm::createGlobalOptimizerPass());
  optimiser->add(llvm::createDeadArgEliminationPass());
  optimiser->add(llvm::createGlobalDCEPass());
  optimiser->add(llvm::createTailCallEliminationPass());

  for (std::size_t i = 0; i < passes; ++i) {
    module_optimiser.run(_b.module);
  }
}

llvm::Function* IrCommon::get_trampoline_function(
    const Type& function_type, bool forward)
{
  // Trampoline functions must be generated for every function type that might
  // be called externally or referenced by valid Function objects; reverse
  // trampolines must be generated for every function type which might be a
  // C++ function. This means:
  //
  // - trampolines for types of top-level functions;
  // - trampolines for types of generated global accessor functions;
  // - reverse trampolines for types of context functions;
  // - if a function type has some generated trampoline, its return function
  //   type must have the same kind of trampoline generated;
  // - if a function type has some generated trampoline, each argument function
  //   type must have the opposite kind of trampoline generated.
  // - the return type of any function type whose return type is also a function
  //   type, and for which a trampoline has been generated (transitively). This
  //   includes types of globals that have function type.
  //
  // We generate reverse trampolines on a per-program basis for inlining
  // purposes; forward trampolines are uniqued globally and generated when they
  // are first needed.
  //
  // Careful! User types have been erased by this point. Clients must erase
  // user types before looking up trampoline functions.
  //
  // It might be possible to further erase function pointers to minimise the
  // number of functions needed.
  auto it = _trampoline_map.find(Type::erased_t(function_type));
  if (it != _trampoline_map.end()) {
    return it->second;
  }

  // LLVM bytecode calling convention has some significant drawbacks that make
  // it unsuitable for direct interop with native code. Most importantly,
  // calling convention for vectors in undefined; vectors can't be passed
  // directly at all. More subtly, calling convention for value structures
  // may be target-dependent.
  //
  // It's possible this implementation is overly conservative, but it's
  // definitely going to work. Essentially, we unpack vectors into individual
  // values, and convert return values to pointer arguments; the trampoline
  // takes the actual function to be called as the final argument.
  const Type& return_t = function_type.function_return();
  // Handle the transitive closure.
  if (return_t.is_function()) {
    get_trampoline_function(return_t, forward);
  }
  for (const auto& t : function_type.function_args()) {
    if (t.is_function()) {
      get_reverse_trampoline_function(t, forward);
    }
  }

  if (!forward) {
    return nullptr;
  }
  auto ext_function_type =
      get_trampoline_type(_b.raw_function_type(function_type), false);

  // Generate the function code.
  auto function = llvm::Function::Create(
      ext_function_type, llvm::Function::ExternalLinkage,
      "!trampoline", &_b.module);
  auto block = llvm::BasicBlock::Create(_b.b.getContext(), "entry", function);
  _b.b.SetInsertPoint(block);

  std::vector<llvm::Value*> call_args;
  auto callee = --function->arg_end();
  callee->setName("target");

  // Translate trampoline arguments to an LLVM-internal argument list.
  auto jt = function->arg_begin();
  std::size_t return_args =
      return_t.is_void() ? 0 :
      return_t.is_function() ? 2 : 1;
  for (std::size_t i = 0; i < return_args; ++i) {
    jt++->setName(return_args == 1 ? "r" : i ? "r_eptr" : "r_fptr");
  }

  for (std::size_t i = 0; i < function_type.function_args().size(); ++i) {
    const Type& t = function_type.function_args()[i];

    if (t.is_vector()) {
      std::size_t size = t.vector_size();
      llvm::Value* v = t.is_ivec() ? _b.constant_ivec(0, size) :
                                     _b.constant_fvec(0, size);

      jt->setName("a" + std::to_string(i));
      for (std::size_t j = 0; j < size; ++j) {
        v = _b.b.CreateInsertElement(
            v, _b.b.CreateLoad(_b.b.CreateGEP(jt, _b.constant_int(j))),
            _b.constant_int(j));
      }
      ++jt;
      call_args.push_back(v);
      continue;
    }
    if (t.is_function()) {
      jt->setName("a" + std::to_string(i) + "_fptr");
      llvm::Value* fptr = jt++;
      jt->setName("a" + std::to_string(i) + "_eptr");
      llvm::Value* eptr = jt++;

      call_args.push_back(_b.function_value(t, fptr, eptr));
      continue;
    }

    jt->setName("a" + std::to_string(i));
    call_args.push_back(jt++);
  }
  // Environment pointer is last parameter.
  jt->setName("env");
  call_args.push_back(jt);

  // Do the call and translate the result back to native calling convention.
  llvm::Value* result = _b.b.CreateCall(callee, call_args);
  if (return_t.is_vector()) {
    for (std::size_t i = 0; i < return_t.vector_size(); ++i) {
      _b.b.CreateStore(
          _b.b.CreateExtractElement(result, _b.constant_int(i)),
          _b.b.CreateGEP(function->arg_begin(), _b.constant_int(i)));
    }
  }
  else if (return_t.is_function()) {
    llvm::Value* fptr = _b.b.CreateExtractValue(result, 0);
    llvm::Value* eptr = _b.b.CreateExtractValue(result, 1);

    auto it = function->arg_begin();
    _b.b.CreateStore(fptr, it++);
    _b.b.CreateStore(eptr, it++);
  }
  else if (!return_t.is_void()) {
    _b.b.CreateStore(result, function->arg_begin());
  }
  _b.b.CreateRetVoid();
  _trampoline_map.emplace(Type::erased_t(function_type), function);
  return function;
}

llvm::Function* IrCommon::get_reverse_trampoline_function(
    const Type& function_type, bool forward)
{
  auto it = _reverse_trampoline_map.find(Type::erased_t(function_type));
  if (it != _reverse_trampoline_map.end()) {
    return it->second;
  }
  // Handle transitive closure.
  const Type& return_t = function_type.function_return();
  if (return_t.is_function()) {
    get_reverse_trampoline_function(return_t, forward);
  }
  for (const auto& t : function_type.function_args()) {
    if (t.is_function()) {
      get_trampoline_function(t, forward);
    }
  }
  if (forward) {
    return nullptr;
  }
  // Trampolines on the C++ side have been populated by template instantiations.
  // We may be providing a null pointer here, if C++ never uses this type, so
  // don't generate the function which will be unlinkable.
  void_fp external_trampoline_ptr =
      get_cpp_trampoline_lookup_map()[Type::erased_t(function_type)];
  if (!external_trampoline_ptr) {
    return nullptr;
  }

  // Construct the type of the trampoline function (with extra argument for
  // target pointer).
  auto internal_type =
      get_function_type_with_target(_b.raw_function_type(function_type));
  // Generate it.
  auto function = llvm::Function::Create(
      internal_type, llvm::Function::InternalLinkage,
      "!reverse_trampoline", &_b.module);
  auto block = llvm::BasicBlock::Create(_b.b.getContext(), "entry", function);
  _b.b.SetInsertPoint(block);

  std::vector<llvm::Value*> args;
  auto handle = [&]
  {
    std::size_t i = 0;
    for (auto it = function->arg_begin();
         it != function->arg_end(); ++it, ++i) {
      auto jt = it;
      if (++jt == function->arg_end()) {
        // Target is the last argument.
        it->setName("target");
      }
      else if (++jt == function->arg_end()) {
        // Environment pointer is second-last argument.
        it->setName("env");
      }
      else {
        it->setName("a" + std::to_string(i));
      }

      if (it->getType()->isVectorTy()) {
        llvm::Type* array = llvm::ArrayType::get(
            it->getType()->getVectorElementType(),
            it->getType()->getVectorNumElements());
        llvm::Value* alloc = _b.b.CreateAlloca(array, nullptr);
        for (std::size_t j = 0;
             j < it->getType()->getVectorNumElements(); ++j) {
          std::vector<llvm::Value*> indices{
              _b.constant_int(0), _b.constant_int(j)};
          _b.b.CreateStore(
              _b.b.CreateExtractElement(it, _b.constant_int(j)),
              _b.b.CreateGEP(alloc, indices));
        }
        std::vector<llvm::Value*> indices{
              _b.constant_int(0), _b.constant_int(0)};
        args.push_back(_b.b.CreateGEP(alloc, indices));
        continue;
      }
      if (it->getType()->isStructTy()) {
        llvm::Value* fptr = _b.b.CreateExtractValue(it, 0);
        llvm::Value* eptr = _b.b.CreateExtractValue(it, 1);
        args.push_back(fptr);
        args.push_back(eptr);
        continue;
      }
      args.push_back(it);
    }

    auto external_type = get_trampoline_type(internal_type, true);
    auto external_trampoline = _b.get_native_function(
        "external_trampoline", external_trampoline_ptr, external_type);
    _b.b.CreateCall(external_trampoline, args);
  };

  if (return_t.is_void()) {
    handle();
    _b.b.CreateRetVoid();
  }
  else if (return_t.is_vector()) {
    std::size_t size = return_t.vector_size();
    llvm::Type* array = llvm::ArrayType::get(
        return_t.is_ivec() ? _b.int_type() : _b.float_type(), size);
    llvm::Value* alloc = _b.b.CreateAlloca(array, nullptr);
    std::vector<llvm::Value*> indices{_b.constant_int(0), _b.constant_int(0)};
    args.push_back(_b.b.CreateGEP(alloc, indices));
    handle();

    llvm::Value* v = return_t.is_ivec() ?
        _b.constant_ivec(0, size) : _b.constant_fvec(0, size);
    for (std::size_t i = 0; i < size; ++i) {
      std::vector<llvm::Value*> indices{_b.constant_int(0), _b.constant_int(i)};
      v = _b.b.CreateInsertElement(
          v, _b.b.CreateLoad(_b.b.CreateGEP(alloc, indices)),
          _b.constant_int(i));
    }
    _b.b.CreateRet(v);
  }
  else if (return_t.is_function()) {
    llvm::Type* t = internal_type->getReturnType()->getStructElementType(0);
    llvm::Value* fptr = _b.b.CreateAlloca(t, nullptr);
    llvm::Value* eptr = _b.b.CreateAlloca(_b.void_ptr_type(), nullptr);
    args.push_back(fptr);
    args.push_back(eptr);
    handle();

    _b.b.CreateRet(_b.function_value(
        return_t, _b.b.CreateLoad(fptr), _b.b.CreateLoad(eptr)));
  }
  else {
    llvm::Value* r = _b.b.CreateAlloca(internal_type->getReturnType(), nullptr);
    args.push_back(r);
    handle();

    _b.b.CreateRet(_b.b.CreateLoad(r));
  }
  _reverse_trampoline_map.emplace(Type::erased_t(function_type), function);
  return function;
}

auto IrCommon::get_trampoline_map() const -> const trampoline_map&
{
  return _trampoline_map;
}

auto IrCommon::get_reverse_trampoline_map() const -> const trampoline_map&
{
  return _reverse_trampoline_map;
}

llvm::FunctionType* IrCommon::get_function_type_with_target(
    llvm::FunctionType* type) const
{
  // Construct the function type which includes an extra target type at
  // the end. This is kind of weird. We could make every function have an
  // unused target parameter at the end to avoid this weird casting and
  // switching. Not sure whether that's a better idea. It would avoid having
  // to branch on every function call (in case we want to pass a target).
  std::vector<llvm::Type*> ft_args;
  for (auto it = type->param_begin(); it != type->param_end(); ++it) {
    ft_args.push_back(*it);
  }
  ft_args.push_back(_b.void_ptr_type());
  return llvm::FunctionType::get(type->getReturnType(), ft_args, false);
}

llvm::FunctionType* IrCommon::get_trampoline_type(
    llvm::FunctionType* function_type, bool reverse) const
{
  std::vector<llvm::Type*> args;
  auto add_type = [&](llvm::Type* t, bool to_ptr)
  {
    if (t->isVectorTy()) {
      args.push_back(llvm::PointerType::get(t->getVectorElementType(), 0));
      return;
    }
    if (t->isStructTy()) {
      auto u = (llvm::StructType*)t;
      for (auto it = u->element_begin(); it != u->element_end(); ++it) {
        args.push_back(to_ptr ? llvm::PointerType::get(*it, 0) : *it);
      }
      return;
    }
    args.push_back(to_ptr ? llvm::PointerType::get(t, 0) : t);
  };

  auto return_type = function_type->getReturnType();
  if (!return_type->isVoidTy()) {
    add_type(return_type, true);
  }
  for (auto it = function_type->param_begin();
       it != function_type->param_end(); ++it) {
    add_type(*it, false);
  }

  if (!reverse) {
    // Argument is pointer to Yang code function. (For reverse, the C++ function
    // target argument is implicit in function_type.)
    args.push_back(llvm::PointerType::get(function_type, 0));
  }
  return llvm::FunctionType::get(_b.void_type(), args, false);
}

void_fp YangTrampolineGlobals::get_trampoline_function(
    const Type& function_type)
{
  auto& trampoline_map = get_instance()._trampoline_map;
  // We need an extra layer of caching to avoid optimising the function
  // every time we need it.
  auto it = trampoline_map.find(Type::erased_t(function_type));
  if (it != trampoline_map.end()) {
    return (void_fp)(std::intptr_t)
        get_instance()._engine->getPointerToFunction(it->second);
  }

  llvm::Function* function =
      get_instance()._common.get_trampoline_function(function_type, true);
  get_instance()._common.optimise_ir(function);
  trampoline_map[Type::erased_t(function_type)] = function;
  auto ptr = (void_fp)(std::intptr_t)
      get_instance()._engine->getPointerToFunction(function);
  return ptr;
}

YangTrampolineGlobals::YangTrampolineGlobals()
  : _module(create_module())
  , _engine(llvm::EngineBuilder(_module).setErrorStr(&_error).create())
  , _common(*_module, *_engine, _static_data)
{
  if (!_engine) {
    delete _module;
    throw RuntimeError("couldn't create execution engine: " + _error);
  }
  _engine->DisableSymbolSearching();
}

YangTrampolineGlobals::~YangTrampolineGlobals()
{
}

YangTrampolineGlobals& YangTrampolineGlobals::get_instance()
{
  // This must be lazily-initialised rather than a static class member; it
  // depends on LLVM static initialisation.
  static YangTrampolineGlobals instance;
  return instance;
}

llvm::Module* YangTrampolineGlobals::create_module()
{
  // Make sure to initialise native target first!
  llvm::InitializeNativeTarget();
  return new llvm::Module("!trampoline_globals", _context);
}

}} // ::yang::internal
