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
#include <yang/error.h>
#include <yang/type_info.h>

namespace yang {
namespace internal {

IrCommon::IrCommon(llvm::Module& module, llvm::ExecutionEngine& engine)
  : _module(module)
  , _engine(engine)
  , _builder(module.getContext())
{
}

void IrCommon::optimise_ir(llvm::Function* function) const
{
  llvm::PassManager module_optimiser;
  llvm::FunctionPassManager function_optimiser(&_module);

  auto& optimiser = function ?
      (llvm::PassManager&)function_optimiser : module_optimiser;
  optimiser.add(new llvm::DataLayout(*_engine.getDataLayout()));

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

  if (function) {
    function_optimiser.run(*function);
    return;
  }

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

  // TODO: work out if there's others we should run, or in a different order.
  module_optimiser.run(_module);
}

llvm::Function* IrCommon::get_trampoline_function(
    const yang::Type& function_type, bool forward)
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
  auto it = _trampoline_map.find(function_type.erase_user_types());
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
  yang::Type return_type = function_type.get_function_return_type();
  // Handle the transitive closure.
  if (return_type.is_function()) {
    get_trampoline_function(return_type, forward);
  }
  for (std::size_t i = 0; i < function_type.get_function_num_args(); ++i) {
    yang::Type t = function_type.get_function_arg_type(i);
    if (t.is_function()) {
      get_reverse_trampoline_function(t, forward);
    }
  }

  if (!forward) {
    return nullptr;
  }
  auto llvm_type = function_type_from_generic(get_llvm_type(function_type));
  auto llvm_return_type = llvm_type->getReturnType();
  auto ext_function_type = get_trampoline_type(llvm_type, false);

  // Generate the function code.
  auto function = llvm::Function::Create(
      ext_function_type, llvm::Function::ExternalLinkage,
      "!trampoline", &_module);
  auto block = llvm::BasicBlock::Create(b().getContext(), "entry", function);
  b().SetInsertPoint(block);

  std::vector<llvm::Value*> call_args;
  auto callee = --function->arg_end();
  callee->setName("target");

  // Translate trampoline arguments to an LLVM-internal argument list.
  auto jt = function->arg_begin();
  for (std::size_t i = 0;
       i < get_trampoline_num_return_args(llvm_return_type); ++i) {
    jt->setName("r" + std::to_string(i));
    ++jt;
  }
  std::size_t i = 0;
  for (auto it = llvm_type->param_begin();
       it != llvm_type->param_end(); ++it, ++i) {
    if ((*it)->isVectorTy()) {
      std::size_t size = (*it)->getVectorNumElements();
      llvm::Value* v = (*it)->isIntOrIntVectorTy() ?
          constant_vector(constant_int(0), size) :
          constant_vector(constant_float(0), size);

      for (std::size_t j = 0; j < size; ++j) {
        jt->setName("a" + std::to_string(i) + "_" + std::to_string(j));
        v = b().CreateInsertElement(v, jt, constant_int(j), "vec");
        ++jt;
      }
      call_args.push_back(v);
      continue;
    }
    if ((*it)->isStructTy()) {
      jt->setName("a" + std::to_string(i) + "_fptr");
      llvm::Value* fptr = jt++;
      jt->setName("a" + std::to_string(i) + "_eptr");
      llvm::Value* eptr = jt++;

      call_args.push_back(generic_function_value(fptr, eptr));
      continue;
    }
    // Environment pointer is last parameter.
    auto kt = it;
    jt->setName(
        ++kt == llvm_type->param_end() ? "env" : "a" + std::to_string(i));
    call_args.push_back(jt++);
  }

  // Do the call and translate the result back to native calling convention.
  llvm::Value* result = b().CreateCall(callee, call_args);
  if (llvm_return_type->isVectorTy()) {
    auto it = function->arg_begin();
    for (std::size_t i = 0; i < llvm_return_type->getVectorNumElements(); ++i) {
      llvm::Value* v = b().CreateExtractElement(result, constant_int(i), "vec");
      b().CreateStore(v, it++);
    }
  }
  else if (llvm_return_type->isStructTy()) {
    llvm::Value* fptr = b().CreateExtractValue(result, 0, "fptr");
    llvm::Value* eptr = b().CreateExtractValue(result, 1, "eptr");

    auto it = function->arg_begin();
    b().CreateStore(fptr, it++);
    b().CreateStore(eptr, it++);
  }
  else if (!llvm_return_type->isVoidTy()) {
    b().CreateStore(result, function->arg_begin());
  }
  b().CreateRetVoid();
  _trampoline_map.emplace(function_type.erase_user_types(), function);
  return function;
}

llvm::Function* IrCommon::get_reverse_trampoline_function(
    const yang::Type& function_type, bool forward)
{
  auto it = _reverse_trampoline_map.find(function_type.erase_user_types());
  if (it != _reverse_trampoline_map.end()) {
    return it->second;
  }
  // Handle transitive closure.
  if (function_type.get_function_return_type().is_function()) {
    get_reverse_trampoline_function(
        function_type.get_function_return_type(), forward);
  }
  for (std::size_t i = 0; i < function_type.get_function_num_args(); ++i) {
    yang::Type t = function_type.get_function_arg_type(i);
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
  yang::void_fp external_trampoline_ptr =
      get_cpp_trampoline_lookup_map()[function_type.erase_user_types()];
  if (!external_trampoline_ptr) {
    return nullptr;
  }

  // Construct the type of the trampoline function (with extra argument for
  // target pointer).
  auto internal_type =
      get_function_type_with_target(get_llvm_type(function_type));

  // Generate it.
  auto function = llvm::Function::Create(
      internal_type, llvm::Function::InternalLinkage,
      "!reverse_trampoline", &_module);
  auto block = llvm::BasicBlock::Create(b().getContext(), "entry", function);
  b().SetInsertPoint(block);

  llvm::Type* return_type = internal_type->getReturnType();
  std::size_t return_args = get_trampoline_num_return_args(return_type);

  std::vector<llvm::Value*> return_allocs;
  std::vector<llvm::Value*> args;
  // Handle return args.
  for (std::size_t i = 0; i < return_args; ++i) {
    llvm::Type* t =
        return_type->isVectorTy() ? return_type->getVectorElementType() :
        return_type->isStructTy() ?
            ((llvm::StructType*)return_type)->getElementType(i) : return_type;
    llvm::Value* v = b().CreateAlloca(t, nullptr, "r" + std::to_string(i));
    return_allocs.push_back(v);
    args.push_back(v);
  }

  std::size_t i = 0;
  for (auto it = function->arg_begin(); it != function->arg_end(); ++it, ++i) {
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
      for (std::size_t j = 0; j < it->getType()->getVectorNumElements(); ++j) {
        llvm::Value* v = b().CreateExtractElement(it, constant_int(j), "vec");
        args.push_back(v);
      }
      continue;
    }
    if (it->getType()->isStructTy()) {
      llvm::Value* fptr = b().CreateExtractValue(it, 0, "fptr");
      llvm::Value* eptr = b().CreateExtractValue(it, 1, "eptr");
      args.push_back(fptr);
      args.push_back(eptr);
      continue;
    }
    args.push_back(it);
  }

  auto external_type = get_trampoline_type(internal_type, true);
  auto external_trampoline = get_native_function(
      "external_trampoline", external_trampoline_ptr, external_type);
  b().CreateCall(external_trampoline, args);

  if (return_type->isVoidTy()) {
    b().CreateRetVoid();
  }
  else if (return_type->isVectorTy()) {
    llvm::Value* v = constant_vector(
        return_type->isIntOrIntVectorTy() ? constant_int(0) : constant_float(0),
        return_args);
    for (std::size_t i = 0; i < return_args; ++i) {
      v = b().CreateInsertElement(
          v, b().CreateLoad(return_allocs[i], "load"), constant_int(i), "vec");
    }
    b().CreateRet(v);
  }
  else if (return_type->isStructTy()) {
    b().CreateRet(generic_function_value(
        b().CreateLoad(return_allocs[0], "fptr"),
        b().CreateLoad(return_allocs[1], "eptr")));
  }
  else {
    b().CreateRet(b().CreateLoad(return_allocs[0], "ret"));
  }
  _reverse_trampoline_map.emplace(function_type.erase_user_types(), function);
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

const llvm::IRBuilder<>& IrCommon::b() const
{
  return _builder;
}

llvm::IRBuilder<>& IrCommon::b()
{
  return _builder;
}

llvm::PointerType* IrCommon::void_ptr_type() const
{
  // LLVM doesn't have a built-in void pointer type, so just use a pointer
  // to whatever.
  return llvm::PointerType::get(int_type(), 0);
}

llvm::Type* IrCommon::void_type() const
{
  return llvm::Type::getVoidTy(b().getContext());
}

llvm::Type* IrCommon::int_type() const
{
  return llvm::IntegerType::get(
      b().getContext(), 8 * sizeof(yang::int_t));
}

llvm::Type* IrCommon::float_type() const
{
  return llvm::Type::getDoubleTy(b().getContext());
}

llvm::Type* IrCommon::vector_type(llvm::Type* type, std::size_t n) const
{
  return llvm::VectorType::get(type, n);
}

llvm::Constant* IrCommon::constant_int(yang::int_t value) const
{
  return llvm::ConstantInt::getSigned(int_type(), value);
}

llvm::Constant* IrCommon::constant_float(yang::float_t value) const
{
  return llvm::ConstantFP::get(b().getContext(), llvm::APFloat(value));
}

llvm::Constant* IrCommon::constant_vector(
    const std::vector<llvm::Constant*>& values) const
{
  return llvm::ConstantVector::get(values);
}

llvm::Value* IrCommon::constant_ptr(void* ptr)
{
  // To construct a constant pointer, we need to do a bit of machine-dependent
  // stuff.
  llvm::Type* int_ptr =
      llvm::IntegerType::get(b().getContext(), 8 * sizeof(ptr));
  llvm::Constant* const_int =
      llvm::ConstantInt::get(int_ptr, (std::size_t)ptr);
  llvm::Value* const_ptr =
      llvm::ConstantExpr::getIntToPtr(const_int, void_ptr_type());
  return const_ptr;
}

llvm::Constant* IrCommon::constant_vector(
    llvm::Constant* value, std::size_t n) const
{
  return llvm::ConstantVector::getSplat(n, value);
}

llvm::Type* IrCommon::generic_function_type(llvm::Type* type) const
{
  std::vector<llvm::Type*> types;
  // Yang function pointer or C++ pointer (which is not actually a function
  // pointer at all, but we have to store the type somehow; this is fairly
  // hacky).
  types.push_back(type);
  // Pointer to environment (global data structure or closure structure).
  types.push_back(void_ptr_type());

  return llvm::StructType::get(b().getContext(), types);
}

llvm::Type* IrCommon::generic_function_type(
    llvm::Type* return_type, const std::vector<llvm::Type*>& arg_types) const
{
  return generic_function_type(llvm::PointerType::get(
      llvm::FunctionType::get(return_type, arg_types, false), 0));
}

llvm::FunctionType* IrCommon::function_type_from_generic(
    llvm::Type* generic_function_type) const
{
  auto struct_type = (llvm::StructType*)generic_function_type;
  auto f_type = (*struct_type->element_begin())->getPointerElementType();
  return (llvm::FunctionType*)f_type;
}

llvm::Value* IrCommon::generic_function_value_null(
    llvm::StructType* generic_function_type) const
{
  std::vector<llvm::Constant*> values;
  values.push_back(llvm::ConstantPointerNull::get(
      (llvm::PointerType*)generic_function_type->getElementType(0)));
  values.push_back(llvm::ConstantPointerNull::get(void_ptr_type()));
  return llvm::ConstantStruct::get(generic_function_type, values);
}

llvm::Value* IrCommon::generic_function_value(
    llvm::Value* function_ptr, llvm::Value* env_ptr)
{
  auto type = (llvm::StructType*)generic_function_type(function_ptr->getType());
  llvm::Value* v = generic_function_value_null(type);

  v = b().CreateInsertValue(v, function_ptr, 0, "fptr");
  if (env_ptr) {
    // Must be bitcast to void pointer, since it may be a global data type or
    // closure data type..
    llvm::Value* cast = b().CreateBitCast(env_ptr, void_ptr_type());
    v = b().CreateInsertValue(v, cast, 1, "eptr");
  }
  return v;
}

llvm::Value* IrCommon::generic_function_value(const GenericFunction& function)
{
  void* fptr;
  void* eptr;
  function.ptr->get_representation(&fptr, &eptr);
  llvm::Type* ft = llvm::PointerType::get(
      function_type_from_generic(get_llvm_type(function.type)), 0);

  llvm::Value* v = b().CreateBitCast(constant_ptr(fptr), ft, "fun");
  if (!eptr) {
    // Native functions don't need an environment pointer.
    return generic_function_value(v, nullptr);
  }
  return generic_function_value(v, constant_ptr(eptr));
}

llvm::FunctionType* IrCommon::get_function_type_with_target(
    llvm::Type* function_type) const
{
  // Construct the function type which includes an extra target type at
  // the end. This is kind of weird. We could make every function have an
  // unused target parameter at the end to avoid this weird casting and
  // switching. Not sure whether that's a better idea. It would avoid having
  // to branch on every function call (in case we want to pass a target).
  auto f_type = function_type_from_generic(function_type);
  std::vector<llvm::Type*> ft_args;
  for (auto it = f_type->param_begin(); it != f_type->param_end(); ++it) {
    ft_args.push_back(*it);
  }
  ft_args.push_back(void_ptr_type());
  return llvm::FunctionType::get(f_type->getReturnType(), ft_args, false);
}

llvm::Type* IrCommon::get_llvm_type(const yang::Type& t) const
{
  if (t.is_function()) {
    std::vector<llvm::Type*> args;
    for (std::size_t i = 0; i < t.get_function_num_args(); ++i) {
      args.push_back(get_llvm_type(t.get_function_arg_type(i)));
    }
    args.push_back(void_ptr_type());

    return generic_function_type(
        get_llvm_type(t.get_function_return_type()), args);
  }
  if (t.is_int()) {
    return int_type();
  }
  if (t.is_float()) {
    return float_type();
  }
  if (t.is_int_vector()) {
    return vector_type(int_type(), t.get_vector_size());
  }
  if (t.is_float_vector()) {
    return vector_type(float_type(), t.get_vector_size());
  }
  if (t.is_user_type()) {
    return void_ptr_type();
  }
  return void_type();
}

yang::Type IrCommon::get_yang_type(llvm::Type* t) const
{
  yang::Type r;
  if (t == void_ptr_type()) {
    // We can't reconstruct the full user type from the void pointer. This means
    // we treat all user-types as equivalent for the purposes of trampoline
    // function generation (which makes sense).
    r._base = yang::Type::USER_TYPE;
  }
  else if (t->isFunctionTy() || t->isStructTy()) {
    auto ft = t->isStructTy() ?
        function_type_from_generic(t) : (llvm::FunctionType*)t;
    r._base = yang::Type::FUNCTION;
    r._elements.push_back(get_yang_type(ft->getReturnType()));
    // Make sure to skip the environment pointer.
    for (std::size_t i = 0; i < ft->getFunctionNumParams() - 1; ++i) {
      r._elements.push_back(get_yang_type(ft->getFunctionParamType(i)));
    }
  }
  else if (t->isIntOrIntVectorTy()) {
    r._base = yang::Type::INT;
  }
  else if (t->isFPOrFPVectorTy()) {
    r._base = yang::Type::FLOAT;
  }
  r._count = t->isVectorTy() ? t->getVectorNumElements() : 1;
  return r;
}

llvm::Function* IrCommon::get_native_function(
    const std::string& name, yang::void_fp native_fp,
    llvm::FunctionType* type) const
{
  // We use special !-prefixed names for native functions so that they can't
  // be confused with regular user-defined functions (e.g. "malloc" is not
  // reserved).
  llvm::Function* llvm_function = llvm::Function::Create(
      type, llvm::Function::ExternalLinkage, "!" + name, &_module);
  // We need to explicitly link the LLVM function to the native function.
  // More (technically) undefined behaviour here.
  _engine.addGlobalMapping(llvm_function, (void*)(std::intptr_t)native_fp);
  return llvm_function;
}

llvm::FunctionType* IrCommon::get_trampoline_type(
    llvm::FunctionType* function_type, bool reverse) const
{
  std::vector<llvm::Type*> args;
  auto add_type = [&](llvm::Type* t, bool to_ptr)
  {
    if (t->isVectorTy()) {
      for (std::size_t i = 0; i < t->getVectorNumElements(); ++i) {
        auto elem = t->getVectorElementType();
        args.push_back(to_ptr ? llvm::PointerType::get(elem, 0) : elem);
      }
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
  return llvm::FunctionType::get(void_type(), args, false);
}

std::size_t IrCommon::get_trampoline_num_return_args(
    llvm::Type* return_type) const
{
  return
      return_type->isVoidTy() ? 0 :
      return_type->isVectorTy() ? return_type->getVectorNumElements() :
      return_type->isStructTy() ? 2 : 1;
}

yang::void_fp YangTrampolineGlobals::get_trampoline_function(
    const yang::Type& function_type)
{
  auto& trampoline_map = get_instance()._trampoline_map;
  // We need an extra layer of caching to avoid optimising the function
  // every time we need it.
  auto it = trampoline_map.find(function_type.erase_user_types());
  if (it != trampoline_map.end()) {
    return (yang::void_fp)(std::intptr_t)
        get_instance()._engine->getPointerToFunction(it->second);
  }

  llvm::Function* function =
      get_instance()._common.get_trampoline_function(function_type, true);
  get_instance()._common.optimise_ir(function);
  trampoline_map[function_type.erase_user_types()] = function;
  auto ptr = (yang::void_fp)(std::intptr_t)
      get_instance()._engine->getPointerToFunction(function);
  return ptr;
}

YangTrampolineGlobals::YangTrampolineGlobals()
  : _module(create_module())
  , _engine(llvm::EngineBuilder(_module).setErrorStr(&_error).create())
  , _common(*_module, *_engine)
{
  if (!_engine) {
    delete _module;
    throw yang::runtime_error("couldn't create execution engine: " + _error);
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

// End namespace yang::internal.
}
}
