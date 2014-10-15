//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/instance.h>

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/IR/Module.h>
#include <yang/program.h>
#include <yang/runtime_error.h>

namespace yang {

Instance::Instance(const Program& program)
  : _global_data(nullptr)
  , _program(program._internals.get())
{
  if (!program.success()) {
    throw runtime_error(
        program._internals->name +
        ": instantiating program which did not compile successfully");
  }
  void* global_alloc = get_native_fp("!global_alloc");
  typedef internal::Prefix* (*alloc_fp)();
  _global_data = ((alloc_fp)(std::intptr_t)global_alloc)();
  _global_data->parent = (internal::Prefix*)program._internals.get();
  internal::update_structure_refcount((internal::Prefix*)_program, 1);
}

const std::unordered_map<std::string, Type>& Instance::get_functions() const
{
  return _program->functions;
}

const std::unordered_map<std::string, Global>& Instance::get_globals() const
{
  return _program->globals;
}

void* Instance::get_native_fp(const std::string& name) const
{
  return get_native_fp(_program->module->getFunction(name));
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
  return _program->engine->getPointerToFunction(ir_fp);
}

void Instance::check_global(const std::string& name, const Type& type,
                            bool for_modification) const
{
  auto it = _program->globals.find(name);
  if (it == _program->globals.end()) {
    throw runtime_error(
        _program->name + ": requested global `" + name + "` does not exist");
  }
  if (type != it->second.type) {
    throw runtime_error(
        _program->name + ": global `" +
        it->second.type.string(*_program->context)
        + " " + name + "` accessed via incompatible type `" +
        type.string(*_program->context) + "`");
  }
  if (for_modification && it->second.is_const) {
    throw runtime_error(
        _program->name + ": constant global `" +
        it->second.type.string(*_program->context) + " " + name +
        "` cannot be modified");
  }
}

void Instance::check_function(const std::string& name, const Type& type) const
{
  auto it = _program->functions.find(name);
  if (it == _program->functions.end()) {
    throw runtime_error(
        _program->name +
        ": requested function `" + name + "` does not exist");
  }
  if (type != it->second) {
    throw runtime_error(
        _program->name + ": function `" +
        it->second.string(*_program->context) +
        " " + name + "` accessed via incompatible type `" +
        type.string(*_program->context) + "`");
  }
}

} // ::yang
