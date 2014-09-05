//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/instance.h>

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/IR/Module.h>
#include <yang/program.h>

namespace yang {

Instance::Instance(const Program& program)
  : _internals(nullptr)
  , _global_data(nullptr)
{
  if (!program.success()) {
    throw runtime_error(
        program._internals->name +
        ": instantiating program which did not compile successfully");
  }
  _internals = new internal::InstanceInternals{program._internals};

  void* global_alloc = get_native_fp("!global_alloc");
  typedef void* (*alloc_fp)();
  _global_data = ((alloc_fp)(std::intptr_t)global_alloc)();

  *(void**)_global_data = _internals;
  internal::update_structure_refcount((internal::Prefix*)_global_data, 1);
}

Instance::~Instance()
{
  internal::update_structure_refcount((internal::Prefix*)_global_data, -1);
}

Instance::Instance(const Instance& instance)
  : _internals(instance._internals)
  , _global_data(instance._global_data)
{
  internal::update_structure_refcount((internal::Prefix*)_global_data, 1);
}

Instance& Instance::operator=(const Instance& instance) 
{
  if (this == &instance) {
    return *this;
  }
  internal::update_structure_refcount((internal::Prefix*)_global_data, -1);
  _internals = instance._internals;
  _global_data = instance._global_data;
  internal::update_structure_refcount((internal::Prefix*)_global_data, 1);
  return *this;
}

const function_table& Instance::get_functions() const
{
  return _internals->program->functions;
}

const global_table& Instance::get_globals() const
{
  return _internals->program->globals;
}

void* Instance::get_native_fp(const std::string& name) const
{
  return get_native_fp(_internals->program->module->getFunction(name));
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
  return _internals->program->engine->getPointerToFunction(ir_fp);
}

void Instance::check_global(const std::string& name, const Type& type,
                            bool for_modification) const
{
  auto it = _internals->program->globals.find(name);
  if (it == _internals->program->globals.end()) {
    throw runtime_error(
        _internals->program->name +
        ": requested global `" + name + "` does not exist");
  }
  if (type != it->second.type) {
    throw runtime_error(
        _internals->program->name + ": global `" +
        it->second.type.string(*_internals->program->context) + " " + name +
        "` accessed via incompatible type `" +
        type.string(*_internals->program->context) + "`");
  }
  if (for_modification && it->second.is_const) {
    throw runtime_error(
        _internals->program->name + ": constant global `" +
        it->second.type.string(*_internals->program->context) + " " + name +
        "` cannot be modified");
  }
}

void Instance::check_function(const std::string& name, const Type& type) const
{
  auto it = _internals->program->functions.find(name);
  if (it == _internals->program->functions.end()) {
    throw runtime_error(
        _internals->program->name +
        ": requested function `" + name + "` does not exist");
  }
  if (type != it->second) {
    throw runtime_error(
        _internals->program->name + ": function `" +
        it->second.string(*_internals->program->context) +
        " " + name + "` accessed via incompatible type `" +
        type.string(*_internals->program->context) + "`");
  }
}

// End namespace yang.
}
