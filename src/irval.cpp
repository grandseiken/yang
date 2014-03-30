//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "irval.h"

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/IR/Module.h>
#include <yang/refcounting.h>
#include <yang/type_info.h>

namespace std {
  std::size_t hash<yang::internal::LexScope::metadata_t>::operator()(
      yang::internal::LexScope::metadata_t v) const
  {
    return v;
  }
}

namespace yang {
namespace internal {

Structure::entry::entry(const yang::Type& type, std::size_t index)
  : type(type)
  , index(index)
{
}

Structure::Structure()
  : type(nullptr)
  , destructor(nullptr)
  , refout_query(nullptr)
  , refout_count(0)
{
}

Value::Value()
  : type(yang::Type::void_t())
  , irval(nullptr)
{
}

Value::Value(const yang::Type& type)
  : type(type)
  , irval(nullptr)
{
}

Value::Value(const yang::Type& type, llvm::Value* irval)
  : type(type)
  , irval(irval)
{
}

Value::operator llvm::Value*() const
{
  return irval;
}
  
llvm::PointerType* Builder::void_ptr_type() const
{
  // LLVM doesn't have a built-in void pointer type, so just use a pointer
  // to whatever.
  return llvm::PointerType::get(int_type(), 0);
}

llvm::Type* Builder::void_type() const
{
  return llvm::Type::getVoidTy(b.getContext());
}

llvm::Type* Builder::int_type() const
{
  return llvm::IntegerType::get(
      b.getContext(), 8 * sizeof(yang::int_t));
}

llvm::Type* Builder::float_type() const
{
  return llvm::Type::getDoubleTy(b.getContext());
}

llvm::Type* Builder::int_vector_type(std::size_t n) const
{
  return llvm::VectorType::get(int_type(), n);
}

llvm::Type* Builder::float_vector_type(std::size_t n) const
{
  return llvm::VectorType::get(float_type(), n);
}

llvm::FunctionType* Builder::raw_function_type(const yang::Type& type) const
{
  std::vector<llvm::Type*> args;
  for (std::size_t i = 0; i < type.get_function_num_args(); ++i) {
    args.push_back(get_llvm_type(type.get_function_arg_type(i)));
  }
  args.push_back(void_ptr_type());

  return llvm::FunctionType::get(
      get_llvm_type(type.get_function_return_type()), args, false);
}

llvm::StructType* Builder::gen_function_type() const
{
  std::vector<llvm::Type*> types{void_ptr_type(), void_ptr_type()};
  return llvm::StructType::get(b.getContext(), types);
}

llvm::Constant* Builder::constant_ptr(void* ptr) const
{
  // To construct a constant pointer, we need to do a bit of machine-dependent
  // stuff.
  llvm::Type* int_ptr =
      llvm::IntegerType::get(b.getContext(), 8 * sizeof(ptr));
  llvm::Constant* const_int =
      llvm::ConstantInt::get(int_ptr, (std::size_t)ptr);
  return llvm::ConstantExpr::getIntToPtr(const_int, void_ptr_type());
}

Value Builder::constant_int(yang::int_t value) const
{
  return Value(yang::Type::int_t(),
               llvm::ConstantInt::getSigned(int_type(), value));
}

Value Builder::constant_float(yang::float_t value) const
{
  return Value(yang::Type::float_t(),
               llvm::ConstantFP::get(b.getContext(), llvm::APFloat(value)));
}

Value Builder::constant_int_vector(yang::int_t value, std::size_t n) const
{
  auto constant = (llvm::Constant*)constant_int(value).irval;
  return Value(yang::Type::int_vector_t(n),
               llvm::ConstantVector::getSplat(n, constant));
}

Value Builder::constant_float_vector(yang::float_t value, std::size_t n) const
{
  auto constant = (llvm::Constant*)constant_float(value).irval;
  return Value(yang::Type::float_vector_t(n),
               llvm::ConstantVector::getSplat(n, constant));
}

Value Builder::function_value_null(const yang::Type& function_type) const
{
  std::vector<llvm::Constant*> values;
  values.push_back(llvm::ConstantPointerNull::get(void_ptr_type()));
  values.push_back(llvm::ConstantPointerNull::get(void_ptr_type()));
  return Value(function_type,
               llvm::ConstantStruct::get(gen_function_type(), values));
}

Value Builder::function_value(const yang::Type& function_type,
                              llvm::Value* fptr, llvm::Value* eptr)
{
  Value v = function_value_null(function_type);
  llvm::Value* cast = b.CreateBitCast(fptr, void_ptr_type());
  v.irval = b.CreateInsertValue(v.irval, cast, 0);
  if (eptr) {
    // Must be bitcast to void pointer, since it may be a global data type or
    // closure data type..
    llvm::Value* cast = b.CreateBitCast(eptr, void_ptr_type());
    v.irval = b.CreateInsertValue(v.irval, cast, 1);
  }
  return v;
}

Value Builder::function_value(const GenericFunction& function)
{
  void* fptr;
  void* eptr;
  function.ptr->get_representation(&fptr, &eptr);

  if (!eptr) {
    // Native functions don't need an environment pointer.
    return function_value(function.type, constant_ptr(fptr), nullptr);
  }
  return function_value(function.type, constant_ptr(fptr), constant_ptr(eptr));
}

Value Builder::default_for_type(const yang::Type& type, int_t fill) const
{
  if (type.is_function()) {
    return function_value_null(type);
  }
  if (type.is_int()) {
    return constant_int(fill);
  }
  if (type.is_float()) {
    return constant_float(fill);
  }
  if (type.is_int_vector()) {
    return constant_int_vector(fill, type.get_vector_size());
  }
  if (type.is_float_vector()) {
    return constant_float_vector(fill, type.get_vector_size());
  }
  return Value(type, constant_ptr(nullptr));
}

llvm::Type* Builder::get_llvm_type(const yang::Type& type) const
{
  if (type.is_function()) {
    return gen_function_type();
  }
  if (type.is_int()) {
    return int_type();
  }
  if (type.is_float()) {
    return float_type();
  }
  if (type.is_int_vector()) {
    return int_vector_type(type.get_vector_size());
  }
  if (type.is_float_vector()) {
    return float_vector_type(type.get_vector_size());
  }
  if (type.is_user_type()) {
    return void_ptr_type();
  }
  return void_type();
}

llvm::Function* Builder::get_native_function(
    const std::string& name, yang::void_fp native_fp,
    llvm::FunctionType* type) const
{
  // We use special !-prefixed names for native functions so that they can't
  // be confused with regular user-defined functions (e.g. "malloc" is not
  // reserved).
  llvm::Function* llvm_function = llvm::Function::Create(
      type, llvm::Function::ExternalLinkage, "!" + name, &module);
  // We need to explicitly link the LLVM function to the native function.
  // More (technically) undefined behaviour here.
  engine.addGlobalMapping(llvm_function, (void*)(std::intptr_t)native_fp);
  return llvm_function;
}

LexScope::LexScope(Builder& builder, bool create_functions)
  : symbol_table(Value())
  , metadata(nullptr)
  , _b(builder)
  , _cleanup_structures(nullptr)
  , _destroy_internals(nullptr)
  , _update_refcount(nullptr)
{
  _rc_locals.emplace_back();
  if (!create_functions) {
    return;
  }

  _cleanup_structures = _b.get_native_function(
      "cleanup_structures", (void_fp)&cleanup_structures,
      llvm::FunctionType::get(_b.void_type(), false));
  _destroy_internals = _b.get_native_function(
      "destroy_internals", (void_fp)&::yang::internal::destroy_internals,
      llvm::FunctionType::get(_b.void_type(), _b.void_ptr_type(), false));

  // Create the internal general refcounting function.
  auto rc_type = llvm::FunctionType::get(
      _b.void_type(),
      std::vector<llvm::Type*>{_b.void_ptr_type(), _b.int_type()}, false);
  auto rc_function = _b.get_native_function(
      "rc_function", (void_fp)&update_function_refcount, rc_type);
  auto rc_structure = _b.get_native_function(
      "rc_structure", (void_fp)&update_structure_refcount, rc_type);

  std::vector<llvm::Type*> args{
      _b.void_ptr_type(), _b.void_ptr_type(), _b.int_type()};
  _update_refcount = llvm::Function::Create(
      llvm::FunctionType::get(_b.void_type(), args, false),
      llvm::Function::InternalLinkage, "!update_refcount", &_b.module);
  auto entry = llvm::BasicBlock::Create(
      _b.b.getContext(), "entry", _update_refcount);
  auto function_block = llvm::BasicBlock::Create(
      _b.b.getContext(), "function", _update_refcount);
  auto else_block = llvm::BasicBlock::Create(
      _b.b.getContext(), "else", _update_refcount);
  auto structure_block = llvm::BasicBlock::Create(
      _b.b.getContext(), "structure", _update_refcount);
  auto last_block = llvm::BasicBlock::Create(
      _b.b.getContext(), "last", _update_refcount);

  _b.b.SetInsertPoint(entry);
  auto fptr = _update_refcount->arg_begin();
  auto eptr = ++_update_refcount->arg_begin();
  auto change = ++++_update_refcount->arg_begin();
  llvm::Value* fval = _b.b.CreateAnd(
      _b.b.CreateIsNotNull(fptr), _b.b.CreateIsNull(eptr));
  llvm::Value* eval = _b.b.CreateIsNotNull(eptr);
  _b.b.CreateCondBr(fval, function_block, else_block);

  _b.b.SetInsertPoint(function_block);
  _b.b.CreateCall2(rc_function, fptr, change);
  _b.b.CreateRetVoid();

  _b.b.SetInsertPoint(else_block);
  _b.b.CreateCondBr(eval, structure_block, last_block);

  _b.b.SetInsertPoint(structure_block);
  _b.b.CreateCall2(rc_structure, eptr, change);
  _b.b.CreateRetVoid();

  _b.b.SetInsertPoint(last_block);
  _b.b.CreateRetVoid();
}

LexScope LexScope::next_lex_scope() const
{
  LexScope scope(_b, false);
  scope._cleanup_structures = _cleanup_structures;
  scope._destroy_internals = _destroy_internals;
  scope._update_refcount = _update_refcount;
  return scope;
}

void LexScope::push_scope(bool loop_scope)
{
  if (loop_scope) {
    _rc_loop_indices.emplace_back(_rc_locals.size());
  }
  _rc_locals.emplace_back();
  metadata.push();
  symbol_table.push();
}

void LexScope::pop_scope(bool loop_scope)
{
  dereference_scoped_locals();
  symbol_table.pop();
  metadata.pop();
  _rc_locals.pop_back();
  if (loop_scope) {
    _rc_loop_indices.pop_back();
  }
}

void LexScope::init_structure_type(
    const symbol_frame& symbols, bool global_data, bool has_parent)
{
  std::string name = global_data ? "global_data" : "closure";

  // Since the structure may contain pointers to functions which take the type
  // itself as an argument, the variable must be set (to an opaque type).
  auto struct_type =
      llvm::StructType::create(_b.b.getContext(), name);
  structure.type = llvm::PointerType::get(struct_type, 0);
  auto free_type =
      llvm::FunctionType::get(_b.void_type(), structure.type, false);
  std::vector<llvm::Type*> query_args{
      structure.type, llvm::PointerType::get(_b.void_ptr_type(), 0)};
  auto query_type = llvm::FunctionType::get(_b.void_type(), query_args, false);
  std::vector<llvm::Type*> type_list;

  // The first element in closure structures is a pointer to the parent
  // structure. When has_parent is false this will be null.
  type_list.push_back(_b.void_ptr_type());
  // The second element is a pointer to the reference counter.
  type_list.push_back(_b.int_type());
  // The third is a pointer to the destructor.
  type_list.push_back(llvm::PointerType::get(free_type, 0));
  // The fourth is the number of outgoing references this structure has.
  type_list.push_back(_b.int_type());
  // The fifth is the pointer to the reference query function.
  type_list.push_back(llvm::PointerType::get(query_type, 0));
  // The destructor and refcount information could totally be factored out into
  // some static "vtable" block if we wanted.

  std::size_t number = 5;
  for (const auto& pair : symbols) {
    // Type-calculation must be kept up-to-date with new types.
    type_list.push_back(_b.get_llvm_type(pair.second));
    structure.table[pair.first] = Structure::entry(pair.second, number++);
  }
  struct_type->setBody(type_list, false);

  // Create the destruction function.
  auto prev_block = _b.b.GetInsertBlock();
  structure.destructor = llvm::Function::Create(
      free_type, llvm::Function::InternalLinkage, "!free_" + name, &_b.module);
  auto free_block = llvm::BasicBlock::Create(
      _b.b.getContext(), "entry", structure.destructor);
  _b.b.SetInsertPoint(free_block);
  auto it = structure.destructor->arg_begin();
  it->setName("struct");
  // Make sure to decrement the reference count on each global variable on
  // destruction.
  for (const auto& pair : structure.table) {
    Value old = memory_load(
        pair.second.type, structure_ptr(&*it, pair.second.index));
    update_reference_count(old, -1);
  }
  if (has_parent) {
    llvm::Value* parent = memory_load(
        yang::Type::void_t(), structure_ptr(&*it, 0));
    if (global_data) {
      _b.b.CreateCall(_destroy_internals, parent);
    }
    else {
      update_reference_count(nullptr, parent, -1);
    }
  }
  _b.b.CreateRetVoid();

  // Create the reference query function.
  structure.refout_query = llvm::Function::Create(
      query_type, llvm::Function::InternalLinkage,
      "!query_" + name, &_b.module);
  auto query_block = llvm::BasicBlock::Create(
      _b.b.getContext(), "entry", structure.refout_query);
  it = structure.refout_query->arg_begin();
  auto jt = it;
  ++jt;
  it->setName("struct");
  jt->setName("output");

  // Output pointers for each function value environment and parent pointer.
  _b.b.SetInsertPoint(query_block);
  auto refout = [&](llvm::Value* v)
  {
    std::vector<llvm::Value*> indices{
        _b.constant_int(structure.refout_count++)};
    _b.b.CreateAlignedStore(v, _b.b.CreateGEP(jt, indices), 1);
  };

  for (const auto& pair : structure.table) {
    if (!pair.second.type.is_function()) {
      continue;
    }
    llvm::Value* f = memory_load(
        pair.second.type, structure_ptr(&*it, pair.second.index));
    refout(_b.b.CreateExtractValue(f, 1));
  }
  if (has_parent && !global_data) {
    refout(memory_load(yang::Type::void_t(), structure_ptr(&*it, 0)));
  }
  _b.b.CreateRetVoid();

  _b.b.SetInsertPoint(prev_block);
}

llvm::Value* LexScope::allocate_structure_value()
{
  llvm::Type* llvm_size_t =
      llvm::IntegerType::get(_b.b.getContext(), 8 * sizeof(std::size_t));

  _b.b.CreateCall(_cleanup_structures);
  auto malloc_ptr = _b.get_native_function(
      "malloc", (yang::void_fp)&malloc,
      llvm::FunctionType::get(structure.type, llvm_size_t, false));

  // Compute sizeof(type) by indexing one past the null pointer.
  llvm::Value* size_of =
      _b.b.CreateIntToPtr(_b.constant_int(0), structure.type);
  size_of = _b.b.CreateGEP(size_of, _b.constant_int(1));
  size_of = _b.b.CreatePtrToInt(size_of, llvm_size_t);

  // Call malloc, make sure refcounted memory is initialised, and return the
  // pointer.
  llvm::Value* v = _b.b.CreateCall(malloc_ptr, size_of);
  memory_store(Value(yang::Type::void_t(), _b.constant_ptr(nullptr)),
               structure_ptr(v, 0));

  memory_store(_b.constant_int(0), structure_ptr(v, 1));
  memory_store(Value(yang::Type::void_t(), structure.destructor),
               structure_ptr(v, 2));

  memory_store(_b.constant_int(structure.refout_count), structure_ptr(v, 3));
  memory_store(Value(yang::Type::void_t(), structure.refout_query),
                     structure_ptr(v, 4));
  for (const auto& pair : structure.table) {
    memory_init(_b.b, structure_ptr(v, pair.second.index));
  }
  return v;
}

llvm::Value* LexScope::allocate_closure_struct(llvm::Value* parent_ptr)
{
  llvm::Value* closure_value = allocate_structure_value();
  // Store parent pointer in the first slot.
  auto parent_void_ptr =
      _b.b.CreateBitCast(parent_ptr, _b.void_ptr_type());
  memory_store(Value(yang::Type::void_t(), parent_void_ptr),
               structure_ptr(closure_value, 0));
  update_reference_count(nullptr, parent_void_ptr, 1);
  // Set up the symbol-table for all the rest. If a closed variable "v"
  // appears in scope #1, for instance, we store "v/1" in the symbol table
  // with a pointer into the closure-structure.
  //
  // Then, when we reach the actual declaration of v in scope #1, we store
  // into "v/1" and copy the symbol table entry for "v/1" to "v" for that
  // scope and its children (as in the argument list code).
  for (const auto& pair : structure.table) {
    llvm::Value* ptr = structure_ptr(closure_value, pair.second.index);
    symbol_table.add(pair.first, Value(pair.second.type, ptr));
  }
  return closure_value;
}

llvm::Value* LexScope::structure_ptr(llvm::Value* ptr, std::size_t index)
{
  // The first index indexes the structure data pointer itself, i.e. to obtain
  // the one and only global data structure at that memory location.
  std::vector<llvm::Value*> indexes{_b.constant_int(0), _b.constant_int(index)};
  return _b.b.CreateGEP(ptr, indexes);
}

llvm::BasicBlock* LexScope::create_block(
    metadata_t meta, const std::string& name)
{
  auto parent = _b.b.GetInsertBlock() ?
      _b.b.GetInsertBlock()->getParent() : nullptr;
  auto block = llvm::BasicBlock::Create(_b.b.getContext(), name, parent);
  metadata.add(meta, block);
  return block;
}

llvm::BasicBlock* LexScope::get_block(metadata_t meta)
{
  return (llvm::BasicBlock*)metadata[meta];
}

Value LexScope::memory_load(const yang::Type& type, llvm::Value* ptr)
{
  // Loads out of the global data structure must be byte-aligned! I don't
  // entirely understand why, but leaving the default will segfault at random
  // sometimes for certain types (e.g. float vectors).
  return Value(type, _b.b.CreateAlignedLoad(ptr, 1));
}

void LexScope::memory_init(llvm::IRBuilder<>& pos, llvm::Value* ptr)
{
  // We need to make sure ref-counted memory locations are initialised with
  // something sensible. Otherwise, the first store will try to decrement
  // the refcount on something undefined. (Important in particular for variable
  // declarations which are "executed" more than once.)
  llvm::Type* elem = ptr->getType()->getPointerElementType();
  if (elem->isStructTy()) {
    // Null Yang function. Passing void as type is OK since it isn't used.
    pos.CreateAlignedStore(
        _b.function_value_null(yang::Type::void_t()), ptr, 1);
  }
}

void LexScope::memory_store(const Value& value, llvm::Value* ptr)
{
  Value old = memory_load(value.type, ptr);
  update_reference_count(old, -1);
  update_reference_count(value, 1);
  _b.b.CreateAlignedStore(value, ptr, 1);
}

void LexScope::refcount_init(const Value& value)
{
  _rc_locals.back().push_back(value);
}

void LexScope::update_reference_count(const Value& value, int_t change)
{
  if (!value.type.is_function()) {
    return;
  }
  // This reference counting will be inlined by the LLVM optimiser.
  llvm::Value* fptr = _b.b.CreateExtractValue(value, 0);
  llvm::Value* eptr = _b.b.CreateExtractValue(value, 1);
  update_reference_count(fptr, eptr, change);
}

void LexScope::update_reference_count(
    llvm::Value* fptr, llvm::Value* eptr, int_t change)
{
  fptr = fptr ?
      _b.b.CreateBitCast(fptr, _b.void_ptr_type()) : _b.constant_ptr(nullptr);
  eptr = eptr ?
      _b.b.CreateBitCast(eptr, _b.void_ptr_type()) : _b.constant_ptr(nullptr);
  std::vector<llvm::Value*> args{fptr, eptr, _b.constant_int(change)};
  _b.b.CreateCall(_update_refcount, args);
}

void LexScope::dereference_scoped_locals()
{
  dereference_scoped_locals(_rc_locals.size() - 1);
}

void LexScope::dereference_scoped_locals(std::size_t first_scope)
{
  for (std::size_t i = first_scope; i < _rc_locals.size(); ++i) {
    for (const Value& v : _rc_locals[i]) {
      if (!v.irval->getType()->isPointerTy()) {
        update_reference_count(v, -1);
        continue;
      }
      Value load = memory_load(v.type, v);
      update_reference_count(load, -1);
      memory_init(_b.b, v);
    }
  }
}

void LexScope::dereference_loop_locals()
{
  dereference_scoped_locals(_rc_loop_indices.back());
}

// End namespace yang::internal.
}
}
