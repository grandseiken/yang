//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "irval.h"
#include <yang/type_info.h>

namespace yang {
namespace internal {

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

llvm::Type* Value::llvm_type() const
{
  return irval ? irval->getType() : nullptr;
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
  std::vector<llvm::Type*> types;
  // Pointer to function.
  types.push_back(void_ptr_type());
  // Pointer to environment (global data structure or closure structure).
  types.push_back(void_ptr_type());

  return llvm::StructType::get(b.getContext(), types);
}

llvm::Constant* Builder::constant_ptr(void* ptr)
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
  v.irval = b.CreateInsertValue(v.irval, cast, 0, "fptr");
  if (eptr) {
    // Must be bitcast to void pointer, since it may be a global data type or
    // closure data type..
    llvm::Value* cast = b.CreateBitCast(eptr, void_ptr_type());
    v.irval = b.CreateInsertValue(v.irval, cast, 1, "eptr");
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

llvm::Type* Builder::get_llvm_type(const yang::Type& t) const
{
  if (t.is_function()) {
    return gen_function_type();
  }
  if (t.is_int()) {
    return int_type();
  }
  if (t.is_float()) {
    return float_type();
  }
  if (t.is_int_vector()) {
    return int_vector_type(t.get_vector_size());
  }
  if (t.is_float_vector()) {
    return float_vector_type(t.get_vector_size());
  }
  if (t.is_user_type()) {
    return void_ptr_type();
  }
  return void_type();
}

// End namespace yang::internal.
}
}
