//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "irval.h"
#include <yang/type_info.h>

namespace yang {
namespace internal {

Value::Value(const yang::Type& type, llvm::Value* irval)
  : type(type)
  , irval(irval)
{
}

Value::Value(llvm::Value* irval)
  : type(yang::Type::void_t())
  , irval(irval)
{
}

Value::Value(const yang::Type& type)
  : type(type)
  , irval(nullptr)
{
}

llvm::Type* Value::llvm_type() const
{
  return irval ? irval->getType() : nullptr;
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

llvm::Type* Builder::vector_type(llvm::Type* type, std::size_t n) const
{
  return llvm::VectorType::get(type, n);
}

llvm::Type* Builder::generic_function_type(llvm::Type* type) const
{
  std::vector<llvm::Type*> types;
  // Yang function pointer or C++ pointer (which is not actually a function
  // pointer at all, but we have to store the type somehow; this is fairly
  // hacky).
  types.push_back(type);
  // Pointer to environment (global data structure or closure structure).
  types.push_back(void_ptr_type());

  return llvm::StructType::get(b.getContext(), types);
}

llvm::Type* Builder::generic_function_type(
    llvm::Type* return_type, const std::vector<llvm::Type*>& arg_types) const
{
  return generic_function_type(llvm::PointerType::get(
      llvm::FunctionType::get(return_type, arg_types, false), 0));
}

llvm::FunctionType* Builder::function_type_from_generic(
    llvm::Type* generic_function_type) const
{
  auto struct_type = (llvm::StructType*)generic_function_type;
  auto f_type = (*struct_type->element_begin())->getPointerElementType();
  return (llvm::FunctionType*)f_type;
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

llvm::Value* Builder::generic_function_value_null(
    llvm::StructType* generic_function_type) const
{
  std::vector<llvm::Constant*> values;
  values.push_back(llvm::ConstantPointerNull::get(
      (llvm::PointerType*)generic_function_type->getElementType(0)));
  values.push_back(llvm::ConstantPointerNull::get(void_ptr_type()));
  return llvm::ConstantStruct::get(generic_function_type, values);
}

llvm::Value* Builder::generic_function_value(
    llvm::Value* function_ptr, llvm::Value* env_ptr)
{
  auto type = (llvm::StructType*)generic_function_type(function_ptr->getType());
  llvm::Value* v = generic_function_value_null(type);

  v = b.CreateInsertValue(v, function_ptr, 0, "fptr");
  if (env_ptr) {
    // Must be bitcast to void pointer, since it may be a global data type or
    // closure data type..
    llvm::Value* cast = b.CreateBitCast(env_ptr, void_ptr_type());
    v = b.CreateInsertValue(v, cast, 1, "eptr");
  }
  return v;
}

llvm::Value* Builder::generic_function_value(const GenericFunction& function)
{
  void* fptr;
  void* eptr;
  function.ptr->get_representation(&fptr, &eptr);
  llvm::Type* ft = llvm::PointerType::get(
      function_type_from_generic(get_llvm_type(function.type)), 0);

  llvm::Value* v = b.CreateBitCast(constant_ptr(fptr), ft, "fun");
  if (!eptr) {
    // Native functions don't need an environment pointer.
    return generic_function_value(v, nullptr);
  }
  return generic_function_value(v, constant_ptr(eptr));
}

llvm::Type* Builder::get_llvm_type(const yang::Type& t) const
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

yang::Type Builder::get_yang_type(llvm::Type* t) const
{
  yang::Type r = yang::Type::void_t();
  if (t == void_ptr_type()) {
    // We can't reconstruct the full user type from the void pointer. This means
    // we treat all user-types as equivalent for the purposes of trampoline
    // function generation (which makes sense).
    r = yang::Type::user_t();
  }
  else if (t->isFunctionTy() || t->isStructTy()) {
    auto ft = t->isStructTy() ?
        function_type_from_generic(t) : (llvm::FunctionType*)t;
    std::vector<yang::Type> args;
    // Make sure to skip the environment pointer.
    for (std::size_t i = 0; i < ft->getFunctionNumParams() - 1; ++i) {
      args.push_back(get_yang_type(ft->getFunctionParamType(i)));
    }
    r = yang::Type::function_t(get_yang_type(ft->getReturnType()), args);
  }
  else if (t->isIntOrIntVectorTy()) {
    r = t->isVectorTy() ?
        yang::Type::int_vector_t(t->getVectorNumElements()) :
        yang::Type::int_t();
  }
  else if (t->isFPOrFPVectorTy()) {
    r = t->isVectorTy() ?
        yang::Type::float_vector_t(t->getVectorNumElements()) :
        yang::Type::float_t();
  }
  return r;
}


// End namespace yang::internal.
}
}
