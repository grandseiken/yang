//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "irval.h"

namespace yang {
namespace internal {

Value::Value(const yang::Type& type, llvm::Value* irval)
  : type(type)
  , irval(irval)
{
}

llvm::Type* Value::llvm_type() const
{
  return irval->getType();
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

llvm::Constant* Builder::constant_int(yang::int_t value) const
{
  return llvm::ConstantInt::getSigned(int_type(), value);
}

llvm::Constant* Builder::constant_float(yang::float_t value) const
{
  return llvm::ConstantFP::get(b.getContext(), llvm::APFloat(value));
}

llvm::Constant* Builder::constant_int_vector(
    yang::int_t value, std::size_t n) const
{
  return llvm::ConstantVector::getSplat(n, constant_int(value));
}

llvm::Constant* Builder::constant_float_vector(
    yang::float_t value, std::size_t n) const
{
  return llvm::ConstantVector::getSplat(n, constant_float(value));
}

// End namespace yang::internal.
}
}
