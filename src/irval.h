//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_IRVAL_H
#define YANG_SRC_IRVAL_H

#include <yang/typedefs.h>
#include <yang/type.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>

namespace llvm {
  class Type;
  class Value;
}

namespace yang {
namespace internal {

// A Yang value wraps an LLVM IR value and associates it with a Yang type,
// instead of a raw LLVM type.
struct Value {
  Value();
  Value(const yang::Type& type);
  Value(const yang::Type& type, llvm::Value* irval);

  llvm::Type* llvm_type() const;
  operator llvm::Value*() const;

  yang::Type type;
  llvm::Value* irval;
};

// Wraps an LLVM IR builder with some convenience functions.
struct Builder {
  // LLVM type construction.
  llvm::PointerType* void_ptr_type() const;
  llvm::Type* void_type() const;
  llvm::Type* int_type() const;
  llvm::Type* float_type() const;
  llvm::Type* int_vector_type(std::size_t n) const;
  llvm::Type* float_vector_type(std::size_t n) const;

  // Functions.
  llvm::FunctionType* raw_function_type(llvm::Type* gen_function_type) const;
  llvm::Type* gen_function_type(llvm::Type* raw_function_type) const;

  // Value construction.
  llvm::Constant* constant_ptr(void* ptr);
  Value constant_int(yang::int_t value) const;
  Value constant_float(yang::float_t value) const;
  Value constant_int_vector(yang::int_t value, std::size_t n) const;
  Value constant_float_vector(yang::float_t value, std::size_t n) const;

  // Functions.
  Value function_value_null(const yang::Type& function_type) const;
  Value function_value(const yang::Type& function_type,
                       llvm::Value* fptr, llvm::Value* ptr);
  Value function_value(const GenericFunction& function);

  // Convert back and forth between equivalent Yang and LLVM types.
  llvm::Type* get_llvm_type(const yang::Type& t) const;
  yang::Type get_yang_type(llvm::Type* t) const;

  // Builder functions.
  llvm::IRBuilder<> b;

};

// End namespace yang::internal.
}
}

#endif
