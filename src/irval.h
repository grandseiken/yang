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
  Value(const yang::Type& type, llvm::Value* irval);

  llvm::Type* llvm_type() const;

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
  llvm::Type* vector_type(llvm::Type* type, std::size_t n) const;

  // Constant construction.
  llvm::Constant* constant_ptr(void* ptr);
  llvm::Constant* constant_int(yang::int_t value) const;
  llvm::Constant* constant_float(yang::float_t value) const;
  llvm::Constant* constant_int_vector(yang::int_t value, std::size_t n) const;
  llvm::Constant*
      constant_float_vector(yang::float_t value, std::size_t n) const;

  llvm::IRBuilder<> b;
};

// End namespace yang::internal.
}
}

#endif
