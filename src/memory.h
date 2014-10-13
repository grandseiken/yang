//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_YANG_MEMORY_H
#define YANG_SRC_YANG_MEMORY_H

#include <cstddef>
#include <unordered_set>
#include <yang/refcounting.h>

namespace yang {
namespace internal {

// Instrumented memory allocation for debugging.
std::unordered_set<Prefix*>& get_instrumented_heap();
void* instrumented_malloc(std::size_t size);
void instrumented_free(void* ptr);

}} // ::yang::internal

#ifdef DEBUG
#define YANG_MALLOC yang::internal::instrumented_malloc
#define YANG_FREE yang::internal::instrumented_free
#else
#define YANG_MALLOC malloc
#define YANG_FREE free
#endif

#endif
