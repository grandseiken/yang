//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/native.h>

namespace yang {
  namespace internal {
    std::vector<NativeFunction<void>*> NativeFunction<void>::unreferenced;
  }
}
