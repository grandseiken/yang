//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/function.h>
#include "ircommon.h"

namespace yang {
namespace internal {

std::unordered_set<void*>& get_structure_cleanup_list()
{
  static std::unordered_set<void*> cleanup_list;
  return cleanup_list;
}

void update_structure_refcount(void* structure, int_t change)
{
  struct prefix {
    void* parent;
    int_t refcount;
    void_fp free;
  };

  auto t = (prefix*)structure;
  if (!(t->refcount += change)) {
    get_structure_cleanup_list().insert(structure);
  }
  else {
    get_structure_cleanup_list().erase(structure);
  }
}

yang::void_fp get_global_trampoline_function(const yang::Type& type)
{
  return YangTrampolineGlobals::get_trampoline_function(type);
}

// End namespace yang::internal.
}
}
