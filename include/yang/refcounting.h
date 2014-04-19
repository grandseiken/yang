//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_REFCOUNTING_H
#define YANG_INCLUDE_YANG_REFCOUNTING_H

#include <memory>
#include <string>
#include <unordered_set>
#include <unordered_map>
#include "typedefs.h"
#include "type.h"

namespace yang {
class Context;

namespace internal {
template<typename T>
class NativeFunction;
struct InstanceInternals;

// Prefix structure of all global data and closure structures.
struct Prefix {
  Prefix* parent;

  // Current reference count.
  int_t refcount;
  // Pointer to destructor.
  void (*free)(Prefix*);

  // Number of outbound references.
  int_t refouts;
  // Pointer to outbound reference query function.
  void (*query)(Prefix*, Prefix**);

  // Start of the actual fields.
  void* data;
};

// Get list of instance internals which are to be cleaned up.
std::unordered_set<InstanceInternals*>& get_instance_cleanup_list();
// Get list of structures which are to be cleaned up.
std::unordered_set<Prefix*>& get_structure_cleanup_list();
// Get list of structures which may be involved in a cycle.
std::unordered_set<Prefix*>& get_structure_possible_cycle_list();
// Do structure refcounting.
void update_structure_refcount(Prefix* structure, int_t change);
// Do function refcounting.
void update_function_refcount(NativeFunction<void>* target, int_t change);
// Clean up structures that are no longer referenced.
void cleanup_structures();
// Destroy program internals at some point.
void destroy_internals(InstanceInternals* global_parent);

// End namespace yang.
}

// Refcount a managed user type on the C++ side.
template<typename T>
class Ref {
public:

  Ref(const Ref& ref);
  ~Ref();
  Ref& operator=(const Ref& ref);
  T* operator->();

private:

  // Null Ref must never be returned to client code!
  Ref();
  Ref(internal::Prefix* wrap);
  internal::Prefix* _wrap;

};

template<typename T>
Ref<T>::Ref(const Ref& ref)
  : _wrap(ref._wrap)
{
  if (_wrap) {
    internal::update_structure_refcount(_wrap, 1);
  }
}

template<typename T>
Ref<T>::~Ref()
{
  if (_wrap) {
    internal::update_structure_refcount(_wrap, -1);
  }
}

template<typename T>
Ref<T>& Ref<T>::operator=(const Ref& ref)
{
  if (_wrap == ref._wrap) {
    return;
  }
  if (_wrap) {
    internal::update_structure_refcount(_wrap, -1);
  }
  _wrap = ref._wrap;
  if (_wrap) {
    internal::update_structure_refcount(_wrap, 1);
  }
}

template<typename T>
T* Ref<T>::operator->()
{
  return (T*)_wrap->data;
}

template<typename T>
Ref<T>::Ref()
  : _wrap(nullptr)
{
}

template<typename T>
Ref<T>::Ref(internal::Prefix* wrap)
  : _wrap(wrap)
{
  if (_wrap) {
    internal::update_structure_refcount(_wrap, 1);
  }
}

// End namespace internal.
}

#endif
