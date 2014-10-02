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

template<typename>
struct ValueInitialise;

template<typename...>
struct TrampolineCallArgs;
template<typename>
struct TrampolineCallReturn;
template<typename, typename...>
struct TrampolineCall;

template<typename, typename>
struct ReverseTrampolineCallArgs;
template<typename, typename...>
struct ReverseTrampolineCallReturn;

// Prefix structure of all global data and closure structures.
struct Vtable;
struct Prefix {
  Prefix* parent;

  // Current reference count.
  int_t refcount;

  // Pointer to vtable containing (in order) destructor, outgoing reference
  // count and refcount query function.
  Vtable* vtable;

  // Start of the actual fields.
  void* data;
};

// Get list of structures which are to be cleaned up.
std::unordered_set<Prefix*>& get_structure_cleanup_list();
// Get list of structures which may be involved in a cycle.
std::unordered_set<Prefix*>& get_structure_possible_cycle_list();
// Do structure refcounting.
void update_structure_refcount(Prefix* structure, int_t change);
// Clean up structures that are no longer referenced.
void cleanup_structures();

// End namespace internal.
}

// Functions for debugging refcounting or memory issues.
std::size_t heap_objects_count();
std::string heap_dump();

// Refcount a managed user type on the C++ side.
template<typename T>
class Ref {
public:

  Ref(const Ref& ref);
  ~Ref();
  Ref& operator=(const Ref& ref);

  T& operator*() const;
  T* operator->() const;
  T* get() const;

private:

  template<typename...>
  friend struct internal::TrampolineCallArgs;
  template<typename>
  friend struct internal::TrampolineCallReturn;
  template<typename, typename...>
  friend struct internal::TrampolineCall;
  template<typename, typename>
  friend struct internal::ReverseTrampolineCallArgs;
  template<typename, typename...>
  friend struct internal::ReverseTrampolineCallReturn;
  friend struct internal::ValueInitialise<Ref>;

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
T& Ref<T>::operator*() const
{
  return *(T*)_wrap->data;
}

template<typename T>
T* Ref<T>::operator->() const
{
  return (T*)_wrap->data;
}

template<typename T>
T* Ref<T>::get() const
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

// End namespace yang.
}

#endif
