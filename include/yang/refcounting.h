//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_REFCOUNTING_H
#define YANG_INCLUDE_YANG_REFCOUNTING_H

#include <string>
#include <type_traits>
#include <unordered_set>
#include "typedefs.h"
#include "type.h"

namespace yang {
namespace internal {
template<typename>
struct Raw;

// Refcounting structures.
struct StaticDataEntry {
  virtual ~StaticDataEntry() {}
};

struct Prefix;
struct Vtable : StaticDataEntry {
  typedef void (*destructor_t)(Prefix*);
  typedef void (*refout_query_t)(Prefix*, Prefix**);
  Vtable(destructor_t dtor, std::size_t refout_count, refout_query_t query);

  // Destructor pointer.
  destructor_t destructor;

  // Outgoing reference count and query function.
  std::size_t refout_count;
  refout_query_t refout_query;
};

// Prefix structure of all global data and closure structures.
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

void* allocate_yang_structure(std::size_t size);
// Get list of structures which are to be cleaned up.
std::unordered_set<Prefix*>& get_structure_cleanup_list();
// Get list of structures which may be involved in a cycle.
std::unordered_set<Prefix*>& get_structure_possible_cycle_list();
// Do structure refcounting.
void update_structure_refcount(Prefix* structure, int_t change);
// Clean up structures that are no longer referenced.
void cleanup_structures();

// Hooks into the yang reference-counting runtime. The type T must have the
// Prefix type as a prefix.
template<typename T>
class RefcountHook {
public:

  RefcountHook();
  RefcountHook(T* structure);
  ~RefcountHook();

  RefcountHook(const RefcountHook& hook);
  RefcountHook& operator=(const RefcountHook& hook);

  RefcountHook(RefcountHook&& hook);
  RefcountHook& operator=(RefcountHook&& hook);

  T& operator*() const;
  T* operator->() const;
  T* get() const;

  bool operator==(const RefcountHook& hook) const;
  bool operator!=(const RefcountHook& hook) const;

private:

  static void destructor(Prefix* structure);
  static Vtable vtable;
  T* _structure;

};

template<typename T>
RefcountHook<T>::RefcountHook()
  : _structure(nullptr)
{
  static_assert(!std::is_same<typename std::remove_cv<T>::type, Prefix>::value,
                "opaque Prefixes should not be allocated in C++");
  cleanup_structures();
  _structure = new (allocate_yang_structure(sizeof(T))) T;
  _structure->parent = nullptr;
  _structure->refcount = 0;
  _structure->vtable = &vtable;
  update_structure_refcount((Prefix*)_structure, 1);
}

template<typename T>
RefcountHook<T>::RefcountHook(T* structure)
  : _structure(structure)
{
  if (_structure) {
    update_structure_refcount((Prefix*)_structure, 1);
  }
}

template<typename T>
RefcountHook<T>::~RefcountHook()
{
  if (_structure) {
    update_structure_refcount((Prefix*)_structure, -1);
  }
}

template<typename T>
RefcountHook<T>::RefcountHook(const RefcountHook& hook)
  : RefcountHook(hook._structure)
{
}

template<typename T>
RefcountHook<T>& RefcountHook<T>::operator=(const RefcountHook& hook)
{
  if (this == &hook) {
    return *this;
  }
  if (_structure) {
    update_structure_refcount((Prefix*)_structure, -1);
  }
  if ((_structure = hook._structure)) {
    update_structure_refcount((Prefix*)_structure, 1);
  }
  return *this;
}

template<typename T>
RefcountHook<T>::RefcountHook(RefcountHook&& hook)
  : _structure(hook._structure)
{
  hook._structure = nullptr;
}

template<typename T>
RefcountHook<T>& RefcountHook<T>::operator=(RefcountHook&& hook)
{
  std::swap(_structure, hook._structure);
  return *this;
}

template<typename T>
T& RefcountHook<T>::operator*() const
{
  return *_structure;
}

template<typename T>
T* RefcountHook<T>::operator->() const
{
  return _structure;
}

template<typename T>
T* RefcountHook<T>::get() const
{
  return _structure;
}

template<typename T>
bool RefcountHook<T>::operator==(const RefcountHook& hook) const
{
  return _structure == hook._structure;
}

template<typename T>
bool RefcountHook<T>::operator!=(const RefcountHook& hook) const
{
  return !operator==(hook);
}

template<typename T>
void RefcountHook<T>::destructor(Prefix* structure)
{
  ((T*)structure)->~T();
}

template<typename T>
Vtable RefcountHook<T>::vtable(destructor, 0, nullptr);

// End namespace internal.
}

// Functions for debugging refcounting or memory issues.
std::size_t heap_objects_count();
std::string heap_dump();

// Refcount a managed user type on the C++ side.
template<typename T>
class Ref {
public:

  T& operator*() const;
  T* operator->() const;
  T* get() const;

private:

  friend struct internal::Raw<Ref>;
  Ref(internal::Prefix* wrap);
  internal::RefcountHook<internal::Prefix> _wrap;

};

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
Ref<T>::Ref(internal::Prefix* wrap)
  : _wrap(wrap)
{
}

// End namespace yang.
}

#endif
