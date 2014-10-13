//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/refcounting.h>

#include <sstream>
#include <yang/internals.h>
#include <yang/native.h>
#include "irval.h"
#include "memory.h"

// All of this is first draft and very naive.
// TODO: use "modern" reference counting instead. That is:
// - Each object has a reference count and a "dirty" bit
// - When a reference in an object is changed, if not dirty, write the old
//   values of all its references to global log array and set dirty bit. If
//   dirty, do nothing
// - Never update reference counts while program is running. Instead, when
//   we do GC, simply go through the log array and subtract one from each old
//   value, and add one to the current new value
// - Some details with object creation, don't both to log nulls, etc. Dirty bit
//   could be per-reference rather than per-object
// - Cycle detection by current algorithm, or by mark and sweep starting at
//   objects whose references have, since the last run, been decremented to a
//   nonzero value
// - Many more variations: see e.g.
//     http://www.cs.technion.ac.il/
//         users/wwwb/cgi-bin/tr-get.cgi/2006/PHD/PHD-2006-10.pdf
//     http://users.cecs.anu.edu.au/~steveb/downloads/pdf/rc-ismm-2012.pdf
//
// As well as being more efficient this will make TCO much easier by eliminating
// calls to refcount function when objects are going out of scope!
namespace {

void cleanup_cyclic_structures()
{
  typedef yang::internal::Prefix Prefix;
  // This algorithm is just about the simplest one possible. Something more
  // advanced should be implemented eventually, e.g.
  // http://researcher.watson.ibm.com/
  //     researcher/files/us-bacon/Paz05Efficient.pdf
  //
  // For each potential structure, we store the number of references to it among
  // such structures, and a reverse lookup of the structures that depend upon
  // it.
  struct reference_data_t {
    std::size_t count = 0;
    std::unordered_set<Prefix*> dependents;
  };
  std::unordered_map<Prefix*, reference_data_t> data;
  auto copy = yang::internal::get_structure_possible_cycle_list();

  // First, for each structure potentially involved in a cycle, call its
  // reference query function to determine what references it holds.
  for (Prefix* v : copy) {
    if (!v->vtable->refout_count) {
      continue;
    }
    // Query function takes a pointer to block of memory with space for the
    // pointers.
    Prefix** output = new Prefix*[v->vtable->refout_count];
    v->vtable->refout_query(v, output);

    // Update the data structure. The query function returns parent pointer as
    // well as function value environment pointers.
    for (std::size_t i = 0; i < v->vtable->refout_count; ++i) {
      ++data[output[i]].count;
      data[output[i]].dependents.insert(v);
    }
    delete[] output;
  }

  // Now, can collect any structure satisfying
  // (1) its total reference count matches the count we've seen
  // (2) *and* all of its dependents also satisfy condition (1).
  //
  // First form the set matching (1).
  std::unordered_set<Prefix*> matched;
  for (Prefix* v : copy) {
    if ((yang::int_t)data[v].count != v->refcount) {
      continue;
    }
    matched.insert(v);
  }

  // Then, iteratively trim the set down by preserving only those structures
  // matching condition (2) until we reach a fixed point.
  bool something_excluded = true;
  while (something_excluded) {
    something_excluded = false;
    std::unordered_set<Prefix*> preserve;
    for (Prefix* v : matched) {
      bool all_deps = true;
      for (Prefix* u : data[v].dependents) {
        if (!matched.count(u)) {
          all_deps = false;
          break;
        }
      }

      if (all_deps) {
        preserve.insert(v);
      }
      else {
        something_excluded = true;
      }
    }
    matched = preserve;
  }

  // Don't bother doing the delete here: just shove them into the cleanup
  // list and wait for the next time around.
  for (Prefix* v : matched) {
    yang::internal::get_structure_possible_cycle_list().erase(v);
    yang::internal::get_structure_cleanup_list().insert(v);
  }
}

// End anonymous namespace.
}

namespace yang {
namespace internal {

void* allocate_yang_structure(std::size_t size)
{
  return YANG_MALLOC(size);
}

std::unordered_set<Prefix*>& get_structure_cleanup_list()
{
  static std::unordered_set<Prefix*> cleanup_list;
  return cleanup_list;
}

std::unordered_set<Prefix*>& get_structure_possible_cycle_list()
{
  static std::unordered_set<Prefix*> possible_cycle_list;
  return possible_cycle_list;
}

void update_structure_refcount(Prefix* structure, int_t change)
{
  if (!structure) {
    return;
  }
  if (!(structure->refcount += change)) {
    get_structure_cleanup_list().insert(structure);
    get_structure_possible_cycle_list().erase(structure);
  }
  else {
    get_structure_cleanup_list().erase(structure);
    // A structure can only be involved in some cycle once its reference count
    // has been decremented at least once.
    if (change < 0) {
      get_structure_possible_cycle_list().insert(structure);
    }
  }
}

void cleanup_structures()
{
  std::unordered_set<Prefix*> already_freed;
  // Iteratively free everything that is not referenced, then everything
  // that isn't referenced after that, and so on.
  do {
    auto copy = get_structure_cleanup_list();
    get_structure_cleanup_list().clear();

    std::unordered_set<Prefix*> to_free;
    for (Prefix* v : copy) {
      // It's possible the iterative process will add something to the cleanup
      // list that we just freed, though, since this loop also frees cycles
      // detected by the cycle detector. That could be fixed.
      if (already_freed.count(v)) {
        continue;
      }
      already_freed.insert(v);

      // Call destructor.
      v->vtable->destructor(v);
      to_free.insert(v);
    }
    // Since current the cleanup list may contain a cycle of dependents, we must
    // call all of their destructors before freeing any: otherwise there are
    // undefined writes.
    for (Prefix* v : to_free) {
      YANG_FREE(v);
    }

    // This probably shouldn't be done so often? But, for destructor semantics,
    // it's nice for this function to be idempotent...
    cleanup_cyclic_structures();
  }
  while (!get_structure_cleanup_list().empty());
}

} // ::internal

std::size_t heap_objects_count()
{
  return internal::get_instrumented_heap().size();
}

std::string heap_dump()
{
  std::stringstream ss;
  ss << heap_objects_count() << " total object(s) allocated on Yang heap\n\n";
  for (internal::Prefix* v : internal::get_instrumented_heap()) {
    ss << "Object " << v << ":\n\thas vtable " << v->vtable <<
        "\n\thas reference count " << v->refcount <<
        "\n\thas parent object " << v->parent << "\n";

    if (!v->vtable->refout_count) {
      ss << "\n";
      continue;
    }

    internal::Prefix** output = new internal::Prefix*[v->vtable->refout_count];
    v->vtable->refout_query(v, output);
    for (std::size_t i = 0; i < v->vtable->refout_count; ++i) {
      ss << "\tholds a reference to object " << output[i] << "\n";
    }
    delete output;
    ss << "\n";
  }
  return ss.str();
}

} // ::yang
