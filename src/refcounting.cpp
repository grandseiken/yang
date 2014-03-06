//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/native.h>
#include <yang/pipeline.h>
#include <yang/refcounting.h>

// All of this is a first draft and probably very naive.
namespace yang {
namespace internal {

std::unordered_set<InstanceInternals*>& get_instance_cleanup_list()
{
  static std::unordered_set<InstanceInternals*> cleanup_list;
  return cleanup_list;
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
  auto t = (Prefix*)structure;
  if (!(t->refcount += change)) {
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

void update_function_refcount(NativeFunction<void>* target, int_t change)
{
  while (change > 0) {
    target->take_reference();
    --change;
  }
  while (change < 0) {
    target->release_reference();
    ++change;
  }
}

void update_refcount(
    NativeFunction<void>* target, Prefix* structure, int_t change)
{
  if (target && !structure) {
    update_function_refcount(target, change);
  }
  else if (structure) {
    update_structure_refcount(structure, change);
  }
}

void cleanup_cyclic_structures()
{
  // This algorithm is just about the simplest one possible. Something more
  // advanced should be implemented eventually, e.g.
  // http://researcher.watson.ibm.com/
  //     researcher/files/us-bacon/Paz05Efficient.pdf
  //
  // For each potential structure, we store the number of references to it among
  // such structures, and a reverse lookup of the structures that depend upon
  // it.
  struct reference_data_t {
    std::size_t count;
    std::unordered_set<Prefix*> dependents;
  };
  std::unordered_map<Prefix*, reference_data_t> data;
  auto copy = get_structure_possible_cycle_list();

  // First, for each structure potentially involved in a cycle, call its
  // reference query function to determine what references it holds.
  for (Prefix* v : copy) {
    // Query function takes a pointer to block of memory with space for the
    // pointers.
    Prefix** output = new Prefix*[v->refouts];
    v->query(v, output);

    // Update the data structure. The query function returns parent pointer as
    // well as function value environment pointers.
    for (int_t i = 0; i < v->refouts; ++i) {
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
    if ((int_t)data[v].count != v->refcount) {
      continue;
    }
    matched.insert(v);
  }

  // Then, repeatedly trim the set down by preserving only those structures
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

  // Don't bother doing the delete here: simply shove them into the cleanup
  // list and wait for the next time around.
  for (Prefix* v : matched) {
    get_structure_possible_cycle_list().erase(v);
    get_structure_cleanup_list().insert(v);
  }
}

void cleanup_structures()
{
  std::unordered_set<Prefix*> already_freed;
  // Iteratively free everything that is not referenced, then everything
  // that isn't referenced after that, and so on.
  while (!get_structure_cleanup_list().empty()) {
    auto copy = get_structure_cleanup_list();
    get_structure_cleanup_list().clear();

    for (Prefix* v : copy) {
      // It's possible the iterative process will add something to the cleanup
      // list that we just freed, though, since this loop also frees cycles
      // detected by the cycle detector. That could be fixed.
      if (already_freed.count(v)) {
        continue;
      }
      already_freed.insert(v);

      // Call destructor.
      v->free(v);
      free(v);
    }
  }
  // This definitely shouldn't be done so often.
  cleanup_cyclic_structures();
  // Clean up program internals.
  for (InstanceInternals* internals : get_instance_cleanup_list()) {
    delete internals;
  }
  get_instance_cleanup_list().clear();
}

void destroy_internals(InstanceInternals* global_parent)
{
  get_instance_cleanup_list().insert(global_parent);
}

// End namespace yang::internal.
}
}
