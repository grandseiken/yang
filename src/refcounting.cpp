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

void cleanup_cyclic_structures()
{
  std::unordered_map<Prefix*, std::size_t> references;
  std::unordered_map<Prefix*, std::unordered_set<Prefix*>> dependents;
  auto copy = get_structure_possible_cycle_list();

  for (Prefix* v : copy) {
    Prefix** output = new Prefix*[v->refouts];
    v->query(v, output);

    ++references[v->parent];
    dependents[v->parent].insert(v);
    for (int_t i = 0; i < v->refouts; ++i) {
      ++references[output[i]];
      dependents[output[i]].insert(v);
    }
    delete[] output;
  }

  std::unordered_set<Prefix*> matched;
  for (Prefix* v : copy) {
    if ((int_t)references[v] != v->refcount) {
      continue;
    }
    matched.insert(v);
  }

  bool something_excluded = false;
  do {
    something_excluded = false;
    std::unordered_set<Prefix*> preserve;
    for (Prefix* v : matched) {
      bool all_deps = true;
      for (Prefix* u : dependents[v]) {
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
  while (something_excluded);

  for (Prefix* v : matched) {
    get_structure_possible_cycle_list().erase(v);
    get_structure_cleanup_list().insert(v);
  }
}

void cleanup_structures()
{
  std::unordered_set<Prefix*> already_freed;
  while (!get_structure_cleanup_list().empty()) {
    auto copy = get_structure_cleanup_list();
    get_structure_cleanup_list().clear();
    std::unordered_set<Prefix*> to_free;

    for (Prefix* v : copy) {
      if (already_freed.count(v)) {
        continue;
      }
      already_freed.insert(v);
      to_free.insert(v);
      v->free(v);
    }
    for (Prefix* v : to_free) {
      free(v);
    }
  }
  // This probably shouldn't be done so often.
  cleanup_cyclic_structures();
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
