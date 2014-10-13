//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_TABLE_H
#define YANG_SRC_TABLE_H

#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace yang {
namespace internal {

template<typename K, typename V>
class SymbolTable {
public:

  typedef std::pair<K, V> pair;

  SymbolTable();
  explicit SymbolTable(const V& default_value);

  // Push or pop a frame from the symbol table.
  void push();
  void pop();
  // Depth of frames in the symbol table. Guaranteed to be at least 1.
  std::size_t size() const;

  // Add to the top frame, or remove from the top frame in which it appears.
  void add(const K& symbol, const V& v);
  void remove(const K& symbol);

  // Add or remove from an arbitrary frame.
  void add(const K& symbol, std::size_t frame, const V& v);
  void remove(const K& symbol, std::size_t frame);

  // Symbol existence for whole table or a particular frame.
  bool has(const K& symbol) const;
  bool has(const K& symbol, std::size_t frame) const;

  // Get list of symbols in a particular frame-range [min_frame, max_frame).
  void get_symbols(std::vector<pair>& output,
                   std::size_t min_frame, std::size_t max_frame) const;

  // Get index of the topmost frame in which the symbol is defined.
  std::size_t index(const K& symbol) const;

  // Value retrieval.
  const V& operator[](const K& symbol) const;
  /***/ V& operator[](const K& symbol);
  const V& get(const K& symbol, std::size_t frame) const;
  /***/ V& get(const K& symbol, std::size_t frame);

private:

  V _default;
  typedef std::unordered_map<K, V> scope;
  std::vector<scope> _stack;

};

template<typename K, typename V>
SymbolTable<K, V>::SymbolTable()
{
  push();
}

template<typename K, typename V>
SymbolTable<K, V>::SymbolTable(const V& default_value)
  : _default(default_value)
{
  push();
}

template<typename K, typename V>
void SymbolTable<K, V>::push()
{
  _stack.emplace_back();
}

template<typename K, typename V>
void SymbolTable<K, V>::pop()
{
  if (size() > 1) {
    _stack.pop_back();
  }
}

template<typename K, typename V>
std::size_t SymbolTable<K, V>::size() const
{
  return _stack.size();
}

template<typename K, typename V>
void SymbolTable<K, V>::add(const K& symbol, const V& v)
{
  _stack.back().emplace(symbol, v);
}

template<typename K, typename V>
void SymbolTable<K, V>::remove(const K& symbol)
{
  for (auto it = _stack.rbegin(); it != _stack.rend(); ++it) {
    auto jt = it->find(symbol);
    if (jt != it->end()) {
      it->erase(jt);
      return;
    }
  }
}

template<typename K, typename V>
void SymbolTable<K, V>::add(const K& symbol, std::size_t frame, const V& v)
{
  if (frame < size()) {
    _stack[frame].emplace(symbol, v);
  }
}

template<typename K, typename V>
void SymbolTable<K, V>::remove(const K& symbol, std::size_t frame)
{
  if (frame < size()) {
    _stack[frame].erase(symbol);
  }
}

template<typename K, typename V>
bool SymbolTable<K, V>::has(const K& symbol) const
{
  for (auto it = _stack.rbegin(); it != _stack.rend(); ++it) {
    if (it->find(symbol) != it->end()) {
      return true;
    }
  }
  return false;
}

template<typename K, typename V>
bool SymbolTable<K, V>::has(const K& symbol, std::size_t frame) const
{
  return frame < size() && _stack[frame].find(symbol) != _stack[frame].end();
}

template<typename K, typename V>
void SymbolTable<K, V>::get_symbols(
    std::vector<pair>& output,
    std::size_t min_frame, std::size_t max_frame) const
{
  for (std::size_t i = min_frame; i < max_frame && i < size(); ++i) {
    for (const auto& pair : _stack[i]) {
      output.push_back(pair);
    }
  }
}

template<typename K, typename V>
std::size_t SymbolTable<K, V>::index(const K& symbol) const
{
  std::size_t i = size();
  while (i) {
    --i;
    auto jt = _stack[i].find(symbol);
    if (jt != _stack[i].end()) {
      return i;
    }
  }
  return _stack.size();
}

template<typename K, typename V>
const V& SymbolTable<K, V>::operator[](const K& symbol) const
{
  for (auto it = _stack.rbegin(); it != _stack.rend(); ++it) {
    auto jt = it->find(symbol);
    if (jt != it->end()) {
      return jt->second;
    }
  }
  return _default;
}

template<typename K, typename V>
V& SymbolTable<K, V>::operator[](const K& symbol)
{
  for (auto it = _stack.rbegin(); it != _stack.rend(); ++it) {
    auto jt = it->find(symbol);
    if (jt != it->end()) {
      return jt->second;
    }
  }
  return _default;
}

template<typename K, typename V>
const V& SymbolTable<K, V>::get(const K& symbol, std::size_t frame) const
{
  if (frame >= size()) {
    return _default;
  }
  auto it = _stack[frame].find(symbol);
  if (it != _stack[frame].end()) {
    return it->second;
  }
  return _default;
}

template<typename K, typename V>
V& SymbolTable<K, V>::get(const K& symbol, std::size_t frame)
{
  if (frame >= size()) {
    return _default;
  }
  auto it = _stack[frame].find(symbol);
  if (it != _stack[frame].end()) {
    return it->second;
  }
  return _default;
}

}} // ::yang::internal

#endif
