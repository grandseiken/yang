//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_VALUE_H
#define YANG_INCLUDE_YANG_VALUE_H

#include <memory>
#include "error.h"
#include "type.h"
#include "type_info.h"

namespace yang {

// A generic value wrapper than can store a value of any Yang type.
// TODO: can this be used for anything useful? For example, calling Yang
// functions using only Values rather than explicit types.
// Context::register_namespace(const std::string&, const Instance&) is an
// example of the kind of place this could really help. Ideally that should
// be easily implementable in terms of the rest of the public interface of
// Context.
class Value {
public:

  template<typename T>
  Value(const T& t);
  inline ~Value();

  inline const Type& type() const;
  template<typename T>
  const T& as() const;

private:

  struct deleter_base {
    virtual void operator()(void* data) const = 0;
  };
  template<typename T>
  struct deleter : deleter_base {
    void operator()(void* data) const override;
  };

  Type _type;
  void* _data;
  std::unique_ptr<deleter_base> _deleter;

};

template<typename T>
Value::Value(const T& t)
  : _type(type_of<T>())
  , _data(new T(t))
  , _deleter(new deleter<T>)
{
}

Value::~Value()
{
  (*_deleter)(_data);
}

const Type& Value::type() const
{
  return _type;
}

template<typename T>
const T& Value::as() const
{
  Type type = type_of<T>();
  if (_type != type) {
    throw runtime_error(
        "accessed " + _type.string() + " value as " + type.string());
  }
  return *(T*)_data;
}

template<typename T>
void Value::deleter<T>::operator()(void* data) const
{
  delete (T*)data;
}

// End namespace yang.
}

#endif
