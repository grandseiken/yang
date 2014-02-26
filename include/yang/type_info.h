//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_TYPE_INFO_H
#define YANG_INCLUDE_YANG_TYPE_INFO_H

#include "function.h"
#include "type.h"
#include "typedefs.h"

namespace yang {
class Context;

namespace internal {

// Currently, it's not clear how it would be possible to also support user
// functions taking arguments by const-ref, since the calling convention
// can't be known when Yang code makes the call. We could require const-ref
// arguments where it matters (vectors and functions); or maybe somehow wrap
// one in the other.
// TODO: can it be done with judicious std::remove_reference?
template<typename T>
struct TypeInfo {
  static_assert(sizeof(T) != sizeof(T), "use of unsupported type");

  // Avoid extra unnecessary error messages (beyond static assert above).
  yang::Type operator()(const Context&) const
  {
    return {};
  }

  yang::Type operator()() const
  {
    return {};
  }
};

template<>
struct TypeInfo<void> {
  yang::Type operator()(const Context&) const
  {
    return {};
  }

  yang::Type operator()() const
  {
    return {};
  }
};

template<>
struct TypeInfo<yang::int_t> {
  yang::Type operator()(const Context&) const
  {
    return operator()();
  }

  yang::Type operator()() const
  {
    yang::Type t;
    t._base = yang::Type::INT;
    return t;
  }
};

template<>
struct TypeInfo<yang::float_t> {
  yang::Type operator()(const Context&) const
  {
    return operator()();
  }

  yang::Type operator()() const
  {
    yang::Type t;
    t._base = yang::Type::FLOAT;
    return t;
  }
};

template<std::size_t N>
struct TypeInfo<ivec_t<N>> {
  yang::Type operator()(const Context&) const
  {
    return operator()();
  }

  yang::Type operator()() const
  {
    yang::Type t;
    t._base = yang::Type::INT;
    t._count = N;
    return t;
  }
};

template<std::size_t N>
struct TypeInfo<fvec_t<N>> {
  yang::Type operator()(const Context&) const
  {
    return operator()();
  }

  yang::Type operator()() const
  {
    yang::Type t;
    t._base = yang::Type::FLOAT;
    t._count = N;
    return t;
  }
};

// Templating TypeInfo on Function<R(Args...)> leads to ugly code, e.g. to
// retrieve a Function object for a function of Yang type int()():
//
//   auto f = instance.get_function<Function<Function<yang::int_t()>()>>("f");
//
// It's tempting to think we could template simply on R(Args...) instead, but
// that leads to the syntax:
//
//   auto f = instance.get_function<yang::int_t()()>("f");
//
// which, since C++ functions can only return *pointers* to other functions, is
// unfortunately illegal. For now, the only option right now is judicious use
// of typedefs (and perhaps a shorter typedef for Function).
template<typename R>
struct TypeInfo<Function<R()>> {
  yang::Type operator()(const Context& context) const
  {
    yang::Type t;
    t._base = yang::Type::FUNCTION;
    t._elements.push_back(TypeInfo<R>()(context));
    return t;
  }

  yang::Type operator()() const
  {
    yang::Type t;
    t._base = yang::Type::FUNCTION;
    t._elements.push_back(TypeInfo<R>()());
    return t;
  }
};

template<typename R, typename A, typename... Args>
struct TypeInfo<Function<R(A, Args...)>> {
  yang::Type operator()(const Context& context) const
  {
    yang::Type t = TypeInfo<Function<R(Args...)>>()(context);
    t._elements.insert(++t._elements.begin(), TypeInfo<A>()(context));
    return t;
  }

  yang::Type operator()() const
  {
    yang::Type t = TypeInfo<Function<R(Args...)>>()();
    t._elements.insert(++t._elements.begin(), TypeInfo<A>()());
    return t;
  }
};

// End namespace yang::internal.
}
}

#endif
