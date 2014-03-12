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
    return yang::Type::void_t();
  }

  yang::Type operator()() const
  {
    return yang::Type::void_t();
  }
};

template<>
struct TypeInfo<void> {
  yang::Type operator()(const Context&) const
  {
    return yang::Type::void_t();
  }

  yang::Type operator()() const
  {
    return yang::Type::void_t();
  }
};

template<>
struct TypeInfo<yang::int_t> {
  yang::Type operator()(const Context&) const
  {
    return yang::Type::int_t();
  }

  yang::Type operator()() const
  {
    return yang::Type::int_t();
  }
};

template<>
struct TypeInfo<yang::float_t> {
  yang::Type operator()(const Context&) const
  {
    return yang::Type::float_t();
  }

  yang::Type operator()() const
  {
    return yang::Type::float_t();
  }
};

template<std::size_t N>
struct TypeInfo<ivec_t<N>> {
  yang::Type operator()(const Context&) const
  {
    return yang::Type::int_vector_t(N);
  }

  yang::Type operator()() const
  {
    return yang::Type::int_vector_t(N);
  }
};

template<std::size_t N>
struct TypeInfo<fvec_t<N>> {
  yang::Type operator()(const Context&) const
  {
    return yang::Type::float_vector_t(N);
  }

  yang::Type operator()() const
  {
    return yang::Type::float_vector_t(N);
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
template<typename...>
struct ArgsTypeInfo {};
template<>
struct ArgsTypeInfo<> {
  void operator()(const Context&, const std::vector<yang::Type>&) const {}
  void operator()(const std::vector<yang::Type>&) const {}
};
template<typename A, typename... Args>
struct ArgsTypeInfo<A, Args...> {
  void operator()(const Context& context, std::vector<yang::Type>& output) const
  {
    output.push_back(TypeInfo<A>()(context));
    ArgsTypeInfo<Args...>()(context, output);
  }

  void operator()(std::vector<yang::Type>& output) const
  {
    output.push_back(TypeInfo<A>()());
    ArgsTypeInfo<Args...>()(output);
  }
};

template<typename R, typename... Args>
struct TypeInfo<Function<R(Args...)>> {
  yang::Type operator()(const Context& context) const
  {
    std::vector<yang::Type> args;
    ArgsTypeInfo<Args...>()(context, args);
    return yang::Type::function_t(TypeInfo<R>()(context), args);
  }

  yang::Type operator()() const
  {
    std::vector<yang::Type> args;
    ArgsTypeInfo<Args...>()(args);
    return yang::Type::function_t(TypeInfo<R>()(), args);
  }
};

// End namespace yang::internal.
}
}

#endif
