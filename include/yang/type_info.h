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
namespace internal {

template<typename>
struct TypeInfo;
struct ContextInternals;

// TODO: should support C++ functions taking arguments by const reference, but
// it's a bit tricky. It has to be done in the reverse trampoline somehow,
// possibly by looking up the calling convention in the NativeFunction object?
template<typename T>
struct TypeInfoImpl {
  static_assert(sizeof(T) != sizeof(T), "use of unsupported type");

  // Avoid extra unnecessary error messages (beyond static assert above).
  yang::Type operator()(const ContextInternals&, bool = false) const
  {
    return yang::Type::void_t();
  }

  yang::Type operator()() const
  {
    return yang::Type::void_t();
  }
};

template<>
struct TypeInfoImpl<void> {
  yang::Type operator()(const ContextInternals&, bool = false) const
  {
    return yang::Type::void_t();
  }

  yang::Type operator()() const
  {
    return yang::Type::void_t();
  }
};

template<>
struct TypeInfoImpl<int_t> {
  yang::Type operator()(const ContextInternals&, bool = false) const
  {
    return yang::Type::int_t();
  }

  yang::Type operator()() const
  {
    return yang::Type::int_t();
  }
};

template<>
struct TypeInfoImpl<float_t> {
  yang::Type operator()(const ContextInternals&, bool = false) const
  {
    return yang::Type::float_t();
  }

  yang::Type operator()() const
  {
    return yang::Type::float_t();
  }
};

template<std::size_t N>
struct TypeInfoImpl<ivec_t<N>> {
  yang::Type operator()(const ContextInternals&, bool = false) const
  {
    return yang::Type::int_vector_t(N);
  }

  yang::Type operator()() const
  {
    return yang::Type::int_vector_t(N);
  }
};

template<std::size_t N>
struct TypeInfoImpl<fvec_t<N>> {
  yang::Type operator()(const ContextInternals&, bool = false) const
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
  void operator()(const std::vector<yang::Type>&,
                  const ContextInternals&, bool = false) const {}
  void operator()(const std::vector<yang::Type>&) const {}
};
template<typename A, typename... Args>
struct ArgsTypeInfo<A, Args...> {
  void operator()(std::vector<yang::Type>& output,
                  const ContextInternals& context,
                  bool ignore_managed_mismatch = false) const
  {
    output.push_back(TypeInfo<A>()(context, ignore_managed_mismatch));
    ArgsTypeInfo<Args...>()(output, context, ignore_managed_mismatch);
  }

  void operator()(std::vector<yang::Type>& output) const
  {
    output.push_back(TypeInfo<A>()());
    ArgsTypeInfo<Args...>()(output);
  }
};

template<typename R, typename... Args>
struct TypeInfoImpl<Function<R(Args...)>> {
  yang::Type operator()(const ContextInternals& context,
                        bool ignore_managed_mismatch = false) const
  {
    std::vector<yang::Type> args;
    ArgsTypeInfo<Args...>()(args, context, ignore_managed_mismatch);
    return yang::Type::function_t(
        TypeInfo<R>()(context, ignore_managed_mismatch), args);
  }

  yang::Type operator()() const
  {
    std::vector<yang::Type> args;
    ArgsTypeInfo<Args...>()(args);
    return yang::Type::function_t(TypeInfo<R>()(), args);
  }
};

// Wrapper in case we need to add other modifiers.
template<typename T>
struct TypeInfoBase {
  yang::Type operator()(const ContextInternals& context,
                        bool ignore_managed_mismatch = false) const
  {
    return TypeInfoImpl<T>()(context, ignore_managed_mismatch);
  }

  yang::Type operator()() const
  {
    return TypeInfoImpl<T>()();
  }
};
template<typename T>
struct TypeInfo : TypeInfoBase<T> {};

// End namespace yang::internal.
}
}

#endif
