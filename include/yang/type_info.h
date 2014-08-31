//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_TYPE_INFO_H
#define YANG_INCLUDE_YANG_TYPE_INFO_H

#include "type.h"
#include "typedefs.h"

namespace yang {
template<typename>
class Function;
template<typename>
class Ref;

namespace internal {
template<typename>
struct TypeInfo;
struct ContextInternals;

template<typename T>
struct TypeInfoImpl {
  static_assert(sizeof(T) != sizeof(T), "use of unsupported type");

  // Avoid extra unnecessary error messages (beyond static assert above).
  Type operator()() const
  {
    return Type::void_t();
  }
};

template<>
struct TypeInfoImpl<void> {
  Type operator()() const
  {
    return Type::void_t();
  }
};

template<>
struct TypeInfoImpl<int_t> {
  Type operator()() const
  {
    return Type::int_t();
  }
};

template<>
struct TypeInfoImpl<float_t> {
  Type operator()() const
  {
    return Type::float_t();
  }
};

template<std::size_t N>
struct TypeInfoImpl<ivec_t<N>> {
  Type operator()() const
  {
    return Type::ivec_t(N);
  }
};

template<std::size_t N>
struct TypeInfoImpl<fvec_t<N>> {
  Type operator()() const
  {
    return Type::fvec_t(N);
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
  void operator()(const std::vector<Type>&) const {}
};
template<typename A, typename... Args>
struct ArgsTypeInfo<A, Args...> {
  void operator()(std::vector<Type>& output) const
  {
    output.push_back(TypeInfo<A>()());
    ArgsTypeInfo<Args...>()(output);
  }
};

template<typename R, typename... Args>
struct TypeInfoImpl<Function<R(Args...)>> {
  Type operator()() const
  {
    std::vector<Type> args;
    ArgsTypeInfo<Args...>()(args);
    return Type::function_t(TypeInfo<R>()(), args);
  }
};

template<typename T>
struct TypeInfoImpl<T*> {
  Type operator()() const
  {
    return Type::raw_user_t<T>();
  }
};

template<typename T>
struct TypeInfoImpl<Ref<T>> {
  Type operator()() const
  {
    return Type::managed_user_t<T>();
  }
};

// Wrapper for other modifiers.
template<typename T>
struct TypeInfoBase {
  Type operator()() const
  {
    return TypeInfoImpl<T>()();
  }
};
template<typename T>
struct TypeInfoError {
  static_assert(
      sizeof(T) != sizeof(T),
      "yang::Functions taking arguments by reference are not yet supported");
};
// TODO: should support C++ functions taking arguments by const reference, but
// it's a bit tricky. It has to be done in the reverse trampoline somehow,
// possibly by looking up the calling convention in the NativeFunction object?
template<typename T>
struct TypeInfo : TypeInfoBase<T> {};
template<typename T>
struct TypeInfo<T&> : TypeInfoError<T> {};
template<typename T>
struct TypeInfo<T&&> : TypeInfoError<T> {};
template<typename T>
struct TypeInfo<const T&> : TypeInfoError<T> {};
template<typename T>
struct TypeInfo<const T&&> : TypeInfoError<T> {};

// End namespace internal.
}

template<typename T>
Type type_of()
{
  return internal::TypeInfo<T>()();
}

// End namespace yang.
}

#endif
