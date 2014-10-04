//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_TRAMPOLINE_H
#define YANG_INCLUDE_YANG_TRAMPOLINE_H

#include <functional>
#include <unordered_map>
#include "meta_list.h"
#include "native.h"
#include "refcounting.h"
#include "type_info.h"

namespace yang {
namespace internal {
struct Prefix;
struct RawFunction {
  void* function_ptr;
  Prefix* environment_ptr;
};

// Converts between rich C++ client types and bare types that can be passed to
// Yang trampolines.
// TODO: it would almost certainly be simpler to pass vectors as direct pointers
// to arrays and deal with them on the Yang side.
template<typename T>
struct Raw {
  typedef T type;
  const T& from(const T& t) const
  {
    return t;
  }
  const T& to(const T& t) const
  {
    return t;
  }
};
template<typename T>
struct Raw<Ref<T>> {
  typedef Prefix* type;
  Ref<T> from(Prefix* t) const
  {
    return Ref<T>(t);
  }
  Prefix* to(const Ref<T>& t) const
  {
    return t._wrap.get();
  }
};
template<typename R, typename... Args>
struct Raw<Function<R(Args...)>> {
  typedef RawFunction type;
  Function<R(Args...)> from(const RawFunction& t) const
  {
    return Function<R(Args...)>(t);
  }
  RawFunction to(const Function<R(Args...)>& t) const
  {
    return t.get_raw_representation();
  }
};
template<>
struct Raw<void> {
  typedef void type;
};

// Function call instrumentation for converting native types to the Yang calling
// convention for trampoline functions. TrampolineArgs unpacks the argument
// list; TrampolineReturn unpacks the temporary return object into output
// pointers.
template<typename...>
struct TrampolineArgs;
template<>
struct TrampolineArgs<> {
  typedef List<> type;
  List<> operator()() const
  {
    return {};
  }
};
template<typename A, typename... Args>
struct TrampolineArgs<A, Args...> {
  typedef typename Join<
      List<A>, typename TrampolineArgs<Args...>::type>::type type;
  type operator()(const A& arg, const Args&... args) const
  {
    return join(list(arg), TrampolineArgs<Args...>()(args...));
  }
};
template<typename T, std::size_t N, typename... Args>
struct TrampolineArgs<vec<T, N>, Args...> {
  typedef typename TrampolineArgs<Args...>::type rest;
  typedef typename Join<typename Mul<N, T>::type, rest>::type type;

  template<std::size_t... I>
  type helper(const vec<T, N>& arg, const typename Raw<Args>::type&... args,
              const Indices<I...>&) const
  {
    return join(typename Mul<N, T>::type(arg[I]...),
                TrampolineArgs<Args...>()(args...));
  }

  type operator()(const vec<T, N>& arg,
                  const typename Raw<Args>::type&... args) const
  {
    return helper(arg, args..., range<0, N>());
  }
};
template<typename... Args>
struct TrampolineArgs<RawFunction, Args...> {
  typedef typename TrampolineArgs<Args...>::type rest;
  typedef typename Join<List<void*, Prefix*>, rest>::type type;
  type operator()(const RawFunction& arg, const Args&... args) const
  {
    return join(list(arg.function_ptr, arg.environment_ptr),
                TrampolineArgs<Args...>()(args...));
  }
};

template<typename R>
struct TrampolineReturn {
  typedef List<R*> type;
  type operator()(R& result) const
  {
    return type(&result);
  }
};
template<typename T, std::size_t N>
struct TrampolineReturn<vec<T, N>> {
  typedef typename Mul<N, T*>::type type;

  template<std::size_t... I>
  type helper(T* result, const Indices<I...>&) const
  {
    return type((I + result)...);
  }

  type operator()(vec<T, N>& result) const
  {
    return helper(result.elements, range<0, N>());
  }
};
template<>
struct TrampolineReturn<RawFunction> {
  typedef List<void**, Prefix**> type;
  type operator()(RawFunction& result) const
  {
    return type(&result.function_ptr, &result.environment_ptr);
  }
};
template<>
struct TrampolineReturn<void> {
  typedef List<> type;
};

// Combine the above into a complete function call.
template<typename>
struct Functions;
template<typename... Args>
struct Functions<List<Args...>> {
  typedef void (*fp_type)(Args...);
  typedef std::function<void(Args...)> type;
};
template<typename R, typename... Args>
struct TrampolineType {
  typedef typename Join<
      typename TrampolineReturn<R>::type,
      typename TrampolineArgs<Args...>::type>::type join_type;
  typedef typename Functions<join_type>::type type;
  typedef typename Functions<join_type>::fp_type fp_type;
};

template<typename R, typename... Args>
struct TrampolineCall {
  typedef TrampolineType<
      typename Raw<R>::type, typename Raw<Args>::type...> type;

  R operator()(const typename type::type& function, const Args&... args) const
  {
    typename Raw<R>::type temporary;
    list_call(function, join(
        TrampolineReturn<typename Raw<R>::type>()(temporary),
        TrampolineArgs<typename Raw<Args>::type...>()(
            Raw<Args>().to(args)...)));
    return Raw<R>().from(temporary);
  }
};

// Specialisation for void return just to avoid declaring a variable of type
// void.
template<typename... Args>
struct TrampolineCall<void, Args...> {
  typedef TrampolineType<void, typename Raw<Args>::type...> type;

  void operator()(const typename type::type& function,
                  const Args&... args) const
  {
    list_call(function, TrampolineArgs<typename Raw<Args>::type...>()(
        Raw<Args>().to(args)...));
  }
};

// Function call instrumentation for reverse trampoline conversion to native
// calling convention. ReverseTrampolineArgs corresponds to TrampolineArgs, and
// so on.
template<typename, typename>
struct ReverseTrampolineArgs;
template<typename T, typename U>
auto rtcall_args(const U& u) -> decltype(ReverseTrampolineArgs<T, U>()(u))
{
  return ReverseTrampolineArgs<T, U>()(u);
}
template<>
struct ReverseTrampolineArgs<List<>, List<>> {
  List<> operator()(const List<>&) const
  {
    return {};
  }
};
template<typename... Args, typename... Brgs, typename A>
struct ReverseTrampolineArgs<List<A, Args...>, List<Brgs...>> {
  List<A, Args...> operator()(const List<Brgs...>& brgs) const
  {
    typedef typename IndexRange<1, sizeof...(Brgs) - 1>::type range;
    return join(list(std::get<0>(brgs)),
                rtcall_args<List<Args...>>(sublist<range>(brgs)));
  }
};
template<typename... Args, typename... Brgs, typename T, std::size_t N>
struct ReverseTrampolineArgs<List<vec<T, N>, Args...>, List<Brgs...>> {
  template<std::size_t... I>
  List<vec<T, N>, Args...> helper(const List<Brgs...>& brgs,
                                  const Indices<I...>&) const
  {
    typedef typename IndexRange<N, sizeof...(Brgs) - N>::type range;
    return join(list(vec<T, N>(std::get<I>(brgs)...)),
                rtcall_args<List<Args...>>(sublist<range>(brgs)));
  }

  List<vec<T, N>, Args...> operator()(const List<Brgs...>& brgs) const
  {
    return helper(brgs, range<0, N>());
  }
};
template<typename... Args, typename... Brgs>
struct ReverseTrampolineArgs<List<RawFunction, Args...>, List<Brgs...>> {
  List<RawFunction, Args...> operator()(const List<Brgs...>& brgs) const
  {
    RawFunction f{std::get<0>(brgs), std::get<1>(brgs)};
    typedef typename IndexRange<2, sizeof...(Brgs) - 2>::type range;
    return join(list(f), rtcall_args<List<Args...>>(sublist<range>(brgs)));
  }
};

// Return-value assigned to pointer outputs.
template<typename, typename...>
struct ReverseTrampolineReturn;
template<typename R>
struct ReverseTrampolineReturn<R, R*> {
  void operator()(const R& result, R* ptr) const
  {
    *ptr = result;
  }
};
template<typename T, std::size_t N, typename... Args>
struct ReverseTrampolineReturn<vec<T, N>, Args...> {
  template<std::size_t... I>
  void helper(const vec<T, N>& result, const List<Args...>& args,
              const Indices<I...>&) const
  {
    ignore((*std::get<I>(args) = result[I])...);
  }

  void operator()(const vec<T, N>& result, const Args&... args) const
  {
    helper(result, List<Args...>(args...), range<0, N>());
  }
};
template<>
struct ReverseTrampolineReturn<RawFunction, void**, Prefix**> {
  void operator()(const RawFunction& result,
                  void** fptr, Prefix** eptr) const
  {
    *fptr = result.function_ptr;
    *eptr = result.environment_ptr;
  }
};

// Combine into reverse function call.
template<typename... Args>
struct FromRawList {
  template<std::size_t... I>
  List<Args...> helper(const List<typename Raw<Args>::type...>& args,
                       const Indices<I...>&) const
  {
    return List<Args...>(Raw<Args>().from(std::get<I>(args))...);
  }

  List<Args...> operator()(const List<typename Raw<Args>::type...>& args) const
  {
    return helper(args, range<0, sizeof...(Args)>());
  }
};
template<typename, typename, typename>
struct ReverseTrampolineCall;

// For convenience, this template takes both the native template arguments R and
// Brgs, and their instantiation lists TrampolineReturn<Raw<R>::type>::type and
// TrampolineArgs<Raw<Brgs>::type>::type respectively.
template<typename R, typename... Args, typename... ReturnBrgs, typename... Brgs>
struct ReverseTrampolineCall<R(Args...), List<ReturnBrgs...>, List<Brgs...>> {
  // No reference args; this is the function directly called from Yang code.
  // Takes the environment pointer argument, even though it isn't used: that's
  // more complicated than it's worth to exclude, and could be useful for
  // debugging sometime anyway.
  static void call(ReturnBrgs... return_brgs, Brgs... brgs, void*,
                   const NativeFunctionInternals* target)
  {
    R result = list_call(target->get<R, Args...>(), FromRawList<Args...>()(
        rtcall_args<List<typename Raw<Args>::type...>>(list(brgs...))));
    ReverseTrampolineReturn<typename Raw<R>::type, ReturnBrgs...>()(
        Raw<R>().to(result), return_brgs...);
  }
};

// Specialisation for void returns.
template<typename... Args, typename... Brgs>
struct ReverseTrampolineCall<void(Args...), List<>, List<Brgs...>> {
  static void call(Brgs... brgs, void*, const NativeFunctionInternals* target)
  {
    list_call(target->get<void, Args...>(), FromRawList<Args...>()(
        rtcall_args<List<typename Raw<Args>::type...>>(list(brgs...))));
  }
};

// Reverse trampolines on the C++ side must be instatiated at C++ compile-time,
// but which ones we need depends on information not available until we compile
// Yang at run-time. Solution: figure out which ones might be necessary at
// compile-time by inspecting template arguments to Context::register_function,
// Instance::get_function, and so on. Generate these and use static
// initialisation trickery to generate a lookup table from Yang type.
typedef std::unordered_map<Type, void_fp> cpp_trampoline_lookup_map;
inline cpp_trampoline_lookup_map& get_cpp_trampoline_lookup_map()
{
  static cpp_trampoline_lookup_map map;
  return map;
}

template<typename R, typename... Args>
struct ReverseTrampolineLookupGenerator {
  ReverseTrampolineLookupGenerator()
  {
    auto& map = get_cpp_trampoline_lookup_map();
    void_fp ptr = (void_fp)&internal::ReverseTrampolineCall<
        R(Args...),
        typename internal::TrampolineReturn<typename Raw<R>::type>::type,
        typename internal::TrampolineArgs<typename Raw<Args>::type...>::type
    >::call;
    map.emplace(Type::erased_t(type_of<Function<R(Args...)>>()), ptr);
  }

  void operator()() const
  {
    // Make sure everything is instantiated. Plain typedefs doesn't seem to be
    // enough to do it.
  }
};

template<typename T>
struct GenerateForwardTrampolineLookupTable {
  void operator()() const {}
};
template<typename T>
struct GenerateReverseTrampolineLookupTable {
  void operator()() const {}
};
template<typename T>
struct GenerateReverseTrampolineLookupTableInner {
  void operator()() const {}
};

template<typename R>
struct GenerateForwardTrampolineLookupTable<Function<R()>> {
  typedef GenerateForwardTrampolineLookupTable<R> r;
  void operator()() const
  {
    r()();
  }
};
template<typename R, typename A, typename... Args>
struct GenerateForwardTrampolineLookupTable<Function<R(A, Args...)>> {
  typedef GenerateReverseTrampolineLookupTable<A> a;
  typedef GenerateForwardTrampolineLookupTable<Function<R(Args...)>> next;
  void operator()() const
  {
    a()();
    next()();
  }
};

template<typename R, typename... Args>
struct GenerateReverseTrampolineLookupTable<Function<R(Args...)>> {
  typedef
      GenerateReverseTrampolineLookupTableInner<Function<R(Args...)>> inner;
  static ReverseTrampolineLookupGenerator<R, Args...> generator;
  void operator()() const
  {
    inner()();
    generator();
  }
};
template<typename R, typename... Args>
ReverseTrampolineLookupGenerator<R, Args...>
    GenerateReverseTrampolineLookupTable<Function<R(Args...)>>::generator;

template<typename R, typename A, typename... Args>
struct GenerateReverseTrampolineLookupTableInner<Function<R(A, Args...)>> {
  typedef GenerateForwardTrampolineLookupTable<A> a;
  typedef GenerateReverseTrampolineLookupTableInner<Function<R(Args...)>> next;
  void operator()() const
  {
    a()();
    next()();
  }
};
template<typename R>
struct GenerateReverseTrampolineLookupTableInner<Function<R()>> {
  typedef GenerateReverseTrampolineLookupTable<R> r;
  void operator()() const
  {
    r()();
  }
};

// End namespace internal.
}
}

#endif
