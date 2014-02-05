//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_TYPE_INFO_H
#define YANG_SRC_TYPE_INFO_H

#include <functional>
#include <tuple>
#include <unordered_map>

#include "log.h"
#include "native.h"
#include "type.h"

namespace yang {

class Context;
class Instance;
class Program;

namespace internal {

template<typename>
struct FunctionConstruct;
template<typename>
struct TypeInfo;

template<typename...>
struct TrampolineCallArgs;
template<typename>
struct TrampolineCallReturn;
template<typename, typename...>
struct TrampolineCall;

template<typename, typename>
struct ReverseTrampolineCallArgs;
template<typename, typename...>
struct ReverseTrampolineCallReturn;

// End namespace internal.
}

// Opaque Yang function object.
template<typename T>
class Function {
  static_assert(sizeof(T) != sizeof(T), "use of non-function type");
};

template<typename R, typename... Args>
class Function<R(Args...)> {
public:

  // Get the type corresponding to this function type as a yang Type object.
  static Type get_type(const Context& context);

  // Invoke the function.
  R operator()(const Args&... args) const;

private:

  // If this is a Yang function, get the program instance it references.
  // Otherwise, return a null pointer.
  Instance* get_instance() const;

  template<typename...>
  friend struct internal::TrampolineCallArgs;
  template<typename>
  friend struct internal::TrampolineCallReturn;
  template<typename, typename...>
  friend struct internal::TrampolineCall;

  template<typename, typename>
  friend struct internal::ReverseTrampolineCallArgs;
  template<typename, typename...>
  friend struct internal::ReverseTrampolineCallReturn;

  template<typename>
  friend struct internal::FunctionConstruct;
  template<typename>
  friend class Function;
  friend class Instance;

  // Invariant: Function objects returned to client code must never be null.
  // They must reference a genuine Yang function or C++ function, so that they
  // can be invoked or passed to Yang code. Library code that return Functions
  // to client code must throw rather than returning something unusable.
  Function()
    : _function(nullptr)
    , _env(nullptr)
    , _target(nullptr) {}

  yang::void_fp _function;
  void* _env;
  yang::void_fp _target;

};

template<typename R, typename... Args>
Type Function<R(Args...)>::get_type(const Context& context)
{
  internal::TypeInfo<Function<R(Args...)>> info;
  return info(context);
}

template<typename R, typename... Args>
Instance* Function<R(Args...)>::get_instance() const
{
  // Standard guarantees that pointer to structure points to its first member,
  // and the pointer to the program instance is always the first element of
  // the global data structure; so, we can just cast it to an instance
  // pointer.
  //
  // This will change when the environment pointer can also point to a closure
  // structure.
  if (!_env) {
    return nullptr;
  }
  return *(Instance**)_env;
}

namespace internal {

template<typename T>
struct FunctionConstruct {
  static_assert(sizeof(T) != sizeof(T), "use of non-function type");
  T operator()(yang::void_fp, void*) const
  {
    return {};
  }
};
template<typename R, typename... Args>
struct FunctionConstruct<Function<R(Args...)>> {
  Function<R(Args...)> operator()(yang::void_fp function, void* env) const
  {
    Function<R(Args...)> f;
    f._function = function;
    f._env = env;
    return f;
  }
};

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

// Boost's metaprogramming list implementation fakes variadic templates for
// C++03 compatibility. This makes extracting the type-list directly as a
// comma-separated type list for constructing e.g. function pointer types
// difficult.
template<typename... T>
using List = std::tuple<T...>;
template<typename... T>
List<T...> list(const T&... t)
{
  return List<T...>(t...);
}
template<typename... T>
void ignore(const T&...) {}

// Integer pack range.
template<std::size_t... N>
struct Indices {};
template<std::size_t Min, std::size_t N, std::size_t... I>
struct IndexRange {
  typedef typename IndexRange<Min, N - 1, Min + N - 1, I...>::type type;
};
template<std::size_t Min, std::size_t... N>
struct IndexRange<Min, 0, N...> {
  typedef Indices<N...> type;
};
template<std::size_t Min, std::size_t N>
auto range() -> typename IndexRange<Min, N>::type
{
  return typename IndexRange<Min, N>::type();
}

// Multiply list.
template<std::size_t N, typename T, typename... U>
struct Mul {
  typedef typename Mul<N - 1, T, T, U...>::type type;
};
template<typename T, typename... U>
struct Mul<0, T, U...> {
  typedef List<U...> type;
};

// Join two lists.
template<typename T, typename U>
struct Join {};
template<typename... T, typename... U>
struct Join<List<T...>, List<U...>> {
  typedef List<T..., U...> type;

  template<std::size_t... I, std::size_t... J>
  type helper(const List<T...>& t, const List<U...>& u,
              const Indices<I...>&, const Indices<J...>&) const
  {
    return type(std::get<I>(t)..., std::get<J>(u)...);
  }

  type operator()(const List<T...>& t, const List<U...>& u) const
  {
    return helper(t, u, range<0, sizeof...(T)>(), range<0, sizeof...(U)>());
  }
};
template<typename T, typename U>
auto join(const T& t, const U& u) -> decltype(Join<T, U>()(t, u))
{
  return Join<T, U>()(t, u);
}

// Get a sublist from a list.
template<typename, typename>
struct Sublist {};
template<std::size_t... I, typename... T>
struct Sublist<Indices<I...>, List<T...>> {
  typedef List<typename std::tuple_element<I, List<T...>>::type...> type;

  type operator()(const List<T...>& t) const
  {
    return type(std::get<I>(t)...);
  }
};
template<typename I, typename T>
auto sublist(const T& t) -> decltype(Sublist<I, T>()(t))
{
  return Sublist<I, T>()(t);
}

// Some function metadata.
template<typename R, typename... Args>
struct Functions {
  typedef R (*fp_type)(Args...);
  typedef std::function<R(Args...)> type;
};
template<typename R, typename... Args>
struct Functions<R, List<Args...>> {
  typedef typename Functions<R, Args...>::fp_type fp_type;
  typedef typename Functions<R, Args...>::type type;
};

// Call function with args from a tuple.
// TODO: allow const references in context functions.
template<typename R, typename... Args, std::size_t... I>
R list_call(const std::function<R(Args...)>& f, const List<Args...>& args,
            const Indices<I...>&)
{
  return f(std::get<I>(args)...);
}
template<typename R, typename... Args>
R list_call(const std::function<R(Args...)>& f, const List<Args...>& args)
{
  return list_call(f, args, range<0, sizeof...(Args)>());
}

// Templates for converting native function types into the corresponding
// Yang trampoline function types.
template<typename R>
struct TrampolineReturn {
  typedef List<R*> type;
};
template<>
struct TrampolineReturn<void> {
  typedef List<> type;
};
template<typename T>
struct TrampolineReturn<vec<T, 2>> {
  typedef List<T*, T*> type;
};
template <typename T, std::size_t N>
struct TrampolineReturn<vec<T, N>> {
  typedef typename Mul<N, T*>::type type;
};
template<typename R, typename... Args>
struct TrampolineReturn<Function<R(Args...)>> {
  typedef List<yang::void_fp*, void**, yang::void_fp*> type;
};

template<typename... Args>
struct TrampolineArgs {};
template<typename A, typename... Args>
struct TrampolineArgs<A, Args...> {
  typedef typename Join<
      List<A>, typename TrampolineArgs<Args...>::type>::type type;
};
template<>
struct TrampolineArgs<> {
  typedef List<> type;
};
template<typename T, std::size_t N, typename... Args>
struct TrampolineArgs<vec<T, N>, Args...> {
  typedef typename TrampolineArgs<Args...>::type rest;
  typedef typename Join<typename Mul<N, T>::type, rest>::type type;
};
template<typename R, typename... Args, typename... Brgs>
struct TrampolineArgs<Function<R(Args...)>, Brgs...> {
  typedef typename Join<
      List<yang::void_fp, void*, yang::void_fp>,
      typename TrampolineArgs<Brgs...>::type>::type type;
};

template<typename R, typename... Args>
struct TrampolineType {
  typedef typename Join<
      typename TrampolineReturn<R>::type,
      typename TrampolineArgs<Args...>::type>::type join_type;
  typedef typename Functions<void, join_type>::type type;
  typedef typename Functions<void, join_type>::fp_type fp_type;
};

// Function call instrumentation for converting native types to the Yang calling
// convention for trampoline functions. TrampolineCallArgs unpacks the argument
// list; TrampolineCallReturn unpacks the return value.
template<typename...>
struct TrampolineCallArgs {};

// TrampolineCallArgs base case.
template<>
struct TrampolineCallArgs<> {
  List<> operator()() const
  {
    return {};
  }
};

// TrampolineCallArgs unpacking of a single primitive.
template<typename A, typename... Args>
struct TrampolineCallArgs<A, Args...> {
  typedef typename TrampolineArgs<A, Args...>::type type;

  type operator()(const A& arg, const Args&... args) const
  {
    return join(list(arg), TrampolineCallArgs<Args...>()(args...));
  }
};

// TrampolineCallArgs unpacking of a vector.
template<typename T, std::size_t N, typename... Args>
struct TrampolineCallArgs<vec<T, N>, Args...> {
  typedef typename TrampolineArgs<vec<T, N>, Args...>::type type;

  template<std::size_t... I>
  type helper(const vec<T, N>& arg, const Args&... args,
              const Indices<I...>&) const
  {
    return join(Mul<N, T>::type(arg[I]...),
                TrampolineCallArgs<Args...>()(args...));
  }

  type operator()(const vec<T, N>& arg, const Args&... args) const
  {
    return helper(arg, args..., range<0, N>());
  }
};

// TrampolineCallArgs unpacking of a function.
template<typename R, typename... Args, typename... Brgs>
struct TrampolineCallArgs<Function<R(Args...)>, Brgs...> {
  typedef typename TrampolineArgs<Function<R(Args...)>, Brgs...>::type type;

  type operator()(const Function<R(Args...)>& arg, const Brgs&... brgs) const
  {
    return join(list(arg._function, arg._env, arg._target),
                TrampolineCallArgs<Brgs...>()(brgs...));
  }
};

// TrampolineCallReturn for a primitive return.
template<typename R>
struct TrampolineCallReturn {
  typedef typename TrampolineReturn<R>::type type;

  type operator()(R& result) const
  {
    return type(&result);
  }
};

// TrampolineCallReturn for a vector return.
template<typename T, std::size_t N>
struct TrampolineCallReturn<vec<T, N>> {
  typedef typename TrampolineReturn<vec<T, N>>::type type;

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

// TrampolineCallReturn for a function return.
template<typename R, typename... Args>
struct TrampolineCallReturn<Function<R(Args...)>> {
  typedef typename TrampolineReturn<Function<R(Args...)>>::type type;

  type operator()(Function<R(Args...)>& result) const
  {
    return type(&result._function, &result._env, &result._target);
  }
};

// Combine the above into a complete function call.
template<typename R, typename... Args>
struct TrampolineCall {
  typedef typename TrampolineType<R, Args...>::type type;
  typedef typename TrampolineType<R, Args...>::fp_type fp_type;

  R operator()(const type& function, const Args&... args) const
  {
    R result;
    list_call(function, join(TrampolineCallReturn<R>()(result),
                             TrampolineCallArgs<Args...>()(args...)));
    return result;
  }
};

// Specialisation for void return just to avoid declaring a variable of type
// void.
template<typename... Args>
struct TrampolineCall<void, Args...> {
  typedef typename TrampolineType<void, Args...>::type type;
  typedef typename TrampolineType<void, Args...>::fp_type fp_type;

  void operator()(const type& function, const Args&... args) const
  {
    list_call(function, TrampolineCallArgs<Args...>()(args...));
  }
};

// Function call instrumentation for reverse trampoline conversion to native
// calling convention. ReverseTrampolineCallArgs corresponds to
// TrampolineCallArgs, and so on.
template<typename, typename>
struct ReverseTrampolineCallArgs {};
template<typename T, typename U>
auto rtcall_args(const U& u)
  -> decltype(ReverseTrampolineCallArgs<T, U>()(u))
{
  return ReverseTrampolineCallArgs<T, U>()(u);
}
template<>
struct ReverseTrampolineCallArgs<List<>, List<>> {
  List<> operator()(const List<>&) const
  {
    return {};
  }
};
template<typename... Args, typename... Brgs, typename A>
struct ReverseTrampolineCallArgs<List<A, Args...>, List<Brgs...>> {
  List<A, Args...> operator()(const List<Brgs...>& brgs) const
  {
    typedef typename IndexRange<1, sizeof...(Brgs) - 1>::type range;
    return join(list(std::get<0>(brgs)),
                rtcall_args<List<Args...>>(sublist<range>(brgs)));
  }
};
template<typename... Args, typename... Brgs, typename T, std::size_t N>
struct ReverseTrampolineCallArgs<List<vec<T, N>, Args...>, List<Brgs...>> {
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
template<typename... Args, typename... Brgs, typename S, typename... Crgs>
struct ReverseTrampolineCallArgs<
    List<Function<S(Crgs...)>, Args...>, List<Brgs...>> {
  List<Function<S(Crgs...)>, Args...> operator()(
      const List<Brgs...>& brgs) const
  {
    Function<S(Crgs...)> fn_object;
    fn_object._function = std::get<0>(brgs);
    fn_object._env = std::get<1>(brgs);
    fn_object._target = std::get<2>(brgs);

    typedef typename IndexRange<3, sizeof...(Brgs) - 3>::type range;
    return join(list(fn_object),
                rtcall_args<List<Args...>>(sublist<range>(brgs)));
  }
};

// Return-value assigned to pointer outputs.
template<typename, typename...>
struct ReverseTrampolineCallReturn {};
template<typename R>
struct ReverseTrampolineCallReturn<R, R*> {
  void operator()(const R& result, R* ptr) const
  {
    *ptr = result;
  }
};
template<typename T, std::size_t N, typename... Args>
struct ReverseTrampolineCallReturn<vec<T, N>, Args...> {
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
template<typename R, typename... Args>
struct ReverseTrampolineCallReturn<Function<R(Args...)>,
                                   yang::void_fp*, void**, yang::void_fp*> {
  void operator()(const Function<R(Args...)>& result,
                  yang::void_fp* fptr, void** eptr, yang::void_fp* tptr) const
  {
    *fptr = result._function;
    *eptr = result._env;
    *tptr = result._target;
  }
};

// Combine into reverse function call.
template<typename, typename, typename>
struct ReverseTrampolineCall {};

// For convenience, this template takes both the native template arguments R and
// Brgs, and their instantiation lists TrampolineReturn<R>::type and
// TrampolineArgs<Brgs>::type respectively.
template<typename R, typename... Args, typename... ReturnBrgs, typename... Brgs>
struct ReverseTrampolineCall<R(Args...), List<ReturnBrgs...>, List<Brgs...>> {
  // No reference args; this is the function directly called from Yang code.
  // Currently takes an environment pointer, but has no use for it.
  static void call(ReturnBrgs... return_brgs, Brgs... brgs, void*, void* target)
  {
    auto f = (const NativeFunction<void>*)target;
    R result = list_call(f->get<R, Args...>(),
                         rtcall_args<List<Args...>>(list(brgs...)));
    ReverseTrampolineCallReturn<R, ReturnBrgs...>()(result, return_brgs...);
  }
};

// Specialisation for void returns.
template<typename... Args, typename... Brgs>
struct ReverseTrampolineCall<void(Args...), List<>, List<Brgs...>> {
  static void call(Brgs... brgs, void*, void* target)
  {
    auto f = (const NativeFunction<void>*)target;
    list_call(f->get<void, Args...>(),
              rtcall_args<List<Args...>>(list(brgs...)));
  }
};

// Reverse trampolines on the C++ side must be instatiated at C++ compile-time,
// but which ones we need depends on information not available until we compile
// Yang at run-time. Solution: figure out which ones might be necessary at
// compile-time by inspecting template arguments to Context::register_function,
// Instance::get_function, and so on. Generate these and use static
// initialisation trickery to generate a lookup table from Yang type.
typedef std::unordered_map<yang::Type, yang::void_fp> cpp_trampoline_lookup_map;
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
    yang::Type t = TypeInfo<Function<R(Args...)>>()();
    yang::void_fp ptr = (yang::void_fp)&internal::ReverseTrampolineCall<
        R(Args...),
        typename internal::TrampolineReturn<R>::type,
        typename internal::TrampolineArgs<Args...>::type>::call;
    map.emplace(t, ptr);
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

// End namespace yang::internal.
}
}

#endif
