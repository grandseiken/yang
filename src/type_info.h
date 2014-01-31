#ifndef YANG_SRC_TYPE_INFO_H
#define YANG_SRC_TYPE_INFO_H

#include <functional>
#include <tuple>

#include "log.h"
#include "native.h"
#include "type.h"

namespace yang {

class Context;
class Instance;
class Program;

namespace internal {

// Some indirection to avoid defining InstanceCheck in a weird place.
const std::string& get_instance_name(const Instance& instance);
const Program& get_instance_program(const Instance& instance);

template<typename>
struct TypeInfo;
template<typename>
struct ValueConstruct;

template<typename...>
struct TrampolineCallArgs;
template<typename, typename...>
struct TrampolineCallReturn;
template<typename, typename...>
struct TrampolineCall;

template<typename, typename, typename>
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

  // Get the program instance that this function references.
  Instance& get_instance() const;
  // Get the type corresponding to this function type as a yang Type object.
  static Type get_type(const Context& context);

  // True if the object references a non-null function. It is an error to pass
  // a null function object to a yang program instance, or invoke it.
  explicit operator bool() const;

  // Invoke the function.
  R operator()(const Args&... args) const;

private:

  template<typename...>
  friend struct internal::TrampolineCallArgs;
  template<typename, typename...>
  friend struct internal::TrampolineCallReturn;
  template<typename, typename...>
  friend struct internal::TrampolineCall;

  template<typename, typename, typename>
  friend struct internal::ReverseTrampolineCallArgs;
  template<typename, typename...>
  friend struct internal::ReverseTrampolineCallReturn;

  template<typename>
  friend struct internal::ValueConstruct;
  template<typename>
  friend class Function;
  friend class Instance;

  Function(Instance& instance)
    : _function(nullptr)
    , _instance(&instance) {}

  yang::void_fp _function;
  Instance* _instance;

};

template<typename R, typename... Args>
Instance& Function<R(Args...)>::get_instance() const
{
  return *_instance;
}

template<typename R, typename... Args>
Type Function<R(Args...)>::get_type(const Context& context)
{
  internal::TypeInfo<Function<R(Args...)>> info;
  return info(context);
}

template<typename R, typename... Args>
Function<R(Args...)>::operator bool() const
{
  return _function;
}

namespace internal {

template<typename T>
struct TypeInfo {
  static_assert(sizeof(T) != sizeof(T), "use of unsupported type");

  yang::Type operator()(const Context&) const
  {
    // This function exists to avoid extra unnecessary error messages (beyond
    // static assert above).
    return {};
  }
};

template<>
struct TypeInfo<void> {
  yang::Type operator()(const Context&) const
  {
    return {};
  }
};

template<>
struct TypeInfo<yang::int_t> {
  yang::Type operator()(const Context&) const
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
    yang::Type t;
    t._base = yang::Type::FLOAT;
    return t;
  }
};

template<std::size_t N>
struct TypeInfo<ivec_t<N>> {
  yang::Type operator()(const Context&) const
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
    TypeInfo<R> return_type;

    yang::Type t;
    t._base = yang::Type::FUNCTION;
    t._elements.push_back(return_type(context));
    return t;
  }
};

template<typename R, typename A, typename... Args>
struct TypeInfo<Function<R(A, Args...)>> {
  yang::Type operator()(const Context& context) const
  {
    TypeInfo<Function<R(Args...)>> first;
    TypeInfo<A> arg_type;

    yang::Type t = first(context);
    t._elements.insert(++t._elements.begin(), arg_type(context));
    return t;
  }
};

// Construct a value and (for function types) set the associated instance.
template<typename T>
struct ValueConstruct {
  T operator()(Instance&) const
  {
    return T();
  }

  template<typename U>
  void set_void_fp(U&, yang::void_fp ptr) const
  {
    static_assert(sizeof(U) != sizeof(U), "use of non-function type");
  }
};

template<typename R, typename... Args>
struct ValueConstruct<Function<R(Args...)>> {
  Function<R(Args...)> operator()(Instance& instance) const
  {
    return Function<R(Args...)>(instance);
  }

  void set_void_fp(Function<R(Args...)>& function, yang::void_fp ptr) const
  {
    function._function = ptr;
  }
};

// Check that all Functions given in an argument list match some particular
// program instance.
template<typename...>
struct InstanceCheck {};
template<>
struct InstanceCheck<> {
  bool operator()(const Instance&) const
  {
    return true;
  }
};
template<typename A, typename... Args>
struct InstanceCheck<A, Args...> {
  bool operator()(const Instance& instance,
                  const A&, const Args&... args) const
  {
    InstanceCheck<Args...> next;
    return next(instance, args...);
  }
};
template<typename FR, typename... FArgs, typename... Args>
struct InstanceCheck<Function<FR(FArgs...)>, Args...> {
  bool operator()(const Instance& instance,
                  const Function<FR(FArgs...)>& arg, const Args&... args) const
  {
    bool result = true;
    InstanceCheck<Args...> next;
    if (!arg) {
      log_err(get_instance_name(instance),
              ": passed null function object");
      result = false;
    }
    else {
      if (&get_instance_program(arg.get_instance()) !=
          &get_instance_program(instance)) {
        log_err(get_instance_name(instance),
                ": passed function referencing different program ",
                 get_instance_name(arg.get_instance()));
        result = false;
      }
      if (&arg.get_instance() != &instance) {
        log_err(get_instance_name(instance),
                ": passed function referencing different program instance");
        result = false;
      }
    }
    // Don't short-circuit; want to print the rest of the error messages.
    return next(instance, args...) && result;
  }
};

// Boost's metaprogramming list implementation fakes variadic templates for
// C++03 compatibility. This makes extracting the type-list directly as a
// comma-separated type list for constructing e.g. function pointer types
// difficult.
template<typename... T>
using List = std::tuple<T...>;
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
};

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

// Some function metadata.
template<typename R, typename... Args>
struct Functions {
  typedef R (*fp_type)(Args...);
  typedef std::function<R(Args...)> f_type;
};
template<typename R, typename... Args>
struct Functions<R, List<Args...>> {
  typedef typename Functions<R, Args...>::fp_type fp_type;
  typedef typename Functions<R, Args...>::f_type f_type;
};

// Standard bind function requires explicit placeholders for all elements, which
// makes it unusable in the variadic setting.
// TODO: the recursive approach we're using for argument passing means we wrap a
// function in as many lambdas (and std::functions) as there are arguments,
// before finally invoking it. It's probably possible to construct a tuple of
// the arguments and bind everything at once.
template<typename, typename, typename>
struct BindFirstHelperInternal {};
template<typename R, typename... Args, typename... Brgs>
struct BindFirstHelperInternal<R, List<Args...>, List<Brgs...>> {
  template<std::size_t... I>
  std::function<R(Brgs...)> operator()(
      const std::function<R(Args..., Brgs...)>& function,
      const Args&... args, const Indices<I...>&) const
  {
    // Work around bug in GCC/C++ standard: ambiguous whether argument packs can
    // be captured in lambdas.
    List<Args...> args_tuple(args...);
    // Close by value so the function object is copied! Otherwise it can cause an
    // infinite loop. I don't fully understand why; is the function object being
    // mutated later somehow? We also need to copy the argument, as it may be a
    // temporary that won't last (particularly for return value pointers).
    return [=](const Brgs&... brgs)
    {
      return function(std::get<I>(args_tuple)..., brgs...);
    };
  }
};
// Wrapper stuff to make deducing template arguments at all possible.
template<typename>
struct BindFirstHelper {};
template<typename R, typename... Args>
struct BindFirstHelper<R(Args...)> {
  template<typename... Brgs>
  std::function<R(Brgs...)> operator()(
      const std::function<R(Args..., Brgs...)> function,
      const Args&... args) const
  {
    return BindFirstHelperInternal<R, List<Args...>, List<Brgs...>>()(
        function, args..., range<0, sizeof...(Args)>());
  }
};
template<typename R, typename... Brgs, typename... Args>
auto bind_first(const std::function<R(Brgs...)>& function, const Args&... args)
  -> decltype(BindFirstHelper<R(Args...)>()(function, args...))
{
  return BindFirstHelper<R(Args...)>()(function, args...);
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
  typedef List<yang::void_fp*> type;
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
      List<yang::void_fp>, typename TrampolineArgs<Brgs...>::type>::type type;
};

template<typename R, typename... Args>
struct TrampolineType {
  typedef typename Join<
      typename TrampolineReturn<R>::type,
      typename TrampolineArgs<Args...>::type>::type type;
  typedef typename Functions<void, type>::fp_type fp_type;
  typedef typename Functions<void, type>::f_type f_type;
};

// Function call instrumentation for converting native types to the Yang calling
// convention for trampoline functions. TrampolineCallArgs unpacks the argument
// list; TrampolineCallReturn unpacks the return value.
template<typename...>
struct TrampolineCallArgs {};

// TrampolineCallArgs base case.
template<>
struct TrampolineCallArgs<> {
  typedef typename TrampolineType<void>::f_type f_type;

  void operator()(const f_type& function) const
  {
    function();
  }
};

// TrampolineCallArgs unpacking of a single primitive.
template<typename A, typename... Args>
struct TrampolineCallArgs<A, Args...> {
  typedef typename TrampolineType<void, A, Args...>::f_type f_type;

  void operator()(
      const f_type& function, const A& arg, const Args&... args) const
  {
    TrampolineCallArgs<Args...>()(bind_first(function, arg), args...);
  }
};

// TrampolineCallArgs unpacking of a vector.
template<typename T, std::size_t N, typename... Args>
struct TrampolineCallArgs<vec<T, N>, Args...> {
  typedef typename TrampolineType<void, vec<T, N>, Args...>::f_type f_type;
  typedef typename TrampolineType<void, Args...>::f_type bound_f_type;

  template<std::size_t... I>
  bound_f_type helper(
      const f_type& function, const vec<T, N>& arg, const Indices<I...>&) const
  {
    return bind_first(function, arg[I]...);
  }

  void operator()(
      const f_type& function, const vec<T, N>& arg, const Args&... args) const
  {
    auto f = helper(function, arg, range<0, N>());
    TrampolineCallArgs<Args...>()(f, args...);
  }
};

// TrampolineCallArgs unpacking of a function.
template<typename R, typename... Args, typename... Brgs>
struct TrampolineCallArgs<Function<R(Args...)>, Brgs...> {
  typedef typename TrampolineType<
      void, Function<R(Args...)>, Brgs...>::f_type f_type;

  void operator()(
      const f_type& function, const Function<R(Args...)>& arg,
      const Brgs&... args) const
  {
    TrampolineCallArgs<Brgs...>()(bind_first(function, arg._function), args...);
  }
};

// TrampolineCallReturn for a primitive return.
template<typename R, typename... Args>
struct TrampolineCallReturn {
  typedef typename TrampolineType<R, Args...>::f_type f_type;
  typedef typename TrampolineType<void, Args...>::f_type bound_f_type;

  bound_f_type operator()(const f_type& function, R& result) const
  {
    return bind_first(function, &result);
  }
};

// TrampolineCallReturn for a vector return.
template<typename T, std::size_t N, typename... Args>
struct TrampolineCallReturn<vec<T, N>, Args...> {
  typedef typename TrampolineType<vec<T, N>, Args...>::f_type f_type;
  typedef typename TrampolineType<void, Args...>::f_type bound_f_type;

  template<std::size_t... I>
  bound_f_type helper(const f_type& function, T* result,
                      const Indices<I...>&) const
  {
    return bind_first(function, (I + result)...);
  }

  bound_f_type operator()(const f_type& function, vec<T, N>& result) const
  {
    return helper(function, result.elements, range<0, N>());
  }
};

// TrampolineCallReturn for a function return.
template<typename R, typename... Args, typename... Brgs>
struct TrampolineCallReturn<Function<R(Args...)>, Brgs...> {
  typedef typename TrampolineType<
      Function<R(Args...)>, Brgs...>::f_type f_type;
  typedef typename TrampolineType<void, Brgs...>::f_type bound_f_type;

  bound_f_type operator()(
      const f_type& function, Function<R(Args...)>& result) const
  {
    return bind_first(function, &result._function);
  }
};

// Combine the above into a complete function call.
template<typename R, typename... Args>
struct TrampolineCall {
  typedef typename TrampolineType<R, Args...>::fp_type fp_type;
  typedef typename TrampolineType<R, Args...>::f_type f_type;

  R operator()(Instance& instance,
               const f_type& function, const Args&... args) const
  {
    typedef ValueConstruct<R> construct_type;
    typedef TrampolineCallReturn<R, Args...> return_type;
    typedef TrampolineCallArgs<Args...> args_type;

    R result = construct_type()(instance);
    auto return_bound_function = return_type()(function, result);
    args_type()(return_bound_function, args...);
    return result;
  }
};

// Specialisation for void return just to avoid declaring a variable of type
// void.
template<typename... Args>
struct TrampolineCall<void, Args...> {
  typedef typename TrampolineType<void, Args...>::fp_type fp_type;
  typedef typename TrampolineType<void, Args...>::f_type f_type;

  void operator()(Instance&,
                  const f_type& function, const Args&... args) const
  {
    typedef TrampolineCallArgs<Args...> args_type;
    args_type()(function, args...);
  }
};

// Function call instrumentation for reverse trampoline conversion to native
// calling convention. ReverseTrampolineCallArgs corresponds to
// TrampolineCallArgs, and so on.
template<typename, typename, typename>
struct ReverseTrampolineCallArgs {};
template<typename R>
struct ReverseTrampolineCallArgs<R, List<>, List<>> {
  R operator()(const List<>&, Instance&,
               const std::function<R()>& target) const
  {
    return target();
  }
};
template<typename R, typename... Args, typename... Brgs, typename A>
struct ReverseTrampolineCallArgs<R, List<A, Args...>, List<Brgs...>> {
  R operator()(const List<Brgs...>& args, Instance& instance,
               const std::function<R(A, Args...)>& target) const
  {
    typedef Sublist<typename IndexRange<1, sizeof...(Brgs) - 1>::type,
                    List<Brgs...>> sublist;
    return ReverseTrampolineCallArgs<
        R, List<Args...>, typename sublist::type>()(
        sublist()(args), instance, bind_first(target, std::get<0>(args)));
  }
};
template<typename R, typename... Args, typename... Brgs,
         typename T, std::size_t N>
struct ReverseTrampolineCallArgs<
    R, List<vec<T, N>, Args...>, List<Brgs...>> {
  template<std::size_t... I>
  R helper(const List<Brgs...>& args, Instance& instance,
           const std::function<R(vec<T, N>, Args...)>& target,
           const Indices<I...>&) const
  {
    typedef Sublist<typename IndexRange<N, sizeof...(Brgs) - N>::type,
                    List<Brgs...>> sublist;
    return ReverseTrampolineCallArgs<
        R, List<Args...>, typename sublist::type>()(
        sublist()(args), instance,
        bind_first(target, vec<T, N>(std::get<I>(args)...)));
  }

  R operator()(const List<Brgs...>& args, Instance& instance,
               const std::function<R(vec<T, N>, Args...)>& target) const
  {
    return helper(args, instance, target, range<0, N>());
  }
};
template<typename R, typename... Args, typename... Brgs,
         typename S, typename... Crgs>
struct ReverseTrampolineCallArgs<
    R, List<Function<S(Crgs...)>, Args...>, List<Brgs...>> {
  R operator()(
      const List<Brgs...>& args, Instance& instance,
      const std::function<R(Function<S(Crgs...)>, Args...)>& target) const
  {
    // Standard guarantees that pointer to structure points to its first member,
    // and the pointer to the program instance is always the first element of
    // the global data structure; so, we can just cast it to an instance
    // pointer.
    Function<S(Crgs...)> fn_object(instance);
    fn_object._function = std::get<0>(args);

    typedef Sublist<typename IndexRange<1, sizeof...(Brgs) - 1>::type,
                    List<Brgs...>> sublist;
    return ReverseTrampolineCallArgs<
        R, List<Args...>, typename sublist::type>()(
        sublist()(args), instance, bind_first(target, fn_object));
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
struct ReverseTrampolineCallReturn<Function<R(Args...)>, yang::void_fp*> {
  void operator()(const Function<R(Args...)>& result, yang::void_fp* ptr) const
  {
    *ptr = result._function;
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
  static void call(ReturnBrgs... return_args, void* global_data, Brgs... args,
                   void* target)
  {
    // Standard guarantees that pointer to structure points to its first member,
    // and the pointer to the program instance is always the first element of
    // the global data structure; so, we can just cast it to an instance
    // pointer.
    Instance& instance = **(Instance**)global_data;

    typedef ReverseTrampolineCallReturn<R, ReturnBrgs...> return_type;
    typedef ReverseTrampolineCallArgs<
        R, List<Args...>, List<Brgs...>> args_type;

    auto f = (const NativeFunction<void>*)target;
    R result = args_type()(
        List<Brgs...>(args...), instance, f->get<R, Args...>());

    // Check C++ isn't returning a pointer to a function on a different program
    // instance. If it is, we need to return *something*, so set result to
    // default (null pointer), so that at least any use will probably fail fast.
    InstanceCheck<R> instance_check;
    if (!instance_check(instance, result)) {
      result = ValueConstruct<R>()(instance);
    }
    return_type()(result, return_args...);
  }
};

// Specialisation for void returns.
template<typename... Args, typename... Brgs>
struct ReverseTrampolineCall<void(Args...), List<>, List<Brgs...>> {
  static void call(void* global_data, Brgs... args, void* target)
  {
    Instance& instance = **(Instance**)global_data;

    typedef ReverseTrampolineCallArgs<
        void, List<Args...>, List<Brgs...>> args_type;

    auto f = (const NativeFunction<void>*)target;
    args_type()(List<Brgs...>(args...), instance, f->get<void, Args...>());
  }
};

// End namespace yang::internal.
}
}

#endif
