//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_META_LIST_H
#define YANG_INCLUDE_YANG_META_LIST_H

namespace yang {
namespace internal {

// Proper compile-time list (can't use boost MPL, it fakes variadic templates).
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

// Call function with args from a List.
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

// End namespace yang::internal.
}
}

#endif
