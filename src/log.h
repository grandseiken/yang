#ifndef YANG_SRC_LOG_H
#define YANG_SRC_LOG_H

#include <iostream>

namespace {
namespace internal {

inline void log(std::ostream&)
{
}

template<typename T, typename... U>
inline void log(std::ostream& stream, const T& arg, const U&... args)
{
  stream << arg;
  log(stream, args...);
}

template<typename... T>
inline void ignore(const T&...)
{
}

// End namespace internal.
}

template<typename... T>
inline void log_debug(const T&... args)
{
#ifdef DEBUG
  internal::log(std::cout, args...);
  std::cout << std::endl;
#else
  internal::ignore(args...);
#endif
}

template<typename... T>
inline void log_info(const T&... args)
{
  internal::log(std::cout, args...);
  std::cout << std::endl;
}

template<typename... T>
inline void log_err(const T&... args)
{
  internal::log(std::cerr, args...);
  std::cerr << std::endl;
}

// End anonymous namespace.
}

#endif
