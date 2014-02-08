//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <iostream>
#include <fstream>

#include "log.h"
#include "yang.h"

typedef yang::int_t int_t;
template<typename T>
using Fn = yang::Function<T>;

int main(int argc, char** argv)
{
  if (argc < 2) {
    log_err("No input file specified");
    return 1;
  }

  std::string path = argv[1];
  std::string contents;
  try {
    std::ifstream file(path, std::ios::in | std::ios::binary);
    if (!file) {
      log_err(path, " is not a file");
      return 1;
    }

    file.seekg(0, std::ios::end);
    contents.resize(file.tellg());
    file.seekg(0, std::ios::beg);
    file.read(&contents[0], contents.length());
  }
  catch (const std::ofstream::failure& e) {
    log_err("Read of ", path, " failed");
    return 1;
  }
  yang::Context context;

  // TODO: test.
  struct Test {};
  Test test;
  auto get_test = [&]()
  {
    log_info("in test, returning ", &test);
    return &test;
  };
  context.register_type<Test>("Test");
  context.register_function("get_test", Fn<Test*()>(get_test));
  auto id_fn = [&](Fn<int_t(int_t)> f)
  {
    return f;
  };
  context.register_function(
      "id_fn", Fn<Fn<int_t(int_t)>(Fn<int_t(int_t)>)>(id_fn));
  auto test_foo = [&](Test* t, Fn<int_t(int_t)> f, int_t a)
  {
    log_info("in Test:foo, self is ", t, ", a is ", a);
    return 2 * f(a);
  };
  context.register_member_function<Test>(
      "foo", Fn<int_t(Test*, Fn<int_t(int_t)>, int_t)>(test_foo));

  yang::Program program(context, path, contents);
  if (!program.success()) {
    return 1;
  }
  for (const auto& pair : program.get_globals()) {
    if (pair.second.is_exported()) {
      log_info("global [", pair.second.string(), "] ", pair.first);
    }
  }
  for (const auto& pair : program.get_functions()) {
    log_info("function [", pair.second.string(), "] ", pair.first);
  }

  log_info("Source:\n", program.print_ast());
  log_info("IR:\n", program.print_ir());

  // TODO: test.
  yang::Instance instance(program);
  auto t = instance.get_global<Test*>("test");
  log_info("global test is ", t);

  auto h = instance.get_function<Fn<Fn<int_t(int_t)>()>>("h");
  log_info("g(h()): ", instance.call<int_t>("g", h()));

  yang::Instance jnstance(program);
  auto iinc = instance.get_function<Fn<void()>>("increment");
  auto jinc = jnstance.get_function<Fn<void()>>("increment");
  auto icall = instance.get_function<Fn<void(Fn<void()>)>>("call_void");

  iinc();
  icall(iinc);
  icall(jinc);
  log_info("instance i: ", instance.get_global<int_t>("i"));
  log_info("jnstance i: ", jnstance.get_global<int_t>("i"));

  auto f = [](int_t t)
  {
    return t + 5;
  };
  auto lambda = Fn<int_t(int_t)>(f);
  log_info("g(lambda): ", jnstance.call<int_t>("g", lambda));
  return 0;
}
