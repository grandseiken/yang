#include <iostream>
#include <fstream>

#include "log.h"
#include "context.h"
#include "pipeline.h"

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
  context.register_function("get_test", std::function<Test*()>(get_test));
  auto test_foo = [&](Test* t, yang::int_t a)
  {
    log_info("in Test:foo, self is ", t, ", a is ", a);
    return 2 * a;
  };
  context.register_member_function<Test>(
      "foo", std::function<yang::int_t(Test*, yang::int_t)>(test_foo));

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
  log_info("f(): ", instance.call<yang::int_t>("f", 11));
  return 0;
}
