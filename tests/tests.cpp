//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {

// Fixture implementation.
YangTest::YangTest()
  : _program_id(0)
  , _user_value_id(0)
  , _muser_value_id(0)
{
}

Context YangTest::context(bool with_types)
{
  Context context;
  if (!with_types) {
    return context;
  }
  // Standard context has a user type registed and a function to produce them.
  // Each one gets a different ID.
  context.register_type<user_type>("UserType");
  auto get_user_type = [this]
  {
    user_type* u = new user_type{_user_value_id++};
    _user_values.emplace_back(u);
    return u;
  };

  auto constructor = make_fn([this]
  {
    muser_type* m = new muser_type{_muser_value_id++};
    return m;
  });
  auto destructor = make_fn([](muser_type* m)
  {
    delete m;
  });

  context.register_member_function("foo", make_fn([](user_type*){}));
  context.register_function("get_user_type", make_fn(get_user_type));
  context.register_type("MuserType", constructor, destructor);
  return context;
}

Program YangTest::program_suppress_errors(const std::string& contents)
{
  return program_suppress_errors(context(), contents);
}

Program YangTest::program_suppress_errors(const Context& context,
                                          const std::string& contents)
{
  std::string suppress_errors;
  return Program(context, "test" + std::to_string(_program_id++),
                 contents, true, &suppress_errors);
}

Program YangTest::program(const std::string& contents, bool allow_errors)
{
  return program(context(), contents, allow_errors);
}

Program YangTest::program(const Context& context, const std::string& contents,
                          bool allow_errors)
{
  Program program(context, "test" + std::to_string(_program_id++), contents);
  if (!allow_errors) {
    EXPECT_EQ(0, program.get_errors().size()) <<
        "Should compile successfully:\n" << contents << std::endl;
    EXPECT_EQ(0, program.get_warnings().size()) <<
        "Should compile without warnings:\n" << contents << std::endl;
  }
  return program;
}

Instance YangTest::instance(const std::string& contents)
{
  return instance(context(), contents);
}

Instance YangTest::instance(
    const Context& context, const std::string& contents)
{
  return instance(program(context, contents));
}

Instance YangTest::instance(const Program& program)
{
  return Instance(program);
}

// End namespace yang.
}

// To run a subset of tests, use --gtest_filter. For example, for all tests,
//   --gtest_filter=*.*
// or negate some with
//   --gtest_filter=-Foo.*:-*.Bar
//
// To run tests forever, use
//   --gtest_repeat=-1
// (and use top to check for memory leaks).
int main(int argc, char** argv)
{
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
  // TODO: in fact, this is leaking a very small amount of memory that
  // doesn't seem to be down to the test framework itself. A few kilobytes per
  // second. It's kind of weird though: seems to occur when on program
  // instantiation where the program has a few globals, but not if the program
  // is empty. That doesn't make too much sense. Does "top" report actual
  // memory usage or is it maybe just fragmentation overhead or something?
}
