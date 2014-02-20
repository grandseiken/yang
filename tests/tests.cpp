//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <memory>
#include <gtest/gtest.h>
#include <yang/yang.h>

namespace yang {

// Standard test fixture.
class YangTest : public testing::Test {
public:

  YangTest();
  ~YangTest() override {}

protected:

  struct user_type {
    std::size_t id;
  };

  // Deallocate all the objects constructed for this test so far.
  void clear();

  Context& context();
  Program& program_suppress_errors(const std::string& contents);
  Program& program_suppress_errors(const Context& context,
                                   const std::string& contents);
  Program& program(const std::string& contents, bool allow_errors = false);
  Program& program(const Context& context, const std::string& contents,
                   bool allow_errors = false);

  Instance& instance(const std::string& contents);
  Instance& instance(const Context& context, const std::string& contents);
  Instance& instance(const Program& program);

private:

  std::vector<std::unique_ptr<Context>> _contexts;
  std::vector<std::unique_ptr<Program>> _programs;
  std::vector<std::unique_ptr<Instance>> _instances;
  std::vector<std::unique_ptr<user_type>> _user_values;

  std::size_t _program_id;
  std::size_t _user_value_id;

};

// Test headers.
#include "test_apis.h"
#include "test_errors.h"
#include "test_functions.h"
#include "test_refcounting.h"
#include "test_semantics.h"
#include "test_trampolines.h"
#include "test_user_types.h"

// Fixture implementation.
YangTest::YangTest()
  : _program_id(0)
  , _user_value_id(0)
{
}

void YangTest::clear()
{
  _instances.clear();
  _user_values.clear();
  _programs.clear();
  _contexts.clear();
}

Context& YangTest::context()
{
  _contexts.emplace(_contexts.end(), new Context());
  auto& context = *_contexts.back();

  // Standard context has a user type registed and a function to produce them.
  // Each one gets a different ID.
  context.register_type<user_type>("UserType");
  auto get_user_type = [this]()
  {
    user_type* u = new user_type;
    u->id = _user_value_id++;
    _user_values.emplace_back(u);
    return u;
  };
  context.register_member_function<user_type>("foo", make_fn([](user_type*){}));
  context.register_function("get_user_type", make_fn(get_user_type));
  return context;
}

Program& YangTest::program_suppress_errors(const std::string& contents)
{
  return program_suppress_errors(context(), contents);
}

Program& YangTest::program_suppress_errors(const Context& context,
                                           const std::string& contents)
{
  std::string suppress_errors;
  _programs.emplace_back(
      new Program(context, "test" + std::to_string(_program_id++),
                  contents, true, &suppress_errors));
  return *_programs.back();
}

Program& YangTest::program(const std::string& contents, bool allow_errors)
{
  return program(context(), contents, allow_errors);
}

Program& YangTest::program(const Context& context, const std::string& contents,
                           bool allow_errors)
{
  Program* prog =
      new Program(context, "test" + std::to_string(_program_id++), contents);
  _programs.emplace_back(prog);
  if (!allow_errors) {
    EXPECT_EQ(prog->get_errors().size(), 0) <<
        "Should compile successfully:\n" << contents << std::endl;
    EXPECT_EQ(prog->get_warnings().size(), 0) <<
        "Should compile without warnings:\n" << contents << std::endl;
  }
  return *prog;
}

Instance& YangTest::instance(const std::string& contents)
{
  return instance(context(), contents);
}

Instance& YangTest::instance(
    const Context& context, const std::string& contents)
{
  return instance(program(context, contents));
}

Instance& YangTest::instance(const Program& program)
{
  _instances.emplace_back(new Instance(program));
  return *_instances.back();
}

// End namespace yang.
}

// Run all tests.
int main(int argc, char** argv)
{
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
