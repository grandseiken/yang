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
{
}

void YangTest::add_filter(const std::string& filter)
{
  _filters.insert(filter);
}

bool YangTest::filter(const std::string& filter)
{
  return _filters.empty() || _filters.count(filter);
}

void YangTest::clear()
{
  _instances.clear();
  _user_values.clear();
  _programs.clear();
  _contexts.clear();
}

void YangTest::clear_contexts()
{
  _contexts.clear();
}

void YangTest::clear_programs()
{
  _programs.clear();
}

void YangTest::clear_instances()
{
  _instances.clear();
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

std::unordered_set<std::string> YangTest::_filters;

// End namespace yang.
}

// Run all tests.
int main(int argc, char** argv)
{
  auto is = [](const std::string& cmdline, const std::string& arg)
  {
    return cmdline == "-" + arg || cmdline == "--" + arg;
  };
  auto filter = [&](const std::string& cmdline, const std::string& arg)
  {
    if (is(cmdline, arg)) {
      yang::YangTest::add_filter(arg);
      return true;
    }
    return false;
  };

  testing::InitGoogleTest(&argc, argv);
  // Run with commandline argument --forever to run tests over and over, and use
  // "top" or similar to check for memory leaks.
  // TODO: in fact, this *is* leaking (a very small amount of) memory that
  // doesn't seem to be down to the test framework itself. A few kilobytes per
  // second. Fix that.
  bool forever = false;
  // Run with --<test_set> to run only those tests (e.g. --apis).
  for (std::size_t i = 1; i < (std::size_t)argc; ++i) {
    if (is(argv[i], "forever")) {
      forever = true;
      continue;
    }
    if (filter(argv[i], "apis") || filter(argv[i], "errors") ||
        filter(argv[i], "exhaustive") || filter(argv[i], "functions") ||
        filter(argv[i], "refcounting") || filter(argv[i], "semantics") ||
        filter(argv[i], "trampolines") || filter(argv[i], "user_types")) {
      continue;
    }

    std::cerr << "unrecognised argument " << argv[i] << std::endl;
    return 1;
  }

  do {
    if (RUN_ALL_TESTS()) {
      return 1;
    }
  }
  while (forever);
  return 0;
}
