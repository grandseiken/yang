//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <memory>
#include <gtest/gtest.h>
#include <yang/yang.h>

// Standard test fixture.
class YangTest : public testing::Test {
public:

  ~YangTest() override {}

protected:

  struct user_type {
    yang::int_t id;
  };

  yang::Context& context();
  yang::Program& program(const std::string& contents);
  yang::Program& program(const yang::Context& context,
                         const std::string& contents);

  yang::Instance& instance(const std::string& contents);
  yang::Instance& instance(const yang::Context& context,
                           const std::string& contents);
  yang::Instance& instance(const yang::Program& program);

private:

  std::vector<std::unique_ptr<yang::Context>> _contexts;
  std::vector<std::unique_ptr<yang::Program>> _programs;
  std::vector<std::unique_ptr<yang::Instance>> _instances;
  std::vector<std::unique_ptr<user_type>> _user_values;
  yang::int_t _user_value_id;

};

// Test headers.
namespace yang {

#include "test_trampolines.h"

// End namespace yang.
}

yang::Context& YangTest::context()
{
  _contexts.emplace(_contexts.end(), new yang::Context());
  auto& context = *_contexts.back(); 

  context.register_type<user_type>("UserType");
  auto get_user_type = [this]()
  {
    user_type* u = new user_type;
    u->id = _user_value_id++;
    _user_values.emplace_back(u);
    return u;
  };

  context.register_function(
      "get_user_type", yang::Function<user_type*()>(get_user_type));
  return context;
}

yang::Program& YangTest::program(const std::string& contents)
{
  _programs.emplace_back(new yang::Program(context(), "test", contents));
  return *_programs.back();
}

yang::Program& YangTest::program(const yang::Context& context,
                                 const std::string& contents)
{
  _programs.emplace_back(new yang::Program(context, "test", contents));
  return *_programs.back();
}

yang::Instance& YangTest::instance(const std::string& contents)
{
  _instances.emplace_back(new yang::Instance(program(contents)));
  return *_instances.back();
}

yang::Instance& YangTest::instance(const yang::Context& context,
                                   const std::string& contents)
{
  _instances.emplace_back(new yang::Instance(program(context, contents)));
  return *_instances.back();
}

yang::Instance& YangTest::instance(const yang::Program& program)
{
  _instances.emplace_back(new yang::Instance(program));
  return *_instances.back();
}

int main(int argc, char** argv)
{
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
