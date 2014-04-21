//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <memory>
#include <string>
#include <unordered_set>
#include <gtest/gtest.h>
#include <yang/yang.h>

namespace yang {

// Standard test fixture.
class YangTest : public testing::Test {
public:

  YangTest();
  ~YangTest() override {}

  static void add_filter(const std::string& filter);
  static bool filter(const std::string& filter);

protected:

  struct user_type {
    std::size_t id;
  };
  struct muser_type {
    std::size_t id;
  };

  Context context();
  Program program_suppress_errors(const std::string& contents);
  Program program_suppress_errors(const Context& context,
                                  const std::string& contents);
  Program program(const std::string& contents, bool allow_errors = false);
  Program program(const Context& context, const std::string& contents,
                  bool allow_errors = false);

  Instance instance(const std::string& contents);
  Instance instance(const Context& context, const std::string& contents);
  Instance instance(const Program& program);

private:

  std::vector<std::unique_ptr<user_type>> _user_values;
  std::size_t _program_id;
  std::size_t _user_value_id;
  std::size_t _muser_value_id;
  static std::unordered_set<std::string> _filters; 

};

// End namespace yang.
}
