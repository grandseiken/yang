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
  ~YangTest() override;

protected:

  struct UserType {
    std::size_t id;
  };
  struct Other {};
  void force_collection();
  std::size_t get_managed_count();

  Context context(bool with_types = true);
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

  std::vector<std::unique_ptr<UserType>> _user_values;
  std::size_t _program_id;
  std::size_t _user_value_id;
  std::size_t _managed_count;

};

// End namespace yang.
}
