//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {
struct InterfacesTest : YangTest {};

TEST_F(InterfacesTest, Basic)
{
  auto ctxt = context();
  auto prog = program(R"(
interface foo {
  int() get_the_int;
  float() get_the_float;
})");
}

} // ::yang
