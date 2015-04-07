//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {
struct InterfacesTest : YangTest {};

// TODO: implement this.
TEST_F(InterfacesTest, DISABLED_Basic)
{
  struct Basic {
    yang::int_t the_int;
    yang::float_t the_float;
  };
  auto ctxt = context();
  ctxt.register_type<Basic>("Basic", false);
  ctxt.register_member_function("get_the_int", yang::make_fn([](Basic* b)
  {
    return b->the_int;
  }));
  ctxt.register_member_function("get_the_float", yang::make_fn([](Basic* b)
  {
    return b->the_float;
  }));
  auto basic_ctor = yang::make_fn([](yang::int_t i, yang::float_t f)
  {
    return new Basic{i, f};
  });
  auto basic_dtor = yang::make_fn([](Basic* b)
  {
    delete b;
  });
  ctxt.register_type("MBasic", basic_ctor, basic_dtor);
  ctxt.register_member_function("get_the_int", yang::make_fn([](Ref<Basic> b)
  {
    return b->the_int;
  }));
  ctxt.register_member_function("get_the_float", yang::make_fn([](Ref<Basic> b)
  {
    return b->the_float;
  }));

  auto prog = program(ctxt, R"(
interface foo {
  int() get_the_int;
  float() get_the_float;
}

get = float(foo f)
{
  return f.get_the_int(). + f.get_the_float();
}

export get_unmanaged = float(Basic b)
{
  return get(foo{b});
}

export get_managed = float(MBasic b)
{
  return get(foo{b});
})");
  // TODO: what about interfaces defined in subnamespaces?
}

} // ::yang
