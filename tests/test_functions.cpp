//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {

const std::string TestCppFunctionsStr = R"(
export via_context = int(int x)
{
  return cpp(x);
}
export via_scope_resolve = int()
{
  get_user_type();
  return UserType::cpp(get_user_type());
}
export via_explicit_scope_resolve = int()
{
  return UserType::ucpp(get_user_type());
}
export via_member_function = int()
{
  return get_user_type().cpp();
}
export via_argument = int(int(int) x)
{
  return x(5);
}
export via_return = int()
{
  return get_cpp()(6);
}
)";

TEST_F(YangTest, CppFunctions)
{
  if (!filter("functions")) {
    return;
  }

  auto cpp = make_fn([](int_t a)
  {
    return a * 11;
  });
  auto ucpp = make_fn([](user_type* u)
  {
    return int_t(u->id * 11);
  });
  auto get_cpp = make_fn([&]()
  {
    return cpp;
  });

  auto& ctxt = context();
  ctxt.register_function("cpp", cpp);
  ctxt.register_function("UserType::ucpp", ucpp);
  ctxt.register_function("get_cpp", get_cpp);
  ctxt.register_member_function<user_type>("cpp", ucpp);
  auto& inst = instance(ctxt, TestCppFunctionsStr);

  // C++ function called from C++.
  EXPECT_EQ(cpp(3), 33);
  // C++ function called from Yang via context.
  EXPECT_EQ(inst.call<int_t>("via_context", 2), 22);
  // C++ function called from Yang via scope-resolved member function.
  EXPECT_EQ(inst.call<int_t>("via_scope_resolve"), 11);
  // The same for an explicitly-registered function.
  EXPECT_EQ(inst.call<int_t>("via_explicit_scope_resolve"), 22);
  // C++ function called from Yang via member function.
  EXPECT_EQ(inst.call<int_t>("via_member_function"), 33);
  // C++ function passed to Yang.
  EXPECT_EQ(inst.call<int_t>("via_argument", cpp), 55);
  // C++ function returned to Yang.
  EXPECT_EQ(inst.call<int_t>("via_return"), 66);
}

const std::string TestYangFunctionsStr = R"(
global {
  var value = 1;
}
export add = int(int x)
{
  return value += x;
}
export add5 = int()
{
  return add(5);
}
export add_via_context = int()
{
  return context_function(add);
}
export get_add = int()()
{
  return add5;
}
export call_add = int(int(int) x)
{
  return x(13);
}
)";

TEST_F(YangTest, YangFunctions)
{
  if (!filter("functions")) {
    return;
  }

  typedef Function<int_t(int_t)> intf_t;
  auto context_function = make_fn([](intf_t function)
  {
    return function(-2);
  });
  auto& ctxt = context();
  ctxt.register_function("context_function", context_function);

  auto& inst = instance(ctxt, TestYangFunctionsStr);
  auto& jnst = instance(ctxt, TestYangFunctionsStr);

  auto inst_add = inst.get_function<intf_t>("add");
  auto jnst_add = jnst.get_function<intf_t>("add");
  auto inst_add5 = inst.get_function<Function<int_t()>>("add5");

  EXPECT_EQ(inst.get_global<int_t>("value"), 1);
  EXPECT_EQ(jnst.get_global<int_t>("value"), 1);

  // Call Yang function from C++.
  EXPECT_EQ(inst_add(3), 4);
  EXPECT_EQ(inst.get_global<int_t>("value"), 4);
  // Call Yang function from Yang.
  EXPECT_EQ(inst_add5(), 9);
  EXPECT_EQ(inst.get_global<int_t>("value"), 9);
  // Pass Yang function to C++.
  EXPECT_EQ(inst.call<int_t>("add_via_context"), 7);
  EXPECT_EQ(inst.get_global<int_t>("value"), 7);
  // Return Yang function to C++.
  EXPECT_EQ(inst.call<Function<int_t()>>("get_add")(), 12);
  EXPECT_EQ(inst.get_global<int_t>("value"), 12);
  // Call Yang function from a different Yang instance.
  EXPECT_EQ(inst.call<int_t>("call_add", jnst_add), 14);
  EXPECT_EQ(inst.get_global<int_t>("value"), 12);
  EXPECT_EQ(jnst.get_global<int_t>("value"), 14);
}

const std::string TestHighOrderFunctionsStr = R"(
export out = int()()()
{
  return int()()
  {
    return int()
    {
      return 1;
    };
  };
}

export out_in = int(int()()() x)
{
  return x()()();
}

export in_in = int(int(int) x)
{
  return x(1);
}
export in = int(int(int(int)) x)
{
  return x(int(int a) {return 1 + a;});
}
)";

TEST_F(YangTest, HighOrderFunctions)
{
  if (!filter("functions")) {
    return;
  }

  auto& inst = instance(TestHighOrderFunctionsStr);
  typedef Function<int_t()> intf_t;
  typedef Function<intf_t()> intf2_t;
  typedef Function<intf2_t()> intf3_t;
  auto out = inst.get_function<intf3_t>("out");
  auto out_in = inst.get_function<Function<int_t(intf3_t)>>("out_in");
  EXPECT_EQ(out()()(), 1);
  EXPECT_EQ(out_in(out), 1);

  typedef Function<int_t(int_t)> int2f_t;
  typedef Function<int_t(int2f_t)> int3f_t;
  typedef Function<int_t(int3f_t)> int4f_t;
  auto in_in = inst.get_function<int3f_t>("in_in");
  auto in = inst.get_function<int4f_t>("in");
  EXPECT_EQ(in(in_in), 2);
}

const std::string TestFunctionClosuresStr = R"(
global {
  const global_a = 33;
  const global_b = int()
  {
    closed const a = global_a / 3;
    return int()
    {
      return a + global_a;
    }();
  }();
  var global_var = 0;
  var stored = int()
  {
    return 13;
  };
}

export internal = int(int a)
{
  var result = 0;

  closed const b = 4;
  if (true) {
    closed const b = 8;
    const f = int() {return b;};
    result += f();
  }
  {
    closed const b = 1;
    result += int() {return b * 2;}();
  }
  result += int() {return a + b;}();

  closed const c = 5;
  result += int()
  {
    return int()
    {
      closed const d = 4;
      return c + int()
      {
        return int()
        {
          return d + c + global_b;
        }();
      }();
    }();
  }();

  closed var d = 1;
  const f = int()
  {
    return d *= 2;
  };
  result += f() + f() + f();

  return result;
}

export external = int()(int c)
{
  closed var v = 1;
  return int()
  {
    return v += c + ++global_var;
  };
}

export store = void()
{
  closed const t = stored;
  closed const v = ++global_var;
  const f = int()
  {
    return v + 2 * t();
  };
  stored = f;
}

export double = int(int n)
{
  closed const fac = int(int n)
  {
    const next = int()
    {
      return fac(n - 1);
    };
    return n ? n * next() : 1;
  };
  return int()
  {
    return fac(n);
  }();
}
)";

TEST_F(YangTest, FunctionClosures)
{
  if (!filter("functions")) {
    return;
  }

  auto& inst = instance(TestFunctionClosuresStr);
  EXPECT_EQ(inst.call<int_t>("internal", 3), 89);

  typedef Function<int_t()> intf_t;
  auto external = inst.call<intf_t>("external", 4);
  EXPECT_EQ(external(), 6);
  EXPECT_EQ(external(), 12);
  EXPECT_EQ(external(), 19);
  EXPECT_EQ(inst.call<intf_t>("external", 4)(), 9);
  EXPECT_EQ(external(), 28);
  EXPECT_EQ(inst.call<intf_t>("external", 3)(), 10);

  inst.call<void>("store");
  EXPECT_EQ(inst.get_global<intf_t>("stored")(), 33);
  inst.call<void>("store");
  EXPECT_EQ(inst.get_global<intf_t>("stored")(), 74);
  inst.call<void>("store");
  EXPECT_EQ(inst.get_global<intf_t>("stored")(), 157);

  EXPECT_EQ(inst.call<int_t>("double", 5), 120);
}

// End namespace yang.
}