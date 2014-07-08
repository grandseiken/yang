//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {
struct FunctionTest : YangTest {};

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

TEST_F(FunctionTest, Cpp)
{
  auto cpp = make_fn([](int_t a)
  {
    return a * 11;
  });
  auto ucpp = make_fn([](user_type* u)
  {
    return int_t(u->id * 11);
  });
  auto get_cpp = make_fn([&]
  {
    return cpp;
  });

  auto ctxt = context();
  ctxt.register_function("cpp", cpp);
  ctxt.register_member_function("ucpp", ucpp);
  ctxt.register_function("get_cpp", get_cpp);
  ctxt.register_member_function("cpp", ucpp);
  auto inst = instance(ctxt, TestCppFunctionsStr);

  // C++ function called from C++.
  EXPECT_EQ(33, cpp(3));
  // C++ function called from Yang via context.
  EXPECT_EQ(22, inst.call<int_t>("via_context", 2));
  // C++ function called from Yang via scope-resolved member function.
  EXPECT_EQ(11, inst.call<int_t>("via_scope_resolve"));
  // The same for an explicitly-registered function.
  EXPECT_EQ(22, inst.call<int_t>("via_explicit_scope_resolve"));
  // C++ function called from Yang via member function.
  EXPECT_EQ(33, inst.call<int_t>("via_member_function"));
  // C++ function passed to Yang.
  EXPECT_EQ(55, inst.call<int_t>("via_argument", cpp));
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

TEST_F(FunctionTest, Yang)
{
  typedef Function<int_t(int_t)> intf_t;
  auto context_function = make_fn([](intf_t function)
  {
    return function(-2);
  });
  auto ctxt = context();
  ctxt.register_function("context_function", context_function);

  auto inst = instance(ctxt, TestYangFunctionsStr);
  auto jnst = instance(ctxt, TestYangFunctionsStr);

  auto inst_add = inst.get_function<intf_t>("add");
  auto jnst_add = jnst.get_function<intf_t>("add");
  auto inst_add5 = inst.get_function<Function<int_t()>>("add5");

  EXPECT_EQ(1, inst.get_global<int_t>("value"));
  EXPECT_EQ(1, jnst.get_global<int_t>("value"));

  // Call Yang function from C++.
  EXPECT_EQ(4, inst_add(3));
  EXPECT_EQ(4, inst.get_global<int_t>("value"));
  // Call Yang function from Yang.
  EXPECT_EQ(9, inst_add5());
  EXPECT_EQ(9, inst.get_global<int_t>("value"));
  // Pass Yang function to C++.
  EXPECT_EQ(7, inst.call<int_t>("add_via_context"));
  EXPECT_EQ(7, inst.get_global<int_t>("value"));
  // Return Yang function to C++.
  EXPECT_EQ(12, inst.call<Function<int_t()>>("get_add")());
  EXPECT_EQ(12, inst.get_global<int_t>("value"));
  // Call Yang function from a different Yang instance.
  EXPECT_EQ(14, inst.call<int_t>("call_add", jnst_add));
  EXPECT_EQ(12, inst.get_global<int_t>("value"));
  EXPECT_EQ(14, jnst.get_global<int_t>("value"));
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

TEST_F(FunctionTest, HighOrder)
{
  auto inst = instance(TestHighOrderFunctionsStr);
  typedef Function<int_t()> intf_t;
  typedef Function<intf_t()> intf2_t;
  typedef Function<intf2_t()> intf3_t;
  auto out = inst.get_function<intf3_t>("out");
  auto out_in = inst.get_function<Function<int_t(intf3_t)>>("out_in");
  EXPECT_EQ(1, out()()());
  EXPECT_EQ(1, out_in(out));

  typedef Function<int_t(int_t)> int2f_t;
  typedef Function<int_t(int2f_t)> int3f_t;
  typedef Function<int_t(int3f_t)> int4f_t;
  auto in_in = inst.get_function<int3f_t>("in_in");
  auto in = inst.get_function<int4f_t>("in");
  EXPECT_EQ(2, in(in_in));
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

TEST_F(FunctionTest, Closures)
{
  auto inst = instance(TestFunctionClosuresStr);
  EXPECT_EQ(89, inst.call<int_t>("internal", 3));

  typedef Function<int_t()> intf_t;
  auto external = inst.call<intf_t>("external", 4);
  EXPECT_EQ(6, external());
  EXPECT_EQ(12, external());
  EXPECT_EQ(19, external());
  EXPECT_EQ(9, inst.call<intf_t>("external", 4)());
  EXPECT_EQ(28, external());
  EXPECT_EQ(10, inst.call<intf_t>("external", 3)());

  inst.call<void>("store");
  EXPECT_EQ(33, inst.get_global<intf_t>("stored")());
  inst.call<void>("store");
  EXPECT_EQ(74, inst.get_global<intf_t>("stored")());
  inst.call<void>("store");
  EXPECT_EQ(157, inst.get_global<intf_t>("stored")());

  EXPECT_EQ(120, inst.call<int_t>("double", 5));
}

// End namespace yang.
}
