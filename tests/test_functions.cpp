//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {

struct FunctionTest : YangTest {
  Function<int_t(int_t)> cpp() const
  {
    return make_fn([](int_t a)
    {
      return a * 11;
    });
  }

  Function<int_t(UserType*)> ucpp() const
  {
    return make_fn([](UserType* u)
    {
      return int_t(u->id * 11);
    });
  }
};

TEST_F(FunctionTest, NativeViaFunction)
{
  EXPECT_EQ(33, cpp()(3));
}

TEST_F(FunctionTest, NativeViaContext)
{
  auto ctxt = context();
  ctxt.register_function("cpp", cpp());

  auto inst = instance(ctxt, R"(
export via_context = int(int x)
{
  return cpp(x);
})");
  EXPECT_EQ(22, inst.call<int_t>("via_context", 2));
}

TEST_F(FunctionTest, NativeViaExplicitMember)
{
  auto ctxt = context();
  ctxt.register_member_function("ucpp", ucpp());

  auto inst = instance(ctxt, R"(
export via_scope_resolve = int()
{
  get_user_type();
  return UserType::ucpp(get_user_type());
})");
  EXPECT_EQ(11, inst.call<int_t>("via_scope_resolve"));
}

TEST_F(FunctionTest, NativeViaMember)
{
  auto ctxt = context();
  ctxt.register_member_function("ucpp", ucpp());

  auto inst = instance(ctxt, R"(
export via_member_function = int()
{
  get_user_type();
  return get_user_type().ucpp();
})");
  EXPECT_EQ(11, inst.call<int_t>("via_member_function"));
}

TEST_F(FunctionTest, NativeViaArgument)
{
  auto inst = instance(R"(
export via_argument = int(int(int) x)
{
  return x(5);
})");
  EXPECT_EQ(55, inst.call<int_t>("via_argument", cpp()));
}

TEST_F(FunctionTest, NativeViaReturn)
{
  auto ctxt = context();
  ctxt.register_function("get_cpp", make_fn([&]
  {
    return cpp();
  }));

  auto inst = instance(ctxt, R"(
export via_return = int()
{
  return get_cpp()(6);
})");
  EXPECT_EQ(inst.call<int_t>("via_return"), 66);
}

TEST_F(FunctionTest, YangViaYang)
{
  auto inst = instance(R"(
export global var value = 1;
export add = int(int x)
{
  return value += x;
}
export add5 = int()
{
  return add(5);
})");

  ASSERT_EQ(1, inst.get_global<int_t>("value"));
  EXPECT_EQ(4, inst.call<int_t>("add", 3));
  EXPECT_EQ(4, inst.get_global<int_t>("value"));
  EXPECT_EQ(9, inst.call<int_t>("add5"));
  EXPECT_EQ(9, inst.get_global<int_t>("value"));
}

TEST_F(FunctionTest, YangViaArgument)
{
  typedef Function<int_t(int_t)> intf_t;
  auto ctxt = context();
  ctxt.register_function("context_function", make_fn([](intf_t function)
  {
    return function(-2);
  }));

  auto inst = instance(ctxt, R"(
export global var value = 1;
export add = int(int x)
{
  return value += x;
}
export add_via_context = int()
{
  return context_function(add);
})");
  ASSERT_EQ(1, inst.get_global<int_t>("value"));
  EXPECT_EQ(-1, inst.call<int_t>("add_via_context"));
  EXPECT_EQ(-1, inst.get_global<int_t>("value"));
}

TEST_F(FunctionTest, YangViaReturn)
{
  auto inst = instance(R"(
export global var value = 1;
export get_add = int()()
{
  return int()
  {
    return value += 5;
  };
})");

  typedef Function<int_t()> intf_t;
  ASSERT_EQ(1, inst.get_global<int_t>("value"));
  EXPECT_EQ(6, inst.call<intf_t>("get_add")());
  EXPECT_EQ(6, inst.get_global<int_t>("value"));
}

TEST_F(FunctionTest, YangViaOtherInstance)
{
  const std::string source = R"(
export global var value = 1;
export add = int(int x)
{
  return value += x;
}
export call_add = int(int(int) x)
{
  return x(13);
})";

  auto inst = instance(source);
  auto jnst = instance(source);
  ASSERT_EQ(1, inst.get_global<int_t>("value"));
  ASSERT_EQ(1, jnst.get_global<int_t>("value"));

  typedef Function<int_t(int_t)> intf_t;
  auto jnst_add = jnst.get_function<intf_t>("add");
  EXPECT_EQ(14, inst.call<int_t>("call_add", jnst_add));
  EXPECT_EQ(14, jnst.get_global<int_t>("value"));
  EXPECT_EQ(1, inst.get_global<int_t>("value"));
}

TEST_F(FunctionTest, HighOrderOut)
{
  auto inst = instance(R"(
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
})");

  typedef Function<int_t()> intf_t;
  typedef Function<intf_t()> intf2_t;
  typedef Function<intf2_t()> intf3_t;
  auto out = inst.get_function<intf3_t>("out");
  auto out_in = inst.get_function<Function<int_t(intf3_t)>>("out_in");
  EXPECT_EQ(1, out()()());
  EXPECT_EQ(1, out_in(out));
}

TEST_F(FunctionTest, HighOrderIn)
{
  auto inst = instance(R"(
export in_in = int(int(int) x)
{
  return x(1);
}
export in = int(int(int(int)) x)
{
  return x(int(int a) {return 1 + a;});
})");

  typedef Function<int_t(int_t)> intf_t;
  typedef Function<int_t(intf_t)> intf2_t;
  typedef Function<int_t(intf2_t)> intf3_t;
  auto in_in = inst.get_function<intf2_t>("in_in");
  auto in = inst.get_function<intf3_t>("in");
  EXPECT_EQ(2, in(in_in));
}

TEST_F(FunctionTest, ClosuresInternal)
{
  auto inst = instance(R"(
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
})");
  EXPECT_EQ(89, inst.call<int_t>("internal", 3));
}

TEST_F(FunctionTest, ClosuresExternal)
{
  auto inst = instance(R"(
global var global_var = 0;
export external = int()(int c)
{
  closed var v = 1;
  return int()
  {
    return v += c + ++global_var;
  };
})");

  typedef Function<int_t()> intf_t;
  auto external = inst.call<intf_t>("external", 4);
  EXPECT_EQ(6, external());
  EXPECT_EQ(12, external());
  EXPECT_EQ(19, external());
  EXPECT_EQ(9, inst.call<intf_t>("external", 4)());
  EXPECT_EQ(28, external());
  EXPECT_EQ(10, inst.call<intf_t>("external", 3)());
}

TEST_F(FunctionTest, ClosuresStored)
{
  auto inst = instance(R"(
global var global_var = 6;
export global var stored = int()
{
  return 13;
};

export store = void()
{
  closed const t = stored;
  closed const v = ++global_var;
  const f = int()
  {
    return v + 2 * t();
  };
  stored = f;
})");

  typedef Function<int_t()> intf_t;
  inst.call<void>("store");
  EXPECT_EQ(33, inst.get_global<intf_t>("stored")());
  inst.call<void>("store");
  EXPECT_EQ(74, inst.get_global<intf_t>("stored")());
  inst.call<void>("store");
  EXPECT_EQ(157, inst.get_global<intf_t>("stored")());
}

TEST_F(FunctionTest, ClosuresDouble)
{
  auto inst = instance(R"(
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
)");
  EXPECT_EQ(120, inst.call<int_t>("double", 5));
}

// End namespace yang.
}
