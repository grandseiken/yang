//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {

struct RefcountingTest : YangTest {};

const std::string TestFunctionRefCountingStr = R"(
export global {
  const noop = int()
  {
    return 0;
  };
  var stored = int()
  {
    return 0;
  };

  {
    const t = stored;
    t;
  }
}

export store = void(int() x)
{
  stored = x;
}

export call = int()
{
  return stored();
}

export get = int()()
{
  return stored;
}

export pass_through = int()(int()() x)
{
  const y = x();
  return stored = y;
}

export store_in_local = void()
{
  const temp = stored;
  var i = 0;
  for (const v = temp; i < 10; ++i) {
    const t = v;
    const tt = stored;
    t;
    tt;
  }
  for (i = 0; (const t = stored)() && i < 4; ++i) {
    const tt = t;
    tt;
  }
  stored = noop;
  stored = temp;
}

export store_in_local_callout = void(void()() x)
{
  const temp = stored;
  if (1) {
    const t = stored;
    t;
  }
  else (const t = stored)() && t();
  if (0) {
    const t = stored;
    t;
  }
  if ((const t = stored)()) {
    const tt = t;
    tt;
  }
  do {
    const t = stored;
    t;
  }
  while (!((const t = stored)() && t()));
  stored = noop;
  x()();
  stored = temp;
}

export overwrite_and_return = int()()
{
  const temp = stored;
  stored = noop;
  return temp;
}

export temporaries = int()
{
  const f = int(int() a, int() b)
  {
    return a() + b();
  };
  const g = int()(int n)
  {
    return int()
    {
      return n;
    };
  };
  return f(get_fn(), get_fn()) + f(g(3), g(7));
}

export loops = int()
{
  const alloc = int()()
  {
    closed var n = 0;
    return int()
    {
      return ++n;
    };
  };

  var result = 0;
  while (const v = alloc()()) {
    const u = alloc();
    if (!result) {
      result += u();
      continue;
    }
    result += v;
    result += u();
    if (result) {
      result += alloc()();
      break;
    }
  }

  var i = 0;
  do {
    ++i;
    const v = alloc();
    result += v();
    if (i == 1) {
      continue;
    }
    result += v();
    if (i == 2) {
      break;
    }
  }
  while (result += alloc()());

  for (const t = alloc();
       t() + (const u = alloc()()) + alloc()(); alloc()) {
    if (result == 9) {
      const w = alloc();
      result += t() + u + w();
      continue;
    }
    if (result > 9) {
      result += t() + u;
      break;
    }
  }

  do {const t = alloc(); t(); break;} while(true);
  for (const t = alloc(); !t();) result += t();

  return result;
}
)";

TEST_F(RefcountingTest, Functions)
{
  auto ctxt = context();
  int_t n = 0;
  ctxt.register_function("get_fn", make_fn([&]
  {
    return make_fn([&]{return ++n;});
  }));

  auto inst = instance(ctxt, TestFunctionRefCountingStr);
  typedef Function<int_t()> intf_t;
  EXPECT_EQ(0, inst.get_global<intf_t>("stored")());

  // No refcounting necessary.
  auto f = make_fn([]
  {
    return 13;
  });
  inst.call<void>("store", f);
  EXPECT_EQ(13, inst.call<int_t>("call"));
  EXPECT_EQ(13, inst.call<intf_t>("get")());

  {
    // Goes out of scope before we use it.
    auto g = make_fn([]
    {
      return 42;
    });
    inst.call<void>("store", g);
  }
  EXPECT_EQ(42, inst.call<int_t>("call"));
  EXPECT_EQ(42, inst.call<intf_t>("get")());

  {
    // The same thing but via set_global.
    auto g = make_fn([]
    {
      return 43;
    });
    inst.set_global<intf_t>("stored", g);
  }
  EXPECT_EQ(43, inst.call<int_t>("call"));
  EXPECT_EQ(43, inst.call<intf_t>("get")());

  // Goes out of scope before we even return to Yang.
  int_t h_contents = 99;
  auto h = make_fn([&]
  {
    // Make it a bit complicated so it's not all optimised away.
    std::function<int_t()> t = [&]
    {
      return ++h_contents;
    };
    return make_fn(t);
  });
  auto h_prime = inst.call<intf_t>("pass_through", h);
  EXPECT_EQ(100, h_prime());

  // Stored only in Yang locals for a while.
  inst.call<void>("store_in_local");
  EXPECT_EQ(106, inst.get_global<intf_t>("stored")());

  // Stored only in local while constructing new functions in C++.
  auto callout = make_fn([]
  {
    return make_fn([]{});
  });
  inst.call<void>("store_in_local_callout", callout);
  EXPECT_EQ(110, inst.get_global<intf_t>("stored")());

  EXPECT_EQ(111, inst.call<intf_t>("overwrite_and_return")());
  EXPECT_EQ(13, inst.call<int_t>("temporaries"));
  EXPECT_EQ(18, inst.call<int_t>("loops"));

  // It'd be nice to test memory usage and that everything is actually getting
  // destroyed at the end, but it's not clear how to do that unintrusively.
}

const std::string TestStructureRefCountingStr = R"(
export cycles = int()()
{
  closed var n = 0;
  closed var ff = int() {return ++n;};
  const f = int()
  {
    closed var m = 0;
    closed var gg = int() {return ++m;};
    const g = int()
    {
      closed var l = 0;
      closed var hh = int() {return 0;};
      const h = int()
      {
        const r = ++l + gg() + ff();
        ff = h;
        gg = h;
        hh = h;
        const dasd = g; dasd;
        return r;
      };
      hh = h;
      return hh();
    };
    return g();
  };
  return f;
}

global const u = get_user_type();
export global {
  var f = int(int) {return 0;};
  var g = f;
  do {
    closed const t = u.f;
    void() {
      f = t;
      g = get_user_type().f;
    }();
  } while (false);
}
)";

TEST_F(RefcountingTest, Structures)
{
  auto ctxt = context();
  auto member = make_fn([](user_type* t, int_t a)
  {
    return int_t(t->id * 2 * a);
  });
  ctxt.register_member_function("f", member);

  auto inst = instance(ctxt, TestStructureRefCountingStr);
  typedef Function<int_t()> intf_t;
  auto cycles = inst.call<intf_t>("cycles");
  EXPECT_EQ(3, cycles());

  typedef Function<int_t(int_t)> intf2_t;
  EXPECT_EQ(0, inst.get_global<intf2_t>("f")(4));
  EXPECT_EQ(8, inst.get_global<intf2_t>("g")(4));
}

TEST_F(RefcountingTest, Objects)
{
  Instance inst = instance("");
  {
    Program prog = program("");
    {
      auto ctxt = context();
      ctxt.register_function("f", make_fn([]
      {
        return int_t(17);
      }));
      prog = program(Context(ctxt), "export g = int() {return f();}");
    }
    {
      inst = instance(Program(prog));
    }
  }
  auto jnst = instance("");
  jnst = inst;
  {
    auto knst = jnst;
    knst = instance("");
  }
  EXPECT_EQ(17, Instance(jnst).call<int_t>("g"));
}

const std::string TestStringRefCountingStr = R"(
export test = string(int a)
{
  return a > 1 ? "str" : a ? """str""" : "st" "r";
}
)";

TEST_F(RefcountingTest, Strings)
{
  auto ctxt = context();
  ctxt.register_type<const char>("string", true);

  const char* str = nullptr;
  {
    auto inst = instance(ctxt, TestStringRefCountingStr);
    str = inst.call<Ref<const char>>("test", 2).get();
    EXPECT_EQ(std::string(str), std::string("str"));
    str = inst.call<Ref<const char>>("test", 1).get();
    EXPECT_EQ(std::string(str), std::string("str"));
    str = inst.call<Ref<const char>>("test", 0).get();
  }
  EXPECT_EQ(std::string(str), std::string("str"));
}

// End namespace yang.
}
