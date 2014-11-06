//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {
struct RefcountingTest : YangTest {};

TEST_F(RefcountingTest, StoredFunctions)
{
  auto inst = instance(R"(
export global var stored = int() {return 0;};
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
})");

  typedef Function<int_t()> intf_t;
  EXPECT_EQ(0, inst.get_global<intf_t>("stored")());

  // Goes out of scope before we use it.
  inst.call<void>("store", make_fn([]{return 42;}));
  EXPECT_EQ(42, inst.call<int_t>("call"));
  EXPECT_EQ(42, inst.call<intf_t>("get")());

  // The same thing but via set_global.
  inst.set_global<intf_t>("stored", make_fn([]{return 43;}));
  EXPECT_EQ(43, inst.call<int_t>("call"));
  EXPECT_EQ(43, inst.call<intf_t>("get")());
}

TEST_F(RefcountingTest, AdvancedStoredFunctions)
{
  auto inst = instance(R"(
global const noop = int() {return 0;};
export global var stored = noop;

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
})");

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

  typedef Function<int_t()> intf_t;
  auto h_prime = inst.call<intf_t>("pass_through", h);
  EXPECT_EQ(100, h_prime());
  EXPECT_EQ(101, inst.get_global<intf_t>("stored")());

  // Stored only in Yang locals for a while.
  inst.call<void>("store_in_local");
  EXPECT_EQ(107, inst.get_global<intf_t>("stored")());

  // Stored only in local while constructing new functions in C++.
  auto callout = make_fn([]
  {
    return make_fn([]{});
  });
  inst.call<void>("store_in_local_callout", callout);
  EXPECT_EQ(111, inst.get_global<intf_t>("stored")());

  EXPECT_EQ(112, inst.call<intf_t>("overwrite_and_return")());
}

TEST_F(RefcountingTest, FunctionTemporaries)
{
  auto ctxt = context();
  int_t n = 0;
  ctxt.register_function("get_fn", make_fn([&]
  {
    return make_fn([&]{return ++n;});
  }));

  auto inst = instance(ctxt, R"(
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
})");

  EXPECT_EQ(13, inst.call<int_t>("temporaries"));
}

TEST_F(RefcountingTest, ClosureLoops)
{
  auto inst = instance(R"(
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
    const v = alloc();
    result += v();
    if (!i++) {
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
})");
  EXPECT_EQ(18, inst.call<int_t>("loops"));
}

TEST_F(RefcountingTest, CyclicClosures)
{
  auto inst = instance(R"(
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
})");

  typedef Function<int_t()> intf_t;
  EXPECT_EQ(3, inst.call<intf_t>("cycles")());
}

TEST_F(RefcountingTest, GlobalClosures)
{
  auto ctxt = context();
  ctxt.register_member_function("f", make_fn([](UserType* t, int_t a)
  {
    return int_t(t->id * 2 * a);
  }));

  auto inst = instance(ctxt, R"(
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
})");

  typedef Function<int_t(int_t)> intf_t;
  EXPECT_EQ(0, inst.get_global<intf_t>("f")(4));
  EXPECT_EQ(8, inst.get_global<intf_t>("g")(4));
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

TEST_F(RefcountingTest, YangViaYang)
{
  typedef Function<int_t()> intf_t;
  auto f = make_fn([]{return int_t(0);});
  {
    auto inst = instance(R"(
export f = int()()
{
  closed var t = 0;
  return int()
  {
    return t++;
  };
})");
    f = inst.call<intf_t>("f");
  }
  force_collection();
  EXPECT_EQ(f(), 0);
  EXPECT_EQ(f(), 1);
  EXPECT_EQ(f(), 2);
}

TEST_F(RefcountingTest, NativeViaYang)
{
  typedef Function<int_t()> intf_t;
  auto f = make_fn([]{return int_t(0);});
  {
    auto ctxt = context();
    ctxt.register_function("from_context", make_fn([]{
      return int_t(-1);
    }));

    auto inst = instance(ctxt, R"(
export f = int()()
{
  return from_context;
})");
    f = inst.call<intf_t>("f");
  }
  force_collection();
  EXPECT_EQ(f(), -1);
}

TEST_F(RefcountingTest, ReverseNativeViaYang)
{
  typedef Function<int_t()> intf_t;
  auto f = make_fn([]{return int_t(0);});
  {
    auto ctxt = context();
    ctxt.register_function("from_yang", make_fn([&](intf_t g){
      f = g;
    }));

    auto inst = instance(ctxt, R"(
export f = void(int() g)
{
  from_yang(g);
})");
    inst.call<void>("f", make_fn([]{return int_t(-1);}));
  }
  force_collection();
  EXPECT_EQ(-1, f());
}

TEST_F(RefcountingTest, Strings)
{
  auto ctxt = context();
  ctxt.register_type<const char>("string", true);

  const char* str = nullptr;
  {
    auto inst = instance(ctxt, R"(
export test = string(int a)
{
  return a > 1 ? "str" : a ? """str""" : "st" "r";
})");
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
