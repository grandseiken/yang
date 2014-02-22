//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
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
  for (i = 0; i < 5 && (const t = stored)(); ++i) {
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
)";

TEST_F(YangTest, FunctionRefCounting)
{
  auto& inst = instance(TestFunctionRefCountingStr);
  typedef Function<int_t()> intf_t;
  // No refcounting necessary.
  auto f = make_fn([]()
  {
    return 13;
  });
  inst.call<void>("store", f);
  EXPECT_EQ(inst.call<int_t>("call"), 13);
  EXPECT_EQ(inst.call<intf_t>("get")(), 13);

  {
    // Goes out of scope before we use it.
    auto g = make_fn([]()
    {
      return 42;
    });
    inst.call<void>("store", g);
  }
  EXPECT_EQ(inst.call<int_t>("call"), 42);
  EXPECT_EQ(inst.call<intf_t>("get")(), 42);

  {
    // The same thing but via set_global.
    auto g = make_fn([]()
    {
      return 43;
    });
    inst.set_global<intf_t>("stored", g);
  }
  EXPECT_EQ(inst.call<int_t>("call"), 43);
  EXPECT_EQ(inst.call<intf_t>("get")(), 43);

  // Goes out of scope before we even return to Yang.
  int_t h_contents = 99;
  auto h = make_fn([&]()
  {
    // Make it a bit complicated so it's not all optimised away.
    std::function<int_t()> t = [&]()
    {
      return ++h_contents;
    };
    return make_fn(t);
  });
  auto h_prime = inst.call<intf_t>("pass_through", h);
  EXPECT_EQ(h_prime(), 100);

  // Stored only in Yang locals for a while.
  inst.call<void>("store_in_local");
  EXPECT_EQ(inst.get_global<intf_t>("stored")(), 106);

  // Stored only in local while constructing new functions in C++.
  auto callout = make_fn([]()
  {
    return make_fn([](){});
  });
  inst.call<void>("store_in_local_callout", callout);
  EXPECT_EQ(inst.get_global<intf_t>("stored")(), 110);

  EXPECT_EQ(inst.call<intf_t>("overwrite_and_return")(), 111);
  // It'd be nice to test memory usage and that everything is actually getting
  // destroyed at the end, but it's not clear how to do that unintrusively.
}

// TODO: implement and test closure refcounting.
