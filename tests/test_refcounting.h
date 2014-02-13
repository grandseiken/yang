//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
const std::string TestFunctionRefCountingStr = R"(
export global {
  var noop = int()
  {
    return 0;
  };
  var stored = int()
  {
    return 0;
  };

  {
    const t = stored;
  }
};

export store = void(int() x)
{
  stored = x;
};

export call = int()
{
  return stored();
};

export get = int()()
{
  return stored;
};

export pass_through = int()(int()() x)
{
  const y = x();
  return stored = y;
};

export store_in_local = void()
{
  var temp = stored;
  {
    const t = temp;
  }
  stored = noop;
  stored = temp;
};

export store_in_local_callout = void(void()() x)
{
  var temp = stored;
  stored = noop;
  x()();
  stored = temp;
};

export overwrite_and_return = int()()
{
  var temp = stored;
  stored = noop;
  return temp;
};
)";

TEST_F(YangTest, FunctionRefCounting)
{
  auto& inst = instance(TestFunctionRefCountingStr);
  typedef Function<int_t()> intf_t;

  // No refcounting necessary.
  auto f = intf_t([]()
  {
    return 13;
  });
  inst.call<void>("store", f);
  EXPECT_EQ(inst.call<int_t>("call"), 13);
  EXPECT_EQ(inst.call<intf_t>("get")(), 13);

  {
    // Goes out of scope before we use it.
    auto g = intf_t([]()
    {
      return 42;
    });
    inst.call<void>("store", g);
  }
  EXPECT_EQ(inst.call<int_t>("call"), 42);
  EXPECT_EQ(inst.call<intf_t>("get")(), 42);

  {
    // The same thing but via set_global.
    auto g = intf_t([]()
    {
      return 43;
    });
    inst.set_global<intf_t>("stored", g);
  }
  EXPECT_EQ(inst.call<int_t>("call"), 43);
  EXPECT_EQ(inst.call<intf_t>("get")(), 43);

  // Goes out of scope before we even return to Yang.
  int_t h_contents = 99;
  auto h = Function<intf_t()>([&]()
  {
    std::function<int_t()> t = [&]()
    {
      return ++h_contents;
    };
    return intf_t(t);
  });
  auto h_prime = inst.call<intf_t>("pass_through", h);
  EXPECT_EQ(h_prime(), 100);

  // Stored only in Yang locals for a while.
  inst.call<void>("store_in_local");
  EXPECT_EQ(inst.get_global<intf_t>("stored")(), 101);

  // Stored only in local while constructing new functions in C++.
  auto callout = Function<Function<void()>()>([]()
  {
    return Function<void()>([](){});
  });
  inst.call<void>("store_in_local_callout", callout);
  EXPECT_EQ(inst.get_global<intf_t>("stored")(), 102);

  EXPECT_EQ(inst.call<intf_t>("overwrite_and_return")(), 103);
  // It'd be nice to test memory usage and that everything is actually getting
  // destroyed at the end, but it's not clear how to do that unintrusively.
}
