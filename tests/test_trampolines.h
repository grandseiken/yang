//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
TEST_F(YangTest, Trampolines)
{
  std::string str = R"(

  export test_int = int(int x)
  {
    return x * 2;
  };
  export test_float = float(float x)
  {
    return x * 2.;
  };
  export test_int4 = int4(int4 x)
  {
    return x * 2;
  };
  export test_float4 = float4(float4 x)
  {
    return x * 2.;
  };
  export test_function = int(int)(int(int) x)
  {
    return x;
  };
  export test_user_type = UserType(UserType x)
  {
    return x;
  };

  )";

  auto& inst = instance(str);
  EXPECT_EQ(inst.call<int_t>("test_int", 5), 10);
  EXPECT_EQ(inst.call<float_t>("test_float", 1.0 / 8), 1.0 / 4);

  ivec_t<4> ivec{1, 2, 3, 4};
  EXPECT_EQ(inst.call<ivec_t<4>>("test_int4", ivec), 2 * ivec);

  fvec_t<4> fvec{1.0 / 8, 1.0 / 4, 1.0 / 2, 1.0};
  EXPECT_EQ(inst.call<fvec_t<4>>("test_float4", fvec), 2. * fvec);

  typedef Function<int_t(int_t)> intf_t;
  auto test_int = inst.get_function<intf_t>("test_int");
  auto r = inst.call<intf_t>("test_function", test_int);
  EXPECT_EQ(test_int(1), r(1));

  user_type u;
  EXPECT_EQ(inst.call<user_type*>("test_user_type", &u), &u);
}

TEST_F(YangTest, ReverseTrampolines)
{
  std::string str = R"(

  export test_int = int()
  {
    return context_int(5);
  };
  export test_float = float()
  {
    return context_float(5.);
  };
  export test_int4 = int4()
  {
    return context_int4((0, -1, -2, -3));
  };
  export test_float4 = float4()
  {
    return context_float4((-.5, -1., -1.5, -2.));
  };
  export test_function = int(int)()
  {
    return context_function(int(int x) {return x * 2;});
  };
  export test_user_type = UserType()
  {
    return context_user_type(get_user_type());
  };

  )";

  typedef Function<int_t(int_t)> intf_t;
  auto context_int = [](int_t x)
  {
    return x;
  };
  auto context_float = [](float_t x)
  {
    return x;
  };
  auto context_int4 = [](ivec_t<4> x)
  {
    return x;
  };
  auto context_float4 = [](fvec_t<4> x)
  {
    return x;
  };
  auto context_function = [](intf_t x)
  {
    return x;
  };
  auto context_user_type = [](user_type* x)
  {
    return x;
  };

  auto& ctxt = context();
  ctxt.register_function("context_int", Function<int_t(int_t)>(context_int));
  ctxt.register_function(
      "context_float", Function<float_t(float_t)>(context_float));
  ctxt.register_function(
      "context_int4", Function<ivec_t<4>(ivec_t<4>)>(context_int4));
  ctxt.register_function(
      "context_float4", Function<fvec_t<4>(fvec_t<4>)>(context_float4));
  ctxt.register_function(
      "context_function", Function<intf_t(intf_t)>(context_function));
  ctxt.register_function(
      "context_user_type", Function<user_type*(user_type*)>(context_user_type));

  auto& inst = instance(ctxt, str);
  EXPECT_EQ(inst.call<int_t>("test_int"), 5);
  EXPECT_EQ(inst.call<float_t>("test_float"), 5.);
  EXPECT_EQ(inst.call<ivec_t<4>>("test_int4"), ivec_t<4>(0, -1, -2, -3));
  EXPECT_EQ(
      inst.call<fvec_t<4>>("test_float4"), fvec_t<4>(-.5, -1., -1.5, -2.));
  EXPECT_EQ(inst.call<intf_t>("test_function")(16), 32);
  EXPECT_EQ(inst.call<user_type*>("test_user_type")->id, 0);
}
