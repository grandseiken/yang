//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {

const std::string TestTrampolinesStr = R"(
export test_int = int(int x)
{
  return x * 2;
}
export test_float = float(float x)
{
  return x * 2.;
}
export test_int4 = int4(int4 x)
{
  return x * 2;
}
export test_float4 = float4(float4 x)
{
  return x * 2.;
}
export test_function = int(int)(int(int) x)
{
  return x;
}
export test_user_type = UserType(UserType x)
{
  return x;
}
export test_muser_type = MuserType(MuserType x)
{
  return x;
}
export get_muser_type = MuserType()
{
  return MuserType();
}
)";

TEST_F(YangTest, Trampolines)
{
  if (!filter("trampolines")) {
    return;
  }

  auto inst = instance(TestTrampolinesStr);
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

  auto m = inst.call<Ref<muser_type>>("get_muser_type");
  EXPECT_EQ(inst.call<Ref<muser_type>>("test_muser_type", m)->id, m->id);
}

const std::string TestReverseTrampolinesStr = R"(
export test_int = int()
{
  return context_int(5);
}
export test_float = float()
{
  return context_float(5.);
}
export test_int4 = int4()
{
  return context_int4((0, -1, -2, -3));
}
export test_float4 = float4()
{
  return context_float4((-.5, -1., -1.5, -2.));
}
export test_function = int(int)()
{
  return context_function(int(int x) {return x * 2;});
}
export test_user_type = UserType()
{
  return context_user_type(get_user_type());
}
export test_muser_type = MuserType()
{
  return context_muser_type(MuserType());
}
)";

TEST_F(YangTest, ReverseTrampolines)
{
  if (!filter("trampolines")) {
    return;
  }

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
  auto context_muser_type = [](Ref<muser_type> x)
  {
    return x;
  };

  auto ctxt = context();
  ctxt.register_function("context_int", make_fn(context_int));
  ctxt.register_function("context_float", make_fn(context_float));
  ctxt.register_function("context_int4", make_fn(context_int4));
  ctxt.register_function("context_float4", make_fn(context_float4));
  ctxt.register_function("context_function", make_fn(context_function));
  ctxt.register_function("context_user_type", make_fn(context_user_type));
  ctxt.register_function("context_muser_type", make_fn(context_muser_type));

  auto inst = instance(ctxt, TestReverseTrampolinesStr);
  EXPECT_EQ(inst.call<int_t>("test_int"), 5);
  EXPECT_EQ(inst.call<float_t>("test_float"), 5.);
  EXPECT_EQ(inst.call<ivec_t<4>>("test_int4"), ivec_t<4>(0, -1, -2, -3));
  EXPECT_EQ(
      inst.call<fvec_t<4>>("test_float4"), fvec_t<4>(-.5, -1., -1.5, -2.));
  EXPECT_EQ(inst.call<intf_t>("test_function")(16), 32);
  EXPECT_EQ(inst.call<user_type*>("test_user_type")->id, 0);
  EXPECT_EQ(inst.call<Ref<muser_type>>("test_muser_type")->id, 0);
}

const std::string TestMultiArgTrampolinesStr = R"(
export test = int(int a, int4 b, float c, int(int) d, float2 e, UserType f)
{
  return context_test(a, b, c, d, e, f);
}
)";

TEST_F(YangTest, MultiArgTrampolines)
{
  if (!filter("trampolines")) {
    return;
  }

  auto ctxt = context();
  typedef Function<int_t(int_t)> intf_t;
  auto context_test = [](
      int_t a, ivec_t<4> b, float_t c, intf_t d, fvec_t<2> e, user_type* f)
  {
    return d(a) + b[0] + b[1] + b[2] + b[3] +
        int_t(c) + int_t(e[x]) + int_t(e[y]) + int_t(f->id);
  };
  ctxt.register_function("context_test", make_fn(context_test));

  auto d = [](int_t a)
  {
    return a * 2;
  };
  user_type u{12};
  int_t result = instance(ctxt, TestMultiArgTrampolinesStr).call<int_t>(
      "test",
      2, ivec_t<4>{1, 2, 3, 4}, 3.5, make_fn(d), fvec_t<2>{7.1, 6.9}, &u);
  EXPECT_EQ(result, 42);
}

// End namespace yang.
}
