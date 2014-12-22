//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {
struct TrampolinesTest : YangTest {};

TEST_F(TrampolinesTest, ForwardInt)
{
  auto inst = instance(R"(
export f = int(int x)
{
  return x * 2;
}
)");
  EXPECT_EQ(10, inst.call<int_t>("f", 5));
}

TEST_F(TrampolinesTest, ForwardFloat)
{
  auto inst = instance(R"(
export f = float(float x)
{
  return x * 2.;
}
)");
  EXPECT_EQ(1. / 4, inst.call<float_t>("f", 1. / 8));
}

TEST_F(TrampolinesTest, ForwardIvec)
{
  auto inst = instance(R"(
export f = int4(int4 x)
{
  return x * 2;
}
)");
  ivec_t<4> in{1, 2, 3, 4};
  ivec_t<4> out{2, 4, 6, 8};
  EXPECT_EQ(out, inst.call<ivec_t<4>>("f", in));
}

TEST_F(TrampolinesTest, ForwardFvec)
{
  auto inst = instance(R"(
export f = float4(float4 x)
{
  return x * 2.;
}
)");
  fvec_t<4> in{1. / 8, 1. / 4, 1. / 2, 1.};
  fvec_t<4> out{1. / 4, 1. / 2, 1., 2.};
  EXPECT_EQ(out, inst.call<fvec_t<4>>("f", in));
}

TEST_F(TrampolinesTest, ForwardFunction)
{
  typedef Function<int_t(int_t)> intf_t;
  auto inst = instance(R"(
export f = int(int)(int(int) x)
{
  return x;
}
export g = int(int x)
{
  return x * 2;
}
)");
  auto g = inst.get_function<intf_t>("g");
  EXPECT_EQ(g(1), inst.call<intf_t>("f", g)(1));
}

TEST_F(TrampolinesTest, ForwardUserType)
{
  auto inst = instance(R"(
export f = UserType(UserType x)
{
  return x;
}
)");
  UserType u;
  EXPECT_EQ(&u, inst.call<UserType*>("f", &u));
}

TEST_F(TrampolinesTest, ForwardManagedUserType)
{
  auto inst = instance(R"(
export f = MuserType(MuserType x)
{
  return x;
}
export get_muser_type = MuserType()
{
  return MuserType();
}
)");
  auto u = inst.call<Ref<UserType>>("get_muser_type");
  EXPECT_EQ(u->id, inst.call<Ref<UserType>>("f", u)->id);
}

TEST_F(TrampolinesTest, ReverseInt)
{
  auto ctxt = context();
  ctxt.register_function("context", make_fn([](int_t x)
  {
    return x;
  }));

  auto inst = instance(ctxt, R"(
export f = int()
{
  return context(5);
}
)");
  EXPECT_EQ(5, inst.call<int_t>("f"));
}

TEST_F(TrampolinesTest, ReverseFloat)
{
  auto ctxt = context();
  ctxt.register_function("context", make_fn([](float_t x)
  {
    return x;
  }));

  auto inst = instance(ctxt, R"(
export f = float()
{
  return context(5.);
}
)");
  EXPECT_EQ(5., inst.call<float_t>("f"));
}

TEST_F(TrampolinesTest, ReverseIvec)
{
  auto ctxt = context();
  ctxt.register_function("context", make_fn([](ivec_t<4> x)
  {
    return x;
  }));

  auto inst = instance(ctxt, R"(
export f = int4()
{
  return context((0, -1, -2, -3));
}
)");
  EXPECT_EQ(ivec_t<4>(0, -1, -2, -3), inst.call<ivec_t<4>>("f"));
}

TEST_F(TrampolinesTest, ReverseFvec)
{
  auto ctxt = context();
  ctxt.register_function("context", make_fn([](fvec_t<4> x)
  {
    return x;
  }));

  auto inst = instance(ctxt, R"(
export f = float4()
{
  return context((-.5, -1., -1.5, -2.));
}
)");
  EXPECT_EQ(fvec_t<4>(-.5, -1., -1.5, -2.), inst.call<fvec_t<4>>("f"));
}

TEST_F(TrampolinesTest, ReverseFunction)
{
  typedef Function<int_t(int_t)> intf_t;
  auto ctxt = context();
  ctxt.register_function("context", make_fn([](intf_t x)
  {
    return x;
  }));

  auto inst = instance(ctxt, R"(
export f = int(int)()
{
  return context(int(int x) {return x * 2;});
}
)");
  EXPECT_EQ(32, inst.call<intf_t>("f")(16));
}

TEST_F(TrampolinesTest, ReverseUserType)
{
  auto ctxt = context();
  ctxt.register_function("context", make_fn([](UserType* x)
  {
    return x;
  }));

  auto inst = instance(ctxt, R"(
export f = UserType()
{
  return context(get_user_type());
}
)");
  EXPECT_EQ(0, inst.call<UserType*>("f")->id);
}

TEST_F(TrampolinesTest, ReverseManagedUserType)
{
  auto ctxt = context();
  ctxt.register_function("context", make_fn([](Ref<UserType> x)
  {
    return x;
  }));

  auto inst = instance(ctxt, R"(
export f = MuserType()
{
  return context(MuserType());
}
)");
  EXPECT_EQ(0, inst.call<Ref<UserType>>("f")->id);
}

TEST_F(TrampolinesTest, MultiArg)
{
  typedef Function<int_t(int_t)> intf_t;
  auto ctxt = context();
  ctxt.register_function("context", make_fn([](
      int_t a, ivec_t<4> b, float_t c, intf_t d, fvec_t<2> e, UserType* f)
  {
    return d(a) + b[0] + b[1] + b[2] + b[3] +
        int_t(c) + int_t(e[x]) + int_t(e[y]) + int_t(f->id);
  }));
  auto inst = instance(ctxt, R"(
export f = int(int a, int4 b, float c, int(int) d, float2 e, UserType f)
{
  return context(a, b, c, d, e, f);
}
)");

  auto d = make_fn([](int_t a)
  {
    return a * 2;
  });
  UserType u{12};
  int_t result = inst.call<int_t>(
      "f", 2, ivec_t<4>{1, 2, 3, 4}, 3.5, d, fvec_t<2>{7.1, 6.9}, &u);
  EXPECT_EQ(42, result);
}

} // ::yang
