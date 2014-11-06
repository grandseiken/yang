//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {
struct UserTypesTest : YangTest {};

TEST_F(UserTypesTest, IncorrectTypes)
{
  auto ctxt = context();
  auto inst = instance(R"(
export takes_user_type = void(UserType) {}
export takes_user_type_f = void(void(UserType)) {}
)");
  struct Unknown;
  UserType u;
  Other o;

  ASSERT_NO_THROW(inst.call<void>("takes_user_type", &u));
  EXPECT_THROW(inst.call<void>("takes_user_type", &o), RuntimeError);
  EXPECT_THROW(inst.call<void>("takes_user_type", (Unknown*)nullptr),
               RuntimeError);

  ASSERT_NO_THROW(
      inst.call<void>("takes_user_type_f", make_fn([](UserType*){})));
  EXPECT_THROW(
      inst.call<void>("takes_user_type_f", make_fn([](Other*){})),
      RuntimeError);
  EXPECT_THROW(
      inst.call<void>("takes_user_type_f", make_fn([](Unknown*){})),
      RuntimeError);
}

TEST_F(UserTypesTest, RawObjectPassing)
{
  auto ctxt = context();
  ctxt.register_member_function("set_id", make_fn([](UserType* u, int_t id)
  {
    u->id = id;
  }));

  auto inst = instance(ctxt, R"(
export global {
  var u = get_user_type();
  const v = u;
}
export returns_user_type = UserType()
{
  u = get_user_type();
  return u;
}
export takes_user_type = void(UserType u)
{
  return u.set_id(99);
})");

  inst.call<UserType*>("returns_user_type");
  inst.call<UserType*>("returns_user_type");
  UserType* u = inst.call<UserType*>("returns_user_type");
  EXPECT_EQ(3, u->id);

  inst.call<void>("takes_user_type", u);
  EXPECT_EQ(99, u->id);
  EXPECT_EQ(99, inst.get_global<UserType*>("u")->id);
  EXPECT_EQ(0, inst.get_global<UserType*>("v")->id);
}

TEST_F(UserTypesTest, RawMemberFunctions)
{
  auto ctxt = context();
  int_t extract_value = 0;
  ctxt.register_function("null_user_type", make_fn([]
  {
    return (UserType*)nullptr;
  }));
  ctxt.register_member_function("extract", make_fn([&](UserType* u)
  {
    extract_value = u->id;
  }));

  auto inst = instance(ctxt, R"(
global var uu = null_user_type();
export takes_user_type = void(UserType u)
{
  uu = u;
  const extract = u.extract;
  extract();
}
export extract_uu = void()
{
  takes_user_type(uu);
})");

  auto takes_user_type =
      inst.get_function<Function<void(UserType*)>>("takes_user_type");
  UserType u{49};
  takes_user_type(&u);
  EXPECT_EQ(49, extract_value);
  u.id = 64;
  inst.call<void>("extract_uu");
  EXPECT_EQ(64, extract_value);
}

TEST_F(UserTypesTest, RawStealMemberFunction)
{
  auto inst = instance(R"(
export global var f = int() {return -1;};
export steal_function = void(UserType u)
{
  f = u.get_id;
})");

  typedef Function<int_t()> intf_t;
  UserType u{99};
  EXPECT_EQ(-1, inst.get_global<intf_t>("f")());
  inst.call<void>("steal_function", &u);
  EXPECT_EQ(99, inst.get_global<intf_t>("f")());
}

TEST_F(UserTypesTest, ManagedRef)
{
  auto inst = instance(R"(
export make_user_type = MuserType()
{
  return MuserType();
})");

  {
    auto ref = inst.call<Ref<UserType>>("make_user_type");
    auto sef = inst.call<Ref<UserType>>("make_user_type");
    EXPECT_EQ(0, ref->id);
    EXPECT_EQ(1, sef->id);
    EXPECT_EQ(2, get_managed_count());
  }
  EXPECT_EQ(0, get_managed_count());
}

TEST_F(UserTypesTest, ManagedGlobal)
{
  {
    auto inst = instance(R"(
export global {
  const con = MuserType;
  var m = con();
})");

    EXPECT_EQ(1, get_managed_count());  
    auto con = inst.get_global<Function<Ref<UserType>()>>("con");
    EXPECT_EQ(1, con()->id);
    EXPECT_EQ(1, get_managed_count());  
  }
  EXPECT_EQ(0, get_managed_count());
}

TEST_F(UserTypesTest, ManagedLocal)
{
  {
    auto inst = instance(R"(
global var m = MuserType();
export global var c = int() {return 0;};
export get_some_count = int()
{
  const t = c();
  c = m.get_id;
  m = MuserType();
  return t;
})");

    EXPECT_EQ(0, inst.call<int_t>("get_some_count"));
    EXPECT_EQ(0, inst.call<int_t>("get_some_count"));
    EXPECT_EQ(1, inst.call<int_t>("get_some_count"));
    EXPECT_EQ(2, inst.call<int_t>("get_some_count"));
    EXPECT_EQ(2, get_managed_count());
  }
  EXPECT_EQ(0, get_managed_count());
}

TEST_F(UserTypesTest, ManagedClosure)
{
  auto inst = instance(R"(
export get_count_closure = int()()
{
  closed const t = MuserType();
  return int()
  {
    return t.get_id();
  };
})");

  {
    auto get_count_closure = inst.call<Function<int_t()>>("get_count_closure");
    EXPECT_EQ(0, get_count_closure());
    EXPECT_EQ(1, get_managed_count());
  }
  EXPECT_EQ(0, get_managed_count());
}

TEST_F(UserTypesTest, RawTypedefs)
{
  UserType u{0};
  auto ctxt = context(false);
  ctxt.register_type<UserType>("u1");
  ctxt.register_type<UserType>("u2");
  ctxt.register_function("get_u", make_fn([&]{return &u;}));
  ctxt.register_member_function("mfn", make_fn([](UserType*)
  {
    return (int_t)4;
  }));

  auto inst = instance(ctxt, R"(
export foo = u1()
{
  return get_u();
}
export bar = u2()
{
  return get_u();
}
export do_it = int()
{
  const a = foo();
  const b = bar();
  return foo().mfn() + b.mfn() +
         u2::mfn(a) + u1::mfn(bar());
})");
  EXPECT_EQ(16, inst.call<int_t>("do_it"));
}

TEST_F(UserTypesTest, ManagedTypedefs)
{
  UserType u{0};
  auto ctxt = context(false);
  ctxt.register_type<UserType>("u");
  ctxt.register_function("get_u", make_fn([&]{return &u;}));

  auto ma_ctor = [](UserType* u)
  {
    ++u->id;
    return u;
  };
  auto ma_dtor = [](UserType* u)
  {
    --u->id;
  };
  ctxt.register_type("mu", make_fn(ma_ctor), make_fn(ma_dtor));

  {
    ASSERT_EQ(0, u.id);
    auto inst = instance(ctxt, R"(
export foo = u()
{
  return get_u();
}
export bar = mu()
{
  return mu(foo());
}
)");
    auto ref = inst.call<Ref<UserType>>("bar");
    EXPECT_EQ(1, u.id);
  }
  force_collection();
  EXPECT_EQ(0, u.id);
}

// End namespace yang.
}
