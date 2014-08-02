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
  struct unknown {};
  user_type u;
  other o;
  unknown unk;

  ASSERT_NO_THROW(inst.call<void>("takes_user_type", &u));
  EXPECT_THROW(inst.call<void>("takes_user_type", &o), runtime_error);
  EXPECT_THROW(inst.call<void>("takes_user_type", &unk), runtime_error);

  ASSERT_NO_THROW(
      inst.call<void>("takes_user_type_f", make_fn([](user_type*){})));
  EXPECT_THROW(
      inst.call<void>("takes_user_type_f", make_fn([](other*){})),
      runtime_error);
  EXPECT_THROW(
      inst.call<void>("takes_user_type_f", make_fn([](unknown*){})),
      runtime_error);
}

TEST_F(UserTypesTest, UnmanagedObjectPassing)
{
  auto ctxt = context();
  ctxt.register_member_function("set_id", make_fn([](user_type* u, int_t id)
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

  inst.call<user_type*>("returns_user_type");
  inst.call<user_type*>("returns_user_type");
  user_type* u = inst.call<user_type*>("returns_user_type");
  EXPECT_EQ(3, u->id);

  inst.call<void>("takes_user_type", u);
  EXPECT_EQ(99, u->id);
  EXPECT_EQ(99, inst.get_global<user_type*>("u")->id);
  EXPECT_EQ(0, inst.get_global<user_type*>("v")->id);
}

TEST_F(UserTypesTest, UnmanagedMemberFunctions)
{
  auto ctxt = context();
  int_t extract_value = 0;
  ctxt.register_function("null_user_type", make_fn([]
  {
    return (user_type*)nullptr;
  }));
  ctxt.register_member_function("extract", make_fn([&](user_type* u)
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
      inst.get_function<Function<void(user_type*)>>("takes_user_type");
  user_type u{49};
  takes_user_type(&u);
  EXPECT_EQ(49, extract_value);
  u.id = 64;
  inst.call<void>("extract_uu");
  EXPECT_EQ(64, extract_value);
}

TEST_F(UserTypesTest, UnmanagedStealMemberFunction)
{
  auto inst = instance(R"(
export global var f = int() {return -1;};
export steal_function = void(UserType u)
{
  f = u.get_id;
})");

  typedef Function<int_t()> intf_t;
  user_type u{99};
  user_type v{64};
  EXPECT_EQ(-1, inst.get_global<intf_t>("f")());
  inst.call<void>("steal_function", &u);
}

TEST_F(UserTypesTest, ManagedRef)
{
  auto inst = instance(R"(
export make_user_type = MuserType()
{
  return MuserType();
})");

  {
    auto ref = inst.call<Ref<user_type>>("make_user_type");
    auto sef = inst.call<Ref<user_type>>("make_user_type");
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
    auto con = inst.get_global<Function<Ref<user_type>()>>("con");
    // TODO: throw an exception here, and the *next* test fails. Is something
    // not exception-safe here?
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

const std::string TestUserTypedefsStrA = R"(
export foo = a1()
{
  return get_a();
}
export bar = a2()
{
  return get_a();
}
export do_it = int()
{
  const a = foo();
  const b = bar();
  return foo().afn() + b.afn() +
         a2::afn(a) + a1::afn(bar());
}
)";

const std::string TestUserTypedefsStrB = R"(
export foo = a1()
{
  return get_a();
}
export bar = ma()
{
  return ma(foo());
}
)";

TEST_F(UserTypesTest, Typedefs)
{
  struct type_a {
    std::size_t count;
  };
  type_a the_a{0};

  auto afn = [](type_a*){
    return (int_t)4;
  };
  auto ctxt = context(false);
  ctxt.register_member_function("afn", make_fn(afn));
  ctxt.register_type<type_a>("a1");
  ctxt.register_type<type_a>("a2");
  ctxt.register_function("get_a", make_fn([&]{return &the_a;}));

  auto inst = instance(ctxt, TestUserTypedefsStrA);
  EXPECT_EQ(16, inst.call<int_t>("do_it"));

  auto ma_ctor = [](type_a* a)
  {
    ++a->count;
    return a;
  };
  auto ma_dtor = [](type_a* a)
  {
    --a->count;
  };
  ctxt.register_type("ma", make_fn(ma_ctor), make_fn(ma_dtor));
  {
    ASSERT_EQ(0, the_a.count);
    auto jnst = instance(ctxt, TestUserTypedefsStrB);
    auto ref = jnst.call<Ref<type_a>>("bar");
    EXPECT_EQ(1, the_a.count);
  }
  instance("");
  EXPECT_EQ(0, the_a.count);
}

// End namespace yang.
}
