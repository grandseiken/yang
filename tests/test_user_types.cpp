//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {

struct UserTypesTest : YangTest {};

const std::string TestUserTypesStrA = R"(
global {
  var oo = null_other_type();
}
export takes_user_type = int(UserType u)
{
  return u.get_id();
}
export takes_other_type = void(Other o)
{
  oo = o;
  const extract = o.extract;
  extract();
}
export extract_oo = void()
{
  takes_other_type(oo);
}
)";

const std::string TestUserTypesStrB = R"(
export global {
  var u = get_user_type();
  const v = u;
  var f = int() {return -1;};
}
export returns_user_type = UserType()
{
  u = get_user_type();
  return u;
}
export takes_user_type = void(UserType u)
{
  return u.set_id(99);
}
export call_user_type = void(void(UserType) x, UserType u)
{
  x(u);
}
export call_void = void(void() x)
{
  x();
}

export steal_function = void(UserType u)
{
  f = u.get_id;
}
)";

TEST_F(UserTypesTest, Unmanaged)
{
  struct other_t {
    int_t value;
  };
  int_t extract_value = 0;

  auto get_id = make_fn([](user_type* u)
  {
    return int_t(u->id);
  });
  auto set_id = make_fn([](user_type* u, int_t id)
  {
    u->id = id;
  });
  auto null_other_type = make_fn([]
  {
    return (other_t*)nullptr;
  });
  auto extract = make_fn([&](other_t* o)
  {
    extract_value = o->value;
  });

  auto ctxt = context();
  auto dtxt = context();
  ctxt.register_type<other_t>("Other");
  ctxt.register_function("null_other_type", null_other_type);
  ctxt.register_member_function("extract", extract);
  ctxt.register_member_function("get_id", get_id);
  ctxt.register_member_function("set_id", set_id);
  dtxt.register_member_function("get_id", get_id);
  dtxt.register_member_function("set_id", set_id);

  auto prog = instance(ctxt, TestUserTypesStrA);
  auto qrog = instance(dtxt, TestUserTypesStrB);

  // Do some things with user_type.
  qrog.call<user_type*>("returns_user_type");
  qrog.call<user_type*>("returns_user_type");
  user_type* u = qrog.call<user_type*>("returns_user_type");
  EXPECT_EQ(3, u->id);
  qrog.call<void>("takes_user_type", u);
  EXPECT_EQ(99, u->id);
  EXPECT_EQ(99, prog.call<int_t>("takes_user_type", u));
  EXPECT_EQ(99, qrog.get_global<user_type*>("u")->id);
  EXPECT_EQ(0, qrog.get_global<user_type*>("v")->id);

  // Do some things with other_t.
  auto takes_other_type =
      prog.get_function<Function<void(other_t*)>>("takes_other_type");
  other_t other;
  other.value = 49;
  takes_other_type(&other);
  EXPECT_EQ(49, extract_value);

  // Pass incorrect type to a program that knows about it.
  EXPECT_THROW(prog.call<int_t>("takes_user_type", &other), runtime_error);
  EXPECT_THROW(prog.call<int_t>("takes_other_type", u), runtime_error);
  // Pass incorrect type to a program that doesn't know about it.
  EXPECT_THROW(qrog.call<void>("takes_user_type", &other), runtime_error);
  // Pass function involving incorrect type to same.
  EXPECT_THROW(
      qrog.call<void>("call_user_type", takes_other_type, u), runtime_error);

  // Pass function that does something with another type, but doesn't expose it.
  other.value = 64;
  auto extract_oo = prog.get_function<Function<void()>>("extract_oo");
  qrog.call<void>("call_void", extract_oo);
  EXPECT_EQ(64, extract_value);

  typedef Function<int_t()> intf_t;
  EXPECT_EQ(-1, qrog.get_global<intf_t>("f")());
  qrog.call<void>("steal_function", u);
  EXPECT_EQ(99, qrog.get_global<intf_t>("f")());
}

const std::string TestManagedUserTypesStr = R"(
export global {
  const con = Managed;
  var m = con(2);
  var c = int() {return 0;};
}
export make_user_type = Managed()
{
  return Managed(17);
}
export get_some_count = int()
{
  const t = c();
  c = m.get_count;
  m = con(4);
  return t;
}
export get_count_closure = int()()
{
  closed const t = Managed(0);
  return int()
  {
    return t.get_count();
  };
}
)";

TEST_F(UserTypesTest, Managed)
{
  std::size_t count = 0;
  struct Managed {
    std::size_t count;
    int_t data;
  };
  struct BadManaged {};
  auto constructor = make_fn([&](int_t data)
  {
    return new Managed{count++, data};
  });
  auto destructor = make_fn([&](Managed* m)
  {
    --count;
    delete m;
  });
  auto get_count = make_fn([](Ref<Managed> m)
  {
    return (int_t)m->count;
  });

  auto ctxt = context();
  ctxt.register_type("Managed", constructor, destructor);
  ctxt.register_member_function("get_count", get_count);
  {
    auto inst = instance(ctxt, TestManagedUserTypesStr);
    {
      auto ref = inst.call<Ref<Managed>>("make_user_type");
      auto sef = inst.call<Ref<Managed>>("make_user_type");
      EXPECT_EQ(1, ref->count);
      EXPECT_EQ(2, sef->count);
      EXPECT_EQ(17, ref->data);
      EXPECT_EQ(3, count);

      auto con = inst.get_global<Function<Ref<Managed>(int_t)>>("con");
      EXPECT_EQ(3, con(0)->count);
      EXPECT_EQ(4, count);
    }
    instance("");
    EXPECT_EQ(1, count);
    EXPECT_EQ(0, inst.call<int_t>("get_some_count"));
    EXPECT_EQ(0, inst.call<int_t>("get_some_count"));
    EXPECT_EQ(1, inst.call<int_t>("get_some_count"));
    EXPECT_EQ(2, inst.call<int_t>("get_some_count"));

    auto get_count_closure = inst.call<Function<int_t()>>("get_count_closure");
    EXPECT_EQ(2, get_count_closure());
  }
  instance("");
  EXPECT_EQ(0, count);
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
