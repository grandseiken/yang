//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {

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

TEST_F(YangTest, UserTypes)
{
  if (!filter("user_types")) {
    return;
  }

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
  auto null_other_type = make_fn([]()
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
  EXPECT_EQ(u->id, 3);
  qrog.call<void>("takes_user_type", u);
  EXPECT_EQ(u->id, 99);
  EXPECT_EQ(prog.call<int_t>("takes_user_type", u), 99);
  EXPECT_EQ(qrog.get_global<user_type*>("u")->id, 99);
  EXPECT_EQ(qrog.get_global<user_type*>("v")->id, 0);

  // Do some things with other_t.
  auto takes_other_type =
      prog.get_function<Function<void(other_t*)>>("takes_other_type");
  other_t other;
  other.value = 49;
  takes_other_type(&other);
  EXPECT_EQ(extract_value, 49);

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
  EXPECT_EQ(extract_value, 64);

  typedef Function<int_t()> intf_t;
  EXPECT_EQ(qrog.get_global<intf_t>("f")(), -1);
  qrog.call<void>("steal_function", u);
  EXPECT_EQ(qrog.get_global<intf_t>("f")(), 99);
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

TEST_F(YangTest, ManagedUserTypes)
{
  if (!filter("user_types")) {
    return;
  }

  std::size_t count = 0;
  struct Managed {
    std::size_t count;
    int_t data;
  };
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
  ctxt.register_managed_type("Managed", constructor, destructor);
  ctxt.register_member_function("get_count", get_count);
  {
    auto inst = instance(ctxt, TestManagedUserTypesStr);
    {
      auto ref = inst.call<Ref<Managed>>("make_user_type");
      auto sef = inst.call<Ref<Managed>>("make_user_type");
      EXPECT_EQ(ref->count, 1);
      EXPECT_EQ(sef->count, 2);
      EXPECT_EQ(ref->data, 17);
      EXPECT_EQ(count, 3);

      auto con = inst.get_global<Function<Ref<Managed>(int_t)>>("con");
      EXPECT_EQ(con(0)->count, 3);
      EXPECT_EQ(count, 4);
    }
    instance("");
    EXPECT_EQ(count, 1);
    EXPECT_EQ(inst.call<int_t>("get_some_count"), 0);
    EXPECT_EQ(inst.call<int_t>("get_some_count"), 0);
    EXPECT_EQ(inst.call<int_t>("get_some_count"), 1);
    EXPECT_EQ(inst.call<int_t>("get_some_count"), 2);

    auto get_count_closure = inst.call<Function<int_t()>>("get_count_closure");
    EXPECT_EQ(get_count_closure(), 2);
  }
  instance("");
  EXPECT_EQ(count, 0);
}

// End namespace yang.
}
