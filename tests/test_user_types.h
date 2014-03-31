//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
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

  auto& ctxt = context();
  auto& dtxt = context();
  ctxt.register_type<other_t>("Other");
  ctxt.register_function("null_other_type", null_other_type);
  ctxt.register_member_function<other_t>("extract", extract);
  ctxt.register_member_function<user_type>("get_id", get_id);
  ctxt.register_member_function<user_type>("set_id", set_id);
  dtxt.register_member_function<user_type>("get_id", get_id);
  dtxt.register_member_function<user_type>("set_id", set_id);

  auto& prog = instance(ctxt, TestUserTypesStrA);
  auto& qrog = instance(dtxt, TestUserTypesStrB);

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
