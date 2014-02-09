//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
TEST_F(YangTest, FunctionTest)
{
  // A weird function type that's never used. Make sure the global trampoline
  // works anyway.
  auto unused = [](int_t, int_t, int_t){return 1;};
  typedef Function<int_t(int_t, int_t, int_t)> uf_t;
  ASSERT_NO_THROW((uf_t(unused)));
  EXPECT_EQ(uf_t(unused)(1, 2, 3), 1);

  // Get type.
  auto& ctxt = context();
  EXPECT_TRUE(uf_t::get_type(ctxt).is_function());

  typedef Function<user_type*()> userf_t;
  Type t = userf_t::get_type(ctxt);
  ASSERT_TRUE(t.is_function());
  EXPECT_TRUE(t.get_function_return_type().is_user_type());
  EXPECT_EQ(t.get_function_return_type().get_user_type_name(), "UserType");

  // Using unregistered type.
  struct undef {};
  typedef Function<undef*()> undeff_t;
  EXPECT_THROW(undeff_t::get_type(ctxt), runtime_error);
}

TEST_F(YangTest, ContextTest)
{
  auto& ctxt = context();

  struct type_a {};
  struct type_b {};
  struct type_c {};

  ASSERT_NO_THROW(ctxt.register_type<type_a>("type_a"));
  EXPECT_THROW(ctxt.register_type<type_a>("type_a"), runtime_error);
  EXPECT_THROW(ctxt.register_type<type_a>("type_b"), runtime_error);
  EXPECT_THROW(ctxt.register_type<type_b>("type_a"), runtime_error);
  EXPECT_NO_THROW(ctxt.register_type<type_b>("type_b"));

  auto voidf = Function<void()>([](){});
  ASSERT_NO_THROW(ctxt.register_function("foo", voidf));
  EXPECT_THROW(ctxt.register_function("foo", voidf), runtime_error);
  EXPECT_NO_THROW(ctxt.register_function("bar", voidf));

  auto voidaf = Function<void(type_a*)>([](type_a*){});
  ASSERT_NO_THROW(ctxt.register_member_function<type_a>("foo", voidaf));
  EXPECT_THROW(
      ctxt.register_member_function<type_a>("foo", voidaf), runtime_error);
  EXPECT_NO_THROW(ctxt.register_member_function<type_a>("bar", voidaf));

  // Conflicts between member and nonmember functions.
  EXPECT_THROW(ctxt.register_function("type_a::foo", voidf), runtime_error);
  EXPECT_NO_THROW(ctxt.register_function("type_a::baz", voidf));
  EXPECT_THROW(
      ctxt.register_member_function<type_a>("baz", voidaf), runtime_error);

  EXPECT_TRUE(ctxt.has_type<type_a>());
  EXPECT_FALSE(ctxt.has_type<type_c>());
  EXPECT_EQ(ctxt.get_type_name<type_a>(), "type_a");
  EXPECT_EQ(ctxt.get_type_name<type_c>(), "");

  // Unregistered types.
  auto voidcf = Function<void(type_c*)>([](type_c*){});
  auto cf = Function<type_c*()>([](){
    return nullptr;
  });
  EXPECT_THROW(
      ctxt.register_member_function<type_c>("foo", voidcf), runtime_error);
  EXPECT_THROW(ctxt.register_function("voidcf", voidcf), runtime_error);
  EXPECT_THROW(ctxt.register_function("cf", cf), runtime_error);
}

std::string TestApisStr = R"(
export global var a = (0, 1, 2);
export global {
  const b = (0., 1., 2.);
};
global {
  var c = 3;
};
global const d = 18;

export f = int(int a)
{
  return a * 2;
};
g = int(int a)
{
  return a * 3;
};
)";

TEST_F(YangTest, ProgramTest)
{
  // General program API.
  auto& ctxt = context();
  auto& prog = program(ctxt, TestApisStr);
  EXPECT_EQ(&prog.get_context(), &ctxt);
  EXPECT_EQ(prog.get_name(), "test0");
  ASSERT_TRUE(prog.success());
  EXPECT_NO_THROW(prog.print_ast());
  EXPECT_NO_THROW(prog.print_ir());

  // Function access.
  ASSERT_EQ(prog.get_functions().size(), 1);
  auto it = prog.get_functions().begin();
  EXPECT_EQ(it->first, "f");
  auto type = it->second;
  EXPECT_TRUE(type.is_const());
  EXPECT_TRUE(type.is_exported());
  ASSERT_TRUE(type.is_function());
  ASSERT_EQ(type.get_function_num_args(), 1);
  EXPECT_TRUE(type.get_function_arg_type(0).is_int());
  EXPECT_TRUE(type.get_function_return_type().is_int());

  // Global access.
  ASSERT_EQ(prog.get_globals().size(), 4);
  ASSERT_NE(prog.get_globals().find("a"), prog.get_globals().end());
  ASSERT_NE(prog.get_globals().find("b"), prog.get_globals().end());
  ASSERT_NE(prog.get_globals().find("c"), prog.get_globals().end());
  ASSERT_NE(prog.get_globals().find("d"), prog.get_globals().end());

  auto at = prog.get_globals().find("a");
  auto bt = prog.get_globals().find("b");
  auto ct = prog.get_globals().find("c");
  auto dt = prog.get_globals().find("d");

  EXPECT_TRUE(at->second.is_vector());
  EXPECT_TRUE(at->second.is_int_vector());
  EXPECT_FALSE(at->second.is_int());
  EXPECT_EQ(at->second.get_vector_size(), 3);
  EXPECT_TRUE(at->second.is_exported());
  EXPECT_FALSE(at->second.is_const());

  EXPECT_TRUE(bt->second.is_vector());
  EXPECT_TRUE(bt->second.is_float_vector());
  EXPECT_FALSE(bt->second.is_float());
  EXPECT_EQ(bt->second.get_vector_size(), 3);
  EXPECT_TRUE(bt->second.is_exported());
  EXPECT_TRUE(bt->second.is_const());

  EXPECT_FALSE(ct->second.is_vector());
  EXPECT_FALSE(ct->second.is_int_vector());
  EXPECT_FALSE(ct->second.is_float());
  EXPECT_TRUE(ct->second.is_int());
  EXPECT_FALSE(ct->second.is_const());
  EXPECT_FALSE(ct->second.is_exported());

  EXPECT_TRUE(dt->second.is_int());
  EXPECT_FALSE(dt->second.is_function());
  EXPECT_FALSE(dt->second.is_user_type());
  EXPECT_FALSE(dt->second.is_float_vector());
  EXPECT_TRUE(dt->second.is_const());
  EXPECT_FALSE(dt->second.is_exported());
}

TEST_F(YangTest, Instance)
{
  typedef Function<int_t(int_t)> intf_t;
  typedef Function<void()> voidf_t;
  auto& prog = program(TestApisStr);
  auto& inst = instance(prog);
  EXPECT_EQ(&inst.get_program(), &prog);

  // Non-existent name.
  EXPECT_THROW(inst.get_function<intf_t>("nonexistent"), runtime_error);
  EXPECT_THROW(inst.call<int_t>("nonexistent"), runtime_error);
  // Not a function.
  EXPECT_THROW(inst.get_function<intf_t>("a"), runtime_error);
  EXPECT_THROW(inst.call<int_t>("a", 0), runtime_error);
  // Non-exported function.
  EXPECT_THROW(inst.get_function<intf_t>("g"), runtime_error);
  EXPECT_THROW(inst.call<int_t>("g", 0), runtime_error);
  // Function accessed via incompatible type.
  EXPECT_THROW(inst.get_function<voidf_t>("f"), runtime_error);
  EXPECT_THROW(inst.call<float_t>("f", 0), runtime_error);
  EXPECT_THROW(inst.call<int_t>("f", 0.), runtime_error);
  // Correct function access.
  EXPECT_NO_THROW(inst.get_function<intf_t>("f"));
  EXPECT_EQ(inst.call<int_t>("f", 1), 2);

  // Non-existent name.
  EXPECT_THROW(inst.get_global<int_t>("nonexistent"), runtime_error);
  EXPECT_THROW(inst.set_global("nonexistent", 0), runtime_error);
  // Not a global.
  EXPECT_THROW(inst.get_global<intf_t>("f"), runtime_error);
  EXPECT_THROW(
      inst.set_global("f", inst.get_function<intf_t>("f")), runtime_error);
  // Global accessed via incompatible type.
  EXPECT_THROW(inst.get_global<int_t>("a"), runtime_error);
  EXPECT_THROW(inst.set_global("a", 0), runtime_error);

  // Export var.
  EXPECT_EQ(inst.get_global<ivec_t<3>>("a"), ivec_t<3>(0, 1, 2));
  EXPECT_NO_THROW(inst.set_global("a", ivec_t<3>{0, -1, -2}));
  EXPECT_EQ(inst.get_global<ivec_t<3>>("a"), ivec_t<3>(0, -1, -2));
  // Export const.
  EXPECT_EQ(inst.get_global<fvec_t<3>>("b"), fvec_t<3>(0., 1., 2.));
  EXPECT_THROW(inst.set_global("b", fvec_t<3>(0., 1., 2.)), runtime_error);
  // Internal var.
  EXPECT_EQ(inst.get_global<int_t>("c"), 3);
  EXPECT_THROW(inst.set_global("c", 4), runtime_error);
  // Internal const.
  EXPECT_EQ(inst.get_global<int_t>("d"), 18);
  EXPECT_THROW(inst.set_global("d", 1), runtime_error);
}
