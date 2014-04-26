//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {

TEST_F(YangTest, TypeOfTest)
{
  if (!filter("apis")) {
    return;
  }

  EXPECT_TRUE(type_of<int_t>().is_int());
  EXPECT_FALSE(type_of<int_t>().is_ivec());
  EXPECT_TRUE(type_of<fvec_t<3>>().is_fvec());
  EXPECT_EQ(type_of<fvec_t<3>>().vector_size(), 3);

  typedef Function<user_type*(int_t)> ft;
  EXPECT_TRUE(type_of<ft>().is_function());
  EXPECT_EQ(type_of<ft>().function_num_args(), 1);
  EXPECT_TRUE(type_of<ft>().function_return().is_user_type());
  EXPECT_FALSE(type_of<ft>().function_return().is_managed_user_type());

  struct a {};
  struct b {};
  EXPECT_EQ(type_of<a*>(), type_of<a*>());
  EXPECT_EQ(type_of<Ref<a>>(), type_of<Ref<a>>());
  EXPECT_NE(type_of<a*>(), type_of<b*>());
  EXPECT_NE(type_of<Ref<a>>(), type_of<Ref<b>>());
  EXPECT_NE(type_of<a*>(), type_of<Ref<a>>());
}

TEST_F(YangTest, FunctionApiTest)
{
  if (!filter("apis")) {
    return;
  }

  // A weird function type that's never used. Make sure the global trampoline
  // works anyway.
  auto unused = [](int_t, int_t, int_t){return 1;};
  typedef Function<int_t(int_t, int_t, int_t)> uf_t;
  ASSERT_NO_THROW((uf_t(unused)));
  EXPECT_EQ(uf_t(unused)(1, 2, 3), 1);
  EXPECT_EQ(make_fn(unused)(1, 2, 3), 1);
}

TEST_F(YangTest, ContextApiTest)
{
  if (!filter("apis")) {
    return;
  }
  auto ctxt = context();

  struct type_a {};
  struct type_b {};
  struct type_c {};
  struct type_d {};

  ASSERT_NO_THROW(ctxt.register_type<type_a>("TypeA"));
  EXPECT_THROW(ctxt.register_type<type_a>("TypeA"), runtime_error);
  EXPECT_THROW(ctxt.register_type<type_b>("TypeA"), runtime_error);
  EXPECT_NO_THROW(ctxt.register_type<type_b>("TypeB"));
  EXPECT_NO_THROW(ctxt.register_type<type_a>("TypeA2"));

  auto voidf = make_fn([]{});
  ASSERT_NO_THROW(ctxt.register_function("foo", voidf));
  EXPECT_THROW(ctxt.register_function("foo", voidf), runtime_error);
  EXPECT_NO_THROW(ctxt.register_function("bar", voidf));

  auto voidaf = make_fn([](type_a*){});
  ASSERT_NO_THROW(ctxt.register_member_function("foo", voidaf));
  EXPECT_THROW(ctxt.register_member_function("foo", voidaf), runtime_error);
  EXPECT_NO_THROW(ctxt.register_member_function("bar", voidaf));

  // Conflicts between member and nonmember functions.
  EXPECT_THROW(ctxt.register_function("TypeA::foo", voidf), runtime_error);
  EXPECT_NO_THROW(ctxt.register_member_function("baz", voidaf));
  EXPECT_THROW(ctxt.register_member_function("baz", voidaf), runtime_error);

  // Bad names.
  auto ccon = make_fn([]{return (type_c*)nullptr;});
  auto voidcf = make_fn([](type_c*){});
  EXPECT_THROW(ctxt.register_function("!", voidf), runtime_error);
  EXPECT_THROW(ctxt.register_member_function("~", voidaf), runtime_error);
  EXPECT_THROW(ctxt.register_type<type_c>("$c"), runtime_error);
  EXPECT_THROW(ctxt.register_type("a-b", ccon, voidcf), runtime_error);

  // Managed/unmanaged simultaeneously.
  auto refaf = make_fn([](Ref<type_a>){});
  auto refcf = make_fn([](Ref<type_c>){});
  ctxt.register_type("TypeC", ccon, voidcf);
  EXPECT_NO_THROW(ctxt.register_member_function("man", voidcf));
  EXPECT_NO_THROW(ctxt.register_member_function("umn", refaf));
  EXPECT_NO_THROW(ctxt.register_member_function("man2", refcf));

  // Constructor conflicts.
  auto dcon = make_fn([]{return (type_d*)nullptr;});
  auto voiddf = make_fn([](type_d*){});
  EXPECT_THROW(ctxt.register_function("TypeC", voidf), runtime_error);
  EXPECT_THROW(ctxt.register_type("bar", dcon, voiddf), runtime_error);

  // Namespace conflicts.
  auto dtxt = context(false);
  EXPECT_THROW(dtxt.register_namespace("dtxt", dtxt), runtime_error);
  EXPECT_THROW(ctxt.register_namespace("TypeC", dtxt), runtime_error);
  EXPECT_THROW(ctxt.register_namespace("%", dtxt), runtime_error);
  ctxt.register_namespace("dtxt", dtxt);
  EXPECT_THROW(ctxt.register_namespace("dtxt", dtxt), runtime_error);
  EXPECT_THROW(ctxt.register_type<type_d>("dtxt"), runtime_error);

  struct type_e {};
  struct type_f {};
  auto etxt = context(false);
  etxt.register_type<type_e>("E");
  ctxt.register_function("make_e", make_fn([]{
    return (type_e*)nullptr;
  }));
  ctxt.register_type<type_e>("Eman");
  EXPECT_NO_THROW(ctxt.register_namespace("whatever", etxt));
  etxt.register_member_function("foo", voidaf);
  EXPECT_NO_THROW(ctxt.register_namespace("again", etxt));
  auto ftxt = context(false);
  ftxt.register_member_function("foo", make_fn([](type_a*){}));
  EXPECT_THROW(ctxt.register_namespace("another", ftxt), runtime_error);
}

const std::string TestApisStr = R"(
export global var a = (0, 1, 2);
export global {
  const b = (0., 1., 2.);
}
global {
  var c = 3;
}
global const d = 18;

export f = int(int a)
{
  return a * 2;
}
g = int(int a)
{
  return a * 3;
}

// Avoid warnings.
global {
  a = a; b; c = c; d; g;
}
)";

TEST_F(YangTest, ProgramApiTest)
{
  if (!filter("apis")) {
    return;
  }

  // General program API.
  auto ctxt = context();
  auto prog = program(ctxt, TestApisStr);
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
  ASSERT_EQ(type.function_num_args(), 1);
  EXPECT_TRUE(type.function_arg(0).is_int());
  EXPECT_TRUE(type.function_return().is_int());

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
  EXPECT_TRUE(at->second.is_ivec());
  EXPECT_FALSE(at->second.is_int());
  EXPECT_EQ(at->second.vector_size(), 3);
  EXPECT_TRUE(at->second.is_exported());
  EXPECT_FALSE(at->second.is_const());

  EXPECT_TRUE(bt->second.is_vector());
  EXPECT_TRUE(bt->second.is_fvec());
  EXPECT_FALSE(bt->second.is_float());
  EXPECT_EQ(bt->second.vector_size(), 3);
  EXPECT_TRUE(bt->second.is_exported());
  EXPECT_TRUE(bt->second.is_const());

  EXPECT_FALSE(ct->second.is_vector());
  EXPECT_FALSE(ct->second.is_ivec());
  EXPECT_FALSE(ct->second.is_float());
  EXPECT_TRUE(ct->second.is_int());
  EXPECT_FALSE(ct->second.is_const());
  EXPECT_FALSE(ct->second.is_exported());

  EXPECT_TRUE(dt->second.is_int());
  EXPECT_FALSE(dt->second.is_function());
  EXPECT_FALSE(dt->second.is_user_type());
  EXPECT_FALSE(dt->second.is_fvec());
  EXPECT_TRUE(dt->second.is_const());
  EXPECT_FALSE(dt->second.is_exported());
}

TEST_F(YangTest, InstanceApiTest)
{
  if (!filter("apis")) {
    return;
  }

  typedef Function<int_t(int_t)> intf_t;
  typedef Function<void()> voidf_t;
  auto prog = program(TestApisStr);
  auto inst = instance(prog);

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

// End namespace yang.
}
