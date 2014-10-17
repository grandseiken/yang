//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {

struct ApiTest : YangTest {
  Program test_program()
  {
    return program(R"(
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
)");
  }

  Instance test_instance()
  {
    return instance(test_program());
  }
};

TEST_F(ApiTest, VecConstructors)
{
  std::stringstream ss;
  ss << ivec_t<4>{};
  EXPECT_EQ("(0, 0, 0, 0)", ss.str());

  ss.str(std::string{});
  ss << ivec_t<2>{1, 1.9f};
  EXPECT_EQ("(1, 1)", ss.str());

  ss.str(std::string{});
  ss << ivec_t<2>{fvec_t<2>{1, 1.9f}};
  EXPECT_EQ("(1, 1)", ss.str());
}

TEST_F(ApiTest, VecIndices)
{
  ivec_t<2> v{1, 2};
  EXPECT_EQ(1, v[0]);
  EXPECT_EQ(2, v[element_accessor<1>{}]);
  EXPECT_EQ(2, v[y]);

  v[1] = 3;
  v[x] = 4;
  EXPECT_EQ(4, v[element_accessor<0>{}]);
  EXPECT_EQ(3, v[1]);
}

TEST_F(ApiTest, VecLogicalOps)
{
  ivec_t<3> v{0, 0, 2};
  ivec_t<3> u{1, 0, 1};

  EXPECT_EQ((bvec_t<3>{0, 0, 1}), v && u);
  EXPECT_EQ((bvec_t<3>{1, 0, 1}), v || u);
  EXPECT_EQ((bvec_t<3>{0, 0, 0}), v && 0);
  EXPECT_EQ((bvec_t<3>{0, 0, 1}), v || 0);
  EXPECT_EQ((bvec_t<3>{1, 0, 1}), 1 && u);
  EXPECT_EQ((bvec_t<3>{1, 1, 1}), 1 || u);
}

TEST_F(ApiTest, VecEqualityOps)
{
  fvec_t<3> v{0, 1, 2};
  fvec_t<3> u{1, 1, 1};

  EXPECT_EQ((bvec_t<3>{0, 1, 0}), v == u);
  EXPECT_EQ((bvec_t<3>{1, 0, 1}), v != u);
  EXPECT_EQ((bvec_t<3>{0, 1, 1}), v >= u);
  EXPECT_EQ((bvec_t<3>{1, 1, 0}), v <= u);
  EXPECT_EQ((bvec_t<3>{0, 0, 1}), v > u);
  EXPECT_EQ((bvec_t<3>{1, 0, 0}), v < u);

  EXPECT_EQ((bvec_t<3>{0, 0, 1}), v == 2.);
  EXPECT_EQ((bvec_t<3>{1, 1, 0}), v != 2.);
  EXPECT_EQ((bvec_t<3>{0, 0, 1}), v >= 2.);
  EXPECT_EQ((bvec_t<3>{1, 1, 1}), v <= 2.);
  EXPECT_EQ((bvec_t<3>{0, 0, 0}), v > 2.);
  EXPECT_EQ((bvec_t<3>{1, 1, 0}), v < 2.);

  EXPECT_EQ((bvec_t<3>{1, 0, 0}), 0. == v);
  EXPECT_EQ((bvec_t<3>{0, 1, 1}), 0. != v);
  EXPECT_EQ((bvec_t<3>{1, 0, 0}), 0. >= v);
  EXPECT_EQ((bvec_t<3>{1, 1, 1}), 0. <= v);
  EXPECT_EQ((bvec_t<3>{0, 0, 0}), 0. > v);
  EXPECT_EQ((bvec_t<3>{0, 1, 1}), 0. < v);
}

TEST_F(ApiTest, VecArithmeticOps)
{
  ivec_t<4> v{0, 1, 2, 3};
  ivec_t<4> u{2, 2, 2, 1};

  EXPECT_EQ((ivec_t<4>{2, 3, 4, 4}), v + u);
  EXPECT_EQ((ivec_t<4>{-2, -1, 0, 2}), v - u);
  EXPECT_EQ((ivec_t<4>{0, 2, 4, 3}), v * u);
  EXPECT_EQ((ivec_t<4>{0, 0, 1, 3}), v / u);
  EXPECT_EQ((ivec_t<4>{0, 1, 0, 0}), v % u);

  EXPECT_EQ((ivec_t<4>{2, 3, 4, 5}), v + 2);
  EXPECT_EQ((ivec_t<4>{-2, -1, 0, 1}), v - 2);
  EXPECT_EQ((ivec_t<4>{0, 2, 4, 6}), v * 2);
  EXPECT_EQ((ivec_t<4>{0, 0, 1, 1}), v / 2);
  EXPECT_EQ((ivec_t<4>{0, 1, 0, 1}), v % 2);

  EXPECT_EQ((ivec_t<4>{2, 3, 4, 5}), 2 + v);
  EXPECT_EQ((ivec_t<4>{2, 1, 0, -1}), 2 - v);
  EXPECT_EQ((ivec_t<4>{0, 2, 4, 6}), 2 * v);
  EXPECT_EQ((ivec_t<4>{1, 1, 1, 2}), 2 / u);
  EXPECT_EQ((ivec_t<4>{0, 0, 0, 0}), 2 % u);
}

TEST_F(ApiTest, VecArithmeticAssignOps)
{
  ivec_t<4> v{0, 1, 2, 3};
  ivec_t<4> u{2, 2, 2, 1};

  EXPECT_EQ((ivec_t<4>{2, 3, 4, 4}), v += u);
  EXPECT_EQ((ivec_t<4>{4, 6, 8, 4}), v *= u);
  EXPECT_EQ((ivec_t<4>{2, 4, 6, 3}), v -= u);
  EXPECT_EQ((ivec_t<4>{1, 2, 3, 3}), v /= u);
  EXPECT_EQ((ivec_t<4>{2, 4, 6, 6}), v *= 2);
  EXPECT_EQ((ivec_t<4>{4, 6, 8, 8}), v += 2);
  EXPECT_EQ((ivec_t<4>{2, 3, 4, 4}), v /= 2);
  EXPECT_EQ((ivec_t<4>{1, 2, 3, 3}), v -= 1);
  EXPECT_EQ((ivec_t<4>{1, 2, 3, 3}), v %= 4);
  EXPECT_EQ((ivec_t<4>{1, 0, 1, 0}), v %= u);
}

TEST_F(ApiTest, VecBitwiseOps)
{
  ivec_t<4> v{0, 1, 2, 3};
  ivec_t<4> u{2, 2, 2, 1};

  EXPECT_EQ((ivec_t<4>{0, 0, 2, 1}), v & u);
  EXPECT_EQ((ivec_t<4>{2, 3, 2, 3}), v | u);
  EXPECT_EQ((ivec_t<4>{2, 3, 0, 2}), v ^ u);
  EXPECT_EQ((ivec_t<4>{0, 4, 8, 6}), v << u);
  EXPECT_EQ((ivec_t<4>{0, 0, 0, 1}), v >> u);

  EXPECT_EQ((ivec_t<4>{0, 0, 2, 2}), v & 2);
  EXPECT_EQ((ivec_t<4>{2, 3, 2, 3}), v | 2);
  EXPECT_EQ((ivec_t<4>{2, 3, 0, 1}), v ^ 2);
  EXPECT_EQ((ivec_t<4>{0, 4, 8, 12}), v << 2);
  EXPECT_EQ((ivec_t<4>{0, 0, 0, 0}), v >> 2);

  EXPECT_EQ((ivec_t<4>{0, 0, 2, 2}), 2 & v);
  EXPECT_EQ((ivec_t<4>{2, 3, 2, 3}), 2 | v);
  EXPECT_EQ((ivec_t<4>{2, 3, 0, 1}), 2 ^ v);
  EXPECT_EQ((ivec_t<4>{2, 4, 8, 16}), 2 << v);
  EXPECT_EQ((ivec_t<4>{0, 0, 0, 1}), 2 >> u);
}

TEST_F(ApiTest, VecBitwiseAssignOps)
{
  ivec_t<4> v{0, 1, 2, 3};
  ivec_t<4> u{2, 3, 2, 1};

  EXPECT_EQ((ivec_t<4>{0, 1, 2, 1}), v &= u);
  EXPECT_EQ((ivec_t<4>{2, 2, 0, 0}), v ^= u);
  EXPECT_EQ((ivec_t<4>{2, 3, 2, 1}), v |= u);
  EXPECT_EQ((ivec_t<4>{8, 24, 8, 2}), v <<= u);
  EXPECT_EQ((ivec_t<4>{9, 25, 9, 3}), v |= 1);
  EXPECT_EQ((ivec_t<4>{1, 17, 1, 3}), v &= 19);
  EXPECT_EQ((ivec_t<4>{0, 8, 0, 1}), v >>= 1);
  EXPECT_EQ((ivec_t<4>{1, 9, 1, 0}), v ^= 1);
  EXPECT_EQ((ivec_t<4>{2, 18, 2, 0}), v <<= 1);
  EXPECT_EQ((ivec_t<4>{0, 2, 0, 0}), v >>= u);
}

TEST_F(ApiTest, VecUnaryOps)
{
  ivec_t<3> v{0, 1, 2};

  EXPECT_EQ((bvec_t<3>{1, 0, 0}), !v);
  EXPECT_EQ((ivec_t<3>{0, 1, 2}), +v);
  EXPECT_EQ((ivec_t<3>{0, -1, -2}), -v);
  EXPECT_EQ((ivec_t<3>{-1, -2, -3}), ~v);
  EXPECT_EQ((ivec_t<3>{0, 1, 2}), v++);
  EXPECT_EQ((ivec_t<3>{2, 3, 4}), ++v);
  EXPECT_EQ((ivec_t<3>{1, 2, 3}), --v);
  EXPECT_EQ((ivec_t<3>{1, 2, 3}), v--);
}

TEST_F(ApiTest, VecEuclideanOps)
{
  ivec_t<4> v{0, -1, 1, -3};
  ivec_t<4> u{4, 3, -2, 2};
  EXPECT_EQ((ivec_t<4>{0, -1, 0, -2}), euclidean_div(v, u));
  EXPECT_EQ((ivec_t<4>{0, 2, 1, 1}), euclidean_mod(v, u));
  EXPECT_EQ((ivec_t<4>{0, -1, 0, -1}), euclidean_div(v, 4));
  EXPECT_EQ((ivec_t<4>{0, 3, 1, 1}), euclidean_mod(v, 4));
  EXPECT_EQ((ivec_t<4>{1, 1, -2, 2}), euclidean_div(4, u));
  EXPECT_EQ((ivec_t<4>{0, 1, 0, 0}), euclidean_mod(4, u));
}

TEST_F(ApiTest, TypeOfInt)
{
  EXPECT_TRUE(Type::of<int_t>().is_int());
  EXPECT_FALSE(Type::of<int_t>().is_ivec());
}

TEST_F(ApiTest, TypeOfFvec)
{
  EXPECT_TRUE(Type::of<fvec_t<3>>().is_fvec());
  EXPECT_EQ(3, Type::of<fvec_t<3>>().vector_size());
}

TEST_F(ApiTest, TypeOfFunction)
{
  typedef Function<user_type*(int_t)> ft;
  EXPECT_TRUE(Type::of<ft>().is_function());
  EXPECT_EQ(1, Type::of<ft>().function_num_args());
  EXPECT_TRUE(Type::of<ft>().function_return().is_user_type());
  EXPECT_FALSE(Type::of<ft>().function_return().is_managed_user_type());
}

TEST_F(ApiTest, TypeOfUserType)
{
  struct a {};
  struct b {};
  EXPECT_EQ(Type::of<a*>(), Type::of<a*>());
  EXPECT_EQ(Type::of<Ref<a>>(), Type::of<Ref<a>>());
  EXPECT_NE(Type::of<a*>(), Type::of<b*>());
  EXPECT_NE(Type::of<Ref<a>>(), Type::of<Ref<b>>());
  EXPECT_NE(Type::of<a*>(), Type::of<Ref<a>>());
}

TEST_F(ApiTest, Function)
{
  // A weird function type that's never used. Make sure the global trampoline
  // works anyway.
  auto unused = [](int_t, int_t, int_t){return 1;};
  typedef Function<int_t(int_t, int_t, int_t)> uf_t;
  ASSERT_NO_THROW((uf_t(unused)));
  EXPECT_EQ(1, uf_t(unused)(1, 2, 3));
  EXPECT_EQ(1, make_fn(unused)(1, 2, 3));
}

TEST_F(ApiTest, MakeFn)
{
  auto lambda = []{
    return int_t(7);
  };
  auto fn = make_fn(lambda);

  ASSERT_EQ(7, fn());
  ASSERT_EQ(7, make_fn(lambda)());
  ASSERT_EQ(7, make_fn(fn)());
  ASSERT_EQ(7, make_fn(make_fn(lambda))());
}

TEST_F(ApiTest, ContextBadNames)
{
  auto ctxt = context();
  struct type_a {};
  auto acon = make_fn([]{return (type_a*)nullptr;});
  auto voidaf = make_fn([](type_a*){});

  EXPECT_THROW(ctxt.register_function("!", voidaf), runtime_error);
  EXPECT_THROW(ctxt.register_member_function("~", voidaf), runtime_error);
  EXPECT_THROW(ctxt.register_type<type_a>("$c"), runtime_error);
  EXPECT_THROW(ctxt.register_type("a-b", acon, voidaf), runtime_error);
  EXPECT_THROW(ctxt.register_constructor("a::b", acon, voidaf), runtime_error);
  EXPECT_THROW(ctxt.register_namespace(" ", context()), runtime_error);
}

TEST_F(ApiTest, ContextTypedefs)
{
  auto ctxt = context();
  struct type_a {};
  struct type_b {};

  ASSERT_NO_THROW(ctxt.register_type<type_a>("TypeA"));
  EXPECT_THROW(ctxt.register_type<type_a>("TypeA"), runtime_error);
  EXPECT_THROW(ctxt.register_type<type_b>("TypeA"), runtime_error);
  EXPECT_NO_THROW(ctxt.register_type<type_b>("TypeB"));
  EXPECT_NO_THROW(ctxt.register_type<type_a>("TypeA2"));
}

TEST_F(ApiTest, ContextFunctions)
{
  auto ctxt = context();
  auto voidf = make_fn([]{});

  ASSERT_NO_THROW(ctxt.register_function("foo", voidf));
  EXPECT_THROW(ctxt.register_function("foo", voidf), runtime_error);
  EXPECT_NO_THROW(ctxt.register_function("bar", voidf));
}

TEST_F(ApiTest, ContextMemberFunctions)
{
  auto ctxt = context();
  struct type_a {};
  auto voidaf = make_fn([](type_a*){});

  ASSERT_NO_THROW(ctxt.register_member_function("foo", voidaf));
  EXPECT_THROW(ctxt.register_member_function("foo", voidaf), runtime_error);
  EXPECT_NO_THROW(ctxt.register_member_function("bar", voidaf));
}

TEST_F(ApiTest, ContextOverlappingUserTypes)
{
  auto ctxt = context();
  struct type_a {};
  struct type_b {};
  auto refaf = make_fn([](Ref<type_a>){});
  auto refbf = make_fn([](Ref<type_b>){});
  auto bcon = make_fn([]{return (type_b*)nullptr;});
  auto voidbf = make_fn([](type_b*){});

  ctxt.register_type<type_a>("TypeA");
  ctxt.register_type("TypeB", bcon, voidbf);
  EXPECT_NO_THROW(ctxt.register_member_function("foo", voidbf));
  EXPECT_NO_THROW(ctxt.register_member_function("foo", refaf));
  EXPECT_NO_THROW(ctxt.register_member_function("foo", refbf));
}

TEST_F(ApiTest, ContextNamespaceConflicts)
{
  auto ctxt = context();
  auto dtxt = context(false);
  struct type_a {};
  auto acon = make_fn([]{return (type_a*)nullptr;});
  auto voidaf = make_fn([](type_a*){});
  ctxt.register_type("TypeA", acon, voidaf);
  ctxt.register_function("bar", voidaf);
  
  EXPECT_THROW(ctxt.register_function("TypeA", voidaf), runtime_error);
  EXPECT_THROW(ctxt.register_type("bar", acon, voidaf), runtime_error);
  EXPECT_THROW(ctxt.register_constructor("bar", acon, voidaf), runtime_error);
  EXPECT_THROW(ctxt.register_constructor("TypeA", acon, voidaf), runtime_error);

  EXPECT_THROW(dtxt.register_namespace("dtxt", dtxt), runtime_error);
  EXPECT_THROW(ctxt.register_namespace("TypeA", dtxt), runtime_error);

  ctxt.register_namespace("dtxt", dtxt);
  EXPECT_THROW(ctxt.register_namespace("dtxt", dtxt), runtime_error);
  EXPECT_THROW(ctxt.register_type<type_a>("dtxt"), runtime_error);
  EXPECT_THROW(ctxt.register_type("dtxt", acon, voidaf), runtime_error);
}

TEST_F(ApiTest, ContextNamespaceIndependence)
{
  auto ctxt = context();
  auto dtxt = context(false);
  struct type_a {};
  auto acon = make_fn([]{return (type_a*)nullptr;});
  auto voidaf = make_fn([](type_a*){});
  ctxt.register_type("TypeA", acon, voidaf);
  ctxt.register_function("foo", voidaf);
  ctxt.register_function("bar", voidaf);
  ctxt.register_namespace("dtxt", dtxt);

  EXPECT_NO_THROW(ctxt.register_namespace("foo", dtxt));
  EXPECT_NO_THROW(ctxt.register_function("dtxt", voidaf));
  EXPECT_NO_THROW(ctxt.register_type<type_a>("bar"));

  auto etxt = context(false);
  etxt.register_type<type_a>("TypeA");
  ctxt.register_function("make_b", make_fn([]{
    return (type_a*)nullptr;
  }));
  ctxt.register_type<type_a>("A");
  EXPECT_NO_THROW(ctxt.register_namespace("whatever", etxt));
  etxt.register_member_function("foo", voidaf);
  EXPECT_NO_THROW(ctxt.register_namespace("again", etxt));

  auto ftxt = context(false);
  ftxt.register_member_function("foo", make_fn([](type_a*){}));
  EXPECT_THROW(ctxt.register_namespace("another", ftxt), runtime_error);
}

TEST_F(ApiTest, ProgramSuccess)
{
  auto prog = program_suppress_errors("works = void() {}");
  ASSERT_TRUE(prog.success());
  EXPECT_EQ("test0", prog.get_name());
  EXPECT_NO_THROW(instance(prog));
  EXPECT_NO_THROW(prog.print_ast());
  EXPECT_NO_THROW(prog.print_ir());
}

TEST_F(ApiTest, ProgramFailure)
{
  auto prog = program_suppress_errors("broken");
  ASSERT_FALSE(prog.success());
  EXPECT_EQ("test0", prog.get_name());
  EXPECT_THROW(instance(prog), runtime_error);
  EXPECT_THROW(prog.print_ast(), runtime_error);
  EXPECT_THROW(prog.print_ir(), runtime_error);
}

TEST_F(ApiTest, ProgramFunctions)
{
  auto prog = test_program();
  ASSERT_EQ(1, prog.get_functions().size());

  auto it = prog.get_functions().begin();
  EXPECT_EQ("f", it->first);
  auto type = it->second;

  ASSERT_TRUE(type.is_function());
  ASSERT_EQ(1, type.function_num_args());
  EXPECT_TRUE(type.function_arg(0).is_int());
  EXPECT_TRUE(type.function_return().is_int());
}

TEST_F(ApiTest, ProgramGlobals)
{
  auto prog = test_program();
  ASSERT_EQ(2, prog.get_globals().size());
  auto at = prog.get_globals().find("a");
  auto bt = prog.get_globals().find("b");

  EXPECT_EQ(prog.get_globals().find("c"), prog.get_globals().end());
  EXPECT_EQ(prog.get_globals().find("d"), prog.get_globals().end());
  ASSERT_NE(at, prog.get_globals().end());
  ASSERT_NE(bt, prog.get_globals().end());

  EXPECT_TRUE(at->second.type.is_vector());
  EXPECT_TRUE(at->second.type.is_ivec());
  EXPECT_FALSE(at->second.type.is_int());
  EXPECT_EQ(3, at->second.type.vector_size());
  EXPECT_FALSE(at->second.is_const);

  EXPECT_TRUE(bt->second.type.is_vector());
  EXPECT_TRUE(bt->second.type.is_fvec());
  EXPECT_FALSE(bt->second.type.is_float());
  EXPECT_EQ(3, bt->second.type.vector_size());
  EXPECT_TRUE(bt->second.is_const);
}

TEST_F(ApiTest, InstanceFunctionMissing)
{
  typedef Function<int_t(int_t)> intf_t;
  auto inst = test_instance();
  EXPECT_THROW(inst.get_function<intf_t>("nonexistent"), runtime_error);
  EXPECT_THROW(inst.call<int_t>("nonexistent"), runtime_error);
}

TEST_F(ApiTest, InstanceFunctionNotFunction)
{
  typedef Function<int_t(int_t)> intf_t;
  auto inst = test_instance();
  EXPECT_THROW(inst.get_function<intf_t>("a"), runtime_error);
  EXPECT_THROW(inst.call<int_t>("a", 0), runtime_error);
}

TEST_F(ApiTest, InstanceFunctionNotExported)
{
  typedef Function<int_t(int_t)> intf_t;
  auto inst = test_instance();
  EXPECT_THROW(inst.get_function<intf_t>("g"), runtime_error);
  EXPECT_THROW(inst.call<int_t>("g", 0), runtime_error);
}

TEST_F(ApiTest, InstanceFunctionIncompatibleType)
{
  typedef Function<void()> voidf_t;
  auto inst = test_instance();
  EXPECT_THROW(inst.get_function<voidf_t>("f"), runtime_error);
  EXPECT_THROW(inst.call<float_t>("f", 0), runtime_error);
  EXPECT_THROW(inst.call<int_t>("f", 0.), runtime_error);
}

TEST_F(ApiTest, InstanceFunction)
{
  typedef Function<int_t(int_t)> intf_t;
  auto inst = test_instance();
  EXPECT_NO_THROW(inst.get_function<intf_t>("f"));
  EXPECT_EQ(2, inst.call<int_t>("f", 1));
}

TEST_F(ApiTest, InstanceGlobalMissing)
{
  auto inst = test_instance();
  EXPECT_THROW(inst.get_global<int_t>("nonexistent"), runtime_error);
  EXPECT_THROW(inst.set_global("nonexistent", 0), runtime_error);
}

TEST_F(ApiTest, InstanceGlobalNotGlobal)
{
  typedef Function<int_t(int_t)> intf_t;
  auto inst = test_instance();
  EXPECT_THROW(inst.get_global<intf_t>("f"), runtime_error);
  EXPECT_THROW(
      inst.set_global("f", inst.get_function<intf_t>("f")), runtime_error);
}

TEST_F(ApiTest, InstanceGlobalIncompatibleType)
{
  auto inst = test_instance();
  EXPECT_THROW(inst.get_global<int_t>("a"), runtime_error);
  EXPECT_THROW(inst.set_global("a", 0), runtime_error);
}

TEST_F(ApiTest, InstanceGlobalExportVar)
{
  auto inst = test_instance();
  EXPECT_EQ(ivec_t<3>(0, 1, 2), inst.get_global<ivec_t<3>>("a"));
  EXPECT_NO_THROW(inst.set_global("a", ivec_t<3>{0, -1, -2}));
  EXPECT_EQ(ivec_t<3>(0, -1, -2), inst.get_global<ivec_t<3>>("a"));
}

TEST_F(ApiTest, InstanceGlobalExportConst)
{
  auto inst = test_instance();
  EXPECT_EQ(fvec_t<3>(0., 1., 2.), inst.get_global<fvec_t<3>>("b"));
  EXPECT_THROW(inst.set_global("b", fvec_t<3>(0., 1., 2.)), runtime_error);
}

TEST_F(ApiTest, InstanceGlobalInternalVar)
{
  auto inst = test_instance();
  EXPECT_THROW(inst.get_global<int_t>("c"), runtime_error);
}

TEST_F(ApiTest, InstanceGlobalInternalConst)
{
  auto inst = test_instance();
  EXPECT_THROW(inst.get_global<int_t>("d"), runtime_error);
}

TEST_F(ApiTest, ErrorInfo)
{
  auto prog = program_suppress_errors(R"(
export x = int()
{
  return
    1.0 +
    1 + /******/ 2;
  /*******/
  x();
}
)");

  const auto& errors = prog.get_errors();
  const auto& warnings = prog.get_warnings();

  ASSERT_EQ(1, errors.size());
  ASSERT_EQ(1, warnings.size());
  const auto& error = errors[0];
  const auto& warning = warnings[0];

  EXPECT_EQ(4, error.node.start_line);
  EXPECT_EQ(4, error.node.end_line);
  EXPECT_EQ(8, error.node.start_column);
  EXPECT_EQ(8, error.node.end_column);
  EXPECT_EQ(37, error.node.start_index);
  EXPECT_EQ(37, error.node.end_index);
  EXPECT_EQ("+", error.node.text);

  EXPECT_EQ(4, error.tree.start_line);
  EXPECT_EQ(5, error.tree.end_line);
  EXPECT_EQ(4, error.tree.start_column);
  EXPECT_EQ(4, error.tree.end_column);
  EXPECT_EQ(33, error.tree.start_index);
  EXPECT_EQ(43, error.tree.end_index);
  EXPECT_EQ("1.0 +\n    1", error.tree.text);

  EXPECT_EQ(7, warning.node.start_line);
  EXPECT_EQ(7, warning.node.end_line);
  EXPECT_EQ(2, warning.node.start_column);
  EXPECT_EQ(5, warning.node.end_column);
  EXPECT_EQ(73, warning.node.start_index);
  EXPECT_EQ(76, warning.node.end_index);
  EXPECT_EQ("x();", warning.node.text);
  EXPECT_EQ("x();", warning.tree.text);
}

// End namespace yang.
}
