//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {
struct SemanticsTest : YangTest {};

const std::string TestSemanticsStr = R"(
export daft_fib = int(int n)
{
  if (n <= 1) {
    return 1;
  }
  else {
    return daft_fib(n - 1) + daft_fib(n - 2);
  }
}

export global {
  const global_inner = int()
  {
    return 42;
  }();
}

export count_to_ten = int()
{
  var a = 0;
  while (a < 10) ++a;
  return a;
}

global {
  var global_i = 0;
}
global
if (true) {
  const one = 1;
  global_i += one;
  global_inner;
}

export ternary_fun = int()
{
  const a = 1 ? (1, 2) : (0, 1);
  const b = (1, 0) ? 2 * (1, 1) * 2 : (2, 3);
  ++global_i;
  return $+a + $*b + global_i;
}

export crazy_combine = int()
{
  var x = count_to_ten;
  var r = 0;
  for (var a = 0; a < 5; ++a) {
    r += x();
    x = a % 2 ? ternary_fun : count_to_ten;
  }
  return r;
}

export shadowing = int(int a)
{
  var shadowing = 0;
  {
    const a = 0;
    while ((const a = 4) && !shadowing) {
      shadowing += a;
    }
    if (a == 0) {
      const a = 1;
      shadowing += a;
    }
  }
  shadowing += a;
  const a = 8;
  shadowing += a;
  return shadowing;
}

export global const ten = count_to_ten();
export global {
  var again = 0;
  for (var i = 0; i < 10; ++i) {
    ++again;
  }
}

export again_mod = void(int a)
{
  ten; // Avoid warnings.
  if (a) {
    ++again;
    return;
  }
  again += 2;
}

export float_literals = float()
{
  return .1 + 1. + 1.1 + .1e0 + 1.E1 + 1.1e-1 +
      1e0 + .0E0 + 1.e-0 + 1.1e+1 + 0e+0;
}

export int_literals = int()
{
  return 128 + 0xff + 0x0 + 0x001 + 0x100 + 0xE;
}

export big_int_literals = int()
{
  return 0xffffffff;
}

export knuth_man_or_boy_test = int(int n)
{
  const a = int(int k, int() x1, int() x2, int() x3, int() x4, int() x5)
  {
    closed var kk = k;
    const b = int()
    {
      --kk;
      return a(kk, b, x1, x2, x3, x4);
    };
    return kk <= 0 ? x4() + x5() : b();
  };
  const k = int()(int n)
  {
    return int() {return n;};
  };
  return a(n, k(1), k(-1), k(-1), k(1), k(0));
}

export odd_ops = int()
{
  return $+((5, 6, 7) % 4) + $+(8 / (1, 2, 4)) +
         $+((1, 2, 3) ** 2) + $+(2 ** (1, 2, 3));
}

export ordering = int()
{
  const ignore = void(int) {};
  var r = const a = (const b = 1) + b;
  for (var b = a; const a = b < 4; ignore((const b = ++b) + b)) {
    r += a + b;
  }

  var c = (1, 2);
  c *= 2;
  return r + $+c;
}

export edit = int()()
{
  closed var ff = int() {return 0;};
  closed var step = 0;
  const f = int()
  {
    const val = ff();
    ff = step == 0 ? int() {return 1;} :
         step == ++0 ? int() {return step;} :
         step == ++++0 ? edit() : ff;
    ++step;
    return val;
  };
  return f;
}

global {
  var i = 0;
}
~global {
  var a = 0; ++a; a;
  --i;
  destruction(i);
}
~global {
  ++i;
  destruction(i);
}
)";

TEST_F(SemanticsTest, Various)
{
  // TODO: just getting started. Need way more semantic tests.
  int_t temp_int = 0;
  {
    auto ctxt = context();
    ctxt.register_function("destruction", make_fn([&](int_t t)
    {
      t ? temp_int += 5 : temp_int *= 5;
    }));

    auto inst = instance(ctxt, TestSemanticsStr);
    EXPECT_EQ(89, inst.call<int_t>("daft_fib", 10));

    EXPECT_EQ(42, inst.get_global<int_t>("global_inner"));
    EXPECT_EQ(10, inst.call<int_t>("count_to_ten"));
    EXPECT_EQ(17, inst.call<int_t>("ternary_fun"));
    EXPECT_EQ(67, inst.call<int_t>("crazy_combine"));
    EXPECT_EQ(15, inst.call<int_t>("shadowing", 2));

    EXPECT_EQ(10, inst.get_global<int_t>("ten"));
    EXPECT_EQ(10, inst.get_global<int_t>("again"));
    inst.call<void>("again_mod", 0);
    EXPECT_EQ(12, inst.get_global<int_t>("again"));
    inst.call<void>("again_mod", 1);
    EXPECT_EQ(13, inst.get_global<int_t>("again"));

    EXPECT_EQ(25.41, inst.call<float_t>("float_literals"));
    EXPECT_EQ(654, inst.call<int_t>("int_literals"));
    EXPECT_EQ(0xffffffff, inst.call<int_t>("big_int_literals"));
    EXPECT_EQ(-1, inst.call<int_t>("big_int_literals"));

    EXPECT_EQ(-67, inst.call<int_t>("knuth_man_or_boy_test", 10));
    EXPECT_EQ(48, inst.call<int_t>("odd_ops"));
    EXPECT_EQ(15, inst.call<int_t>("ordering"));

    typedef Function<int_t()> intf_t;
    {
      auto edit = inst.call<intf_t>("edit");
      EXPECT_EQ(0, edit());
      EXPECT_EQ(1, edit());
      EXPECT_EQ(2, edit());
      EXPECT_EQ(0, edit());
      EXPECT_EQ(1, edit());
      EXPECT_EQ(2, edit());
    }

    // Check destructors work. Current guarantee is that destructor will be
    // called before any new structure (instance, closure, etc) is allocated.
    // Is that good enough?
    EXPECT_EQ(0, temp_int);
  }
  // Force a collection so that destructor is called.
  instance("");
  EXPECT_EQ(25, temp_int);
}

const std::string TestTcoStr = R"(
export rec_fac = int(int n)
{
  return n ? n * rec_fac(n - 1) : 1;
}

export tco_fac = int(int n)
{
  const helper = int(int n, int accum)
  {
    return n == 0 ? accum : helper(n - 1, n * accum);
  };
  return helper(n, 1);
}
)";

TEST_F(SemanticsTest, DISABLED_Tco)
{
  auto inst = instance(TestTcoStr);
  // This is a little tricky: some versions of GNU make contain a bug which sets
  // the stack size to unlimited, and forgets to ever set it back. This means
  // the TCO test will pass when run from the makefile even if TCO is broken.
  //
  // TCO is, anyway, tricky to get right. Trivial tail call examples (with
  // refcounting disabled!) will sometimes be eliminated entirely by LLVM and
  // turned into a loop with accumulator variable. But it's easily upset by
  // minor changes.
  //
  // Getting more general TCO working for true tail calls hasn't yet been
  // successful. Two things are needed at least:
  // - reordering of refcount releases before the tail call;
  // - copying returns into predecessors to avoid returning PHI nodes (seems
  //   like LLVM should help with this, but no luck so far).
  //
  // For now, TCO is broken.
  EXPECT_EQ(720, inst.call<int_t>("rec_fac", 6));
  EXPECT_EQ(720, inst.call<int_t>("tco_fac", 6));
  EXPECT_NO_THROW(inst.call<int_t>("tco_fac", 1000000));
}

const std::string TestContextNamespacesStr = R"(
export fn = int()
{
  const m = outer::MType();
  const t = make_type();

  return outer::inner::f() +
      outer::g() +
      other::
             h() +
      t.mem() + other::Type::mem(t) +
      m.mem() + outer::MType::mem(m) +
      Type::mem(t);
}
)";

TEST_F(SemanticsTest, ContextNamespaces)
{
  struct type {};
  struct inner_type {};
  auto make_inner = make_fn([]
  {
    return new inner_type;
  });
  auto delete_inner = make_fn([](inner_type* t)
  {
    delete t;
  });

  auto inner_inner_ctxt = context(false);
  inner_inner_ctxt.register_function("f", make_fn([]
  {
    return int_t(17);
  }));

  auto inner_ctxt = context(false);
  inner_ctxt.register_namespace("inner", inner_inner_ctxt);
  inner_ctxt.register_function("g", make_fn([]
  {
    return int_t(12);
  }));
  inner_ctxt.register_type("MType", make_inner, delete_inner);
  inner_ctxt.register_member_function("mem", make_fn([](Ref<inner_type>)
  {
    return int_t(1);
  }));

  auto other_ctxt = context(false);
  other_ctxt.register_function("h", make_fn([]
  {
    return int_t(10);
  }));
  other_ctxt.register_type<type>("Type");

  auto ctxt = context();
  ctxt.register_member_function("mem", make_fn([](type*)
  {
    return int_t(2);
  }));
  ctxt.register_namespace("outer", inner_ctxt);
  ctxt.register_namespace("other", other_ctxt);
  type closed_type;
  ctxt.register_function("make_type", make_fn([&]
  {
    return &closed_type;
  }));
  ctxt.register_type<type>("Type");

  auto inst = instance(ctxt, TestContextNamespacesStr);
  EXPECT_EQ(47, inst.call<int_t>("fn"));
}

const std::string TestInstanceNamespacesStrA = R"(
g = UserType()
{
  return get_user_type();
}
export f = UserType()
{
  return g();
}
)";

const std::string TestInstanceNamespacesStrB = R"(
export global {
  var u = inst::f();
}
export g = void()
{
  u = inst::f();
}
export get = Ut()
{
  return inst::f();
}
)";

TEST_F(SemanticsTest, InstanceNamespaces)
{
  auto inst = instance(context(), TestInstanceNamespacesStrA);
  auto ctxt = context(false);
  ctxt.register_namespace("inst", inst);
  ctxt.register_type<user_type>("Ut");
  auto jnst = instance(ctxt, TestInstanceNamespacesStrB);
  EXPECT_EQ(0, jnst.get_global<user_type*>("u")->id);
  jnst.call<void>("g");
  EXPECT_EQ(1, jnst.get_global<user_type*>("u")->id);
  EXPECT_EQ(2, jnst.call<user_type*>("get")->id);

  auto prog = program_suppress_errors(ctxt, "x = void() {inst::g();}");
  EXPECT_FALSE(prog.success());
  prog = program_suppress_errors(ctxt, "x = void(inst::UserType) {}");
  EXPECT_FALSE(prog.success());
  // Register an instance back into the same context it's compiled against.
  ctxt.register_namespace("jnst", jnst);
}

// End namespace yang.
}
