//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
const std::string TestSemanticsStr = R"(
export rec_fac = int(int n)
{
  return n ? n * rec_fac(n - 1) : 1;
}

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
)";

TEST_F(YangTest, SemanticsTest)
{
  auto& inst = instance(TestSemanticsStr);
  // Test recursion.
  EXPECT_EQ(inst.call<int_t>("rec_fac", 6), 720);
  EXPECT_EQ(inst.call<int_t>("daft_fib", 10), 89);
  // Check tail call optimisation is working.
  EXPECT_NO_THROW(inst.call<int_t>("rec_fac", 1000000));

  EXPECT_EQ(inst.get_global<int_t>("global_inner"), 42);
  EXPECT_EQ(inst.call<int_t>("count_to_ten"), 10);
  EXPECT_EQ(inst.call<int_t>("ternary_fun"), 17);
  EXPECT_EQ(inst.call<int_t>("crazy_combine"), 67);
  EXPECT_EQ(inst.call<int_t>("shadowing", 2), 15);

  EXPECT_EQ(inst.get_global<int_t>("ten"), 10);
  EXPECT_EQ(inst.get_global<int_t>("again"), 10);
  inst.call<void>("again_mod", 0);
  EXPECT_EQ(inst.get_global<int_t>("again"), 12);
  inst.call<void>("again_mod", 1);
  EXPECT_EQ(inst.get_global<int_t>("again"), 13);

  EXPECT_EQ(inst.call<float_t>("float_literals"), 25.41);
  EXPECT_EQ(inst.call<int_t>("int_literals"), 654);
  EXPECT_EQ(inst.call<int_t>("big_int_literals"), 0xffffffff);
  EXPECT_EQ(inst.call<int_t>("big_int_literals"), -1);
  // TODO: just getting started. Need way more semantic tests.
}
