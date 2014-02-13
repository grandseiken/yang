//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
const std::string TestSemanticsStr = R"(
export rec_fac = int(int n)
{
  return n ? n * rec_fac(n - 1) : 1;
};

export daft_fib = int(int n)
{
  if (n <= 1) {
    return 1;
  }
  else {
    return daft_fib(n - 1) + daft_fib(n - 2);
  }
};

export global {
  var global_inner = int()
  {
    return 42;
  }();
};

export count_to_ten = int()
{
  var a = 0;
  while (a < 10) ++a;
  return a;
};

global {
  var global_i = 0;
};
global
if (true) {
  const one = 1;
  global_i += one;
}

export ternary_fun = int()
{
  const a = 1 ? (1, 2) : (0, 1);
  const b = (1, 0) ? 2 * (1, 1) * 2 : (2, 3);
  ++global_i;
  return $+a + $*b + global_i;
};

export crazy_combine = int()
{
  var x = count_to_ten;
  var r = 0;
  for (var a = 0; a < 5; ++a) {
    r += x();
    x = a % 2 ? ternary_fun : count_to_ten;
  }
  return r;
};

export shadowing = int(int a)
{
  var shadowing = 0;
  {
    var a = 0;
    while ((var a = 4) && !shadowing) {
      shadowing += a;
    }
    if (a == 0) {
      var a = 1;
      shadowing += a;
    }
  }
  shadowing += a;
  var a = 8;
  shadowing += a;
  return shadowing;
};

export global const ten = count_to_ten();
export global {
  var ten_again = 0;
  for (var i = 0; i < 10; ++i) {
    ++ten_again;
  }
};
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
  EXPECT_EQ(inst.get_global<int_t>("ten_again"), 10);
  // TODO: just getting started. Need way more semantic tests.
}
