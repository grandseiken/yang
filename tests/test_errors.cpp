//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {

const std::string TestStructuredErrorStr = R"(
export x = int()
{
  return
    1.0 +
    1 + /******/ 2;
  /*******/
  x();
}
)";

TEST_F(YangTest, FailureTest)
{
  if (!filter("errors")) {
    return;
  }

  auto success_prog = program_suppress_errors("works = void() {}");
  EXPECT_TRUE(success_prog.success());
  EXPECT_NO_THROW(instance(success_prog));
  auto failure_prog = program_suppress_errors("broken");
  EXPECT_FALSE(failure_prog.success());
  EXPECT_THROW(instance(failure_prog), runtime_error);

  auto prog = program_suppress_errors(TestStructuredErrorStr);
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

const std::string TestMultilineErrorStrA = R"(
export x = int
/**/()
{
  if (1) {
    return 1;
  }
}
)";

const std::string TestMultilineErrorStrB = R"(
x = void()
{
  return 0 +
  0.; // Comment continues for a while.
}
)";

TEST_F(YangTest, ErrorTest)
{
  if (!filter("errors")) {
    return;
  }

  auto ctxt = context();
  struct other {};
  ctxt.register_type<other>("OtherType");
  ctxt.register_function("get_other", make_fn([]{return (other*)nullptr;}));
  ctxt.register_type("MotherType", make_fn([]{return (other*)nullptr;}),
                                   make_fn([](other*){}));

  auto erc = [&](const std::string& str,
                 const std::string& text, std::size_t count)
  {
#ifdef DEBUG
    auto prog = program(ctxt, str, true);
#else
    auto prog = program_suppress_errors(ctxt, str);
#endif
    EXPECT_FALSE(prog.success()) <<
        "Should fail to compile:\n" << str << std::endl;
    const auto& errors = prog.get_errors();
    EXPECT_EQ(count, errors.size()) <<
        "Should have " << count << " error(s):\n" << str << std::endl;
    if (errors.size() >= 1) {
      EXPECT_EQ(text, errors[0].node.text) <<
          "Error should reference given text:\n" << str << std::endl;
    }
  };
  auto err = [&](const std::string& str, const std::string& text)
  {
    erc(str, text, 1);
  };

  // Miscellaneous syntax errors.
  err("/* comment", "t");
  err("broken = {}\n", "{");
  err("EXPORT broken = void() {}", "broken");
  err("global x = 0;", "x");
  err("export = void() {}", "=");
  err("export export = void() {}", "export");
  err("export for = void() {}", "for");
  err("export int4 = void() {}", "int4");
  err("global const x = int;", "int");
  err("global const x = 0", "0");
  err("void() x = void() {}", "x");
  err("+", "+");
  err("#", "#");
  err("x;", ";");
  err("x = 0;", "=");
  err("x = void();", "void");
  err("x = void() {0}", "}");
  err("x = void() {return}", "}");
  err("x = void(return 0;) {}", "return");
  err("x = {};", "{");
  err("x = void() {;", ";");
  err("x = void() };", "}");
  err("x = () {};", ")");
  err("x = void() {if ();}", ")");
  err("x = void() {0, 0;}", ",");

  // Literal syntax errors.
  err("x = void() {0.0.0;}", ".0");
  err("x = void() {0ee0;}", "ee0");
  err("x = void() {0e++0;}", "e");
  err("x = void() {0e--0;}", "e");
  err("x = void() {e0;}", "e0");
  err("x = void() {0..0;}", ".0");
  err("x = void() {0e;}", "e");
  err("x = void() {1e1.0;}", ".0");
  err("x = void() {0x;}", "x");
  err("x = void() {x0;}", "x0");
  err("x = void() {1x0;}", "x0");
  err("x = void() {0xg;}", "xg");
  err("x = void() {0xH;}", "xH");
  err("x = void() {0y0;}", "y0");
  err("x = void() {0x100000000;}", "0x100000000");
  err("x = void() {\"a\na\";}", "\"a\na\"");
  err("x = void() {\"\\'\";}", "\"\\'\"");
  err("x = void() {\"\\1\";}", "\"\\1\"");
  err("x = void() {\";}", "\"");
  err("x = void() {\"\\\";}", "\"");
  err("x = void() {\"\"\";}", "\"");

  // Type / expression mismatches.
  err("x = 0() {}", "0");
  err("x = void(0 a) {}", "0");
  err("x = (void() + void()) {}", "{");
  err("x = void() {int;}", "int");
  err("x = void() {return int();}", "int");

  // Function signature errors.
  err("int = void() {}", "int");
  err("global {return 0;}", "return 0;");
  err("~global {return;}", "return;");
  err("export ~global {}", "export ~global {}");
  err("x = void {}", "{");
  err("x = void(int a, int a) {}", "int a");
  err("x = void(void a) {}", "void a");
  err("x = void() {return 0;}", "0");
  err("x = int() {}", "int()");
  err("x = int() {return 1.;}", ".");
  err("x = int() {return;}", "return;");
  err("x = int() {const x = 1 + 1.; return x;}", "+");

  // Statement errors.
  err("x = void() {if (0.);}", ".");
  err("x = void() {if (return 0;);}", "return");
  err("x = void() {for (0; (0, 0); 0);}", "(");
  err("x = void() {while (x);}", "x");
  err("x = void() {do; while((0, 0));}", "(");
  err("x = void() {break;}", "break;");
  err("x = void() {continue;}", "continue;");
  err("x = void() {int::foo();}", "::");

  // User-type errors.
  err("x = void(err f) {}", "err");
  err("x = void() {UserType::foo();}", "(");
  err("x = void() {0.foo();}", ".");
  err("x = void() {get_user_type().bar();}", ".");
  err("x = void() {get_user_type().foo(1);}", "(");
  err("x = void() {UserType();}", "UserType");
  err("x = void() {MuserType(0);}", "(");
  err("x = OtherType() {return get_user_type();}", "(");
  err("x = MotherType() {return MuserType();}", "(");
  err("x = MuserType() {return get_user_type();}", "(");
  err("x = OtherType() {return MotherType();}", "(");
  err("x = void() {var a = get_other(); a = get_user_type();}", "=");
  err("x = void() {var a = MuserType(); a = MotherType();}", "=");
  err("x = void() {var a = MuserType(); a = get_user_type();}", "=");
  err("x = void() {var a = get_other(); a = MotherType();}", "=");

  // Ternary errors.
  err("x = void() {1. ? 0 : 0;}", "?");
  err("x = void() {1 ? 0 : 0.;}", "?");
  err("x = void() {(1, 1, 1) ? (1, 1) : (1, 1);}", "?");
  err("x = void() {(1, 1) ? 0 : 1;}", "?");
  err("x = void() {(1, 1) ? (1, 1, 1) : (1, 1);}", "?");
  err("x = void() {(1 ? 1 : 2) + 1.0;}", "+");
  err("x = void() {1 ? (1, 1) : 1;}", "?");
  err("x = void() {(1, 1) ? (1, 1) : 1;}", "?");

  // Invocation errors.
  err("x = void(int a) {x(a b);}", "a b");
  err("x = void(int a) {a();}", "(");
  err("x = void(int a) {x();}", "(");
  err("x = void(int a) {x(1, 2);}", "(");
  err("x = void(int a) {x(1.);}", ".");

  // Operator errors.
  err("x = void() {1. | 1.;}", "|");
  err("x = void() {(1, 1) & (1, 1, 1);}", "&");
  err("x = void() {(1 && 1) + 1.;}", "+");
  err("x = void() {1 << 1.;}", "<<");
  err("x = void() {1. >> 1.;}", ">>");
  err("x = void() {1. + (1). + 1;}", "+");
  err("x = void() {(1. == 1.) + 1.;}", "+");
  err("x = void() {$==(1., 1.) * 1.;}", "*");
  err("x = void() {$&&(1., 1.);}", "$");
  err("x = void() {!1.;}", "!");
  err("x = void() {-1 - 1.;}", "-");
  err("x = void() {(1, 1, 1, 1.);}", "(");
  err("x = void() {();}", ")");
  err("x = void() {0 0;}", "0");
  err("x = void() {((1, 1), (1, 1));}", "(");
  err("x = void() {(x, x);}", "x");
  erc("x = void() {(x, (1, 1));}", "x", 2);
  err("x = void() {(1 a, 2);}", "1 a");
  err("x = void() {(1, 2)[1.];}", "[");
  err("x = void() {(1)[1];}", "[");
  err("x = void() {[0];}", "[");
  err("x = void() {1..;}", ".");
  err("x = void() {++x;}", "++");
  err("x = void() {var x = 0.; x += 1;}", "+=");
  err("x = void() {y += 1;}", "y");
  err("x = void() {var x = 1; x *= (1, 1);}", "*=");
  err("x = void() {var x = 1; x *= 1.;}", "*=");
  err("x = void() {\"foo\" + 2;}", "+");
  err("x = void() {var x = \"foo\"; ++foo;}", "foo");

  // Variable declaration and assignment errors.
  err("x = void() {var a;}", ";");
  erc("x = void() {var a + 1 = 0;}", "a", 2);
  err("x = void() {var a = 0; a + 1 = 1;}", "=");
  err("x = void() {a = 0;}", "a");
  err("x = void() {x = x;}", "=");
  err("x = void() {var a = 0; a = 0.;}", "=");
  err("x = void() {var a = 0; var a = 0;}", "=");
  err("x = void() {const a = 0; const a = 0.;}", "=");
  err("x = void() {const a = 0; a = 0;}", "=");
  err("x = void() {UserType = get_user_type();}", "UserType");
  err("x = UserType() {get_user_type = x; return get_user_type();}", "=");
  err("global {var a = 0;} global {var a = 0;}", "=");
  err("global {const a = 0;} global {a = 0;}", "=");
  err("x = void() {} x = void() {}", "=");
  err("x = void() {} global const x = x;", "=");
  err("x = void(int y) {y = 0;}", "=");
  err("x = void() {var a = 0; a += 1.;}", "+=");
  err("x = void() {var a = 0; a += (1, 1);}", "+=");
  err("x = void() {var a = 0; a += void() {};}", "+=");
  err("x = void() {const a = 0; ++a;}", "++");
  err("x = void() {var a = 0; ++a = 1;}", "=");
  err("x = void() {const a = 0; void() {a;};}", "a");
  err("x = void() {var a = 0; void() {a += 1;};}", "a");
  err("x = void() {closed const a = 0; void() {--a;};}", "--");
  err("x = void() {a + var a = 1;}", "a");
  err("x = void() {var a = a;}", "a");

  // Name resolution / scope errors.
  err("x = void() {y;}", "y");
  err("x = void() {y();} y = void() {}", "y");
  err("x = void() {{const a = 0;} void() {a;};}", "a");
  err("global {const a = 0;}; global {a = 0;}", "=");
  err("x = void() {const a = 0;} y = void() {a = 0;}", "a");
  err("x = void() {if (var a = 1); a;}", "a");
  err("x = void() {if (1) {var a = 0;} a;}", "a");
  err("x = void() {for (var a = 1;;); a;}", "a");
  err("x = void() {while (1) {var a = 0;} a;}", "a");
  err("x = void() {do 0; while (var a = 0); a;}", "a");
  err("x = void() {for (0; 0; var a = 0) {a;}}", "a");
  err("x = void() {var a = a;}", "a");
  err("x = void() {{var a = 0;} a;}", "a");
  err("global{{const a = 0;}} x = void() {a;}", "a");
  err("global if (var a = 0); x = void() {a;}", "a");
  err("global if (1) {var a = 0;} x = void() {a;}", "a");
  err("global void() {var a = 0;}; x = void() {a;}", "a");
  err("~global {const a = 0;} x = void() {a;}", "a");

  // Some multi-line errors, just for fun.
  err(TestMultilineErrorStrA, "int\n/**/()");
  err(TestMultilineErrorStrB, "+");
}

TEST_F(YangTest, WarningTest)
{
  if (!filter("errors")) {
    return;
  }

  auto ctxt = context();
  ctxt.register_function("noop", make_fn([]{}));
  auto warc = [&](const std::string& str,
                  const std::string& text, std::size_t count)
  {
#ifdef DEBUG
    auto prog = program(ctxt, str, true);
#else
    auto prog = program_suppress_errors(ctxt, str);
#endif
    EXPECT_TRUE(prog.success()) <<
        "Should compile successfully:\n" << str << std::endl;
    const auto& warnings = prog.get_warnings();
    EXPECT_EQ(count, warnings.size()) <<
        "Should have " << count << " warning(s):\n" << str << std::endl;
    if (warnings.size() >= 1) {
      EXPECT_EQ(text, warnings[0].node.text) <<
          "Warning should reference given text:\n" << str << std::endl;
    }
  };
  auto warn = [&](const std::string& str, const std::string& text)
  {
    warc(str, text, 1);
  };
  auto no_warn = [&](const std::string& str)
  {
    warc(str, "", 0);
  };

  // Avoid warnings.
  no_warn("export x = void(int) {}");
  no_warn("export x = void(int a) {a;}");
  no_warn("export x = void() {const a = 0; a;}");
  no_warn("export x = void() {var a = 0; a = a;}");
  no_warn("export x = void() {var a = 0; ++a;}");
  no_warn("export global {var a = 0; const b = 0;}");
  no_warn("export global {var f = void() {};}");
  no_warn("global {const f = void() {f;};}");
  no_warn("global {const f = void() {}; f;}");
  no_warn("export x = void() {var a = 0; while (a *= 1);}");
  no_warn("export x = void() {var a = 0; for (; a = 1;);}");

  // Warnings.
  warn("export x = void() {const a = 0;}", "=");
  warn("export x = void() {var a = 0; a = 1;}", "=");
  warn("export x = void() {var a = 0; a *= 1;}", "=");
  warn("export x = void() {var a = 0; for (a *= 1;; a *= 1);}", "=");
  warn("export x = void(int a) {}", "int a");
  warn("x = void() {}", "=");
  warn("global {const a = 0;}", "=");
  warn("global {var a = 0;} export x = void() {a = 1;}", "=");
  warn("export x = void() {var f = void() {}; f;}", "=");
  warn("global {var a = 0;}", "=");
  warn("global {var f = void() {f;}; f;}", "=");
  warn("global {const f = void() {};}", "=");
  warn("export x = void() {closed const a = 0;}", "=");
  warn("export x = void() {closed var a = 0; void() {a;}();}", "=");
  warn("global {closed const a = 0; void() {a;}();}", "=");

  // Whitespace warnings.
  warn("\texport x = void() {}", "\t");
  warn("export x = void () {} \n", " \n");
  warn("export x = void() {} //\t", "\t");
  warn("export x = void() {} /* \n*/", " \n");
  warc("export x = void()\n{\n\treturn;  \n}", "\t", 2);

  // Empty if-statements.
  warn("export x = void() {if (1);}", ";");
  warn("export x = void() {if (1) noop(); else;}", ";");
  warn("export x = void() {if (1); else;}", ";");
  no_warn("export x = void() {if (1); else noop();}");

  // Dead code.
  warn("export x = void() {return; noop();}", "noop();");
  warn("export x = int() {return 0; return 0;}", "return 0;");
  warn("export x = void() {if (1) {return; return;}}", "return;");
  warn("export x = void() {if (1) return; else return; return;}", "return;");
  warc(
      "export x = void() {if (1) {return; return;} return; return;}",
      "return;", 2);
  warc(
      "export x = void() {if (1) return; else {return; return;} return;}",
      "return;", 2);
  no_warn("export x = void() {if (1) return; return;}");

  // Operations with no effect.
  warn("export x = void() {1;}", "1");
  warn("export x = void() {true;}", "true");
  warn("export x = void() {1.;}", ".");
  warn("export x = void() {1 + 1;}", "+");
  warn("export x = void() {1 == 1;}", "==");
  warn("export x = void() {$+(1, 1);}", "$");
  warn("export x = void() {!1;}", "!");
  warn("export x = void() {for(1; 0;);}", "1");
  warn("export x = void() {for(; 0; 1);}", "1");
  warn("export x = void() {do 1; while(0);}", "1");
  warn("export x = void() {void() {};}", "void()");
  no_warn("export x = void() {void() {}();}");
  warn("export x = void() {0 && 1;}", "1");
  warn("export x = void() {0 && 0 && 1;}", "1");
  no_warn("export x = int() {0 && x(); return 0;}");
  warn("export x = int() {(x(), 1, x()); return 0;}", "1");
  no_warn("export x = int() {(x(), x()); return 0;}");
  warn("export x = void() {0 ? 0 : 0;}", "?");
  warn("export x = void() {0 ? 0 : 0 ? 0 : 0;}", "?");
  warn("export x = void() {0 ? 0 ? 0 : 0 : 0;}", "?");
  no_warn("export x = int() {0 ? 0 : x(); return 0;}");
  no_warn("export x = int() {0 ? 0 : 0 ? 0 : x(); return 0;}");
}

// End namespace yang.
}
