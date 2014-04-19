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
  return 0;
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

  ASSERT_EQ(errors.size(), 1);
  ASSERT_EQ(warnings.size(), 1);

  const auto& error = errors[0];
  const auto& warning = warnings[0];

  EXPECT_EQ(error.node.start_line, 4);
  EXPECT_EQ(error.node.end_line, 4);
  EXPECT_EQ(error.node.start_column, 8);
  EXPECT_EQ(error.node.end_column, 8);
  EXPECT_EQ(error.node.start_index, 37);
  EXPECT_EQ(error.node.end_index, 37);
  EXPECT_EQ(error.node.text, "+");

  EXPECT_EQ(error.tree.start_line, 4);
  EXPECT_EQ(error.tree.end_line, 5);
  EXPECT_EQ(error.tree.start_column, 4);
  EXPECT_EQ(error.tree.end_column, 4);
  EXPECT_EQ(error.tree.start_index, 33);
  EXPECT_EQ(error.tree.end_index, 43);
  EXPECT_EQ(error.tree.text, "1.0 +\n    1");

  EXPECT_EQ(warning.node.start_line, 7);
  EXPECT_EQ(warning.node.end_line, 7);
  EXPECT_EQ(warning.node.start_column, 2);
  EXPECT_EQ(warning.node.end_column, 5);
  EXPECT_EQ(warning.node.start_index, 73);
  EXPECT_EQ(warning.node.end_index, 76);
  EXPECT_EQ(warning.node.text, "x();");
  EXPECT_EQ(warning.node.text, warning.tree.text);
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
    EXPECT_EQ(errors.size(), count) <<
        "Should have " << count << " error(s):\n" << str << std::endl;
    if (errors.size() >= 1) {
      EXPECT_EQ(errors[0].node.text, text) <<
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
  erc("x = int() {return;}", "return;", 2);

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

  // Variable declaration and assignment errors.
  err("x = void() {var a;}", ";");
  erc("x = void() {var a + 1 = 0;}", "a", 2);
  err("x = void() {var a = 0; a + 1 = 1;}", "=");
  err("x = void() {a = 0;}", "a");
  err("x = void() {x = x;}", "=");
  err("x = void() {var a = 0; a = 0.;}", "=");
  err("x = void() {var a = 0; var a = 0;}", "=");
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
  err("x = void() {closed const a = 0; void() {--a;};}", "--");

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
    EXPECT_EQ(warnings.size(), count) <<
        "Should have " << count << " warning(s):\n" << str << std::endl;
    if (warnings.size() >= 1) {
      EXPECT_EQ(warnings[0].node.text, text) <<
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

  // Warnings.
  warn("export x = void() {const a = 0;}", "=");
  warn("export x = void() {var a = 0; a = 1;}", "=");
  warn("export x = void(int a) {}", "int a");
  warn("x = void() {}", "=");
  warn("global {const a = 0;}", "=");
  warn("global {var a = 0;} export x = void() {a = 1;}", "=");
  warn("export x = void() {var f = void() {}; f;}", "=");
  warn("global {var a = 0;}", "=");
  warn("global {var f = void() {f;}; f;}", "=");
  warn("global {const f = void() {};}", "=");
  warn("export x = void() {closed const a = 0;}", "=");
  warn("export x = void() {closed var a = 0; void() {a;};}", "=");
  warn("global {closed const a = 0; void() {a;};}", "=");

  // Whitespace warnings.
  warn("\texport x = void() {}", "\t");
  warn("export x = void () {} \n", " \n");
  warn("export x = void() {} //\t", "\t");
  warn("export x = void() {} /* \n*/", " \n");
  warc("export x = void()\n{\n\treturn;  \n}", "\t", 2);

  // Empty if-statements.
  warn("export x = void() {if (1);}", ";");
  warn("export x = void() {if (1) 0; else;}", ";");
  warn("export x = void() {if (1); else;}", ";");
  no_warn("export x = void() {if (1); else 0;}");

  // Dead code.
  warn("export x = void() {return; 0;}", "0;");
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
}

// End namespace yang.
}
