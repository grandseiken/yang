//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {

struct ErrorTest : YangTest {
  void assert_error(bool error, bool no_warnings, const std::string& str,
                    const std::vector<std::string>& texts)
  {
#ifdef DEBUG
    auto prog = program(context(), str, true);
#else
    auto prog = program_suppress_errors(context(), str);
#endif
    const auto& errors = prog.get_errors();
    std::string errors_str;
    for (const auto& e : errors) {
      errors_str += e.formatted_message + '\n';
    }
    const auto& warnings = prog.get_warnings();
    std::string warnings_str;
    for (const auto& e : warnings) {
      warnings_str += e.formatted_message + '\n';
    }

    if (error) {
      ASSERT_FALSE(prog.success()) <<
          "Should fail to compile:\n" << str << std::endl;
    }
    else {
      ASSERT_TRUE(prog.success()) <<
          "Should compile successfully:\n" << str <<
          '\n' << errors_str << std::endl;
    }
    const auto& source = error ? errors : warnings;

    std::vector<std::string> t;
    for (const auto& s : texts) {
      t.push_back(s);
    }
    EXPECT_EQ(t.size(), source.size()) <<
        "Should have " << t.size() <<
        (error ? " error(s):\n" : " warning(s):\n") << str <<
        '\n' << (error ? errors_str : warnings_str) << std::endl;
    for (std::size_t i = 0; i < source.size() && i < t.size(); ++i) {
      EXPECT_EQ(t[i], source[i].node.text) <<
          (error ? "Error #" : "Warning #") << (1 + i) <<
          " should reference given text:\n" << str << std::endl;
    }
    if (error && no_warnings) {
      EXPECT_TRUE(warnings.empty()) <<
        "Should have 0 warning(s):\n" <<
        str << '\n' << warnings_str << std::endl;
    }
  };
};

#define ERROR_TEST(name, ...)\
  TEST_F(ErrorTest, name) {assert_error(__VA_ARGS__);}\
  void noop_requiring_semicolon()
#define ERROR(name, program, ...)\
  ERROR_TEST(name, true, false, program, {__VA_ARGS__})
#define ERROR_NO_WARNING(name, program, ...)\
  ERROR_TEST(name, true, true, program, {__VA_ARGS__})
#define WARNING(name, program, ...)\
  ERROR_TEST(name, false, false, program, {__VA_ARGS__})
#define NO_WARNING(name, program)\
  ERROR_TEST(name, false, false, program, {})

// Miscellaneous syntax errors.
ERROR(CommentEof, "/* comment", "t");
ERROR(TopLevelA, "broken = {}\n", "{");
ERROR(TopLevelB, "EXPORT broken = void() {}", "broken");
ERROR(TopLevelC, "global x = 0;", "x");
ERROR(TopLevelD, "export = void() {}", "=");
ERROR(TopLevelE, "export export = void() {}", "export");
ERROR(TopLevelF, "export for = void() {}", "for");
ERROR(TopLevelG, "export int4 = void() {}", "int4", "=");
ERROR(TopLevelH, "global const x = int;", "int");
ERROR(TopLevelI, "global const x = 0", "0");
ERROR(TopLevelJ, "void() x = void() {}", "x");
ERROR(TopLevelK, "+", "+");
ERROR(TopLevelL, "#", "#");
ERROR(TopLevelM, "x;", ";");
ERROR(TopLevelN, "x = 0;", "=");
ERROR(TopLevelO, "x = void();", "void");
ERROR(TopLevelP, "x = {};", "{");
ERROR(TopLevelQ, "x = () {};", ")");
ERROR(TopLevelR, "x = 0; y = float() {return x;}", "=");
ERROR(SyntaxA, "x = void() {0}", "}");
ERROR(SyntaxB, "x = void() {return}", "}");
ERROR(SyntaxC, "x = void(return 0;) {}", "return");
ERROR(SyntaxD, "x = void() {;", ";");
ERROR(SyntaxE, "x = void() };", "}");
ERROR(SyntaxF, "x = void() {if ();}", ")");
ERROR(SyntaxG, "x = void() {0, 0;}", ",");

// Literal syntax errors.
ERROR(FloatLiteralA, "x = void() {0.0.0;}", ".0");
ERROR(FloatLiteralB, "x = void() {0ee0;}", "ee0");
ERROR(FloatLiteralC, "x = void() {0e++0;}", "e");
ERROR(FloatLiteralD, "x = void() {0e--0;}", "e");
ERROR(FloatLiteralE, "x = void() {e0;}", "e0");
ERROR(FloatLiteralF, "x = void() {0..0;}", ".0");
ERROR(FloatLiteralG, "x = void() {0e;}", "e");
ERROR(FloatLiteralH, "x = void() {1e1.0;}", ".0");
ERROR(HexLiteralA, "x = void() {0x;}", "x");
ERROR(HexLiteralB, "x = void() {x0;}", "x0");
ERROR(HexLiteralC, "x = void() {1x0;}", "x0");
ERROR(HexLiteralD, "x = void() {0xg;}", "xg");
ERROR(HexLiteralE, "x = void() {0xH;}", "xH");
ERROR(HexLiteralF, "x = void() {0y0;}", "y0");
ERROR(HexLiteralG, "x = void() {0x100000000;}", "0x100000000");
ERROR(StringLiteralA, "x = void() {\"a\na\";}", "\"a\na\"");
ERROR(StringLiteralB, "x = void() {\"\\'\";}", "\"\\'\"");
ERROR(StringLiteralC, "x = void() {\"\\1\";}", "\"\\1\"");
ERROR(StringLiteralD, "x = void() {\";}", "\"");
ERROR(StringLiteralE, "x = void() {\"\\\";}", "\"");
ERROR(StringLiteralF, "x = void() {\"\"\";}", "\"");

// Type / expression mismatches.
ERROR(TypeExpressionA, "x = 0() {}", "0");
ERROR(TypeExpressionB, "x = void(0 a) {}", "0");
ERROR(TypeExpressionC, "x = (void() + void()) {}", "{");
ERROR(TypeExpressionD, "x = void() {int;}", "int");
ERROR(TypeExpressionE, "x = void() {return int();}", "int");

// Function signature / invocation errors.
ERROR(FunctionA, "int = void() {}", "int", "=");
ERROR(FunctionB, "global {return 0;}", "return 0;");
ERROR(FunctionC, "~global {return;}", "return;");
ERROR(FunctionD, "export ~global {}", "export ~global {}");
ERROR(FunctionE, "x = void {}", "{");
ERROR(FunctionF, "x = void(int a, int a) {}", "int a");
ERROR(FunctionG, "x = void(void a) {}", "void a");
ERROR(FunctionH, "x = void() {return 0;}", "0");
ERROR(FunctionI, "x = int() {}", "int()");
ERROR(FunctionJ, "x = int() {return 1.;}", ".");
ERROR(FunctionK, "x = int() {return;}", "return;");
ERROR(FunctionL, "x = int() {const x = 1 + 1.; return x;}", "+");
ERROR(CallA, "x = void(int a) {x(a b);}", "a b");
ERROR(CallB, "x = void(int a) {a();}", "(");
ERROR(CallC, "x = void(int a) {x();}", "(");
ERROR(CallD, "x = void(int a) {x(1, 2);}", "(");
ERROR(CallE, "x = void(int a) {x(1.);}", ".");

// Statement errors.
ERROR(StatementA, "x = void() {if (0.);}", ".");
ERROR(StatementB, "x = void() {if (return 0;);}", "return");
ERROR(StatementC, "x = void() {for (0; (0, 0); 0);}", "(");
ERROR(StatementD, "x = void() {while (x);}", "x");
ERROR(StatementE, "x = void() {do; while((0, 0));}", "(");
ERROR(StatementF, "x = void() {break;}", "break;");
ERROR(StatementG, "x = void() {continue;}", "continue;");
ERROR(StatementH, "x = void() {int::foo();}", "::");

// User-type errors.
ERROR(UserTypeA, "x = void(err f) {}", "err");
ERROR(UserTypeB, "x = void() {UserType::foo();}", "(");
ERROR(UserTypeC, "x = void() {0.foo();}", ".");
ERROR(UserTypeD, "x = void() {get_user_type().bar();}", ".");
ERROR(UserTypeE, "x = void() {get_user_type().foo(1);}", "(");
ERROR(UserTypeF, "x = void() {UserType();}", "UserType");
ERROR(UserTypeG, "x = void() {MuserType(0);}", "(");
ERROR(UserTypeH, "x = OtherType() {return get_user_type();}", "(");
ERROR(UserTypeI, "x = MotherType() {return MuserType();}", "(");
ERROR(UserTypeJ, "x = MuserType() {return get_user_type();}", "(");
ERROR(UserTypeK, "x = OtherType() {return MotherType();}", "(");
ERROR(UserTypeL, "x = void() {var a = get_other(); a = get_user_type();}", "=");
ERROR(UserTypeM, "x = void() {var a = MuserType(); a = MotherType();}", "=");
ERROR(UserTypeN, "x = void() {var a = MuserType(); a = get_user_type();}", "=");
ERROR(UserTypeO, "x = void() {var a = get_other(); a = MotherType();}", "=");

// Ternary errors.
ERROR(TernaryA, "x = void() {1. ? 0 : 0;}", "?");
ERROR(TernaryB, "x = void() {1 ? 0 : 0.;}", "?");
ERROR(TernaryC, "x = void() {(1, 1, 1) ? (1, 1) : (1, 1);}", "?");
ERROR(TernaryD, "x = void() {(1, 1) ? 0 : 1;}", "?");
ERROR(TernaryE, "x = void() {(1, 1) ? (1, 1, 1) : (1, 1);}", "?");
ERROR(TernaryF, "x = void() {(1 ? 1 : 2) + 1.0;}", "+");
ERROR(TernaryG, "x = void() {1 ? (1, 1) : 1;}", "?");
ERROR(TernaryH, "x = void() {(1, 1) ? (1, 1) : 1;}", "?");

// Operator errors.
ERROR(OpA, "x = void() {1. | 1.;}", "|");
ERROR(OpB, "x = void() {(1, 1) & (1, 1, 1);}", "&");
ERROR(OpC, "x = void() {(1 && 1) + 1.;}", "+");
ERROR(OpD, "x = void() {1 << 1.;}", "<<");
ERROR(OpE, "x = void() {1. >> 1.;}", ">>");
ERROR(OpF, "x = void() {1. + (1). + 1;}", "+");
ERROR(OpG, "x = void() {(1. == 1.) + 1.;}", "+");
ERROR(OpH, "x = void() {$==(1., 1.) * 1.;}", "*");
ERROR(OpI, "x = void() {$&&(1., 1.);}", "$");
ERROR(OpJ, "x = void() {!1.;}", "!");
ERROR(OpK, "x = void() {-1 - 1.;}", "-");
ERROR(OpL, "x = void() {(1, 1, 1, 1.);}", "(");
ERROR(OpM, "x = void() {();}", ")");
ERROR(OpN, "x = void() {0 0;}", "0");
ERROR(OpO, "x = void() {((1, 1), (1, 1));}", "(");
ERROR(OpP, "x = void() {(x, 1);}", "x");
ERROR(OpQ, "x = void() {(1, (1, 1));}", "(");
ERROR(OpR, "x = void() {(1 a, 2);}", "1 a");
ERROR(OpS, "x = void() {(1, 2)[1.];}", "[");
ERROR(OpT, "x = void() {(1)[1];}", "[");
ERROR(OpU, "x = void() {[0];}", "[");
ERROR(OpV, "x = void() {1..;}", ".");
ERROR(OpW, "x = void() {++x;}", "++");
ERROR(OpX, "x = void() {var x = 0.; x += 1;}", "+=");
ERROR(OpY, "x = void() {y += 1;}", "y");
ERROR(OpZ, "x = void() {var x = 1; x *= (1, 1);}", "*=");
ERROR(Op0, "x = void() {var x = 1; x *= 1.;}", "*=");
ERROR(Op1, "x = void() {\"foo\" + 2;}", "+");
ERROR(Op2, "x = void() {var x = \"foo\"; ++foo;}", "foo");

// Variable declaration and assignment errors.
ERROR(VarA, "x = void() {var a;}", ";");
ERROR(VarB, "x = void() {var a = 0; a + 1 = 1;}", "=");
ERROR(VarC, "x = void() {a = 0;}", "a");
ERROR(VarD, "x = void() {x = x;}", "=");
ERROR(VarE, "x = void() {var a = 0; a = 0.;}", "=");
ERROR(VarF, "x = void() {var a = 0; var a = 0;}", "=");
ERROR(VarG, "x = void() {const a = 0; const a = 0.;}", "=");
ERROR(VarH, "x = void() {const a = 0; a = 0;}", "=");
ERROR(VarI, "x = void() {UserType = get_user_type();}", "UserType");
ERROR(VarJ, "x = UserType() {get_user_type = x; return get_user_type();}", "=");
ERROR(VarK, "global {var a = 0;} global {var a = 0;}", "=");
ERROR(VarL, "global {const a = 0;} global {a = 0;}", "=");
ERROR(VarM, "x = void() {} x = void() {}", "=");
ERROR(VarN, "x = void() {} global const x = x;", "=");
ERROR(VarO, "x = void(int y) {y = 0;}", "=");
ERROR(VarP, "x = void() {var a = 0; a += 1.;}", "+=");
ERROR(VarQ, "x = void() {var a = 0; a += (1, 1);}", "+=");
ERROR(VarR, "x = void() {var a = 0; a += void() {};}", "+=");
ERROR(VarS, "x = void() {const a = 0; ++a;}", "++");
ERROR(VarT, "x = void() {const a = 0; void() {a;};}", "a");
ERROR(VarU, "x = void() {var a = 0; void() {a += 1;};}", "a");
ERROR(VarV, "x = void() {closed const a = 0; void() {--a;};}", "--");
ERROR(VarW, "x = void() {a + var a = 1;}", "a");
ERROR(VarX, "x = void() {var a = a;}", "a");
ERROR(VarY, "x = void() {var a + 1 = 0;}", "a", "=");
ERROR(VarZ, "x = void() {0 && (var a = 0); a;}", "a");
ERROR(Var0, "x = void() {0 ? (var a = 0) : (var b = 0); a; b;}", "a", "b");
ERROR(Var1, "global const x = 0; x = void() {}", "=");
ERROR(Var2, "x = void() {MuserType = MuserType;}", "=");
ERROR(Var3, "x = void() {get_user_type = get_user_type;}", "=");
ERROR(Var4, "x = void() {(true ? const a = 1 : const a = 1) = 1;}", "=");
ERROR(Var5, "x = void() {(true ? const a = 1 : var a = 1) = 1;}", "=");
ERROR(Var6, "x = void() {var a = 0; (true ? a : a + 1) = 1;}", "=");
ERROR(Var7, "x = void() {var a = x();}", "=");

// Name resolution / scope errors.
ERROR(ScopeA, "x = void() {y;}", "y");
ERROR(ScopeB, "x = void() {y();} y = void() {}", "y");
ERROR(ScopeC, "x = void() {{const a = 0;} void() {a;};}", "a");
ERROR(ScopeD, "global {const a = 0;}; global {a = 0;}", "=");
ERROR(ScopeE, "x = void() {const a = 0;} y = void() {a = 0;}", "a");
ERROR(ScopeF, "x = void() {if (var a = 1); a;}", "a");
ERROR(ScopeG, "x = void() {if (1) {var a = 0;} a;}", "a");
ERROR(ScopeH, "x = void() {for (var a = 1;;); a;}", "a");
ERROR(ScopeI, "x = void() {while (1) {var a = 0;} a;}", "a");
ERROR(ScopeJ, "x = void() {do 0; while (var a = 0); a;}", "a");
ERROR(ScopeK, "x = void() {for (0; 0; var a = 0) {a;}}", "a");
ERROR(ScopeL, "x = void() {var a = a;}", "a");
ERROR(ScopeM, "x = void() {{var a = 0;} a;}", "a");
ERROR(ScopeN, "global{{const a = 0;}} x = void() {a;}", "a");
ERROR(ScopeO, "global if (var a = 0); x = void() {a;}", "a");
ERROR(ScopeP, "global if (1) {var a = 0;} x = void() {a;}", "a");
ERROR(ScopeQ, "global void() {var a = 0;}; x = void() {a;}", "a");
ERROR(ScopeR, "~global {const a = 0;} x = void() {a;}", "a");

// Some multi-line errors, just for fun.
namespace {
  const std::string multiline_error_a = R"(
export x = int
/**/()
{
  if (1) {
    return 1;
  }
})";
  const std::string multiline_error_b = R"(
x = void()
{
  return 0 +
  0.; // Comment continues for a while.
})";
}
ERROR(MultilineA, multiline_error_a, "int\n/**/()");
ERROR(MultilineB, multiline_error_b, "+");

// Avoid warnings.
NO_WARNING(AvoidWarningA, "export x = void(int) {}");
NO_WARNING(AvoidWarningB, "export x = void(int a) {a;}");
NO_WARNING(AvoidWarningC, "export x = void() {const a = 0; a;}");
NO_WARNING(AvoidWarningD, "export x = void() {var a = 0; a = a;}");
NO_WARNING(AvoidWarningE, "export global {var a = 0; const b = 0;}");
NO_WARNING(AvoidWarningF, "export global {var f = void() {};}");
NO_WARNING(AvoidWarningG, "global {const f = void() {f;};}");
NO_WARNING(AvoidWarningH, "global {const f = void() {}; f;}");
NO_WARNING(AvoidWarningI, "export x = void() {var a = 0; while (a *= 1);}");
NO_WARNING(AvoidWarningJ, "export x = void() {var a = 0; for (; a = 1;);}");
NO_WARNING(AvoidWarningK, "export x = int() {return true ? const x = 0 : 0;}");
NO_WARNING(AvoidWarningL,
        "export x = int() {return true ? const x = 0 : const x = 1;}");

// Variable warnings.
WARNING(WarnVarA, "export x = void() {const a = 0;}", "=");
WARNING(WarnVarB, "export x = void() {var a = 0; a = 1;}", "=");
WARNING(WarnVarC, "export x = void() {var a = 0; a *= 1;}", "=");
WARNING(WarnVarD, "export x = void() {var a = 0; for (a *= 1;; a *= 1);}", "=");
WARNING(WarnVarE, "export x = void(int a) {}", "int a");
WARNING(WarnVarF, "x = void() {}", "=");
WARNING(WarnVarG, "global {const a = 0;}", "=");
WARNING(WarnVarH, "global {var a = 0;} export x = void() {a = 1;}", "=");
WARNING(WarnVarI, "export x = void() {var f = void() {}; f;}", "=");
WARNING(WarnVarJ, "global {var a = 0;}", "=");
WARNING(WarnVarK, "global {var f = void() {f;}; f;}", "=");
WARNING(WarnVarL, "global {const f = void() {};}", "=");
WARNING(WarnVarM, "export x = void() {closed const a = 0;}", "=");
WARNING(WarnVarN, "export x = void() {closed var a = 0; void() {a;}();}", "=");
WARNING(WarnVarO, "global {closed const a = 0; void() {a;}();}", "=");
WARNING(WarnVarP, "export x = void() {var a = 0; ++a;}", "=");

// Whitespace warnings.
WARNING(WarnWhitespaceA, "\texport x = void() {}", "\t");
WARNING(WarnWhiteSpaceB, "export x = void () {} \n", " \n");
WARNING(WarnWhitespaceC, "export x = void() {} //\t", "\t");
WARNING(WarnWhitespaceD, "export x = void() {} /* \n*/", " \n");
WARNING(WarnWhitespaceE, "export x = void()\n{\n\treturn;  \n}", "\t", "  \n");

// Empty if-statements.
WARNING(WarnEmptyIfA, "export x = void() {if (1);}", ";");
WARNING(WarnEmptyIfB, "export x = void() {if (1) noop(); else;}", ";");
WARNING(WarnEmptyIfC, "export x = void() {if (1); else;}", ";");
NO_WARNING(WarnEmptyIfD, "export x = void() {if (1); else noop();}");

// Dead code.
WARNING(WarnDeadCodeA, "export x = void() {return; noop();}", "noop();");
WARNING(WarnDeadCodeB, "export x = int() {return 0; return 0;}", "return 0;");
WARNING(WarnDeadCodeC,
        "export x = void() {if (1) {return; return;}}", "return;");
WARNING(WarnDeadCodeD,
        "export x = void() {if (1) return; else return; return;}", "return;");
WARNING(WarnDeadCodeE,
        "export x = void() {if (1) {return; return;} return; return;}",
        "return;", "return;");
WARNING(WarnDeadCodeF,
        "export x = void() {if (1) return; else {return; return;} return;}",
        "return;", "return;");
NO_WARNING(WarnDeadCodeG, "export x = void() {if (1) return; return;}");

// Operations with no effect.
WARNING(WarnNoEffectA, "export x = void() {1;}", "1");
WARNING(WarnNoEffectB, "export x = void() {true;}", "true");
WARNING(WarnNoEffectC, "export x = void() {1.;}", ".");
WARNING(WarnNoEffectD, "export x = void() {1 + 1;}", "+");
WARNING(WarnNoEffectE, "export x = void() {1 == 1;}", "==");
WARNING(WarnNoEffectF, "export x = void() {$+(1, 1);}", "$");
WARNING(WarnNoEffectG, "export x = void() {!1;}", "!");
WARNING(WarnNoEffectH, "export x = void() {for(1; 0;);}", "1");
WARNING(WarnNoEffectI, "export x = void() {for(; 0; 1);}", "1");
WARNING(WarnNoEffectJ, "export x = void() {do 1; while(0);}", "1");
WARNING(WarnNoEffectK, "export x = void() {void() {};}", "void()");
WARNING(WarnNoEffectL, "export x = void() {0 && 1;}", "1");
WARNING(WarnNoEffectM, "export x = void() {0 && 0 && 1;}", "1");
WARNING(WarnNoEffectN, "export x = int() {(x(), 1, x()); return 0;}", "1");
WARNING(WarnNoEffectO, "export x = void() {0 ? 0 : 0;}", "?");
WARNING(WarnNoEffectP, "export x = void() {0 ? 0 : 0 ? 0 : 0;}", "?");
WARNING(WarnNoEffectQ, "export x = void() {0 ? 0 ? 0 : 0 : 0;}", "?");
NO_WARNING(WarnNoEffectR, "export x = void() {void() {}();}");
NO_WARNING(WarnNoEffectS, "export x = int() {0 && x(); return 0;}");
NO_WARNING(WarnNoEffectT, "export x = int() {(x(), x()); return 0;}");
NO_WARNING(WarnNoEffectU, "export x = int() {0 ? 0 : x(); return 0;}");
NO_WARNING(WarnNoEffectV, "export x = int() {0 ? 0 : 0 ? 0 : x(); return 0;}");

// Lvalue and tag propagation through errors.
ERROR_NO_WARNING(LvalueA,
                 "x = void(int b) {b; var a = void() {}; x(++a);}", "++");
ERROR_NO_WARNING(LvalueB,
                 "x = void(int b) {b; var a = 1; x(a *= (1, 1));}", "*=");
ERROR_NO_WARNING(LvalueC, "x = void(int b) {b; var a = 1.; x(a &= 1.);}", "&=");
ERROR_NO_WARNING(LvalueD, "x = void(int b) {b; var a = 1; x(a *= 1.);}", "*=");
ERROR_NO_WARNING(LvalueE, "x = void(int b) {b; var a = 1; x(a = 1.);}", "=");
ERROR_NO_WARNING(LvalueF, "x = void(int b) {b; var a = 1; x(a[1] = 1);}", "[");
ERROR_NO_WARNING(LvalueG,
                 "x = void(int b) {b; var a = 1; x((0 ? a : 0.) = 1);}", "?");
ERROR_NO_WARNING(LvalueH, "global {--++0;}", "++");
ERROR_NO_WARNING(LvalueI, "export global {--++const a = 0;}", "++");
ERROR_NO_WARNING(LvalueJ, "global {--(0 = 0);}", "=");
ERROR_NO_WARNING(LvalueK, "export global {--((const a = 0) = 0);}", "=");
ERROR_NO_WARNING(LvalueL, "global {--(1[1]);}", "[");
ERROR_NO_WARNING(LvalueM, "export global {--((const a = 1)[1]);}", "[");

// End namespace yang.
}
