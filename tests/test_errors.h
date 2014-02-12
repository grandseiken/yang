//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
TEST_F(YangTest, FailureTest)
{
  auto& success_prog = program_suppress_errors("works = void() {};");
  EXPECT_TRUE(success_prog.success());
  EXPECT_NO_THROW(instance(success_prog));
  auto& failure_prog = program_suppress_errors("broken;");
  EXPECT_FALSE(failure_prog.success());
  EXPECT_THROW(instance(failure_prog), runtime_error);
}

TEST_F(YangTest, ErrorTest)
{
  auto err = [&](const std::string& str)
  {
    EXPECT_FALSE(program_suppress_errors(str).success()) <<
        "Should fail to compile:\n" << str << std::endl;
  };

  // Miscellaneous syntax errors.
  err("\tworks = void() {};");
  err("works = void () {}; \n");
  err("/* comment");
  err("broken = {};\n");
  err("EXPORT broken = void() {};");
  err("global x = 0;");
  err("export = void() {};");
  err("export export = void() {};");
  err("export for = void() {};");
  err("export int4 = void() {};");
  err("global const x = int;");
  err("void() x = void() {};");
  err("+");
  err("#");
  err("x;");
  err("x = 0;");
  err("x = void();");
  err("x = void() {}");
  err("x = void() {0};");
  err("x = void() {return}");
  err("x = void(return 0;) {};");
  err("x = {};");
  err("x = void() {;");
  err("x = void() };");
  err("x = () {};");
  err("x = void() {if ();};");

  // Type / expression mismatches.
  err("x = 0() {};");
  err("x = void(0 a) {};");
  err("x = (void() + void()) {};");
  err("x = void() {int;};");
  err("x = void() {return int(int);};");

  // Function signature errors.
  err("int = void() {};");
  err("global {return 0;};");
  err("x = void {};");
  err("x = void(int) {};");
  err("x = void(int a, int a) {};");
  err("x = void(void a) {};");
  err("x = void() {return 0;};");
  err("x = int() {};");
  err("x = int() {return 1.;};");
  err("x = int() {return;};");

  // Nested function errors.
  err("x = void() {var a = 0; void() {a;};};");

  // Statement errors.
  err("x = void() {if (0.);};");
  err("x = void() {if (return 0;);};");
  err("x = void() {for (0; (0, 0); 0);};");
  err("x = void() {while (x);};");
  err("x = void() {break;};");
  err("x = void() {continue;};");
  err("x = void() {int::foo();};");

  // User-type errors.
  err("x = void(err f) {};");
  err("x = void() {UserType::foo();};");
  err("x = void() {0.foo();};");
  err("x = void() {get_user_type().foo;};");
  err("x = void() {get_user_type().bar();};");
  err("x = void() {get_user_type().foo(1);};");

  // Ternary errors.
  err("x = void() {1. ? 0 : 0;};");
  err("x = void() {1 ? 0 : 0.;};");
  err("x = void() {(1, 1, 1) ? (1, 1) : (1, 1);};");
  err("x = void() {(1, 1) ? 0 : 1;};");
  err("x = void() {(1, 1) ? (1, 1, 1) : (1, 1);};");
  err("x = void() {(1 ? 1 : 2) + 1.0;};");
  err("x = void() {1 ? (1, 1) : 1;};");
  err("x = void() {(1, 1) ? (1, 1) : 1;};");

  // Invocation errors.
  err("x = void(int a) {x(a b);};");
  err("x = void(int a) {a();};");
  err("x = void(int a) {x();};");
  err("x = void(int a) {x(1, 2);};");
  err("x = void(int a) {x(1.);};");

  // Operator errors.
  err("x = void() {1. | 1.;};");
  err("x = void() {(1, 1) & (1, 1, 1);};");
  err("x = void() {(1 && 1) + 1.;};");
  err("x = void() {1 << 1.;};");
  err("x = void() {1. >> 1.;};");
  err("x = void() {1. + (1). + 1;};");
  err("x = void() {(1. == 1.) + 1.;};");
  err("x = void() {$==(1., 1.) * 1;};");
  err("x = void() {!1.;};");
  err("x = void() {-1 - 1.;};");
  err("x = void() {(1, 1, 1, 1.);};");
  err("x = void() {();};");
  err("x = void() {0 0;};");
  err("x = void() {((1, 1), (1, 1));};");
  err("x = void() {(x, x);};");
  err("x = void() {(1 a, 2);};");
  err("x = void() {(1, 2)[1.];};");
  err("x = void() {(1)[1];};");
  err("x = void() {[0];};");
  err("x = void() {1..;};");

  // Variable declaration and assignment errors.
  err("x = void() {var a;};");
  err("x = void() {var a + 1 = 0;};");
  err("x = void() {var a = 0; a + 1 = 1;};");
  err("x = void() {a = 0;};");
  err("x = void() {x = x;};");
  err("x = void() {var a = 0; a = 0.;};");
  err("x = void() {var a = 0; var a = 0;};");
  err("x = void() {const a = 0; a = 0;};");
  err("x = void() {var a = 0; void() {a = 1;};};");
  err("x = void() {UserType = get_user_type();};");
  err("x = UserType() {get_user_type = x; return get_user_type();};");
  err("global {var a = 0;}; global {var a = 0;};");
  err("global {const a = 0;}; global {a = 0;};");
  err("x = void() {}; x = void() {};");
  err("x = void() {}; global const x = x;");

  // Name resolution / scope errors.
  err("x = void() {y;};");
  err("x = void() {y();}; y = void() {};");
  err("global {const a = 0;}; global {a = 0;};");
  err("x = void() {const a = 0;}; y = void() {a = 0;};");
  err("x = void() {if (var a = 1); a;};");
  err("x = void() {if (1) {var a = 0;} a;};");
  err("x = void() {for (var a = 1;;); a;};");
  err("x = void() {while (1) {var a = 0;} a;};");
  err("x = void() {var a = a;};");
  err("x = void() {{var a = a;} a;};");
}
