**Yang** is a scripting language designed for easy embedding in C++ programs.

Some features:
* The language feels mostly like C, except with first-class functions and closures.
* Static type checking, with vector types and user types (raw or reference-counted).
* Programs can be compiled once but instantiated many times. One program can be used as a library of functions for others.
* Fairly nice error messages.
* Yang uses LLVM as a backend, so compiled programs run quite fast.
* Natural, type-safe, no-boilerplate interop between script code and host code. Values can be passed back and forth between C++ and Yang. Functions defined in one can be invoked from the other. Reference-counting (where applicable) just works across the two languages.

Some planned features:
* Structural interfaces: define an interface type containing some member functions. Any value whose type is a user type with matching member functions, or any program instance with matching exported functions, is then covertible to the interface type.
* Reasonable thread-safety guarantees?

Some drawbacks:
* Using Yang involves linking your program against a decent chunk of LLVM, which will increase the size of your distribution by a good 10 megabytes or so. There may be some sort of bytecode backend in the future.
* Yang makes heavy use of C++11 language and library features, so you'll need a recent compiler. There is no C API.

The language and API are not quite stable, so you might not want to use this for anything serious just yet.

Currently, a hello world program looks something like this:

```cpp
#include <iostream>
#include <string>
#include <yang/yang.h>

std::string script_string = R"*(

  export script_hello_world = void()
  {
    native_hello_world();
  }

)*";

int main()
{
  auto native_hello_world = []
  {
    std::cout << "Hello, world!";
  };

  yang::Context context;
  context.register_function("native_hello_world",
                            yang::make_fn(native_hello_world));

  yang::Program program(context, "example", script_string);
  yang::Instance instance(program);

  auto script_hello_world =
      instance.get_function<yang::Function<void()>>("script_hello_world");
  script_hello_world();
}
```
