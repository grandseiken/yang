**Yang** is a scripting language designed for easy embedding in C++ programs.

Some features:
* Static type system including primitive vector types and extendable user types.
* Powerful execution model including instanced scripts, higher-order functions, and lexical closures.
* Good error messages.
* Scripts are JIT-compiled to native code using LLVM for super-fast execution.
* Natural, type-safe, minimal-boilerplate interop between script and host program code. Functions defined in either C++ or Yang can be passed back and forth between the two languages, stored, and invoked arbitrarily.

Some planned features:
* User types can be (optionally) automatically reference-counted.
* Reasonable thread-safety guarantees.
* Scripts can be combined either statically (one script acts as a library of functions for another) or dynamically (one script is passed as an argument to another via an abstract interface).

Some drawbacks:
* Using Yang involves linking your program against a decent chunk of LLVM, which will increase the size of your distribution by a good 10 megabytes or so. There may be some sort of bytecode backend in the future.
* Yang makes heavy use of C++11 language and library features, so you'll need a recent compiler. At the moment there's no C API.

The language and API are still in early stages and very much in flux, so you probably don't want to use this for anything serious just yet.

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
  auto native_hello_world = yang::make_fn([]()
  {
    std::cout << "Hello, world!";
  });

  yang::Context context;
  context.register_function("native_hello_world", native_hello_world);

  yang::Program program(context, "example", script_string);
  yang::Instance instance(program);

  auto script_hello_world =
      instance.get_function<yang::Function<void()>>("script_hello_world");
  script_hello_world();
}
```
