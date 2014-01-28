**Yang** is a scripting language designed for easy embedding in C++ programs.

Some features:
* Static typing
* Scripts are JIT-compiled to native code using LLVM for super-fast execution
* Natural, type-safe, minimal-boilerplate interop between script and host program code.

The language and API are still in early stages and very much in flux, so you probably don't want to use this for anything serious just yet.

The plan is that a hello world program might end up looking something like this:

```cpp
#include <functional>
#include <iostream>
#include <string>
#include <yang/yang.h>

std::string script_string = R"*(

  export script_hello_world = void()
  {
    native_hello_world();
  };

)*";

int main()
{
  std::function<void()> native_hello_world = []()
  {
    std::cout << "Hello, world!";
  };

  yang::Context context;
  context.register_function("native_hello_world", native_hello_world);

  yang::Program program(context, "example", script_string);
  yang::Instance instance(program);

  auto script_hello_world =
      instance.get_function<yang::Function<void()>>("script_hello_world");
  script_hello_world();
}
```
