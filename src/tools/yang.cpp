//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <iostream>
#include <fstream>

#include <yang/yang.h>
#include "../log.h"

int main(int argc, char** argv)
{
  if (argc < 2) {
    log_err("No input file specified");
    return 1;
  }

  std::string path = argv[1];
  std::string contents;
  try {
    std::ifstream file(path, std::ios::in | std::ios::binary);
    if (!file) {
      log_err(path, " is not a file");
      return 1;
    }

    file.seekg(0, std::ios::end);
    contents.resize(file.tellg());
    file.seekg(0, std::ios::beg);
    file.read(&contents[0], contents.length());
  }
  catch (const std::ofstream::failure& e) {
    log_err("Read of ", path, " failed");
    return 1;
  }

  auto print = yang::make_fn([](yang::Ref<const char> str)
  {
    log_info(str.get());
  });
  yang::Context context;
  context.register_member_function("print", print);

  yang::Program program(context, path, contents);
  if (!program.success()) {
    return 1;
  }

  for (const auto& pair : program.get_globals()) {
    log_info("global [", (pair.second.is_const ? "const " : ""),
             pair.second.type.string(context), "] ", pair.first);
  }
  for (const auto& pair : program.get_functions()) {
    log_info("function [", pair.second.string(context), "] ", pair.first);
  }
  log_info("Source:\n", program.print_ast());
  log_info("IR:\n", program.print_ir());

  auto it = program.get_functions().find("main");
  if (it != program.get_functions().end() &&
      it->second.is_function() && it->second.function_num_args() == 0) {
    yang::Instance instance(program);
    if (it->second.function_return().is_void()) {
      log_info("Calling main()...");
      instance.call<void>("main");
    }
    else if (it->second.function_return().is_int()) {
      log_info("Calling main()...");
      log_info("Result: ", instance.call<int>("main"));
    }
  }
  return 0;
}
