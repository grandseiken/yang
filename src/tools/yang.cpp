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
  yang::Context context;
  yang::Program program(context, path, contents);
  if (!program.success()) {
    return 1;
  }
  for (const auto& pair : program.get_globals()) {
    if (pair.second.is_exported()) {
      log_info("global [", pair.second.string(context), "] ", pair.first);
    }
  }
  for (const auto& pair : program.get_functions()) {
    log_info("function [", pair.second.string(context), "] ", pair.first);
  }

  log_info("Source:\n", program.print_ast());
  log_info("IR:\n", program.print_ir());
  return 0;
}
