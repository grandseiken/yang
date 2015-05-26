#==============================================================================#
# This file is part of the Yang software project. It is distributed under the
# MIT License. See LICENSE file for details.
#==============================================================================#
# Targets:
#   lib - the Yang static library
#   tools - various Yang tools
#   docs - generate documentation
#   test - build and run unit tests
#   clean - delete all outputs
#   clean_all - delete all outputs and clean dependencies
# Pass DBG=1 to make debug binaries.
#
# Utilities:
#   todo - print todo lines in all code files
#   add - run git add on all code files
#   wc - print line counts of all code files
#
# External package dependencies:
#   make m4 texinfo texlive python libtinfo
.PHONY: first
first: all
include dependencies/Makefile

# Debug options.
ifeq ($(DBG), 1)
OUTDIR=./Debug
WFLAGS=-Werror -Wall -Wextra -Wpedantic
CFLAGS_CONFIG=-Og -g -ggdb -DDEBUG
LLVM_LIB_DIR=$(LLVM_DIR)/Debug+Asserts/lib
LLVM_BUILD=$(DEPENDENCIES)/llvm_debug.build
else
OUTDIR=./Release
CFLAGS_CONFIG=-O3
LLVM_LIB_DIR=$(LLVM_DIR)/Release/lib
LLVM_BUILD=$(DEPENDENCIES)/llvm_release.build
endif
CC_OBJECT_FILE_PREREQS=$(LLVM_BUILD)

# Output directories.
GENDIR=./gen
OUTDIR_BIN=$(OUTDIR)/bin
OUTDIR_LIB=$(OUTDIR)/lib
OUTDIR_TMP=$(OUTDIR)/build

INCLUDE=./include
SOURCE=./src
TESTS=./tests
DOCS=./docs
DOCGEN=$(DOCS)/source/api
LIB=$(OUTDIR_LIB)/libyang.a
YANGC_BINARY=$(OUTDIR_BIN)/tools/yangc
TESTS_BINARY=$(OUTDIR_BIN)/tests/tests
BINARIES=$(YANGC_BINARY)

# Compilers and interpreters.
export FLEX=$(FLEX_DIR)/flex
export YACC=$(BYACC_DIR)/yacc
export PYTHON=python

CFLAGS_11=-std=c++11
CFLAGS=\
  $(CFLAGS_EXTRA) $(CFLAGS_11) $(CFLAGS_CONFIG) -I$(INCLUDE) \
  -isystem $(LLVM_DIR)/include -isystem $(GTEST_DIR)/include
LFLAGS=\
  $(LFLAGS_EXTRA) -L$(OUTDIR_LIB) \
  -Wl,-Bstatic -lyang \
  -Wl,-Bdynamic -lz -ltinfo -lpthread -ldl

# File listings.
L_FILES=$(wildcard $(SOURCE)/*.l)
Y_FILES=$(wildcard $(SOURCE)/*.y)

L_OUTPUTS=$(subst $(SOURCE)/,$(GENDIR)/,$(L_FILES:.l=.l.cc))
Y_OUTPUTS=$(subst $(SOURCE)/,$(GENDIR)/,$(Y_FILES:.y=.y.cc))
CC_GENERATED_FILES=$(L_OUTPUTS) $(Y_OUTPUTS)

H_FILES=\
  $(wildcard $(SOURCE)/*.h) \
  $(wildcard $(INCLUDE)/yang/*.h) $(wildcard $(TESTS)/*.h)
TOOL_CC_FILES=$(wildcard $(SOURCE)/tools/*.cpp)
TEST_CC_FILES=$(wildcard $(TESTS)/*.cpp)
TEST_OBJECT_FILES=$(addprefix $(OUTDIR_TMP)/,$(TEST_CC_FILES:.cpp=.cpp.o))
CC_SOURCE_FILES=$(wildcard $(SOURCE)/*.cpp) $(TOOL_CC_FILES) $(TEST_CC_FILES)

SOURCE_OBJECT_FILES=$(addprefix $(OUTDIR_TMP)/,$(addsuffix .o,$(wildcard $(SOURCE)/*.cpp) $(CC_GENERATED_FILES)))
INCLUDE_FILES=$(wildcard $(INCLUDE)/*/*.h)
AUTODOC_FILES=\
  $(subst $(INCLUDE)/yang/,$(DOCGEN)/,$(INCLUDE_FILES:.h=.rst))

AUTODOC=$(DOCS)/source/autodoc.py
DOC_FILES=\
  $(DOCS)/source/conf.py \
  $(wildcard $(DOCS)/source/*.rst) \
  $(wildcard $(DOCS)/source/yang/*.*) \
  $(wildcard $(DOCS)/source/yang/*/*)
MISC_FILES=\
  $(AUTODOC) Makefile README.md LICENSE .gitignore dependencies/Makefile
ALL_FILES=\
  $(CC_FILES) $(TOOL_CC_FILES) $(TEST_CC_FILES) \
  $(H_FILES) $(L_FILES) $(Y_FILES) $(MISC_FILES) $(DOC_FILES)

DISABLE_CC_DEPENDENCY_ANALYSIS=true
ifneq ('$(MAKECMDGOALS)', 'docs')
ifneq ('$(MAKECMDGOALS)', 'add')
ifneq ('$(MAKECMDGOALS)', 'todo')
ifneq ('$(MAKECMDGOALS)', 'wc')
ifneq ('$(MAKECMDGOALS)', 'clean')
ifneq ('$(MAKECMDGOALS)', 'clean_all')
DISABLE_CC_DEPENDENCY_ANALYSIS=false
endif
endif
endif
endif
endif
endif
include dependencies/makelib/Makefile

# Master targets.
.PHONY: all
all: lib tools docs .tests_passed
.PHONY: lib
lib: $(LIB)
.PHONY: tools
tools: $(BINARIES)
.PHONY: docs
docs: $(DOCS)/html/index.html
.PHONY: test
test: \
  $(TESTS_BINARY)
	$(TESTS_BINARY)
	touch .tests_passed
.PHONY: clean
clean:
	rm -rf $(OUTDIR) $(GENDIR)
	rm -rf $(LLVM_LIB_DIR)/*.o
	rm -rf $(DOCGEN) $(DOCS)/html
.PHONY: clean_all
clean_all: clean clean_dependencies

# LLVM libraries which we require, and append to the yang library, so that
# users only need to link one library.
LLVM_LIBS=\
  LLVMipo LLVMX86CodeGen LLVMSelectionDAG LLVMX86Desc LLVMX86Info \
  LLVMX86AsmPrinter LLVMX86Utils LLVMJIT LLVMCodeGen LLVMScalarOpts \
  LLVMInstCombine LLVMTransformUtils LLVMipa LLVMAnalysis LLVMTarget \
  LLVMObject LLVMMCParser LLVMBitReader LLVMExecutionEngine LLVMMC \
  LLVMCore LLVMSupport

# Library archiving. For speed, don't rearchive LLVM libraries.
$(LIB): \
  $(LLVM_BUILD) $(OUTDIR_LIB)/.mkdir $(SOURCE_OBJECT_FILES)
	@echo Archiving ./$@
	[ -f ./$@ ]; \
	EXIST=$$?; \
	ar -crsv ./$@ $(SOURCE_OBJECT_FILES); \
	if [ $$EXIST -ne 0 ]; then \
	  cd $(LLVM_LIB_DIR); \
	  for lib in $(LLVM_LIBS); do \
	    ar -x lib$$lib.a && ar -qv ../../../../$@ *.o; \
	    rm *.o; \
	  done; \
	fi

# Tool binary linking.
$(BINARIES): $(OUTDIR_BIN)/%: \
  $(OUTDIR_TMP)/$(SOURCE)/%.cpp.o $(OUTDIR_BIN)/%.mkdir $(LIB)
	@echo Linking ./$@
	$(CXX) -o ./$@ $< $(LFLAGS)

# Flex/YACC files.
$(GENDIR)/%.l.h: \
  $(GENDIR)/%.l.cc
	touch $@ $<
$(GENDIR)/%.l.cc: \
  $(SOURCE)/%.l $(GENDIR)/%.mkdir $(DEPENDENCIES)/flex.build
	@echo Compiling ./$<
	$(FLEX) -P yang_ -o $@ --header-file=$(@:.cc=.h) $<
$(GENDIR)/%.y.h: \
  $(GENDIR)/%.y.cc
	touch $@ $<
$(GENDIR)/%.y.cc: \
  $(SOURCE)/%.y $(GENDIR)/%.mkdir $(DEPENDENCIES)/byacc.build
	@echo Compiling ./$<
	$(YACC) -p yang_ -d -v -o $@ $<

# Test binary.
$(TESTS_BINARY): $(OUTDIR_BIN)/%: \
  $(TEST_OBJECT_FILES) $(OUTDIR_BIN)/%.mkdir $(DEPENDENCIES)/gtest.build $(LIB)
	@echo Linking ./$@
	$(CXX) -o ./$@ $(TEST_OBJECT_FILES) $(LFLAGS) \
	    -L$(GTEST_DIR)/lib -Wl,-Bstatic -lgtest -Wl,-Bdynamic -lpthread
.tests_passed: \
	$(TESTS_BINARY)
	$(TESTS_BINARY)
	touch ./$@

# Documentation generation.
$(DOCGEN)/%.rst: \
  $(INCLUDE)/yang/%.h $(AUTODOC) $(DOCGEN)/.mkdir
	$(PYTHON) $(AUTODOC) $< $@

# Documentation.
SPHINX_BUILD=\
  PYTHONPATH=$${PWD}/$(DEPENDENCIES)/$(PYTHON_INSTALL_DIR) \
  $(DEPENDENCIES)/install/bin/sphinx-build
SPHINX_BUILD_OPTS=\
  -a -E $(DOCS)/source
$(DOCS)/html/index.html: \
  $(DEPENDENCIES)/sphinx.build $(DOC_FILES) $(AUTODOC_FILES) \
  $(DEPENDENCIES)/pygments.build
	$(SPHINX_BUILD) -b html $(SPHINX_BUILD_OPTS) $(DOCS)/html
