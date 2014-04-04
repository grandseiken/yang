#==============================================================================#
# This file is part of the Yang software project. It is distributed under the
# MIT License. See LICENSE file for details.
#==============================================================================#
# Targets:
#   lib - the Yang static library
#   yang - the Yang standalone compiler/checker
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
#   make m4 texinfo texlive
.SUFFIXES:

# Dependency directories.
DEPEND_DIR=./dependencies
BYACC_DIR=$(DEPEND_DIR)/byacc
FLEX_DIR=$(DEPEND_DIR)/flex
GTEST_DIR=$(DEPEND_DIR)/googletest
LLVM_DIR=$(DEPEND_DIR)/llvm

# Final outputs.
ifeq ($(DBG), 1)
OUTDIR=./dbg/bin
LIBDIR=./dbg/lib
LLVM_LIB_DIR=$(LLVM_DIR)/Debug+Asserts/lib
LLVM_BUILD=$(DEPEND_DIR)/llvm_dbg.build
else
OUTDIR=./bin
LIBDIR=./lib
LLVM_LIB_DIR=$(LLVM_DIR)/Release/lib
LLVM_BUILD=$(DEPEND_DIR)/llvm.build
endif

INCLUDE=./include
SOURCE=./src
TESTS=./tests
GEN=./gen
LIB=$(LIBDIR)/libyang.a
YANG_BINARY=$(OUTDIR)/tools/yang
TEST_BINARY=$(OUTDIR)/tests/tests
BINARIES=$(YANG_BINARY)

# Compilers and interpreters.
export SHELL=/bin/sh
export FLEX=$(FLEX_DIR)/flex
export YACC=$(BYACC_DIR)/yacc

C11FLAGS=-std=c++11
CFLAGS=\
	$(CFLAGSEXTRA) $(C11FLAGS) -I$(INCLUDE) \
	-isystem $(LLVM_DIR)/include -isystem $(GTEST_DIR)/include
LFLAGS=\
	$(LFLAGSEXTRA) -L$(LIBDIR) -Wl,-Bstatic -lyang -Wl,-Bdynamic -lpthread -ldl
ifeq ($(DBG), 1)
CFLAGS+=-Og -g -ggdb -DDEBUG
WFLAGS=-Werror -Wall -Wextra -Wpedantic
else
CFLAGS+=-O3
endif

# File listings.
L_FILES=$(wildcard $(SOURCE)/*.l)
Y_FILES=$(wildcard $(SOURCE)/*.y)

L_OUTPUTS=$(subst $(SOURCE)/,$(GEN)/,$(L_FILES:.l=.l.cc))
Y_OUTPUTS=$(subst $(SOURCE)/,$(GEN)/,$(Y_FILES:.y=.y.cc))

H_FILES=\
	$(wildcard $(SOURCE)/*.h) \
	$(wildcard $(INCLUDE)/*/*.h) $(wildcard $(TESTS)/*.h)
CPP_FILES=$(wildcard $(SOURCE)/*.cpp)
SOURCE_FILES=$(CPP_FILES) $(L_OUTPUTS) $(Y_OUTPUTS)
OBJECT_FILES=$(addprefix $(OUTDIR)/,$(addsuffix .o,$(SOURCE_FILES)))

TOOL_CPP_FILES=$(wildcard $(SOURCE)/tools/*.cpp)
TEST_CPP_FILES=$(wildcard $(TESTS)/*.cpp)
TEST_OBJECT_FILES=$(addprefix $(OUTDIR)/,$(TEST_CPP_FILES:.cpp=.cpp.o))

DEP_FILES=\
	$(addprefix $(OUTDIR)/,$(addsuffix .deps,\
	$(SOURCE_FILES) $(TOOL_CPP_FILES) $(TEST_CPP_FILES)))

MISC_FILES=Makefile Makedeps README.md LICENSE .gitignore
ALL_FILES=\
	$(CPP_FILES) $(TOOL_CPP_FILES) $(TEST_CPP_FILES) \
	$(H_FILES) $(L_FILES) $(Y_FILES) $(MISC_FILES)

# Master targets.
.PHONY: all
all: \
	lib $(BINARIES) .tests_passed
.PHONY: lib
lib: \
	$(LIB)
.PHONY: yang
yang: \
	$(YANG_BINARY)
.PHONY: test
test: \
	$(TEST_BINARY)
	$(TEST_BINARY)
	touch .tests_passed
.PHONY: add
add:
	git add $(ALL_FILES)
.PHONY: todo
todo:
	@grep --color -n "T[O]D[O]" $(ALL_FILES)
.PHONY: wc
wc:
	wc $(ALL_FILES)
.PHONY: clean
clean:
	rm -rf $(LIBDIR)
	rm -rf $(OUTDIR)
	rm -rf $(GEN)
	rm -rf $(LLVM_LIB_DIR)/*.o

# Dependency generation. Each source file generates a corresponding .deps file
# (a Makefile containing a .build target), which is then included. Inclusion
# forces regeneration via the rules provided. Deps rule depends on same .build
# target it generates. When the specific .build target doesn't exist, the
# default causes everything to be generated.
.SECONDEXPANSION:
$(DEP_FILES): $(OUTDIR)/%.deps: \
	$(OUTDIR)/%.build $(OUTDIR)/%.mkdir ./Makedeps $(Y_OUTPUTS) $(L_OUTPUTS) \
	$$(subst \
	$$(OUTDIR)/,,$$($$(subst .,_,$$(subst /,_,$$(subst \
	$$(OUTDIR)/,,./$$(@:.deps=))))_LINK:.o=))
	SOURCE_FILE=$(subst $(OUTDIR)/,,./$(@:.deps=)); \
	    echo Generating dependencies for $$SOURCE_FILE; \
	    echo $(addsuffix .cpp,$(subst $(OUTDIR),$(SOURCE),$(BINARIES))) | \
	        fgrep $$SOURCE_FILE > /dev/null; \
	    ./Makedeps $@ $< $$SOURCE_FILE $(OUTDIR) $$?
.PRECIOUS: $(OUTDIR)/%.build
$(OUTDIR)/%.build: \
	./% $(OUTDIR)/%.mkdir
	touch $@

ifneq ('$(MAKECMDGOALS)', 'add')
ifneq ('$(MAKECMDGOALS)', 'todo')
ifneq ('$(MAKECMDGOALS)', 'wc')
ifneq ('$(MAKECMDGOALS)', 'clean')
ifneq ('$(MAKECMDGOALS)', 'clean_all')
-include $(DEP_FILES)
endif
endif
endif
endif
endif

# LLVM libraries which we require, and append to the yang library, so that
# users only need to link one library.
LLVM_LIBS=\
	LLVMipo LLVMX86CodeGen LLVMSelectionDAG LLVMX86Desc LLVMX86Info \
	LLVMX86AsmPrinter LLVMX86Utils LLVMJIT LLVMCodeGen LLVMScalarOpts \
	LLVMInstCombine LLVMTransformUtils LLVMipa LLVMAnalysis \
	LLVMRuntimeDyld LLVMExecutionEngine LLVMTarget LLVMMC LLVMObject \
	LLVMCore LLVMSupport

# Library archiving. For speed, don't rearchive LLVM libraries.
$(LIB): \
	$(LLVM_BUILD) $(LIBDIR)/.mkdir $(OBJECT_FILES)
	@echo Archiving ./$@
	[ -f ./$@ ]; \
	EXIST=$$?; \
	ar -crsv ./$@ $(filter-out %.build,$(filter-out %.mkdir,$^)); \
	if [ $$EXIST -ne 0 ]; then \
	  cd $(LLVM_LIB_DIR); \
	  for lib in $(LLVM_LIBS); do \
	    ar -x lib$$lib.a && ar -qv ../../../../$@ *.o; \
	    rm *.o; \
	  done; \
	fi

# Tool binary linking.
$(BINARIES): $(OUTDIR)/%: \
	$(OUTDIR)/%.mkdir $(LIB) \
	$$(__src$$(subst /,_,$$(subst $$(OUTDIR),,./$$@))_cpp_LINK)
	@echo Linking ./$@
	$(CXX) -o ./$@ $(filter-out %.a,$(filter-out %.mkdir,$^)) $(LFLAGS)

# Object files. References dependencies that must be built before their header
# files are available.
$(OUTDIR)/%.o: \
	$(OUTDIR)/%.build $(OUTDIR)/%.mkdir $(LLVM_BUILD)
	SOURCE_FILE=$(subst $(OUTDIR)/,,./$(<:.build=)); \
	    echo Compiling $$SOURCE_FILE; \
	    $(CXX) -c $(CFLAGS) $(if $(findstring /./gen/,$@),,$(WFLAGS)) \
	    -o $@ $$SOURCE_FILE

# Flex/YACC files.
.PRECIOUS: $(L_OUTPUTS) $(Y_OUTPUTS)
$(GEN)/%.l.h: \
	$(GEN)/%.l.cc
	touch $@ $<
$(GEN)/%.l.cc: \
	$(SOURCE)/%.l $(GEN)/%.mkdir $(DEPEND_DIR)/flex.build
	@echo Compiling ./$<
	$(FLEX) -P yang_ -o $@ --header-file=$(@:.cc=.h) $<
$(GEN)/%.y.h: \
	$(GEN)/%.y.cc
	touch $@ $<
$(GEN)/%.y.cc: \
	$(SOURCE)/%.y $(GEN)/%.mkdir $(DEPEND_DIR)/byacc.build
	@echo Compiling ./$<
	$(YACC) -p yang_ -d -v -o $@ $<

# Test binary.	
$(TEST_BINARY): \
	$(TEST_OBJECT_FILES) $(LIB) $(DEPEND_DIR)/gtest.build
	@echo Linking./$@
	$(CXX) -o ./$@ $(TEST_OBJECT_FILES) $(LFLAGS) \
	    -L$(GTEST_DIR)/lib -Wl,-Bstatic -lgtest -Wl,-Bdynamic -lpthread
.tests_passed: \
	$(TEST_BINARY)
	$(TEST_BINARY)
	touch ./$@

# Ensure a directory exists.
.PRECIOUS: ./%.mkdir
./%.mkdir:
	mkdir -p $(dir $@)
	touch $@

# Makefile for dependencies below here.

# Build Flex.
$(DEPEND_DIR)/flex.build:
	@echo Building Flex
	cd $(FLEX_DIR) && ./configure
	cd $(FLEX_DIR) && $(MAKE)
	touch $@

# Build BYACC.
$(DEPEND_DIR)/byacc.build:
	@echo Building BYACC
	cd $(BYACC_DIR) && ./configure
	cd $(BYACC_DIR) && $(MAKE)
	touch $@

# Build Google Test.
$(DEPEND_DIR)/gtest.build:
	@echo Building Google Test
	cd $(GTEST_DIR) && $(CXX) -isystem include -I. -pthread -c src/gtest-all.cc
	cd $(GTEST_DIR) && mkdir lib && ar -crsv lib/libgtest.a gtest-all.o
	touch $@

# Build LLVM.
LLVM_OPTS=\
  --enable-jit --disable-docs --enable-targets=host \
  --disable-assertions --enable-optimized
$(DEPEND_DIR)/llvm.build:
	@echo Building LLVM
	cd $(LLVM_DIR) && ./configure $(LLVM_OPTS)
	cd $(LLVM_DIR) && $(MAKE)
	touch $@
# Build LLVM in debug mode.
LLVM_DBG_OPTS=\
  --enable-jit --enable-targets=host --disable-docs \
  --enable-assertions --disable-optimized
$(DEPEND_DIR)/llvm_dbg.build:
	@echo Building LLVM
	cd $(LLVM_DIR) && ./configure $(LLVM_DBG_OPTS)
	cd $(LLVM_DIR) && $(MAKE)
	touch $@

# Clean dependencies.
.PHONY: clean_all
clean_all: \
	clean
	rm -f $(DEPEND_DIR)/*.build $(DEPEND_DIR)/.build
	-cd $(BYACC_DIR) && [ -f ./Makefile ] && $(MAKE) clean
	-cd $(FLEX_DIR) && [ -f ./Makefile ] && $(MAKE) clean
	cd $(GTEST_DIR) && rm -rf lib *.o
	-cd $(LLVM_DIR) && $(MAKE) clean
