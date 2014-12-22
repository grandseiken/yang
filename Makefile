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
#   make m4 texinfo texlive python
.SUFFIXES:

# Dependency directories.
DEPEND_DIR=./dependencies
BYACC_DIR=$(DEPEND_DIR)/byacc
FLEX_DIR=$(DEPEND_DIR)/flex
GTEST_DIR=$(DEPEND_DIR)/googletest
LLVM_DIR=$(DEPEND_DIR)/llvm
PYGMENTS_DIR=$(DEPEND_DIR)/pygments
SPHINX_DIR=$(DEPEND_DIR)/sphinx
PYTHON_INSTALL_DIR=install/lib/python

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
DOCS=./docs
DOCGEN=$(DOCS)/source/api
LIB=$(LIBDIR)/libyang.a
YANGC_BINARY=$(OUTDIR)/tools/yangc
TESTS_BINARY=$(OUTDIR)/tests/tests
BINARIES=$(YANGC_BINARY)

# Compilers and interpreters.
export SHELL=/bin/sh
export FLEX=$(FLEX_DIR)/flex
export YACC=$(BYACC_DIR)/yacc
export PYTHON=python

C11FLAGS=-std=c++11
CFLAGS=\
	$(CFLAGSEXTRA) $(C11FLAGS) -I$(INCLUDE) \
	-isystem $(LLVM_DIR)/include -isystem $(GTEST_DIR)/include
LFLAGS=\
	$(LFLAGSEXTRA) -L$(LIBDIR) \
	-Wl,-Bstatic -lyang \
	-Wl,-Bdynamic -lz -ltinfo -lpthread -ldl
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
	$(wildcard $(INCLUDE)/yang/*.h) $(wildcard $(TESTS)/*.h)
CPP_FILES=$(wildcard $(SOURCE)/*.cpp)
SOURCE_FILES=$(CPP_FILES) $(L_OUTPUTS) $(Y_OUTPUTS)
OBJECT_FILES=$(addprefix $(OUTDIR)/,$(addsuffix .o,$(SOURCE_FILES)))
INCLUDE_FILES=$(wildcard $(INCLUDE)/*/*.h)
AUTODOC_FILES=\
	$(subst $(INCLUDE)/yang/,$(DOCGEN)/,$(INCLUDE_FILES:.h=.rst))

TOOL_CPP_FILES=$(wildcard $(SOURCE)/tools/*.cpp)
TEST_CPP_FILES=$(wildcard $(TESTS)/*.cpp)
TEST_OBJECT_FILES=$(addprefix $(OUTDIR)/,$(TEST_CPP_FILES:.cpp=.cpp.o))

DEP_FILES=\
	$(addprefix $(OUTDIR)/,$(addsuffix .deps,\
	$(SOURCE_FILES) $(TOOL_CPP_FILES) $(TEST_CPP_FILES)))

AUTODOC=$(DOCS)/source/autodoc.py
DOC_FILES=\
	$(DOCS)/source/conf.py \
	$(wildcard $(DOCS)/source/*.rst) \
	$(wildcard $(DOCS)/source/yang/*.*) \
	$(wildcard $(DOCS)/source/yang/*/*)
MISC_FILES=\
	$(AUTODOC) Makefile README.md LICENSE .gitignore
ALL_FILES=\
	$(CPP_FILES) $(TOOL_CPP_FILES) $(TEST_CPP_FILES) \
	$(H_FILES) $(L_FILES) $(Y_FILES) $(MISC_FILES) $(DOC_FILES)

# Master targets.
.PHONY: all
all: \
	lib tools docs .tests_passed
.PHONY: lib
lib: \
	$(LIB)
.PHONY: tools
tools: \
	$(BINARIES)
.PHONY: docs
docs: \
	$(DOCS)/html/index.html
.PHONY: test
test: \
	$(TESTS_BINARY)
	$(TESTS_BINARY)
	touch .tests_passed
.PHONY: add
add:
	git add $(ALL_FILES)
.PHONY: todo
todo:
	@grep --color -n "\bT[O]D[O]\b" $(ALL_FILES)
.PHONY: wc
wc:
	wc $(ALL_FILES)
.PHONY: clean
clean:
	rm -rf $(LIBDIR)
	rm -rf $(OUTDIR)
	rm -rf $(GEN)
	rm -rf $(LLVM_LIB_DIR)/*.o
	rm -rf $(DOCGEN) $(DOCS)/html

# Dependency generation. Each source file generates a corresponding .deps file
# (a Makefile containing a .build target), which is then included. Inclusion
# forces regeneration via the rules provided. Deps rule depends on same .build
# target it generates. When the specific .build target doesn't exist, the
# default causes everything to be generated.
.SECONDEXPANSION:
$(DEP_FILES): $(OUTDIR)/%.deps: \
	$(OUTDIR)/%.build $(OUTDIR)/%.mkdir $(Y_OUTPUTS) $(L_OUTPUTS) \
	$$(subst \
	$$(OUTDIR)/,,$$($$(subst .,_,$$(subst /,_,$$(subst \
	$$(OUTDIR)/,,./$$(@:.deps=))))_LINK:.o=))
	SOURCE_FILE=$(subst $(OUTDIR)/,,./$(@:.deps=)); \
	    echo Generating dependencies for $$SOURCE_FILE; \
	    $(CXX) $(CFLAGS) -MM $$SOURCE_FILE | \
	        sed -e 's/.*\.o:/$(subst /,\/,$<)::/g' > $@; \
	    echo "	@touch" $< >> $@
.PRECIOUS: $(OUTDIR)/%.build
$(OUTDIR)/%.build: \
	./% $(OUTDIR)/%.mkdir
	touch $@

ifneq ('$(MAKECMDGOALS)', 'docs')
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
endif

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
	$(OUTDIR)/$(SOURCE)/%.cpp.o $(OUTDIR)/%.mkdir $(LIB)
	@echo Linking ./$@
	$(CXX) -o ./$@ $< $(LFLAGS)

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
$(TESTS_BINARY): \
	$(TEST_OBJECT_FILES) $(LIB) $(DEPEND_DIR)/gtest.build
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
	PYTHONPATH=$${PWD}/$(DEPEND_DIR)/$(PYTHON_INSTALL_DIR) \
	$(DEPEND_DIR)/install/bin/sphinx-build
SPHINX_BUILD_OPTS=\
	-a -E $(DOCS)/source
$(DOCS)/html/index.html: \
	$(DEPEND_DIR)/sphinx.build $(DOC_FILES) $(AUTODOC_FILES) \
	$(DEPEND_DIR)/pygments.build
	$(SPHINX_BUILD) -b html $(SPHINX_BUILD_OPTS) $(DOCS)/html

# Ensure a directory exists.
.PRECIOUS: ./%.mkdir
./%.mkdir:
	mkdir -p $(dir $@)
	touch $@

# Makefile for dependencies below here.

# Build BYACC.
$(DEPEND_DIR)/byacc.build:
	@echo Building BYACC
	cd $(BYACC_DIR) && ./configure
	cd $(BYACC_DIR) && $(MAKE)
	touch $@

# Build Flex.
$(DEPEND_DIR)/flex.build:
	@echo Building Flex
	cd $(FLEX_DIR) && ./configure
	cd $(FLEX_DIR) && $(MAKE)
	touch $@

# Build Google Test.
$(DEPEND_DIR)/gtest.build:
	@echo Building Google Test
	cd $(GTEST_DIR) && $(CXX) -isystem include -I. -pthread -c src/gtest-all.cc
	cd $(GTEST_DIR) && mkdir lib && ar -crsv lib/libgtest.a gtest-all.o
	touch $@

# Build LLVM.
LLVM_COMMON_OPTS=\
  --enable-jit --disable-docs --enable-targets=host
LLVM_OPTS=\
  $(LLVM_COMMON_OPTS) --disable-assertions --enable-optimized
$(DEPEND_DIR)/llvm.build:
	@echo Building LLVM
	cd $(LLVM_DIR) && ./configure $(LLVM_OPTS)
	cd $(LLVM_DIR) && $(MAKE)
	touch $@
# Build LLVM in debug mode.
LLVM_DBG_OPTS=\
  $(LLVM_COMMON_OPTS) --enable-assertions --disable-optimized
$(DEPEND_DIR)/llvm_dbg.build:
	@echo Building LLVM
	cd $(LLVM_DIR) && ./configure $(LLVM_DBG_OPTS)
	cd $(LLVM_DIR) && $(MAKE)
	touch $@

# Build Pygments.
$(DEPEND_DIR)/pygments.build: \
	$(DEPEND_DIR)/$(PYTHON_INSTALL_DIR)/.mkdir
	@echo Building Pygments
	cd $(PYGMENTS_DIR) && $(PYTHON) setup.py build
	cd $(PYGMENTS_DIR) && \
	    PYTHONPATH=$${PWD}/../$(PYTHON_INSTALL_DIR) \
	    $(PYTHON) setup.py install --home $${PWD}/../install
	touch $@

# Build Sphinx. Must depend on pygments, otherwise sphinx will install the
# wrong version.
$(DEPEND_DIR)/sphinx.build: \
	$(DEPEND_DIR)/$(PYTHON_INSTALL_DIR)/.mkdir $(DEPEND_DIR)/pygments.build
	@echo Building Sphinx
	cd $(SPHINX_DIR) && $(PYTHON) setup.py build
	cd $(SPHINX_DIR) && \
	    PYTHONPATH=$${PWD}/../$(PYTHON_INSTALL_DIR) \
	    $(PYTHON) setup.py install --home $${PWD}/../install
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
	cd $(PYGMENTS_DIR) && rm -rf build
	cd $(SPHINX_DIR) && rm -rf build
	cd $(DEPEND_DIR) && rm -rf install
