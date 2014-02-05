#==============================================================================#
# This file is part of the Yang software project. It is distributed under the
# MIT License. See LICENSE file for details.
#==============================================================================#
# Targets:
#   yang - the Yang standalone compiler/checker
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

# Final outputs.
ifeq ($(DBG), 1)
OUTDIR=./dbg
else
OUTDIR=./bin
endif
GEN=./gen
SOURCE=./src
YANG_BINARY=$(OUTDIR)/yang
BINARIES=$(YANG_BINARY)

# Dependency directories.
DEPEND_DIR=./dependencies
BYACC_DIR=$(DEPEND_DIR)/byacc
FLEX_DIR=$(DEPEND_DIR)/flex
LLVM_DIR=$(DEPEND_DIR)/llvm

# Compilers and interpreters.
export SHELL=/bin/sh
export FLEX=$(FLEX_DIR)/flex
export YACC=$(BYACC_DIR)/yacc

# TODO: put LLVM into the top-level directory (also, without asserts).
DEPENDENCY_DIRS=$(LLVM_DIR)/Release+Asserts $(LLVM_DIR)
DEPENDENCY_CFLAGS=\
	$(addprefix -isystem ,$(addsuffix /include,$(DEPENDENCY_DIRS)))
DEPENDENCY_LFLAGS=$(addsuffix /lib,$(addprefix -L,$(DEPENDENCY_DIRS)))

# Compiler flags.
C11FLAGS=-std=c++11
CFLAGS=$(C11FLAGS) $(DEPENDENCY_CFLAGS)
LFLAGS=\
	$(DEPENDENCY_LFLAGS) -Wl,-Bstatic \
	$(shell $(LLVM_DIR)/Release+Asserts/bin/llvm-config --libs) \
	-Wl,-Bdynamic	-lpthread -ldl
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

H_FILES=$(wildcard $(SOURCE)/*.h)
CPP_FILES=$(wildcard $(SOURCE)/*.cpp)
SOURCE_FILES=$(CPP_FILES) $(L_OUTPUTS) $(Y_OUTPUTS)
DEP_FILES=$(addprefix $(OUTDIR)/,$(addsuffix .deps,$(SOURCE_FILES)))
OBJECT_FILES=$(addprefix $(OUTDIR)/,$(addsuffix .o,$(SOURCE_FILES)))

MISC_FILES=Makefile Makedeps README.md LICENSE .gitignore
ALL_FILES=$(CPP_FILES) $(H_FILES) $(L_FILES) $(Y_FILES) $(MISC_FILES)

# Master targets.
.PHONY: all
all: \
	$(BINARIES)
.PHONY: yang
yang: \
	$(YANG_BINARY)
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
	rm -rf $(OUTDIR)
	rm -rf $(GEN)

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
	./% $(H_FILES) $(OUTDIR)/%.mkdir
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

# Binary linking.
$(BINARIES): $(OUTDIR)/%: \
	$(DEPEND_DIR)/.build $(OUTDIR)/%.mkdir \
	$$(__src$$(subst /,_,$$(subst $$(OUTDIR),,./$$@))_cpp_LINK)
	@echo Linking ./$@
	$(CXX) -o ./$@ $(filter-out %.build,$(filter-out %.mkdir,$^)) $(LFLAGS)

# Object files. References dependencies that must be built before their header
# files are available.
$(OUTDIR)/%.o: \
	$(OUTDIR)/%.build $(OUTDIR)/%.mkdir $(DEPEND_DIR)/llvm.build
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

# Ensure a directory exists.
.PRECIOUS: ./%.mkdir
./%.mkdir:
	mkdir -p $(dir $@)
	touch $@

# Makefile for dependencies below here.

# Dependencies.
$(DEPEND_DIR)/.build: \
	$(DEPEND_DIR)/llvm.build
	touch $(DEPEND_DIR)/.build

# Build Flex.
$(DEPEND_DIR)/flex.build:
	@echo Building Flex
	cd $(FLEX_DIR) && ./configure
	cd $(FLEX_DIR) && $(MAKE)
	touch $(DEPEND_DIR)/flex.build

# Build BYACC.
$(DEPEND_DIR)/byacc.build:
	@echo Building BYACC
	cd $(BYACC_DIR) && ./configure
	cd $(BYACC_DIR) && $(MAKE)
	touch $(DEPEND_DIR)/byacc.build

# Build LLVM.
$(DEPEND_DIR)/llvm.build:
	@echo Building LLVM
	cd $(LLVM_DIR) && ./configure
	cd $(LLVM_DIR) && $(MAKE)
	touch $(DEPEND_DIR)/llvm.build

# Clean dependencies.
.PHONY: clean_all
clean_all: \
	clean
	rm -f $(DEPEND_DIR)/*.build $(DEPEND_DIR)/.build
	-cd $(BYACC_DIR) && [ -f ./Makefile ] && $(MAKE) clean
	-cd $(FLEX_DIR) && [ -f ./Makefile ] && $(MAKE) clean
	-cd $(LLVM_DIR) && $(MAKE) clean
