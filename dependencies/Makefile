#==============================================================================#
# This file is part of the Yang software project. It is distributed under the
# MIT License. See LICENSE file for details.
#==============================================================================#

# Dependency directories.
DEPENDENCIES=./dependencies
BYACC_DIR=$(DEPENDENCIES)/byacc
FLEX_DIR=$(DEPENDENCIES)/flex
GTEST_DIR=$(DEPENDENCIES)/googletest
LLVM_DIR=$(DEPENDENCIES)/llvm
PYGMENTS_DIR=$(DEPENDENCIES)/pygments
SPHINX_DIR=$(DEPENDENCIES)/sphinx
PYTHON_INSTALL_DIR=install/lib/python

# Build BYACC.
$(DEPENDENCIES)/byacc.build:
	@echo Building BYACC
	cd $(BYACC_DIR) && ./configure
	cd $(BYACC_DIR) && $(MAKE)
	touch $@

# Build Flex.
$(DEPENDENCIES)/flex.build:
	@echo Building Flex
	cd $(FLEX_DIR) && ./configure
	cd $(FLEX_DIR) && $(MAKE)
	touch $@

# Build Google Test.
$(DEPENDENCIES)/gtest.build:
	@echo Building Google Test
	cd $(GTEST_DIR) && $(CXX) -isystem include -I. -pthread -c src/gtest-all.cc
	cd $(GTEST_DIR) && mkdir lib && ar -crsv lib/libgtest.a gtest-all.o
	touch $@

# Build LLVM.
LLVM_COMMON_OPTS=\
  --enable-jit --disable-docs --enable-targets=host
LLVM_RELEASE_OPTS=\
  $(LLVM_COMMON_OPTS) --disable-assertions --enable-optimized
$(DEPENDENCIES)/llvm_release.build:
	@echo Building LLVM
	cd $(LLVM_DIR) && ./configure $(LLVM_RELEASE_OPTS)
	cd $(LLVM_DIR) && $(MAKE)
	touch $@
# Build LLVM in debug mode.
LLVM_DEBUG_OPTS=\
  $(LLVM_COMMON_OPTS) --enable-assertions --disable-optimized
$(DEPENDENCIES)/llvm_debug.build:
	@echo Building LLVM
	cd $(LLVM_DIR) && ./configure $(LLVM_DEBUG_OPTS)
	cd $(LLVM_DIR) && $(MAKE)
	touch $@

# Build Pygments.
$(DEPENDENCIES)/pygments.build: \
	$(DEPENDENCIES)/$(PYTHON_INSTALL_DIR)/.mkdir
	@echo Building Pygments
	cd $(PYGMENTS_DIR) && $(PYTHON) setup.py build
	cd $(PYGMENTS_DIR) && \
	    PYTHONPATH=$${PWD}/../$(PYTHON_INSTALL_DIR) \
	    $(PYTHON) setup.py install --home $${PWD}/../install
	touch $@

# Build Sphinx. Must depend on pygments, otherwise sphinx will install the
# wrong version.
$(DEPENDENCIES)/sphinx.build: \
	$(DEPENDENCIES)/$(PYTHON_INSTALL_DIR)/.mkdir $(DEPENDENCIES)/pygments.build
	@echo Building Sphinx
	cd $(SPHINX_DIR) && $(PYTHON) setup.py build
	cd $(SPHINX_DIR) && \
	    PYTHONPATH=$${PWD}/../$(PYTHON_INSTALL_DIR) \
	    $(PYTHON) setup.py install --home $${PWD}/../install
	touch $@

.PHONY: clean_dependencies
clean_dependencies:
	rm -f $(DEPENDENCIES)/*.build $(DEPENDENCIES)/.build
	-cd $(BYACC_DIR) && [ -f ./Makefile ] && $(MAKE) clean
	-cd $(FLEX_DIR) && [ -f ./Makefile ] && $(MAKE) clean
	cd $(GTEST_DIR) && rm -rf lib *.o
	-cd $(LLVM_DIR) && $(MAKE) clean
	cd $(PYGMENTS_DIR) && rm -rf build
	cd $(SPHINX_DIR) && rm -rf build
	cd $(DEPENDENCIES) && rm -rf install
