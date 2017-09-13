#!/bin/bash

PYTHON_ROOT=/opt/local/Library/Frameworks/Python.framework/Versions/3.6/
LLVM_ROOT=/Volumes/Data/Development/llvm_build
SRC_ROOT=$(pwd)

PYTHON_INCLUDE=${PYTHON_ROOT}/include/python3.6m
PYTHON_LIB=${PYTHON_ROOT}/lib

LLVM_CONFIG=${LLVM_ROOT}/bin/llvm-config

# FIXME If LLVM was compiled without exception-handling support
# (setting `LLVM_ENABLE_EH` to `ON`), `llvm-config --cxxflags` will include
# the flag `-fno-exceptions`. But this'll conflict with boost-python, which
# makes use of c++ exceptions. A possible workaround would be to add the
# `-fexceptions` to override `-fno-exceptions`, but the consequences of such
# workaround have yet to be assessed.
# A better solution would be to either compile LLVM with exception support,
# or boost python without exception (if possible at all). Once again, the
# consequences of both those options have yet to be assessed.

# AST sources
echo "Building the ast module ..."
c++ -c -std=c++11 \
    -I${PYTHON_INCLUDE} \
    -I${SRC_ROOT} \
    ${SRC_ROOT}/tango/ast/ast.cc -o ${SRC_ROOT}/tango/ast/ast.o
if [ $? -ne 0 ]; then exit 1; fi

# Types sources
echo "Building the types module ..."
c++ -c -std=c++11 \
    -I${PYTHON_INCLUDE} \
    -I${SRC_ROOT} \
    ${SRC_ROOT}/tango/types/types.cc -o ${SRC_ROOT}/tango/types/types.o
if [ $? -ne 0 ]; then exit 1; fi

# LLVM IR generator
echo "Building the LLVM IR generator ..."
c++ -c \
    $(${LLVM_CONFIG} --cxxflags) -fexceptions \
    -I${PYTHON_INCLUDE} \
    -I${SRC_ROOT} \
    ${SRC_ROOT}/tango/irgen/irgen.cc -o ${SRC_ROOT}/tango/irgen/irgen.o
if [ $? -ne 0 ]; then exit 1; fi

# Wrapper module
echo "Building the wrapper module ..."
c++ -c -std=c++11 \
    -I${PYTHON_INCLUDE} \
    -I${SRC_ROOT} \
    ${SRC_ROOT}/tango/wrapper.cc -o ${SRC_ROOT}/tango/wrapper.o
if [ $? -ne 0 ]; then exit 1; fi

echo "Linking ..."
c++ -shared \
    $(${LLVM_CONFIG} --ldflags) \
    -L${PYTHON_LIB} \
    $(${LLVM_CONFIG} --libs) \
    -lncurses \
    -lpython3.6 \
    -lboost_python3 \
    ${SRC_ROOT}/tango/ast/ast.o \
    ${SRC_ROOT}/tango/types/types.o \
    ${SRC_ROOT}/tango/irgen/irgen.o \
    ${SRC_ROOT}/tango/wrapper.o \
    -o ${SRC_ROOT}/tango/wrapper.so
