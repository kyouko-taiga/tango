#!/bin/bash

PYTHON_ROOT=/opt/local/Library/Frameworks/Python.framework/Versions/3.6/
LLVM_ROOT=
SRC_ROOT=$(pwd)

PYTHON_INCLUDE=${PYTHON_ROOT}/include/python3.6m
PYTHON_LIB=${PYTHON_ROOT}/lib

# AST sources
c++ -c -std=c++11 \
    -I${PYTHON_INCLUDE} \
    -I${SRC_ROOT} \
    ${SRC_ROOT}/tango/ast/ast.cc -o ${SRC_ROOT}/tango/ast/ast.o

# Wrapper module
c++ -c -std=c++11 \
    -I${PYTHON_INCLUDE} \
    -I${SRC_ROOT} \
    ${SRC_ROOT}/tango/wrapper.cc -o ${SRC_ROOT}/tango/wrapper.o

c++ -shared \
    -L${PYTHON_LIB} \
    -lpython3.6 \
    -lboost_python3 \
    ${SRC_ROOT}/tango/ast/ast.o ${SRC_ROOT}/tango/wrapper.o -o ${SRC_ROOT}/tango/wrapper.so
