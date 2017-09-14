CC = c++

PYTHON_ROOT    = /opt/local/Library/Frameworks/Python.framework/Versions/3.6/
PYTHON_INCLUDE = $(PYTHON_ROOT)/include/python3.6m
PYTHON_LIB     = $(PYTHON_ROOT)/lib

LLVM_ROOT      = /opt/local/llvm
LLVM_CONFIG    = $(LLVM_ROOT)/bin/llvm-config

CXXFLAGS       = -I$(PYTHON_INCLUDE) -I$(CURDIR)
CXXFLAGS      += $(shell $(LLVM_CONFIG) --cxxflags)

LDFLAGS        = -L$(PYTHON_LIB)
LDFLAGS       += $(shell $(LLVM_CONFIG) --ldflags)
LDFLAGS       += -lncurses -lpython3.6 -lboost_python3
LDFLAGS       += $(shell $(LLVM_CONFIG) --libs)

src            = tango/wrapper.cc
src           += $(shell ls tango/ast/*.cc)
src           += $(shell ls tango/ast/*.cc)
src           += $(shell ls tango/irgen/*.cc)
src           += $(shell ls tango/types/*.cc)
obj            = $(src:.cc=.o)

tango/wrapper.so: $(obj)
	$(CC) -o $@ $^ $(LDFLAGS)

.PHONY: clean
clean:
	rm -f $(obj) trango/wrapper.so
