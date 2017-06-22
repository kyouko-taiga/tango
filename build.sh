# AST sources
c++ -c -std=c++11 \
    tango/ast/ast.cc -o tango/ast/ast.o

# Wrapper module
c++ -c -std=c++11 \
    -I/opt/local/Library/Frameworks/Python.framework/Versions/3.6/include/python3.6m \
    tango/wrapper.cc -o tango/wrapper.o

c++ -shared \
    -L/opt/local/Library/Frameworks/Python.framework/Versions/3.6/lib \
    -lpython3.6 \
    -lboost_python3 \
    tango/ast/ast.o tango/wrapper.o -o tango/wrapper.so

# c++ -shared -std=c++11 \
#     -I/opt/local/Library/Frameworks/Python.framework/Versions/3.6/include/python3.6m \
#     -L/opt/local/Library/Frameworks/Python.framework/Versions/3.6/lib \
#     -lpython3.6 \
#     -lboost_python3 \
#     tango/ast/wrapper.cc \
#     -o tango/ast/ast.so
