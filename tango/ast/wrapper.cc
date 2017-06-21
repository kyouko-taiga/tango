#include <memory>

#include <boost/python.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>

#include "ast.hh"


BOOST_PYTHON_MODULE(ast) {

    using namespace boost::python;
    using namespace tango;

    class_<ASTNode, boost::noncopyable>("Node", no_init);

    class_<ASTNodeList>("NodeList")
        .def(vector_indexing_suite<ASTNodeList, true>());

    class_<Block, bases<ASTNode>>(
        "Block", init<ASTNodeList>())
        .def_readwrite("statements", &Block::statements);

    class_<IntegerLiteral, std::shared_ptr<IntegerLiteral>, bases<ASTNode>>(
        "IntegerLiteral", init<long>())
        .def_readwrite("value", &IntegerLiteral::value);

}
