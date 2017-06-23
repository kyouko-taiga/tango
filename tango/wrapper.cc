#include <memory>

#include <boost/python.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>

#include "tango/ast/ast.hh"
#include "tango/types/types.hh"


BOOST_PYTHON_MODULE(wrapper) {

    using namespace boost::python;
    using namespace tango;

    class_<std::vector<std::string>>("StringList")
        .def(vector_indexing_suite<std::vector<std::string>, true>());

    // -----------------------------------------------------------------------

    class_<TypeBase, boost::noncopyable>("TypeBase", no_init)
        .add_property("is_primitive", make_function(&TypeBase::is_primitive))
        .add_property("is_generic",   make_function(&TypeBase::is_generic))
        .add_property("is_reference", make_function(&TypeBase::is_reference));

    class_<TypeList>("TypeList")
        .def(vector_indexing_suite<TypeList, true>());

    class_<ReferenceType, bases<TypeBase>, boost::noncopyable>(
        "ReferenceType", init<TypePtr>())
        .def_readwrite("referred_type",       &ReferenceType::referred_type);

    class_<FunctionType, bases<TypeBase>, boost::noncopyable>(
        "FunctionType", init<TypeList, std::vector<std::string>, TypePtr>())
        .def_readwrite("domain",              &FunctionType::domain)
        .def_readwrite("labels",              &FunctionType::labels)
        .def_readwrite("codomain",            &FunctionType::codomain);

    class_<NominalType, bases<TypeBase>, boost::noncopyable>(
        "NominalType", no_init)
        .def_readwrite("name",                &NominalType::name);

    class_<BuiltinType, bases<NominalType>, boost::noncopyable>(
        "BuiltinType", init<std::string>());

    // -----------------------------------------------------------------------

    enum_<IdentifierMutability>("IdentifierMutability")
        .value("im_cst", im_cst)
        .value("im_mut", im_mut);

    enum_<Operator>("Operator")
        .value("o_add", o_add)
        .value("o_sub", o_sub)
        .value("o_mul", o_mul)
        .value("o_div", o_div)
        .value("o_cpy", o_div)
        .value("o_ref", o_div)
        .value("o_mov", o_div);

    class_<ASTNodeLocation>("NodeLocation")
        .def_readwrite("line",                &ASTNodeLocation::line)
        .def_readwrite("col",                 &ASTNodeLocation::col);

    class_<ASTNodeMetadata>("NodeMetadata")
        .def_readwrite("start",               &ASTNodeMetadata::start)
        .def_readwrite("end",                 &ASTNodeMetadata::end)
        .def_readwrite("type",                &ASTNodeMetadata::type);

    class_<ASTNode, boost::noncopyable>("Node", no_init)
        .def_readwrite("meta",                &ASTNode::meta);

    class_<ASTNodeList>("NodeList")
        .def(vector_indexing_suite<ASTNodeList, true>());

    class_<Block, bases<ASTNode>>(
        "Block", init<ASTNodeList>())
        .def_readwrite("statements",          &Block::statements);

    class_<Module, bases<ASTNode>>(
        "Module", init<ASTNodePtr, std::string>())
        .def_readwrite("body",                &Module::body)
        .def_readwrite("name",                &Module::name);

    class_<PropDecl, bases<ASTNode>>(
        "PropDecl", init<std::string, IdentifierMutability, ASTNodePtr>())
        .def_readwrite("name",                &PropDecl::name)
        .def_readwrite("mutability",          &PropDecl::mutability)
        .def_readwrite("type_annotation",     &PropDecl::type_annotation);

    class_<ParamDecl, bases<ASTNode>>(
        "ParamDecl", init<std::string, IdentifierMutability, ASTNodePtr>())
        .def_readwrite("name",                &ParamDecl::name)
        .def_readwrite("mutability",          &ParamDecl::mutability)
        .def_readwrite("type_annotation",     &ParamDecl::type_annotation);

    class_<FunDecl, bases<ASTNode>>(
        "FunDecl", init<std::string, ASTNodeList, ASTNodePtr, ASTNodePtr>())
        .def_readwrite("name",                &FunDecl::name)
        .def_readwrite("parameters",          &FunDecl::parameters)
        .def_readwrite("codomain_annotation", &FunDecl::codomain_annotation)
        .def_readwrite("body",                &FunDecl::body);

    class_<Assignment, bases<ASTNode>>(
        "Assignment", init<ASTNodePtr, Operator, ASTNodePtr>())
        .def_readwrite("lvalue",              &Assignment::lvalue)
        .def_readwrite("operator",            &Assignment::op)
        .def_readwrite("rvalue",              &Assignment::rvalue);

    class_<If, bases<ASTNode>>(
        "If", init<ASTNodePtr, ASTNodePtr, ASTNodePtr>())
        .def_readwrite("condition",           &If::condition)
        .def_readwrite("then_block",          &If::then_block)
        .def_readwrite("else_block",          &If::else_block);

    class_<Return, bases<ASTNode>>(
        "Return", init<ASTNodePtr>())
        .def_readwrite("value",               &Return::value);

    class_<BinaryExpr, bases<ASTNode>>(
        "BinaryExpr", init<ASTNodePtr, Operator, ASTNodePtr>())
        .def_readwrite("left",                &BinaryExpr::left)
        .def_readwrite("operator",            &BinaryExpr::op)
        .def_readwrite("right",               &BinaryExpr::right);

    class_<CallArg, bases<ASTNode>>(
        "CallArg", init<std::string, Operator, ASTNodePtr>())
        .def_readwrite("label",               &CallArg::label)
        .def_readwrite("operator",            &CallArg::op)
        .def_readwrite("value",               &CallArg::value);

    class_<Call, bases<ASTNode>>(
        "Call", init<ASTNodePtr, ASTNodeList>())
        .def_readwrite("callee",              &Call::callee)
        .def_readwrite("arguments",           &Call::arguments);

    class_<Identifier, bases<ASTNode>>(
        "Identifier", init<std::string>())
        .def_readwrite("name",                &Identifier::name);

    class_<IntLiteral, std::shared_ptr<IntLiteral>, bases<ASTNode>>(
        "IntLiteral", init<long>())
        .def_readwrite("value",               &IntLiteral::value);

    class_<BoolLiteral, std::shared_ptr<BoolLiteral>, bases<ASTNode>>(
        "BoolLiteral", init<bool>())
        .def_readwrite("value",               &BoolLiteral::value);

}
