#include <memory>

#include <boost/python.hpp>
#include <boost/python/suite/indexing/map_indexing_suite.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>

#include "tango/ast/ast.hh"
#include "tango/types/types.hh"


namespace tango {

    std::shared_ptr<BuiltinType> make_builtin_type(TypeFactory& factory, const std::string& name) {
        return factory.make_type<BuiltinType>(name);
    }

} // namespace tango


BOOST_PYTHON_MODULE(wrapper) {

    using namespace boost::python;
    using namespace tango;

    class_<std::vector<std::string>>("StringList")
        .def(vector_indexing_suite<std::vector<std::string>, true>());

    // -----------------------------------------------------------------------

    class_<TypeBase, boost::noncopyable>("TypeBase", no_init)
        .add_property("is_primitive", make_function(&TypeBase::is_primitive))
        .add_property("is_generic",   make_function(&TypeBase::is_generic))
        .add_property("is_reference", make_function(&TypeBase::is_reference))
        .def(self == self);

    class_<TypeList>("TypeList")
        .def(vector_indexing_suite<TypeList, true>());

    class_<TypeMap>("TypeMap")
        .def(map_indexing_suite<TypeMap, true>());

    class_<TypeUnion, bases<TypeBase>, boost::noncopyable>(
        "TypeUnion", init<>())
        .def_readwrite("types",               &TypeUnion::types)
        .def("add",                           &TypeUnion::add)
        .def("replace_content",               &TypeUnion::replace_content)
        .def("first",                         &TypeUnion::first);

    class_<ReferenceType, bases<TypeBase>, boost::noncopyable>(
        "ReferenceType", init<TypePtr>((arg("referred_type"))))
        .def_readwrite("referred_type",       &ReferenceType::referred_type);

    class_<FunctionType, bases<TypeBase>, boost::noncopyable>(
        "FunctionType", init<optional<TypeList, std::vector<std::string>, TypePtr>>((
            arg("domain"),
            arg("labels"),
            arg("codomain"))))
        .def_readwrite("domain",              &FunctionType::domain)
        .def_readwrite("labels",              &FunctionType::labels)
        .def_readwrite("codomain",            &FunctionType::codomain);

    class_<NominalType, bases<TypeBase>, boost::noncopyable>(
        "NominalType", no_init)
        .def_readwrite("name",                &NominalType::name)
        .def_readwrite("members",             &NominalType::members);

    class_<BuiltinType, std::shared_ptr<BuiltinType>, bases<NominalType>, boost::noncopyable>(
        "BuiltinType", init<std::string>((arg("name"))));

    class_<TypeFactory>("TypeFactory")
        .def("make_builtin_type", &make_builtin_type);

    // -----------------------------------------------------------------------

    enum_<IdentifierMutability>("IdentifierMutability")
        .value("im_cst", im_cst)
        .value("im_mut", im_mut);

    enum_<TypeModifier>("TypeModifier")
        .value("tm_none", tm_none)
        .value("tm_ref",  tm_ref)
        .value("tm_own",  tm_own);

    enum_<Operator>("Operator")
        .value("o_add", o_add)
        .value("o_sub", o_sub)
        .value("o_mul", o_mul)
        .value("o_div", o_div)
        .value("o_cpy", o_div)
        .value("o_ref", o_div)
        .value("o_mov", o_div);

    class_<ASTNodeMetadata>("NodeMetadata")
        .def_readwrite("_py_attrs",           &ASTNodeMetadata::_py_attrs)
        .def_readwrite("type",                &ASTNodeMetadata::type);

    class_<ASTNode, boost::noncopyable>("Node", no_init)
        .def_readwrite("__meta__",            &ASTNode::meta);

    class_<ASTNodeList>("NodeList")
        .def(vector_indexing_suite<ASTNodeList, true>());

    class_<Block, bases<ASTNode>>(
        "Block", init<ASTNodeList>((arg("statements"))))
        .def_readwrite("statements",          &Block::statements);

    class_<ModuleDecl, bases<ASTNode>>(
        "ModuleDecl", init<ASTNodePtr, std::string>((
            arg("body"),
            arg("name"))))
        .def_readwrite("body",                &ModuleDecl::body)
        .def_readwrite("name",                &ModuleDecl::name);

    class_<PropDecl, bases<ASTNode>>(
        "PropDecl", init<std::string, optional<IdentifierMutability, ASTNodePtr>>((
                arg("name"),
                arg("mutability"),
                arg("type_annotation"))))
        .def_readwrite("name",                &PropDecl::name)
        .def_readwrite("mutability",          &PropDecl::mutability)
        .def_readwrite("type_annotation",     &PropDecl::type_annotation);

    class_<ParamDecl, bases<ASTNode>>(
        "ParamDecl", init<std::string, optional<IdentifierMutability, ASTNodePtr>>((
                arg("name"),
                arg("mutability"),
                arg("type_annotation"))))
        .def_readwrite("name",                &ParamDecl::name)
        .def_readwrite("mutability",          &ParamDecl::mutability)
        .def_readwrite("type_annotation",     &ParamDecl::type_annotation);

    class_<FunDecl, bases<ASTNode>>(
        "FunDecl", init<std::string, ASTNodeList, ASTNodePtr, ASTNodePtr>((
            arg("name"),
            arg("parameters"),
            arg("codomain_annotation"),
            arg("body"))))
        .def_readwrite("name",                &FunDecl::name)
        .def_readwrite("parameters",          &FunDecl::parameters)
        .def_readwrite("codomain_annotation", &FunDecl::codomain_annotation)
        .def_readwrite("body",                &FunDecl::body);

    class_<Assignment, bases<ASTNode>>(
        "Assignment", init<ASTNodePtr, Operator, ASTNodePtr>((
            arg("lvalue"),
            arg("operator"),
            arg("rvalue"))))
        .def_readwrite("lvalue",              &Assignment::lvalue)
        .def_readwrite("operator",            &Assignment::op)
        .def_readwrite("rvalue",              &Assignment::rvalue);

    class_<If, bases<ASTNode>>(
        "If", init<ASTNodePtr, ASTNodePtr, ASTNodePtr>((
            arg("condition"),
            arg("then_block"),
            arg("else_block"))))
        .def_readwrite("condition",           &If::condition)
        .def_readwrite("then_block",          &If::then_block)
        .def_readwrite("else_block",          &If::else_block);

    class_<Return, bases<ASTNode>>(
        "Return", init<ASTNodePtr>((arg("value"))))
        .def_readwrite("value",               &Return::value);

    class_<BinaryExpr, bases<ASTNode>>(
        "BinaryExpr", init<ASTNodePtr, Operator, ASTNodePtr>((
            arg("left"),
            arg("operator"),
            arg("right"))))
        .def_readwrite("left",                &BinaryExpr::left)
        .def_readwrite("operator",            &BinaryExpr::op)
        .def_readwrite("right",               &BinaryExpr::right);

    class_<CallArg, bases<ASTNode>>(
        "CallArg", init<std::string, Operator, ASTNodePtr>((
            arg("label"),
            arg("operator"),
            arg("value"))))
        .def_readwrite("label",               &CallArg::label)
        .def_readwrite("operator",            &CallArg::op)
        .def_readwrite("value",               &CallArg::value);

    class_<Call, bases<ASTNode>>(
        "Call", init<ASTNodePtr, ASTNodeList>((
            arg("callee"),
            arg("arguments"))))
        .def_readwrite("callee",              &Call::callee)
        .def_readwrite("arguments",           &Call::arguments);

    class_<Identifier, bases<ASTNode>>(
        "Identifier", init<std::string>((arg("name"))))
        .def_readwrite("name",                &Identifier::name);

    class_<TypeIdentifier, bases<ASTNode>>(
        "TypeIdentifier", init<ASTNodePtr, TypeModifier>((
            arg("signature"),
            arg("modifier"))))
        .def_readwrite("signature",           &TypeIdentifier::signature)
        .def_readwrite("modifier",            &TypeIdentifier::modifier);

    class_<IntLiteral, std::shared_ptr<IntLiteral>, bases<ASTNode>>(
        "IntLiteral", init<long>((arg("value"))))
        .def_readwrite("value",               &IntLiteral::value);

    class_<BoolLiteral, std::shared_ptr<BoolLiteral>, bases<ASTNode>>(
        "BoolLiteral", init<bool>((arg("value"))))
        .def_readwrite("value",               &BoolLiteral::value);

}
