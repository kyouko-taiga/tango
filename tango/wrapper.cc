#include <memory>

#include <boost/python.hpp>
#include <boost/python/suite/indexing/map_indexing_suite.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>

#include "tango/ast/ast.hh"
#include "tango/irgen/irgen.hh"
#include "tango/types/types.hh"


namespace tango {

    boost::python::list get_typemap_keys(TypeMap& m) {
        using namespace boost::python;

        list result;
        for (auto it: m) {
            result.append(it.first);
        }
        return result;
    }

    boost::python::list get_type_union_content(TypeUnion& u) {
        using namespace boost::python;

        list result;
        for (auto t: u.types) {
            result.append(t);
        }
        return result;
    }

    void set_type_union_content(TypeUnion& u, boost::python::list py_types) {
        using namespace boost::python;

        std::size_t length = extract<std::size_t>(py_types.attr("__len__")());
        u.types.clear();
        for (std::size_t i = 0; i < length; ++i) {
            u.add(extract<TypePtr>(py_types[i]));
        }
    }

    std::shared_ptr<TypeName> make_type_name(
        TypeFactory&       factory,
        const std::string& name,
        TypePtr            type)
    {
        return factory.make<TypeName>(name, type);
    }

    std::shared_ptr<TypeVariable> make_type_variable(
        TypeFactory&          factory,
        uint8_t               modifiers,
        boost::python::object id)
    {
        using namespace boost::python;

        if (id == api::object()) {
            id = str(factory.next_variable_id++);
        }
        return factory.make<TypeVariable>(modifiers, id);
    }

    std::shared_ptr<PlaceholderType> make_placeholder_type(
        TypeFactory&          factory,
        uint8_t               modifiers,
        boost::python::object id,
        TypePtr               specialization)
    {
        return factory.make<PlaceholderType>(modifiers, id, specialization);
    }

    std::shared_ptr<FunctionType> make_function_type(
        TypeFactory&        factory,
        uint8_t             modifiers,
        boost::python::list py_domain,
        boost::python::list py_labels,
        TypePtr             codomain)
    {
        using namespace boost::python;

        TypeList                 cc_domain;
        std::vector<std::string> cc_labels;

        std::size_t domain_length = extract<std::size_t>(py_domain.attr("__len__")());
        for (std::size_t i = 0; i < domain_length; ++i) {
            cc_domain.push_back(extract<TypePtr>(py_domain[i]));
            cc_labels.push_back(extract<std::string>(py_labels[i]));
        }

        return factory.make<FunctionType>(modifiers, cc_domain, cc_labels, codomain);
    }

    std::shared_ptr<BuiltinType> make_builtin_type(
        TypeFactory&       factory,
        uint8_t            modifiers,
        const std::string& name)
    {
        return factory.make<BuiltinType>(modifiers, name);
    }

} // namespace tango


BOOST_PYTHON_MODULE(wrapper) {

    using namespace boost::python;
    using namespace tango;

    def("emit_ir", emit_ir, (arg("with_optimizations")=true));

    class_<std::vector<std::string>>("StringList")
        .def(vector_indexing_suite<std::vector<std::string>, true>());

    // -----------------------------------------------------------------------

    enum_<TypeModifier>("TypeModifier")
        .value("cst",  tm_cst)
        .value("mut",  tm_mut)
        .value("stk",  tm_stk)
        .value("shd",  tm_shd)
        .value("val",  tm_val)
        .value("ref",  tm_ref)
        .value("own",  tm_own);

    class_<TypeBase, boost::noncopyable>("TypeBase", no_init)
        .add_property("is_generic", make_function(&TypeBase::is_generic))
        .def_readonly("modifiers",                &TypeBase::modifiers)
        .def(self == self);

    class_<TypeList>("TypeList")
        .def(vector_indexing_suite<TypeList, true>());

    class_<TypeMap>("TypeMap")
        .def(map_indexing_suite<TypeMap, true>())
        .def("keys",                          &get_typemap_keys);

    class_<TypeName, std::shared_ptr<TypeName>, bases<TypeBase>, boost::noncopyable>(
        "TypeName", no_init)
        .def_readonly("name",                 &TypeName::name)
        .def_readonly("type",                 &TypeName::type);

    class_<TypeUnion, std::shared_ptr<TypeUnion>, bases<TypeBase>, boost::noncopyable>(
        "TypeUnion", init<>())
        .add_property("types",                &get_type_union_content, &set_type_union_content)
        .def("add",                           &TypeUnion::add);

    class_<TypeVariable, std::shared_ptr<TypeVariable>, bases<TypeBase>, boost::noncopyable>(
        "TypeVariable", no_init)
        .def_readonly("id",                   &TypeVariable::id);

    class_<PlaceholderType, std::shared_ptr<PlaceholderType>, bases<TypeBase>, boost::noncopyable>(
        "PlaceholderType", no_init)
        .def_readonly("id",                   &PlaceholderType::id)
        .def_readonly("specialization",       &PlaceholderType::specialization);

    class_<FunctionType, std::shared_ptr<FunctionType>, bases<TypeBase>, boost::noncopyable>(
        "FunctionType", no_init)
        .def_readonly("domain",               &FunctionType::domain)
        .def_readonly("labels",               &FunctionType::labels)
        .def_readonly("codomain",             &FunctionType::codomain);

    class_<NominalType, bases<TypeBase>, boost::noncopyable>(
        "NominalType", no_init)
        .def_readonly("name",                 &NominalType::name)
        .def_readonly("members",              &NominalType::members);

    class_<BuiltinType, std::shared_ptr<BuiltinType>, bases<NominalType>, boost::noncopyable>(
        "BuiltinType", no_init);

    class_<TypeFactory>("TypeFactory")
        // def make_name(self, name, type)
        .def("make_name", &make_type_name,
            (arg("name"),
             arg("type")))

        // def make_variable(self, modifiers=0, id=None)
        .def("make_variable", &make_type_variable,
            (arg("modifiers")=0,
             arg("id")=api::object()))

        // def make_placeholder(self, modifiers=0, id, specialization=None)
        .def("make_placeholder", &make_placeholder_type,
            (arg("modifiers")=0,
             arg("id"),
             arg("specialization")=TypePtr(nullptr)))

        // def make_function(self, modifiers=0, domain=[], labels=[], codomain)
        .def("make_function", &make_function_type,
            (arg("modifiers")=0,
             arg("domain")=list(),
             arg("labels")=list(),
             arg("codomain")))

        // def make_builtin(self, modifiers=0, name)
        .def("make_builtin", &make_builtin_type,
            (arg("modifiers")=0,
             arg("name")));

        // -----------------------------------------------------------------------

    enum_<Operator>("Operator")
        .value("add", o_add)
        .value("sub", o_sub)
        .value("mul", o_mul)
        .value("div", o_div)
        .value("cpy", o_cpy)
        .value("ref", o_ref)
        .value("mov", o_mov);

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
        "PropDecl", init<
            std::string,
            optional<ASTNodePtr, ASTNodePtr, Operator>
        >((
            arg("name"),
            arg("type_annotation"),
            arg("initial_value"),
            arg("initial_binding"))))
        .def_readwrite("name",                &PropDecl::name)
        .def_readwrite("type_annotation",     &PropDecl::type_annotation)
        .def_readwrite("initial_value",       &PropDecl::initial_value)
        .def_readwrite("initial_binding",     &PropDecl::initial_binding);

    class_<StructDecl, bases<ASTNode>>(
        "StructDecl", init<std::string, ASTNodePtr>((
            arg("name"),
            arg("body"))))
        .def_readwrite("name",                &StructDecl::name)
        .def_readwrite("body",                &StructDecl::body);

    class_<ParamDecl, bases<ASTNode>>(
        "ParamDecl", init<std::string, optional<ASTNodePtr>>((
            arg("name"),
            arg("type_annotation"))))
        .def_readwrite("name",                &ParamDecl::name)
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

    class_<Argument, bases<ASTNode>>(
        "Argument", init<std::string, Operator, ASTNodePtr>((
            arg("label"),
            arg("operator"),
            arg("value"))))
        .def_readwrite("label",               &Argument::label)
        .def_readwrite("operator",            &Argument::op)
        .def_readwrite("value",               &Argument::value);

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
        "TypeIdentifier", init<ASTNodePtr, uint8_t>((
            arg("signature"),
            arg("modifiers"))))
        .def_readwrite("signature",           &TypeIdentifier::signature)
        .def_readwrite("modifiers",           &TypeIdentifier::modifiers);

    class_<FunSignParam, bases<ASTNode>>(
        "FunSignParam", init<std::string, ASTNodePtr>((
            arg("label"),
            arg("type_annotation"))))
        .def_readwrite("label",               &FunSignParam::label)
        .def_readwrite("type_annotation",     &FunSignParam::type_annotation);

    class_<FunSign, bases<ASTNode>>(
        "FunSign", init<ASTNodeList, ASTNodePtr>((
            arg("parameters"),
            arg("codomain_annotation"))))
        .def_readwrite("parameters",          &FunSign::parameters)
        .def_readwrite("codomain_annotation", &FunSign::codomain_annotation);

    class_<IntLiteral, bases<ASTNode>>(
        "IntLiteral", init<long>((arg("value"))))
        .def_readwrite("value",               &IntLiteral::value);

    class_<DoubleLiteral, bases<ASTNode>>(
        "DoubleLiteral", init<double>((arg("value"))))
        .def_readwrite("value",               &DoubleLiteral::value);

    class_<StringLiteral, bases<ASTNode>>(
        "StringLiteral", init<std::string>((arg("value"))))
        .def_readwrite("value",               &StringLiteral::value);

    class_<BoolLiteral, bases<ASTNode>>(
        "BoolLiteral", init<bool>((arg("value"))))
        .def_readwrite("value",               &BoolLiteral::value);

}
