#pragma once

#include <fstream>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

#include <boost/python.hpp>

#include "tango/types/types.hh"


namespace tango {

    // Forward declarations.
    struct ASTNodeVisitor;

    // Enumerations of operators.
    enum Operator {
        o_add, o_sub, o_mul, o_div,
        o_cpy, o_ref, o_mov,
    };

    // -----------------------------------------------------------------------

    /// Struct for AST node metadata.
    struct ASTNodeMetadata {
        TypePtr type;

        // Since other metadata attributes aren't required by the LLVM code
        // generator, we don't bother storing them as C++ variables and use a
        // python dictionary instead.
        boost::python::dict _py_attrs;
    };

    // -----------------------------------------------------------------------

    /// Base class for all AST nodes.
    struct ASTNode {
        virtual ~ASTNode() {}

        // Has to be implemented in every derived class, or dynamic dispatch
        // wouldn't work.
        virtual void accept(ASTNodeVisitor& visitor) = 0;

        ASTNodeMetadata meta;
    };

    typedef std::shared_ptr<ASTNode> ASTNodePtr;
    typedef std::vector<ASTNodePtr>  ASTNodeList;

    // -----------------------------------------------------------------------

    /// AST node for blocks of instructions.
    struct Block: public ASTNode {
        Block(const ASTNodeList& statements)
            : statements(statements) {}

        void accept(ASTNodeVisitor& visitor);

        ASTNodeList statements;
    };

    // -----------------------------------------------------------------------

    /// AST node for module declarations.
    struct ModuleDecl: public ASTNode {
        ModuleDecl(ASTNodePtr body, const std::string& name)
            : body(body), name(name) {}

        void accept(ASTNodeVisitor& visitor);

        ASTNodePtr  body;
        std::string name;
    };

    // -----------------------------------------------------------------------

    /// AST node for property declarations.
    struct PropDecl: public ASTNode {
        PropDecl(
            const std::string&   name,
            ASTNodePtr           type            = nullptr,
            ASTNodePtr           initial_value   = nullptr,
            Operator             initial_binding = o_cpy)
            : name(name),
              type_annotation(type),
              initial_value(initial_value), initial_binding(initial_binding) {}

        void accept(ASTNodeVisitor& visitor);

        std::string          name;
        ASTNodePtr           type_annotation;
        ASTNodePtr           initial_value;
        Operator             initial_binding;
    };

    // -----------------------------------------------------------------------

    /// AST node for struct declarations.
    struct StructDecl: public ASTNode {
        StructDecl(const std::string& name, ASTNodePtr body)
            : name(name), body(body) {}

        void accept(ASTNodeVisitor& visitor);

        std::string name;
        ASTNodePtr  body;
    };

    // -----------------------------------------------------------------------

    /// AST node for function parameters.
    struct ParamDecl: public ASTNode {
        ParamDecl(
            const std::string&   name,
            ASTNodePtr           type = ASTNodePtr())
            : name(name),
              type_annotation(type) {}

        void accept(ASTNodeVisitor& visitor);

        std::string          name;
        ASTNodePtr           type_annotation;
    };

    // -----------------------------------------------------------------------

    /// AST node for function declarations.
    struct FunDecl: public ASTNode {
        FunDecl(
            const std::string& name,
            const ASTNodeList& parameters,
            ASTNodePtr         codomain,
            ASTNodePtr         body)
            : name(name), parameters(parameters), codomain_annotation(codomain), body(body) {}

        void accept(ASTNodeVisitor& visitor);

        std::string name;
        ASTNodeList parameters;
        ASTNodePtr  codomain_annotation;
        ASTNodePtr  body;
    };

    // -----------------------------------------------------------------------

    /// AST node for assignments.
    struct Assignment: public ASTNode {
        Assignment(
            ASTNodePtr lvalue,
            Operator   op,
            ASTNodePtr rvalue)
            : lvalue(lvalue), op(op), rvalue(rvalue) {}

        void accept(ASTNodeVisitor& visitor);

        ASTNodePtr lvalue;
        Operator   op;
        ASTNodePtr rvalue;
    };

    // -----------------------------------------------------------------------

    /// AST node for conditional statements.
    struct If: public ASTNode {
        If(
            ASTNodePtr condition,
            ASTNodePtr then_block,
            ASTNodePtr else_block)
            : condition(condition), then_block(then_block), else_block(else_block) {}

        void accept(ASTNodeVisitor& visitor);

        ASTNodePtr condition;
        ASTNodePtr then_block;
        ASTNodePtr else_block;
    };

    // -----------------------------------------------------------------------

    /// AST node for return statements.
    struct Return: public ASTNode {
        Return(ASTNodePtr value)
            : value(value) {}

        void accept(ASTNodeVisitor& visitor);

        ASTNodePtr value;
    };

    // -----------------------------------------------------------------------

    /// AST node for binary expressions.
    struct BinaryExpr: public ASTNode {
        BinaryExpr(
            ASTNodePtr left,
            Operator   op,
            ASTNodePtr right)
            : left(left), op(op), right(right) {}

        void accept(ASTNodeVisitor& visitor);

        ASTNodePtr left;
        Operator   op;
        ASTNodePtr right;
    };

    // -----------------------------------------------------------------------

    /// AST node for call arguments.
    struct Argument: public ASTNode {
        Argument(
            const std::string& label,
            Operator           op,
            ASTNodePtr         value)
            : label(label), op(op), value(value) {}

        void accept(ASTNodeVisitor& visitor);

        std::string label;
        Operator    op;
        ASTNodePtr  value;
    };

    // -----------------------------------------------------------------------

    /// AST node for call expressions.
    struct Call: public ASTNode {
        Call(
            ASTNodePtr         callee,
            const ASTNodeList& arguments)
            : callee(callee), arguments(arguments) {}

        void accept(ASTNodeVisitor& visitor);

        ASTNodePtr  callee;
        ASTNodeList arguments;
    };

    // -----------------------------------------------------------------------

    /// AST node for identifiers.
    struct Identifier: public ASTNode {
        Identifier(const std::string& name)
            : name(name) {}

        void accept(ASTNodeVisitor& visitor);

        std::string name;
    };

    // -----------------------------------------------------------------------

    /// AST node for type signatures.
    struct TypeIdentifier: public ASTNode {
        TypeIdentifier(ASTNodePtr signature, uint8_t modifiers)
            : signature(signature), modifiers(modifiers) {}

        void accept(ASTNodeVisitor& visitor);

        ASTNodePtr signature;
        uint8_t    modifiers;
    };

    // -----------------------------------------------------------------------

    /// AST node for parameters of function signatures.
    struct FunSignParam: public ASTNode {
        FunSignParam(const std::string& label, ASTNodePtr type_annotation)
            : label(label), type_annotation(type_annotation) {}

        void accept(ASTNodeVisitor& visitor);

        std::string label;
        ASTNodePtr  type_annotation;
    };

    // -----------------------------------------------------------------------

    /// AST node for function signatures.
    struct FunSign: public ASTNode {
        FunSign(const ASTNodeList& parameters, ASTNodePtr codomain)
            : parameters(parameters), codomain_annotation(codomain) {}

        void accept(ASTNodeVisitor& visitor);

        ASTNodeList parameters;
        ASTNodePtr  codomain_annotation;
    };

    // -----------------------------------------------------------------------

    /// AST node for integer literals.
    struct IntLiteral: public ASTNode {
        IntLiteral(long value)
            : value(value) {}

        void accept(ASTNodeVisitor& visitor);

        long value;
    };

    // -----------------------------------------------------------------------

    /// AST node for double literals.
    struct DoubleLiteral: public ASTNode {
        DoubleLiteral(double value)
            : value(value) {}

        void accept(ASTNodeVisitor& visitor);

        double value;
    };

    // -----------------------------------------------------------------------

    /// AST node for string literals.
    struct StringLiteral: public ASTNode {
        StringLiteral(const std::string& value)
            : value(value) {}

        void accept(ASTNodeVisitor& visitor);

        std::string value;
    };

    // -----------------------------------------------------------------------

    /// AST node for bool literals.
    struct BoolLiteral: public ASTNode {
        BoolLiteral(bool value)
            : value(value) {}

        void accept(ASTNodeVisitor& visitor);

        bool value;
    };

    // -----------------------------------------------------------------------

    struct ASTNodeVisitor {
        virtual ~ASTNodeVisitor() {}

        virtual void visit(Block&          node) = 0;
        virtual void visit(ModuleDecl&     node) = 0;
        virtual void visit(PropDecl&       node) = 0;
        virtual void visit(StructDecl&     node) = 0;
        virtual void visit(ParamDecl&      node) = 0;
        virtual void visit(FunDecl&        node) = 0;
        virtual void visit(Assignment&     node) = 0;
        virtual void visit(If&             node) = 0;
        virtual void visit(Return&         node) = 0;
        virtual void visit(BinaryExpr&     node) = 0;
        virtual void visit(Argument&       node) = 0;
        virtual void visit(Call&           node) = 0;
        virtual void visit(Identifier&     node) = 0;
        virtual void visit(TypeIdentifier& node) = 0;
        virtual void visit(FunSignParam&   node) = 0;
        virtual void visit(FunSign&        node) = 0;
        virtual void visit(IntLiteral&     node) = 0;
        virtual void visit(DoubleLiteral&  node) = 0;
        virtual void visit(StringLiteral&  node) = 0;
        virtual void visit(BoolLiteral&    node) = 0;
    };

} // namespace tango
