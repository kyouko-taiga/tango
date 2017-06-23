#pragma once

#include <fstream>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

#include "tango/types/types.hh"


namespace tango {

    // Forward declarations.
    struct ASTNodeVisitor;

    // Identifier attributes.
    enum IdentifierMutability {
        im_cst, im_mut,
    };

    // Enumerations of operators.
    enum Operator {
        o_add, o_sub, o_mul, o_div,
        o_cpy, o_ref, o_mov,
    };

    // -----------------------------------------------------------------------

    /// Location (start/end) of a node within its source file.
    struct ASTNodeLocation {
        int line = 0;
        int col  = 0;
    };

    /// Struct for AST node metadata.
    struct ASTNodeMetadata {
        ASTNodeLocation start;
        ASTNodeLocation end;

        TypePtr         type;
    };

    // -----------------------------------------------------------------------

    /// Base class for all AST nodes.
    struct ASTNode {
        virtual ~ASTNode() {};

        // Has to be implemented in every derived class, or dynamic dispatch
        // wouldn't work.
        virtual void accept(ASTNodeVisitor& visitor) = 0;

        ASTNodeMetadata meta;

        // Following are metadata about AST nodes.
        // TypePtr md_type;
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
    struct Module: public ASTNode {
        Module(ASTNodePtr body, const std::string& name)
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
            IdentifierMutability im = im_cst,
            ASTNodePtr           type = ASTNodePtr())
            : name(name), mutability(im), type_annotation(type) {}

        void accept(ASTNodeVisitor& visitor);

        std::string          name;
        IdentifierMutability mutability;
        ASTNodePtr           type_annotation;
    };

    // -----------------------------------------------------------------------

    /// AST node for function parameters.
    struct ParamDecl: public ASTNode {
        ParamDecl(
            const std::string&   name,
            IdentifierMutability im = im_cst,
            ASTNodePtr           type = ASTNodePtr())
            : name(name), mutability(im), type_annotation(type) {}

        void accept(ASTNodeVisitor& visitor);

        std::string          name;
        IdentifierMutability mutability;
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

    // AST node for assignments.
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

    // AST node for conditional statements.
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

    // AST node for call arguments.
    struct CallArg: public ASTNode {
        CallArg(
            const std::string& label,
            Operator   op,
            ASTNodePtr value)
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

    /// AST node for integer literals.
    struct IntLiteral: public ASTNode {
        IntLiteral(long value)
            : value(value) {}

        void accept(ASTNodeVisitor& visitor);

        long value;
    };

    // -----------------------------------------------------------------------

    /// AST node for boolean literals.
    struct BoolLiteral: public ASTNode {
        BoolLiteral(bool value)
            : value(value) {}

        void accept(ASTNodeVisitor& visitor);

        bool value;
    };

    // -----------------------------------------------------------------------

    struct ASTNodeVisitor {
        virtual void visit(Block&         node) = 0;
        virtual void visit(Module&        node) = 0;
        virtual void visit(PropDecl&      node) = 0;
        virtual void visit(ParamDecl&     node) = 0;
        virtual void visit(FunDecl&       node) = 0;
        virtual void visit(Assignment&    node) = 0;
        virtual void visit(If&            node) = 0;
        virtual void visit(Return&        node) = 0;
        virtual void visit(BinaryExpr&    node) = 0;
        virtual void visit(CallArg&       node) = 0;
        virtual void visit(Call&          node) = 0;
        virtual void visit(Identifier&    node) = 0;
        virtual void visit(IntLiteral&    node) = 0;
        virtual void visit(BoolLiteral&   node) = 0;
    };

} // namespace tango
