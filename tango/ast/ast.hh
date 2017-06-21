#pragma once

#include <fstream>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>


namespace tango {

    // Identifier attributes.
    enum IdentifierMutability {
        im_cst, im_mut,
    };

    // Enumerations of operators.
    enum Operator {
        add, sub, mul, div,
    };

    enum AssignmentOperator {
        ao_cpy, ao_ref, ao_mov,
    };

    struct ASTNodeVisitor;

    /// Base class for all AST nodes.
    struct ASTNode {
        virtual ~ASTNode() {};

        // Has to be implemented in every derived class, or dynamic dispatch
        // wouldn't work.
        virtual void accept(ASTNodeVisitor& visitor) = 0;

        // Following are metadata about AST nodes.
        // TypePtr md_type;
    };

    typedef std::shared_ptr<ASTNode> ASTNodePtr;
    typedef std::vector<ASTNodePtr> ASTNodeList;

    /// AST node for blocks of instructions.
    struct Block: public ASTNode {
        Block(ASTNodeList statements):
            statements(statements) {}

        void accept(ASTNodeVisitor& visitor);

        ASTNodeList statements;
    };

    // /// Virtual class for AST declaration nodes.
    // struct Decl: public ASTNode {
    //     Decl(const std::string& name): name(name) {}
    //
    //     virtual ~Decl() {};
    //
    //     std::string name;
    // };
    //
    // /// AST node for property declarations.
    // struct PropertyDecl: public Decl {
    //     PropertyDecl(
    //         const std::string&   name,
    //         IdentifierMutability im = im_cst):
    //         Decl(name), mutability(im) {}
    //
    //     void accept(ASTNodeVisitor& visitor);
    //
    //     IdentifierMutability mutability;
    // };
    //
    // /// AST node for function parameters.
    // struct ParamDecl: public Decl {
    //     ParamDecl(
    //         const std::string&   name,
    //         IdentifierMutability im = im_cst):
    //         Decl(name), mutability(im) {}
    //
    //     void accept(ASTNodeVisitor& visitor);
    //
    //     IdentifierMutability mutability;
    // };
    //
    // /// AST node for function declarations.
    // struct FunctionDecl: public Decl {
    //     FunctionDecl(
    //         const std::string&                name,
    //         const std::vector<ParamDecl*>&    parameters,
    //         Block*                            body):
    //         Decl(name), parameters(parameters), body(body) {}
    //
    //     ~FunctionDecl();
    //
    //     void accept(ASTNodeVisitor& visitor);
    //
    //     std::vector<ParamDecl*> parameters;
    //     Block*                  body;
    //
    //     /// Lists the local captures of the function.
    //     std::vector<CapturedValue> capture_list;
    // };
    //
    // // AST node for assignments.
    // struct Assignment: public ASTNode {
    //     Assignment(
    //         ASTNode*           lvalue,
    //         AssignmentOperator op,
    //         ASTNode*           rvalue):
    //         lvalue(lvalue), op(op), rvalue(rvalue) {}
    //
    //     ~Assignment();
    //
    //     void accept(ASTNodeVisitor& visitor);
    //
    //     ASTNode*           lvalue;
    //     AssignmentOperator op;
    //     ASTNode*           rvalue;
    // };
    //
    // // AST node for conditional statements.
    // struct If: public ASTNode {
    //     If(
    //         ASTNode* condition,
    //         Block*   then_block,
    //         Block*   else_block):
    //         condition(condition), then_block(then_block), else_block(else_block) {}
    //
    //     ~If();
    //
    //     void accept(ASTNodeVisitor& visitor);
    //
    //     ASTNode* condition;
    //     Block*   then_block;
    //     Block*   else_block;
    // };
    //
    // /// AST node for return statements.
    // struct Return: public ASTNode {
    //     Return(ASTNode* value): value(value) {}
    //
    //     ~Return();
    //
    //     void accept(ASTNodeVisitor& visitor);
    //
    //     ASTNode* value;
    // };
    //
    // /// AST node for binary expressions.
    // struct BinaryExpr: public ASTNode {
    //     BinaryExpr(
    //         ASTNode* left,
    //         ASTNode* right,
    //         Operator op):
    //         left(left), right(right), op(op) {}
    //
    //     ~BinaryExpr();
    //
    //     void accept(ASTNodeVisitor& visitor);
    //
    //     ASTNode* left;
    //     ASTNode* right;
    //     Operator op;
    // };
    //
    // // AST node for call arguments.
    // struct CallArg: public ASTNode {
    //     CallArg(
    //         const std::string& label,
    //         AssignmentOperator op,
    //         ASTNode*           value):
    //         label(label), op(op), value(value) {}
    //
    //     ~CallArg();
    //
    //     void accept(ASTNodeVisitor& visitor);
    //
    //     std::string        label;
    //     AssignmentOperator op;
    //     ASTNode*           value;
    // };
    //
    // /// AST node for call expressions.
    // struct Call: public ASTNode {
    //     Call(
    //         ASTNode*                     callee,
    //         const std::vector<CallArg*>& arguments):
    //         callee(callee), arguments(arguments) {}
    //
    //     ~Call();
    //
    //     void accept(ASTNodeVisitor& visitor);
    //
    //     ASTNode* callee;
    //     std::vector<CallArg*> arguments;
    // };
    //
    // /// AST node for identifiers.
    // struct Identifier: public ASTNode {
    //     Identifier(const std::string& name): name(name) {}
    //
    //     void accept(ASTNodeVisitor& visitor);
    //
    //     std::string name;
    // };

    /// AST node for integer literals.
    struct IntegerLiteral: public ASTNode {
        IntegerLiteral(long value): value(value) {}

        void accept(ASTNodeVisitor& visitor);

        long value;
    };

    // /// AST node for boolean literals.
    // struct BooleanLiteral: public ASTNode {
    //     BooleanLiteral(bool value): value(value) {}
    //
    //     void accept(ASTNodeVisitor& visitor);
    //
    //     bool value;
    // };

    struct ASTNodeVisitor {
        virtual void visit(Block&          node) = 0;
        // virtual void visit(PropertyDecl&   node) = 0;
        // virtual void visit(ParamDecl&      node) = 0;
        // virtual void visit(FunctionDecl&   node) = 0;
        // virtual void visit(Assignment&     node) = 0;
        // virtual void visit(If&             node) = 0;
        // virtual void visit(Return&         node) = 0;
        // virtual void visit(BinaryExpr&     node) = 0;
        // virtual void visit(Call&           node) = 0;
        // virtual void visit(CallArg&        node) = 0;
        // virtual void visit(Identifier&     node) = 0;
        virtual void visit(IntegerLiteral& node) = 0;
        // virtual void visit(BooleanLiteral& node) = 0;
    };

    std::unique_ptr<tango::ASTNode> read_ast(std::ifstream&);

} // namespace tango
