#include <boost/python.hpp>

#include "ast.hh"


namespace tango {

    void Block::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void ModuleDecl::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void PropDecl::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void ParamDecl::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void FunDecl::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void Assignment::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void If::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void Return::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void BinaryExpr::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void Argument::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void Call::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void Identifier::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void TypeIdentifier::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void FunSignParam::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void FunSign::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void IntLiteral::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void DoubleLiteral::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void StringLiteral::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    void BoolLiteral::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

}
