#pragma once

#include "tango/ast/ast.hh"

namespace tango {

    struct IRGenerator: public ASTNodeVisitor {
        void visit(Block&          node) {}
        void visit(ModuleDecl&     node) {}
        void visit(PropDecl&       node) {}
        void visit(ParamDecl&      node) {}
        void visit(FunDecl&        node) {}
        void visit(Assignment&     node) {}
        void visit(If&             node) {}
        void visit(Return&         node) {}
        void visit(BinaryExpr&     node) {}
        void visit(CallArg&        node) {}
        void visit(Call&           node) {}
        void visit(Identifier&     node) {}
        void visit(TypeIdentifier& node) {}
        void visit(FunSignParam&   node) {}
        void visit(FunSign&        node) {}
        void visit(IntLiteral&     node) {}
        void visit(StringLiteral&  node) {}
    };

} // namespace tango
