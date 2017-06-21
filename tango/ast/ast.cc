#include "ast.hh"


namespace tango {

    void Block::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

    // -----------------------------------------------------------------------

    void IntegerLiteral::accept(ASTNodeVisitor& visitor) {
        visitor.visit(*this);
    }

}
