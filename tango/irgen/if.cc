#include "irgen.hh"


namespace tango {

    void IRGenerator::visit(If& node) {
        // Generate the IR code for the condition.
        node.condition->accept(*this);
        auto condition = this->stack.top();
        this->stack.pop();

        auto fun = this->builder.GetInsertBlock()->getParent();

        // Create blocks for the then and else clauses.
        auto then_block = llvm::BasicBlock::Create(this->module.getContext(), "then", fun);
        auto else_block = llvm::BasicBlock::Create(this->module.getContext(), "else");
        auto cont_block = llvm::BasicBlock::Create(this->module.getContext(), "cont");

        // Create the branch statement.
        this->builder.CreateCondBr(condition, then_block, else_block);

        // Generate the IR code for the then clause.
        this->builder.SetInsertPoint(then_block);
        node.then_block->accept(*this);
        this->builder.CreateBr(cont_block);

        then_block = this->builder.GetInsertBlock();

        // Generate the IR code for the else clause.
        fun->getBasicBlockList().push_back(else_block);
        this->builder.SetInsertPoint(else_block);
        node.else_block->accept(*this);
        this->builder.CreateBr(cont_block);

        else_block = this->builder.GetInsertBlock();

        // Generate the IR for the continuation block.
        fun->getBasicBlockList().push_back(cont_block);
        this->builder.SetInsertPoint(cont_block);
    }

} // namespace tango
