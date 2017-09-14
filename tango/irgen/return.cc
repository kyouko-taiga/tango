#include "irgen.hh"


namespace tango {

    void IRGenerator::visit(Return& node) {
        // Make sure we're generating a function's body.
        assert(builder.GetInsertBlock() != nullptr);

        // Generate the IR code of the return value.
        node.value->accept(*this);
        auto rv = this->stack.top();
        this->stack.pop();

        // Dereference rv if it's a reference.
        if (node.value->meta.type->modifiers & tm_ref) {
            rv = builder.CreateLoad(rv);
        }

        // Store it on the return alloca.
        builder.CreateStore(rv, return_alloca.top());
    }

} // namespace tango
