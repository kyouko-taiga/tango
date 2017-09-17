#include "irgen.hh"


namespace tango {

    void IRGenerator::visit(Assignment& node) {
        // NOTE: For now, we assume lvalues to always represent identifiers.
        // This'll change when we'll implement properties and subscripts.
        auto lvalue_node = std::dynamic_pointer_cast<Identifier>(node.lvalue);
        assert(lvalue_node != nullptr);
        auto lvalue = this->get_symbol_location(lvalue_node->name);

        llvm::Value* rvalue = nullptr;

        if (node.op == o_cpy) {
            // Generate the IR code for the rvalue.
            node.rvalue->accept(*this);
            rvalue = this->stack.top();
            this->stack.pop();

            // If the rvalue is a reference, we dereference it.
            if (node.rvalue->meta.type->modifiers & tm_ref) {
                rvalue = this->builder.CreateLoad(rvalue);
            }

            // If the lvalue is a reference, we dereference it.
            if (node.lvalue->meta.type->modifiers & tm_ref) {
                lvalue = this->builder.CreateLoad(lvalue);
            }
        } else if (node.op == o_ref) {
            // The lvalue should be a reference.
            assert(node.lvalue->meta.type->modifiers & tm_ref);

            // If the rvalue is a reference, we can load its value (i.e. the
            // value of the pointer).
            if (node.rvalue->meta.type->modifiers & tm_ref) {
                node.rvalue->accept(*this);
                rvalue = this->stack.top();
                this->stack.pop();

            // If the rvalue isn't a reference, it should be an identifier and
            // we should use the value of its alloca.
            } else {
                auto rvalue_node = std::dynamic_pointer_cast<Identifier>(node.rvalue);
                assert(rvalue_node != nullptr);
                rvalue = this->get_symbol_location(rvalue_node->name);
            }
        } else {
            // TODO: Handle move assignments.
            assert(false);
        }

        // Create a store instruction.
        this->builder.CreateStore(rvalue, lvalue);
    }

} // namespace tango
