#include "irgen.hh"


namespace tango {

    void IRGenerator::visit(Assignment& node) {
        // TODO: Handle other assignments (cpy, ref, ...).
        assert(node.op == o_cpy);

        // Generate the IR code for the rvalue.
        node.rvalue->accept(*this);
        auto rvalue = this->stack.top();
        this->stack.pop();

        // NOTE: For now, we assume lvalues to always represent identifiers.
        // This'll change when we'll implement properties and subscripts.
        auto lvalue = std::dynamic_pointer_cast<Identifier>(node.lvalue);
        assert(lvalue != nullptr);
        auto lvalue_location = this->get_symbol_location(lvalue->name);

        // If the lvalue is a reference, we dereference its location.
        if (node.lvalue->meta.type->modifiers & tm_ref) {
            lvalue_location = this->builder.CreateLoad(lvalue_location);
        }

        // If the rvalue is a reference, we reference it.
        if (node.rvalue->meta.type->modifiers & tm_ref) {
            rvalue = this->builder.CreateLoad(rvalue);
        }

        // Create a store instruction.
        this->builder.CreateStore(rvalue, lvalue_location);
    }

} // namespace tango
