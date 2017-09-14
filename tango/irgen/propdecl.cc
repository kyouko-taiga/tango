#include "irgen.hh"


namespace tango {

    void IRGenerator::visit(PropDecl& node) {
        // Get the LLVM type of the property.
        auto prop_type = node.meta.type->get_llvm_type(this->module.getContext());

        // Check whether we should declare a local or global variable. If
        // we're not generating the body of a function, we're looking at a
        // global variable.
        auto insert_block = builder.GetInsertBlock();
        if (insert_block == nullptr) {
            // Create a global variable.
            module.getOrInsertGlobal(node.name, prop_type);
            auto global_var = module.getNamedGlobal(node.name);
            global_var->setLinkage(llvm::GlobalVariable::CommonLinkage);

            // Store the variable in the global symbol table.
            globals[node.name] = global_var;
        } else {
            // Get the LLVM function under declaration.
            auto fun = insert_block->getParent();

            // Create an alloca for the variable, and store it as a local
            // symbol table.
            locals.top()[node.name] = create_alloca(fun, prop_type, node.name);
        }

        // TODO: Handle garbage collected variables.
    }

} // namespace tango
