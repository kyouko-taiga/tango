#include "irgen.hh"


namespace tango {

    void IRGenerator::visit(Call& node) {
        // Get the callee object.
        auto callee = std::dynamic_pointer_cast<Identifier>(node.callee);

        // TODO: Handle non-identifier callees.
        assert(callee != nullptr);

        // Get the function.
        auto fun = module.getFunction(callee->name);

        // Set the function arguments.
        std::vector<llvm::Value*> args;
        for (auto child: node.arguments) {
            auto arg = std::dynamic_pointer_cast<Argument>(child);
            assert(arg != nullptr);
            arg->value->accept(*this);
            args.push_back(stack.top());
            stack.pop();
        }

        // Emit the function call.
        stack.push(builder.CreateCall(fun, args));
    }

} // namespace tango
