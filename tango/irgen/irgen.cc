#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>

// Optimizations.
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/Scalar.h>

#include "irgen.hh"


namespace tango {

    void emit_ir(ModuleDecl& ast) {
        llvm::LLVMContext context;
        llvm::IRBuilder<> builder(context);
        llvm::Module      module(ast.name, context);

        auto irgen = IRGenerator(module, builder);
        // irgen.add_main_function()
        ast.accept(irgen);
        // irgen.finish_main_function();

        // Create an optimization pass manager.
        auto pass_manager = llvm::make_unique<llvm::legacy::PassManager>();
        pass_manager->add(llvm::createPromoteMemoryToRegisterPass());
        pass_manager->run(module);

        module.dump();
    }

    IRGenerator::IRGenerator(llvm::Module& mod, llvm::IRBuilder<>& irb):
        module(mod), builder(llvm::IRBuilder<>(mod.getContext())) {}

    llvm::Value* IRGenerator::get_symbol_location(const std::string& name) {
        if (!locals.empty()) {
            auto it = locals.top().find(name);
            if (it != locals.top().end()) {
                return it->second;
            }
        }

        auto it = globals.find(name);
        if (it != globals.end()) {
            return it->second;
        }

        assert(false && "undefined symbol");
    }

    void IRGenerator::visit(Block& node) {
        for (auto statement: node.statements) {
            statement->accept(*this);
        }
    }

    void IRGenerator::visit(ModuleDecl& node) {
        node.body->accept(*this);
    }

    void IRGenerator::visit(Identifier& node) {
        // Look for the identifier in the local/global symbol tables.
        auto loc = this->get_symbol_location(node.name);
        this->stack.push(builder.CreateLoad(loc, node.name.c_str()));
    }

    void IRGenerator::visit(IntLiteral& node) {
        this->stack.push(
            llvm::ConstantInt::get(this->module.getContext(), llvm::APInt(64, node.value, true)));
    }

    llvm::AllocaInst* create_alloca(
        llvm::Function*    fun,
        llvm::Type*        type,
        const std::string& name)
    {
        llvm::IRBuilder<> tmp_builder(&fun->getEntryBlock(), fun->getEntryBlock().begin());
        return tmp_builder.CreateAlloca(type, 0, name);
    }

} // namespace tango
