#include <llvm/IR/Verifier.h>

#include "irgen.hh"


namespace tango {

    void emit_function_body(
        FunDecl&       node,
        llvm::Function*     fun,
        llvm::FunctionType* fun_type,
        IRGenerator&        gen)
    {
        // Create a new basic block to start insertion into.
        auto bb = llvm::BasicBlock::Create(gen.module.getContext(), "entry", fun);
        auto ib = gen.builder.GetInsertBlock();
        gen.builder.SetInsertPoint(bb);

        // Store the alloca the return value.
        gen.return_alloca.push(create_alloca(fun, fun_type->getReturnType(), "rv"));

        // Store the function parameters in its local symbol table.
        IRGenerator::LocalSymbolTable fun_locals;
        for (auto& arg: fun->args()) {
            // Create an alloca for the argument, and store its value.
            auto alloca = create_alloca(fun, arg.getType(), arg.getName());
            gen.builder.CreateStore(&arg, alloca);
            fun_locals[arg.getName()] = alloca;
        }

        gen.locals.push(std::move(fun_locals));

        // Generate the body of the function.
        node.body->accept(gen);
        gen.builder.CreateRet(gen.builder.CreateLoad(gen.return_alloca.top()));

        gen.return_alloca.pop();
        gen.locals.pop();

        if (ib == nullptr) {
            gen.builder.ClearInsertionPoint();
        } else {
            gen.builder.SetInsertPoint(ib);
        }

        // Validate the generated code, checking for consistency.
        llvm::verifyFunction(*fun);
    }

    void emit_global_function(FunDecl& node, IRGenerator& gen) {
        // Global function don't need to be lifted, as they only capture
        // other global symbols.
        llvm::FunctionType* fun_type = static_cast<llvm::FunctionType*>(
            node.meta.type->llvm_type(gen.module.getContext()));

        // Create the LLVM function prototype.
        auto fun = llvm::Function::Create(
            fun_type, llvm::Function::ExternalLinkage, node.name, &gen.module);
        fun->addFnAttr(llvm::Attribute::NoUnwind);

        // Set the name of the function arguments.
        std::size_t idx = 0;
        for (auto& arg: fun->args()) {
            auto parameter = std::static_pointer_cast<ParamDecl>(node.parameters[idx++]);
            arg.setName(parameter->name);
        }

        // Generate the function body.
        // gen.local_captures.push(IRGenerator::LocalCaptures());
        emit_function_body(node, fun, fun_type, gen);
        // gen.local_captures.pop();
    }

    void emit_nested_function(FunDecl& node, IRGenerator& gen) {
        // auto& ctx = gen.module.getContext();
        //
        // // NOTE: We lift a nested function by adding a closure instance to its
        // // parameter, which contain a pointer to the (global) function,
        // // as well as references to the captured values of its free variables.
        // // We use references to captured values rather than the a copies (even
        // // for primitive types) to handle mutable captures.
        // // There are two opportunities of optimization here:
        // // * Non-mutable captures could be passed by values, so as to avoid
        // //   unecessary load statements.
        // // * Non-recursive and non-escaping functions may be lifted by simply
        // //   adding their free variables to their parameter, so as to avoid
        // //   the allocation of a function environment.
        //
        // // Create the type of the function.
        // llvm::FunctionType* fun_type = std::static_pointer_cast<FunctionType>(node.get_type())
        //     ->get_llvm_lifted_type(ctx, gen.tango_types.closure_t);
        //
        // // We need to keep track of which local symbols correspond to captured
        // // values, so we can dereference them from the environment during the
        // // IR generation of the function's body.
        // IRGenerator::LocalCaptures fun_local_captures;
        //
        // // Create the type of the function's enivornment.
        // std::vector<llvm::Type*> env_members;
        // for (auto val: node.capture_list) {
        //     auto free_type = val.decl->get_type()->get_llvm_type(ctx);
        //     env_members.push_back(llvm::PointerType::getUnqual(free_type));
        //     fun_local_captures.push_back(val.decl->name);
        // }
        // fun_local_captures.push_back(node.name);
        // llvm::StructType* env_type = llvm::StructType::create(
        //     ctx, env_members, node.name + "env_t");
        //
        // // Create the LLVM function prototype.
        // auto fun = llvm::Function::Create(
        //     fun_type, llvm::Function::PrivateLinkage, node.name, &gen.module);
        // fun->addFnAttr(llvm::Attribute::NoUnwind);
        //
        // // Set the name of the function arguments.
        // auto arg_it = fun->arg_begin();
        // arg_it->setName(node.name);
        // arg_it++;
        // std::size_t idx = 0;
        // while (arg_it != fun->arg_end()) {
        //     arg_it->setName(node.parameters[idx++]->name);
        //     arg_it++;
        // }
        //
        // // Create a local symbol representing the first-class function object
        // auto current_fun    = gen.builder.GetInsertBlock()->getParent();
        // auto closure_alloca = create_alloca(current_fun, gen.tango_types.closure_t, node.name);
        //
        // // Store the closure info.
        // gen.closures[node.name] = ClosureInfo(
        //     &node, llvm::PointerType::getUnqual(fun_type), env_type);
        // gen.locals.top()[node.name] = closure_alloca;
        //
        // // Store the function pointer.
        // // %0 = getelementptr %closure_t, %closure_t* %<fun_name>, i32 0, i32 0
        // // store i8* bitcast (<fun_ptr> @<fun_name> to i8*), i8** %0
        // auto zero = gen.get_gep_index(0);
        // gen.builder.CreateStore(
        //     gen.builder.CreateBitCast(fun, gen.tango_types.voidp_t),
        //     gen.builder.CreateGEP(closure_alloca, {zero, zero}));
        //
        // // If the function isn't escaping, we can allocate its environment on
        // // the stack.
        // idx             = 0;
        // auto env_alloca = create_alloca(current_fun, env_type, node.name + "env");
        // for (auto val: node.capture_list) {
        //     gen.builder.CreateStore(
        //         gen.get_symbol_location(val.decl->name),
        //         gen.builder.CreateGEP(env_alloca, {zero, gen.get_gep_index(idx)}));
        // }
        //
        // // %1 = getelementptr %closure_t, %closure_t* %<fun_name>, i32 0, i32 1
        // // store i8* null, i8** %1
        // gen.builder.CreateStore(
        //     gen.builder.CreateBitCast(env_alloca, gen.tango_types.voidp_t),
        //     gen.builder.CreateGEP(closure_alloca, {zero, gen.get_gep_index(1)}));
        //
        // // TODO: Handle escaping closures.
        //
        // // Generate the function body.
        // gen.local_captures.push(std::move(fun_local_captures));
        // emit_function_body(node, fun, fun_type, gen);
        // gen.local_captures.pop();
    }

    void IRGenerator::visit(FunDecl& node) {
        // Determine whether we should generate a global or nested function.
        bool is_gloal = (builder.GetInsertBlock() == nullptr);

        if (is_gloal) {
            emit_global_function(node, *this);
        } else {
            emit_nested_function(node, *this);
        }
    }

} // namespace tango
