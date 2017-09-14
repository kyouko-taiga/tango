#pragma once

#include <stack>
#include <string>
#include <unordered_map>
#include <vector>
#include <llvm/IR/IRBuilder.h>

#include "tango/ast/ast.hh"


namespace llvm {

    class AllocaInst;
    class Function;
    class GlobalVariable;
    class Module;
    class Type;
    class Value;

} // namespace llvm


namespace tango {

    void emit_ir(ModuleDecl&);

    struct IRGenerator: public ASTNodeVisitor {

        typedef std::vector<std::string>                               LocalCaptures;
        typedef std::unordered_map<std::string, llvm::AllocaInst*>     LocalSymbolTable;
        typedef std::unordered_map<std::string, llvm::GlobalVariable*> GlobalSymbolTable;

        /// A reference to the LLVM module being generated.
        llvm::Module& module;

        /// The main LLVM IR builder.
        llvm::IRBuilder<> builder;

        /// A stack that lets us accumulate the LLVM values of expressions,
        /// before they are consumed by a statement.
        ///
        /// The stack should be emptied every time after the IR of a
        /// particular statement has been generated.
        std::stack<llvm::Value*> stack;

        /// A stack of maps of local symbols.
        std::stack<LocalSymbolTable> locals;

        /// A map of global symbols.
        GlobalSymbolTable globals;

        /// A stack of pointers to the alloca that represent the return space
        /// of the function declaration being visited.
        std::stack<llvm::AllocaInst*> return_alloca;

        /// A stack of Tango types that represent the return type of the
        /// function declaration being visited.
        std::stack<TypePtr> return_type;

        IRGenerator(llvm::Module&, llvm::IRBuilder<>&);

        /// Returns the location of a symbol from the local or global table.
        llvm::Value* get_symbol_location(const std::string& name);

        void visit(Block&);
        void visit(ModuleDecl&);
        void visit(PropDecl&);
        void visit(ParamDecl&      node) {}
        void visit(FunDecl&);
        void visit(Assignment&     node) {}
        void visit(If&             node) {}
        void visit(Return&);
        void visit(BinaryExpr&     node) {}
        void visit(CallArg&        node) {}
        void visit(Call&           node) {}
        void visit(Identifier&);
        void visit(TypeIdentifier& node) {}
        void visit(FunSignParam&   node) {}
        void visit(FunSign&        node) {}
        void visit(IntLiteral&     node) {}
        void visit(StringLiteral&  node) {}

    };

    /// Create an alloca instruction in the enty block of the function.
    llvm::AllocaInst* create_alloca(
        llvm::Function*    function,
        llvm::Type*        type,
        const std::string& name);

} // namespace tango
