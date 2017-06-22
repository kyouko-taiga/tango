#pragma once

#include <memory>
#include <string>
#include <vector>


namespace llvm {

    class FunctionType;
    class LLVMContext;
    class PointerType;
    class Type;
    class StructType;

} // namespace llvm


namespace tango {

    struct TypeBase {
        virtual ~TypeBase() {}

        virtual bool is_primitive() const = 0;
        virtual bool is_generic()   const = 0;
        virtual bool is_reference() const = 0;

        virtual llvm::Type* get_llvm_type(llvm::LLVMContext&) const = 0;
    };

    typedef std::shared_ptr<TypeBase> TypePtr;

    // -----------------------------------------------------------------------

    struct RefType: public TypeBase {
        RefType(TypePtr rt): referred_type(rt) {}

        bool is_primitive() const { return true; }
        bool is_generic()   const { return this->referred_type->is_generic(); }
        bool is_reference() const { return true; }

        llvm::Type* get_llvm_type(llvm::LLVMContext&) const;

        TypePtr referred_type;

        static TypePtr get(TypePtr rt) {
            return std::make_shared<RefType>(rt);
        }
    };

    // -----------------------------------------------------------------------

    struct NominalType: public TypeBase {
        NominalType(const std::string& name): name(name) {}

        virtual ~NominalType() {}

        bool is_generic()   const { return false; }
        bool is_reference() const { return false; }

        const std::string name;
    };

    // -----------------------------------------------------------------------

    struct FunctionType: public TypeBase {
        FunctionType(const FunctionType&) = delete;
        FunctionType(
            const std::vector<TypePtr>&     domain,
            const std::vector<std::string>& labels,
            TypePtr                         codomain):
            domain(domain), labels(labels), codomain(codomain) {}

        bool is_primitive() const { return false; }
        bool is_generic()   const { return false; }
        bool is_reference() const { return false; }

        llvm::Type* get_llvm_type(llvm::LLVMContext&) const;

        /// Same as #get_llvm_type, but adding a pointer to a closure instance
        /// as its first parameter.
        llvm::FunctionType* get_llvm_lifted_type(
            llvm::LLVMContext&, llvm::StructType* closure_type) const;

        std::vector<TypePtr>     domain;
        std::vector<std::string> labels;
        TypePtr                  codomain;

        static TypePtr get(
            const std::vector<TypePtr>&     domain,
            const std::vector<std::string>& labels,
            TypePtr                         codomain)
        {
            return std::make_shared<FunctionType>(domain, labels, codomain);
        }
    };

    // -----------------------------------------------------------------------

    struct IntType: public NominalType {
        IntType(): NominalType("Int") {}

        bool is_primitive() const { return true; }

        llvm::Type* get_llvm_type(llvm::LLVMContext&) const;

        static TypePtr get() {
            return std::make_shared<IntType>();
        }
    };

} // namespace tango
