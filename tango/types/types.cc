#include <llvm/IR/DerivedTypes.h>

#include "types.hh"


namespace tango {

    bool TypeUnion::operator==(const TypeBase& base_rhs) const {
        if (auto rhs = dynamic_cast<const TypeUnion*>(&base_rhs)) {
            return this->types == rhs->types;
        }
        return false;
    }

    bool operator==(const TypeVariable& lhs, const TypeVariable& rhs) {
        return (lhs.modifiers == rhs.modifiers)
            && (lhs.id == rhs.id);
    }

    bool operator==(const PlaceholderType& lhs, const PlaceholderType& rhs) {
        return (lhs.modifiers == rhs.modifiers)
            && (lhs.id == rhs.id)
            && (lhs.specialization == rhs.specialization);
    }

    bool operator==(const FunctionType& lhs, const FunctionType& rhs) {
        if ((lhs.modifiers != rhs.modifiers) || (lhs.domain.size() != rhs.domain.size())) {
            return false;
        }

        for (std::size_t i = 0; i < lhs.domain.size(); ++i) {
            if ((lhs.domain[i] != rhs.domain[i]) || (lhs.labels[i] != rhs.labels[i])) {
                return false;
            }
        }

        return lhs.codomain == rhs.codomain;
    }

    bool operator==(const NominalType& lhs, const NominalType& rhs) {

        // NOTE: It shouldn't be possible for this function to go on infinite
        // recursion, because it should only be used by `deep_equals`, when
        // the type factory checks whether or not it already registered an
        // equivalent type. And at that time, it isn't possible for the type
        // under construction to already reference itself. Once the type is
        // created, we'll never have to use this function again, as we'll rely
        // on pointer equality instead.

        // FIXME: Unfortunately that assumption doesn't hold. It's may be
        // desirable to build a nominal type even after it can reference itself
        // in two situations in the type solving phase:
        // 1. When struct types are reified.
        // 2. When struct types are being specialized.

        bool result = (lhs.modifiers == rhs.modifiers)
                   && (lhs.name == rhs.name)
                   && (lhs.members.size() == rhs.members.size());

        // Avoid further recursion if we already know the types don't match.
        if (!result) {
            return result;
        }

        // Recursively check for members equality.
        for (auto lit: lhs.members) {
            auto rit = rhs.members.find(lit.first);
            if ((rit == rhs.members.end()) || (lit.second != rit->second)) {
                return false;
            }
        }

        return true;
    }

    bool deep_equals(const TypeBase& lhs, const TypeBase& rhs) {
        if (auto lty = dynamic_cast<const TypeVariable*>(&lhs)) {
            if (auto rty = dynamic_cast<const TypeVariable*>(&rhs)) {
                return *lty == *rty;
            }
        }

        if (auto lty = dynamic_cast<const PlaceholderType*>(&lhs)) {
            if (auto rty = dynamic_cast<const PlaceholderType*>(&rhs)) {
                return *lty == *rty;
            }
        }

        if (auto lty = dynamic_cast<const FunctionType*>(&lhs)) {
            if (auto rty = dynamic_cast<const FunctionType*>(&rhs)) {
                return *lty == *rty;
            }
        }

        if (auto lty = dynamic_cast<const NominalType*>(&lhs)) {
            if (auto rty = dynamic_cast<const NominalType*>(&rhs)) {
                return *lty == *rty;
            }
        }

        return &lhs == &rhs;
    }

    // -----------------------------------------------------------------------

    bool TypeUnion::is_generic() const {
        for (auto t: this->types) {
            if (t->is_generic()) {
                return true;
            }
        }
        return false;
    }

    void TypeUnion::add(TypePtr t) {
        // Make sure we don't create nested unions.
        assert(!t->isa(tc_union) && "nested union");

        if (this->types.find(t) == this->types.end()) {
            this->types.insert(t);
        }
    }

    // -----------------------------------------------------------------------

    bool FunctionType::is_generic() const {
        for (auto t: this->domain) {
            if (t->is_generic()) {
                return true;
            }
        }
        return this->codomain->is_generic();
    }

    // -----------------------------------------------------------------------

    llvm::Type* TypeBase::llvm_type(llvm::LLVMContext& ctx) const {
        auto raw_type = this->llvm_raw_type(ctx);

        if (this->modifiers & tm_ref) {
            return llvm::PointerType::getUnqual(raw_type);
        }

        return raw_type;
    }

    // -----------------------------------------------------------------------

    llvm::Type* FunctionType::llvm_raw_type(llvm::LLVMContext& ctx) const {
        std::vector<llvm::Type*> arg_types;
        for (auto ty: this->domain) {
            arg_types.push_back(ty->llvm_type(ctx));
        }
        return llvm::FunctionType::get(this->codomain->llvm_type(ctx), arg_types, false);
    }

    // -----------------------------------------------------------------------

    llvm::Type* StructType::llvm_raw_type(llvm::LLVMContext& ctx) const {
        if (this->name == "Int") {
            return llvm::Type::getInt64Ty(ctx);
        } else if (this->name == "Bool") {
            return llvm::Type::getInt1Ty(ctx);
        }

        assert(false);
    }

} // namespace tango
