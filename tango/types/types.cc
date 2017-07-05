#include "types.hh"


namespace tango {

    bool operator==(const TypeVariable& lhs, const TypeVariable& rhs) {
        return lhs.id == rhs.id;
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
        return (lhs.modifiers == rhs.modifiers) && (lhs.name == rhs.name);
    }

    bool deep_equals(const TypeBase& lhs, const TypeBase& rhs) {
        if (auto lty = dynamic_cast<const TypeVariable*>(&lhs)) {
            if (auto rty = dynamic_cast<const TypeVariable*>(&rhs)) {
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

    bool TypeUnion::operator==(const TypeBase& base_rhs) const {
        if (auto rhs = dynamic_cast<const TypeUnion*>(&base_rhs)) {
            return this->types == rhs->types;
        }
        return false;
    }

    void TypeUnion::add(TypePtr t) {
        if (this->types.find(t) == this->types.end()) {
            this->types.insert(t);
        }
    }

} // namespace tango
