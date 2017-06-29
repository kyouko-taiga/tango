#include <iostream>
#include "types.hh"


namespace tango {

    bool operator==(const TypeUnion& lhs, const TypeUnion& rhs) {
        if (lhs.types.size() != rhs.types.size()) {
            return false;
        }

        for (auto t: lhs.types) {
            auto it = std::find_if(
                rhs.types.begin(),
                rhs.types.end(),
                [t](TypePtr const& u) { return *t == *u; });
            if (it == rhs.types.end()) {
                return false;
            }
        }

        return true;
    }

    bool operator==(const ReferenceType& lhs, const ReferenceType& rhs) {
        return lhs.referred_type == rhs.referred_type;
    }

    bool operator==(const FunctionType& lhs, const FunctionType& rhs) {
        if (lhs.domain.size() != rhs.domain.size()) {
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
        return lhs.name == rhs.name;
    }

    bool TypeBase::operator==(const TypeBase& rhs) const {
        if (auto lty = dynamic_cast<const TypeUnion*>(this)) {
            if (auto rty = dynamic_cast<const TypeUnion*>(&rhs)) {
                return *lty == *rty;
            }
        }

        if (auto lty = dynamic_cast<const ReferenceType*>(this)) {
            if (auto rty = dynamic_cast<const ReferenceType*>(&rhs)) {
                return *lty == *rty;
            }
        }

        if (auto lty = dynamic_cast<const FunctionType*>(this)) {
            if (auto rty = dynamic_cast<const FunctionType*>(&rhs)) {
                return *lty == *rty;
            }
        }

        if (auto lty = dynamic_cast<const NominalType*>(this)) {
            if (auto rty = dynamic_cast<const NominalType*>(&rhs)) {
                return *lty == *rty;
            }
        }

        return this == &rhs;
    }

    // -----------------------------------------------------------------------

    TypeUnion::TypeUnion(const TypeList& types) {
        // We can't use a set to store the types of the union, as not all
        // types are hashable. There isn't a total order relationship on them
        // neither, so our only option is a O(n^2) algorithm.
        for (auto t: types) {
            this->add(t);
        }
    }

    bool TypeUnion::is_generic() const {
        for (auto t: this->types) {
            if (t->is_generic()) {
                return true;
            }
        }
        return false;
    }

    void TypeUnion::add(TypePtr t) {
        auto it = std::find_if(
            this->types.begin(),
            this->types.end(),
            [t](TypePtr const& u) { return t.get() == u.get(); });

        if (it == this->types.end()) {
            this->types.push_back(t);
        }
    }

    void TypeUnion::replace_content(const TypeList& types) {
        this->types.clear();
        for (auto t: types) {
            this->add(t);
        }
    }

    TypePtr TypeUnion::first() const {
        if (this->types.size() > 0) {
            return this->types[0];
        }
        return nullptr;
    }

    // bool TypeUnion::operator==(const TypeUnion& rhs) const {
    //     if (rhs.types.size() != this->types.size()) {
    //         return false;
    //     }
    //
    //     for (auto t: this->types) {
    //         auto it = std::find_if(
    //             rhs.types.begin(),
    //             rhs.types.end(),
    //             [t](TypePtr const& u) { return t.get() == u.get(); });
    //         if (it == rhs.types.end()) {
    //             return false;
    //         }
    //     }
    //
    //     return true;
    // }
    //
    // // -----------------------------------------------------------------------
    //
    // bool FunctionType::operator==(const FunctionType& rhs) const {
    //     if (rhs.domain.size() != this->domain.size()) {
    //         return false;
    //     }
    //
    //     for (std::size_t i = 0; i < rhs.domain.size(); ++i) {
    //         if ((rhs.domain[i] != this->domain[i]) || (rhs.labels[i] != this->labels[i])) {
    //             return false;
    //         }
    //     }
    //
    //     return rhs.codomain == this->codomain;
    // }
    //
    // // -----------------------------------------------------------------------
    //
    // bool NominalType::operator==(const NominalType& rhs) const {
    //     if (rhs.name != this->name) {
    //         return false;
    //     }
    //
    //     if (rhs.members.size() != this->members.size()) {
    //         return false;
    //     }
    //
    //     for (auto i: this->members) {
    //         auto j = rhs.members.find(i.first);
    //         if ((j == rhs.members.end()) || !(*(i.second) == *(j->second))) {
    //             return false;
    //         }
    //     }
    //
    //     return true;
    // }

} // namespace tango
