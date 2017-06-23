#pragma once

#include <memory>
#include <string>
#include <vector>


namespace tango {

    struct TypeBase {
        virtual ~TypeBase() {}

        virtual bool is_primitive() const = 0;
        virtual bool is_generic()   const = 0;
        virtual bool is_reference() const = 0;
    };

    typedef std::shared_ptr<TypeBase> TypePtr;
    typedef std::vector<TypePtr>      TypeList;

    // -----------------------------------------------------------------------

    struct ReferenceType: public TypeBase {
        ReferenceType(const ReferenceType&) = delete;
        ReferenceType(TypePtr rt)
            : referred_type(rt) {}

        bool is_primitive() const { return true; }
        bool is_generic()   const { return referred_type->is_generic(); }
        bool is_reference() const { return true; }

        TypePtr referred_type;
    };

    // -----------------------------------------------------------------------

    struct FunctionType: public TypeBase {
        FunctionType(const FunctionType&) = delete;
        FunctionType(
            const TypeList&                 domain,
            const std::vector<std::string>& labels,
            TypePtr                         codomain):
            domain(domain), labels(labels), codomain(codomain) {}

        bool is_primitive() const { return false; }
        bool is_generic()   const { return false; }
        bool is_reference() const { return false; }

        TypeList                 domain;
        std::vector<std::string> labels;
        TypePtr                  codomain;
    };

    // -----------------------------------------------------------------------

    struct NominalType: public TypeBase {
        NominalType(const std::string& name)
            : name(name) {}

        virtual ~NominalType() {}

        bool is_reference() const { return false; }

        std::string name;
    };

    // -----------------------------------------------------------------------

    struct BuiltinType: public NominalType {
        // BuiltinType(const BuiltinType&) = delete;
        BuiltinType(const std::string& name)
            : NominalType(name) {}

        bool is_primitive() const {
            return (name == "Int") || (name == "Double") || (name == "Bool");
        }

        bool is_generic() const { return false; }
    };

} // namespace tango
