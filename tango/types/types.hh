#pragma once

#include <algorithm>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

#include <boost/python.hpp>


namespace llvm {

    class LLVMContext;
    class Type;

} // namespace llvm


namespace tango {

    enum TypeClass {
        tc_union       = 1 << 0,
        tc_name        = 1 << 1,
        tc_variable    = 1 << 2,
        tc_placeholder = 1 << 3,
        tc_reference   = 1 << 4,
        tc_function    = 1 << 5,
        tc_nominal     = 1 << 6,
        tc_builtin     = 1 << 7,
        tc_struct      = 1 << 8,
    };

    // Type identifier attributes.
    enum TypeModifier {
        tm_cst = 1 << 0,
        tm_mut = 1 << 1,
        tm_stk = 1 << 2,
        tm_shd = 1 << 3,
        tm_val = 1 << 4,
        tm_ref = 1 << 5,
        tm_own = 1 << 6,
    };

    struct TypeBase {
        TypeBase(uint8_t modifiers = 0)
            : modifiers(modifiers) {}

        virtual ~TypeBase() {}

        virtual bool is_generic()   const { return false; }
        virtual bool isa(TypeClass) const = 0;

        /// Returns the underlying LLVM type implementing this Tango type.
        ///
        /// This method relies on derived implementations of `llvm_raw_type`
        /// to produce the LLVM type corresponding to this Tango type without
        /// modifiers (i.e. stacked value type), and then decorates it to
        /// implement the Tango type modifiers.
        ///
        /// Consider for instance the Tango type `@ref Int`. `llvm_raw_type`
        /// will return `i64` and this method will create a pointer to that
        /// that type to finally return `i64*`.
        llvm::Type* llvm_type(llvm::LLVMContext&) const;

        virtual llvm::Type* llvm_raw_type(llvm::LLVMContext&) const {
            return nullptr;
        }

        // NOTE: In order to simplify equality check between type classes,
        // we maintain a unicity table in the TypeFactory, which lets us use
        // pointer equality.
        // To ensure that we don't create duplicate instances of the same, the
        // TypeFactory creates a new instance of the requested class, and then
        // checks whether or not it is equal to another registered instances.
        // This equality check requires an actual "memberwise" comparison of
        // the type class instances, which is performed by the `deep_equals`
        // function, which dispatches the call to the correct implementation
        // of `==`, using dynamic casting (see implementation file).
        // In C++, because we only manipulate shared pointers to type class
        // instances, this ultimately lets us use `==` directly between
        // std::shared_ptr instances. In Python, we expose this implementation
        // of `==` so that we can check whether pointed objects are the same,
        // rather than comparing instances of std::shared_ptr, which is what
        // Python would default to.
        virtual bool operator==(const TypeBase& rhs) const {
            return this == &rhs;
        }

        uint8_t modifiers;
    };

    typedef std::shared_ptr<TypeBase>      TypePtr;
    typedef std::weak_ptr<TypeBase>        WeakTypePtr;
    typedef std::vector<TypePtr>           TypeList;
    typedef std::map<std::string, TypePtr> TypeMap;

    // -----------------------------------------------------------------------

    struct TypeFactory;

    struct TypeUnion: public TypeBase {
        TypeUnion(const TypeUnion&) = delete;
        TypeUnion() {}

        bool is_generic()      const;
        bool isa(TypeClass tc) const { return tc & tc_union; }

        // NOTE: TypeUnions aren't stored in the unicity table of the TypeFactory
        // because they need to be mutable. As a result, we've to overload the
        // TypeBase's equality check, as it relies on pointer comparison.
        bool operator==(const TypeBase&) const;

        void add(TypePtr);

        std::unordered_set<TypePtr> types;
    };

    // -----------------------------------------------------------------------

    struct TypeName: public TypeBase {
        TypeName(const TypeName&) = delete;
        TypeName(const std::string& name, TypePtr type)
            : name(name), type(type) {}

            bool isa(TypeClass tc) const { return tc & tc_name; }

            std::string name;
            TypePtr     type;
    };

    // -----------------------------------------------------------------------

    struct TypeVariable: public TypeBase {
        TypeVariable(const TypeVariable&) = delete;
        TypeVariable(uint8_t modifiers, boost::python::object id)
            : TypeBase(modifiers), id(id) {}

        // NOTE: We chose to always consider type variables non-generic. The
        // consequence of this choice is that whenever we visit a generic type
        // that has yet to be specialized, we have to type the expression that
        // uses it with another fresh variable.
        // Another approach would be to allow type variables to hold a
        // specialization list, so as to represent "some type specialized as
        // such". This would reduce the number of variables we have to create,
        // but would also make matching and unification harder.
        // bool is_generic() const { return false; }

        bool isa(TypeClass tc) const { return tc & tc_variable; }

        boost::python::object id;
    };

    // -----------------------------------------------------------------------

    struct PlaceholderType: public TypeBase {
        PlaceholderType(const TypeName&) = delete;
        PlaceholderType(
            uint8_t modifiers,
            boost::python::object id,
            TypePtr specialization = nullptr)
            : TypeBase(modifiers), id(id), specialization(specialization) {}

            bool is_generic() const {
                return specialization == nullptr
                    ? true
                    : specialization->is_generic();
            }

            bool isa(TypeClass tc) const { return tc & tc_placeholder; }

            boost::python::object id;
            TypePtr               specialization;
    };

    // -----------------------------------------------------------------------

    struct FunctionType: public TypeBase {
        FunctionType(const FunctionType&) = delete;
        FunctionType(
            uint8_t                         modifiers,
            const TypeList&                 domain   = {},
            const std::vector<std::string>& labels   = {},
            TypePtr                         codomain = nullptr)
            : TypeBase(modifiers),
              domain(domain), labels(labels), codomain(codomain) {}

        bool is_generic()      const;
        bool isa(TypeClass tc) const { return tc & tc_function; }

        llvm::Type* llvm_raw_type(llvm::LLVMContext&) const;

        TypeList                 domain;
        std::vector<std::string> labels;
        TypePtr                  codomain;
    };

    // -----------------------------------------------------------------------

    struct NominalType: public TypeBase {
        NominalType(const NominalType&) = delete;
        NominalType(uint8_t modifiers, const std::string& name, const TypeMap& members)
            : TypeBase(modifiers), name(name), members(members) {}

        virtual ~NominalType() {}

        bool isa(TypeClass tc) const { return tc & tc_nominal; }

        std::string name;
        TypeMap     members;
    };

    // -----------------------------------------------------------------------

    struct StructType: public NominalType {
        StructType(const StructType&) = delete;
        StructType(uint8_t modifiers, const std::string& name, const TypeMap& members)
            : NominalType(modifiers, name, members) {}

        bool isa(TypeClass tc) const { return tc & tc_struct; }

        llvm::Type* llvm_raw_type(llvm::LLVMContext&) const;
    };

    // -----------------------------------------------------------------------

    bool deep_equals(const TypeBase&, const TypeBase&);

    struct TypeFactory {
        template<typename T, typename ... Args>
        std::shared_ptr<T> make(Args&& ... args) {
            auto type_ptr = std::make_shared<T>(std::forward<Args>(args)...);
            for (auto it = types.begin(); it != types.end();) {
                if (it->expired()) {
                    it = types.erase(it);
                } else if (deep_equals(*(it->lock()), *type_ptr)) {
                    return std::static_pointer_cast<T>(it->lock());
                } else {
                    it++;
                }
            }

            types.push_back(type_ptr);
            return type_ptr;
        }

        std::vector<WeakTypePtr> types;
        std::size_t              next_variable_id = 0;
    };

} // namespace tango
