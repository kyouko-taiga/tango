#pragma once

#include <algorithm>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include <boost/python.hpp>


namespace tango {

    enum TypeClass {
        tc_union     = 1 << 0,
        tc_name      = 1 << 1,
        tc_variable  = 1 << 2,
        tc_reference = 1 << 3,
        tc_function  = 1 << 4,
        tc_nominal   = 1 << 5,
        tc_builtin   = 1 << 6,
    };

    struct TypeBase {
        virtual ~TypeBase() {}

        virtual bool is_primitive() const = 0;
        virtual bool is_generic()   const = 0;
        virtual bool is_reference() const = 0;
        virtual bool isa(TypeClass) const = 0;

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
        bool operator==(const TypeBase& rhs) const {
            return this == &rhs;
        }

        // NOTE: We can return the address of the instance as the hash, since
        // there should never be two equilvalent instances in the program
        // outside of the factory's `make` function.
        std::size_t hash() const {
            return reinterpret_cast<const std::size_t>(this);
        }
    };

    typedef std::shared_ptr<TypeBase>      TypePtr;
    typedef std::weak_ptr<TypeBase>        WeakTypePtr;
    typedef std::vector<TypePtr>           TypeList;
    typedef std::map<std::string, TypePtr> TypeMap;

    // -----------------------------------------------------------------------

    struct TypeUnion: public TypeBase {
        TypeUnion(const TypeUnion&) = delete;
        TypeUnion(const TypeList& types = {});

        bool is_primitive()    const { return false; }
        bool is_generic()      const;
        bool is_reference()    const { return false; }
        bool isa(TypeClass tc) const { return tc & tc_union; }

        void    add(TypePtr);
        void    replace_content(const TypeList&);
        TypePtr first() const;

        TypeList types;
    };

    // -----------------------------------------------------------------------

    struct TypeName: public TypeBase {
        TypeName(const TypeName&) = delete;
        TypeName(const std::string& name, TypePtr type)
            : name(name), type(type) {}

            bool is_primitive()    const { return false; }
            bool is_generic()      const { return false; }
            bool is_reference()    const { return false; }
            bool isa(TypeClass tc) const { return tc & tc_name; }

            std::string name;
            TypePtr     type;
    };

    // -----------------------------------------------------------------------

    struct TypeVariable: public TypeBase {
        TypeVariable(const TypeVariable&) = delete;
        TypeVariable(boost::python::object id)
            : id(id) {}

        bool is_primitive()    const { return false; }

        // NOTE: We chose to always consider type variables non-generic. The
        // consequence of this choice is that whenever we visit a generic type
        // that has yet to be specialized, we have to type the expression that
        // uses it with another fresh variable.
        // Another approach would be to allow type variables to hold a
        // specialization list, so as to represent "some type specialized as
        // such". This would reduce the number of variables we have to create,
        // but would also make matching and unification harder.
        bool is_generic()      const { return false; }

        bool is_reference()    const { return false; }
        bool isa(TypeClass tc) const { return tc & tc_variable; }

        boost::python::object id;
    };

    // -----------------------------------------------------------------------

    struct ReferenceType: public TypeBase {
        ReferenceType(const ReferenceType&) = delete;
        ReferenceType(TypePtr rt)
            : referred_type(rt) {}

        bool is_primitive()    const { return true; }
        bool is_generic()      const { return referred_type->is_generic(); }
        bool is_reference()    const { return true; }
        bool isa(TypeClass tc) const { return tc & tc_reference; }

        TypePtr referred_type;
    };

    // -----------------------------------------------------------------------

    struct FunctionType: public TypeBase {
        FunctionType(const FunctionType&) = delete;
        FunctionType(
            const TypeList&                 domain   = {},
            const std::vector<std::string>& labels   = {},
            TypePtr                         codomain = nullptr)
            : domain(domain), labels(labels), codomain(codomain) {}

        bool is_primitive()    const { return false; }
        bool is_generic()      const { return false; }
        bool is_reference()    const { return false; }
        bool isa(TypeClass tc) const { return tc & tc_function; }

        TypeList                 domain;
        std::vector<std::string> labels;
        TypePtr                  codomain;
    };

    // -----------------------------------------------------------------------

    struct NominalType: public TypeBase {
        NominalType(const NominalType&) = delete;
        NominalType(const std::string& name)
            : name(name) {}

        virtual ~NominalType() {}

        bool is_reference()    const { return false; }
        bool isa(TypeClass tc) const { return tc & tc_nominal; }

        std::string name;
        TypeMap     members;
    };

    // -----------------------------------------------------------------------

    struct BuiltinType: public NominalType {
        BuiltinType(const BuiltinType&) = delete;
        BuiltinType(const std::string& name)
            : NominalType(name) {}

        bool is_primitive() const {
            return (name == "Int") || (name == "Double") || (name == "Bool");
        }

        bool is_generic()      const { return false; }
        bool isa(TypeClass tc) const { return tc & (tc_nominal | tc_builtin); }
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
