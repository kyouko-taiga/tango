#pragma once

#include <algorithm>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>


namespace tango {

    enum TypeClass {
        tc_union     = 1 << 0,
        tc_reference = 1 << 2,
        tc_function  = 1 << 3,
        tc_nominal   = 1 << 4,
        tc_builtin   = 1 << 5,
    };

    struct TypeBase {
        virtual ~TypeBase() {}

        virtual bool is_primitive() const = 0;
        virtual bool is_generic()   const = 0;
        virtual bool is_reference() const = 0;
        virtual bool isa(TypeClass) const = 0;

        bool operator==(const TypeBase&) const;
    };

    typedef std::shared_ptr<TypeBase>      TypePtr;
    typedef std::weak_ptr<TypeBase>        WeakTypePtr;
    typedef std::vector<TypePtr>           TypeList;
    typedef std::map<std::string, TypePtr> TypeMap;

    // -----------------------------------------------------------------------

    struct TypeUnion: public TypeBase {
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

    struct TypeFactory {
        template<typename T, typename ... Args>
        std::shared_ptr<T> make_type(Args&& ... args) {
            auto type_ptr = std::make_shared<T>(std::forward<Args>(args)...);
            for (auto it = types.begin(); it != types.end();) {
                if (it->expired()) {
                    it = types.erase(it);
                } else if (*(it->lock()) == *type_ptr) {
                    return std::static_pointer_cast<T>(it->lock());
                } else {
                    it++;
                }
            }

            types.push_back(type_ptr);
            return type_ptr;
        }

        std::vector<WeakTypePtr> types;

        // template<typename T>
        // TypePtr make_nominal_type(const std::string& name) {
        //     for (auto it = nominal_types.begin(); it != nominal_types.end();) {
        //         if (it->second.expired()) {
        //             it = nominal_types.erase(it);
        //         } else if (it->first == name) {
        //             return it->second.lock();
        //         } else {
        //             it++;
        //         }
        //     }
        //
        //     return std::make_shared<BuiltinType>(name);
        // }
        //
        // template<typename ... Args>
        // TypePtr make_function_type(Args&& ... args) {
        //     auto ftype = std::make_shared<FunctionType>(std::forward<Args>(args)...);
        //     auto it    = std::find_if(
        //         structual_types.begin(),
        //         structual_types.end(),
        //         [ftype](WeakTypePtr const& p) {
        //             return !p.expired() and (*(p.lock()) == *ftype);
        //         });
        //
        //     if (it == structual_types.end()) {
        //         structual_types.push_back(ftype);
        //         return ftype;
        //     } else {
        //         return it->second;
        //     }
        // }

        // std::unordered_map<std::string, WeakTypePtr> nominal_types;
        // std::vector<WeakTypePtr>                     structual_types;
    };

} // namespace tango
