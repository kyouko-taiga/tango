import tango.builtin


class BaseType(object):

    def __init__(self, members=None):
        self.members = members or {}

    def is_generic(self, memo=None):
        return False

    def __eq__(self, other):
        return self is other


class GenericType(BaseType):

    def __init__(self, signature):
        super().__init__()
        self.signature = signature

    def is_generic(self, memo=None):
        return True

    def __str__(self):
        return 'Generic(%s)' % self.signature


class TypeUnion(BaseType):

    def __init__(self, types=None):
        super().__init__()
        self.types = []

        # We can't use a set to store the types of the union, as not all types
        # are hashable yet. There isn't a total order relationship on them
        # neither, so our only option is a O(n^2) algorithm.
        if types is not None:
            for t in types:
                self.add(t)

    def is_generic(self, memo=None):
        return any(t.is_generic(memo) for t in self.types)

    def add(self, t):
        if t not in self.types:
            self.types.append(t)

    def replace_content(self, types):
        self.types[:] = []
        for t in types:
            self.add(t)

    def copy(self):
        return TypeUnion(t.copy() if isinstance(t, TypeUnion) else t for t in self.types)

    def first(self):
        return self.types[0]

    def __iter__(self):
        return iter(self.types)

    def __len__(self):
        return len(self.types)

    def __eq__(self, other):
        return (type(self) == type(other)
                and (len(self.types) == len(other.types))
                and all(t in other.types for t in self.types))

    def __hash__(self):
        # We can't hash a type union without a total order on all types.
        return 0

    def __str__(self):
        return '[' + ' | '.join(map(str, self.types)) + ']'


class NominalType(BaseType):

    def __init__(
            self, name, scope, inner_scope=None, members=None,
            generic_parameters=None, specializations=None):

        super().__init__(members)
        self.name = name
        self.scope = scope
        self.inner_scope = inner_scope

        # (String) -> GenericType
        self.generic_parameters = generic_parameters or {}

        # (String) -> Optional<Type>
        self.specializations = specializations or {
            param: None for param in self.generic_parameters
        }

    def is_generic(self, memo=None):
        if memo is None:
            memo = set()

        for member in self.members.values():
            if id(member) in memo:
                continue
            memo.add(id(member))
            if member.is_generic(memo):
                return True

        return False

    def __eq__(self, other):
        if (type(self) != type(other)
            or (self.name != other.name)
            or (self.scope != other.scope)
            or (self.inner_scope != other.inner_scope)):
            return False

        for name in self.members:
            if ((name not in other.members)
                or not _equals(self.members[name], other.members[name])):
                return False
        for name in self.generic_parameters:
            if ((name not in other.generic_parameters)
                or not _equals(self.generic_parameters[name], other.generic_parameters[name])):
                return False
        for name in self.specializations:
            if ((name not in other.specializations)
                or not _equals(self.generic_parameters[name], other.generic_parameters[name])):
                return False

        return True

    def __hash__(self):
        h = 3
        h = (31 ^ h) ^ hash(self.name)
        h = (31 ^ h) ^ hash(self.scope)
        for member in self.members:
            h = (31 ^ h) ^ hash(member)
        return h

    def __str__(self):
        if self.generic_parameters:
            specializations = ', '.join(
                '{}: {}'.format(tp, self.specializations[tp] or '_')
                for tp in self.generic_parameters)
            return '{}<{}> '.format(self.name, specializations)

        return str(self.name)

    def __repr__(self):
        return '<NominalType {}>'.format(str(self))


class StructType(NominalType):
    pass


class EnumType(NominalType):
    pass


class StructuralType(BaseType):
    pass


class FunctionType(StructuralType):

    def __init__(
            self, domain=None, codomain=None, labels=None, attributes=None,
            generic_parameters=None, specializations=None):

        super().__init__()
        self.domain = domain or []
        self.codomain = codomain or tango.builtin.Nothing
        self.labels = labels or [None for _ in self.domain]
        self.attributes = attributes or [set() for _ in self.domain]
        self.generic_parameters = generic_parameters or []
        self.specializations = specializations or {
            param: None for param in self.generic_parameters
        }

    def is_generic(self, memo=None):
        return any(t.is_generic(memo) for t in self.domain) or self.codomain.is_generic(memo)

    def is_compatible_with(self, other):
        # Check if the number of parameters match.
        if len(self.domain) != len(other.domain):
            return False

        for i in range(len(self.domain)):
            # Check if non-generic parameters match.
            if not self.domain[i].is_generic() and (self.domain[i] != other.domain[i]):
                return False
            # Check if parameter labels match.
            if self.labels[i] != other.labels[i]:
                return False
            # Check if the parametter attributes match.
            if self.attributes[i] != other.attributes[i]:
                return False

        # Check if the codomains match.
        if not self.codomain.is_generic() and (self.codomain != other.codomain):
            return False

        return True

    def __eq__(self, other):
        return (type(self) == type(other)
                and (len(self.domain) == len(other.domain))
                and all(t == u for t, u in zip(self.domain, other.domain))
                and (self.codomain == other.codomain)
                and all(l == m for l, m in zip(self.labels, other.labels))
                and all(a == b for a, b in zip(self.attributes, other.attributes)))

    def __hash__(self):
        h = 3
        h = (31 ^ h) ^ hash(self.codomain)
        for i in range(len(self.domain)):
            h = (31 ^ h) ^ hash(self.domain[i])
            h = (31 ^ h) ^ hash(self.labels[i]) if self.labels[i] is not None else 0
            h = (31 ^ h) ^ hash(tuple(sorted(self.attributes[i])))
        return h

    def __str__(self):
        parameters = []
        for i, t in enumerate(self.domain):
            mutability = 'mut' if ('mutable' in self.attributes[i]) else 'cst'
            parameters.append(mutability + ' ' + (self.labels[i] or '_') + ': ' + str(t))

        return '(%s) -> %s' % (', '.join(parameters), self.codomain)


_equality_memo = {}

def _equals(a, b):
    global _equality_memo

    key = (id(a), id(b))
    if key in _equality_memo:
        return _equality_memo[key]

    _equality_memo[key] = True
    result = a == b
    _equality_memo[key] = result
    return result
