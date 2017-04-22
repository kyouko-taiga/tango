from collections import OrderedDict


class BaseType(object):

    def __init__(self, members=None):
        self.members = members or {}

    @property
    def is_generic(self):
        return False

    def __eq__(self, other):
        return self is other


class GenericType(BaseType):

    def __init__(self, signature):
        super().__init__()
        self.signature = signature

    @property
    def is_generic(self):
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

    @property
    def is_generic(self):
        return any(t.is_generic for t in self.types)

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

    def __init__(self, name, scope, members=None):
        super().__init__(members)
        self.name = name
        self.scope = scope

    def __eq__(self, other):
        return (type(self) == type(other)
                and (self.name == other.name)
                and (self.scope == other.scope)
                and (self.members == other.members))

    def __hash__(self):
        h = 3
        h = (31 ^ h) ^ hash(self.name)
        h = (31 ^ h) ^ hash(self.scope)
        for member in self.members:
            h = (31 ^ h) ^ hash(member)
        return h

    def __str__(self):
        return str(self.name)

    def __repr__(self):
        return '<NominalType %s>' % self.name


class StructType(NominalType):
    pass


class EnumType(NominalType):
    pass


class StructuralType(BaseType):
    pass


class FunctionType(StructuralType):

    def __init__(self, domain=None, codomain=None, labels=None, attributes=None):
        super().__init__()
        self.domain = domain or []
        self.codomain = codomain or Nothing
        self.labels = labels or [None for _ in self.domain]
        self.attributes = attributes or [set() for _ in self.domain]

    @property
    def is_generic(self):
        return any(t.is_generic for t in self.domain) or self.codomain.is_generic

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
