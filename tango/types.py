from collections import OrderedDict


class BaseType(object):

    def __init__(self, members=None):
        self.members = members or {}

    @property
    def is_generic(self):
        return False

    def __eq__(self, other):
        return self is other


class TypeTag(BaseType):
    # Types themselves should be typed with `Type`. But there're many
    # instances where we need the type name to refer to the type's itself and
    # not that of the first-class type value (e.g. `Int.+`). Whether the type
    # name should be interpreted as a first-class value or a reference to its
    # own type depends on where the name is used. As a result, it is simpler
    # to store a `TypeTag` object in the solver's environment, so that we can
    # choose what type we want depending on the context.

    def __init__(self, instance_type):
        self.instance_type = instance_type

    def __eq__(self, other):
        return (type(self) == type(other)) and (self.instance_type == other.instance_type)

    def __str__(self):
        return 'TypeTag<%s>' % self.instance_type


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

    def __str__(self):
        return '[' + ' | '.join(map(str, self.types)) + ']'


class NominalType(BaseType):

    def __init__(self, name, members=None):
        super().__init__(members)
        self.name = name

    def __eq__(self, other):
        return (type(self) == type(other)
                and (self.name == other.name)
                and (self.members == other.members))

    def __str__(self):
        return str(self.name)


class StructType(NominalType):
    pass


class EnumType(NominalType):
    pass


class StructuralType(BaseType):
    pass


class FunctionType(StructuralType):

    def __init__(self, domain=None, codomain=None, labels=None):
        super().__init__()
        self.domain = domain or []
        self.codomain = codomain or Nothing
        self.labels = labels or [None for _ in self.domain]

    @property
    def is_generic(self):
        return any(t.is_generic for t in self.domain) or self.codomain.is_generic

    def __eq__(self, other):
        return (type(self) == type(other)
                and (len(self.domain) == len(other.domain))
                and all(t == u for t, u in zip(self.domain, other.domain))
                and (self.codomain == other.codomain)
                and all(l == m for l, m in zip(self.labels, other.labels)))

    def __str__(self):
        return '(%s) -> %s' % (
            ', '.join('%s: %s' % (self.labels[i] or '_', t) for i, t in enumerate(self.domain)),
            self.codomain)
