from collections import OrderedDict


class BaseType(object):

    def __init__(self):
        self.members = {}

    def __eq__(self, other):
        return self is other


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

    def __str__(self):
        return '[' + ' | '.join(map(str, self.types)) + ']'


class NominalType(BaseType):

    def __init__(self, name):
        super().__init__()
        self.name = name

    def __str__(self):
        return str(self.name)


class GenericType(NominalType):
    pass


class StructType(NominalType):

    def __init__(self, name, members=None):
        super().__init__(name)
        self.members = members or {}

    def __eq__(self, other):
        return (type(self) == type(other)
                and (self.name == other.name)
                and (self.members == other.members))


class StructuralType(BaseType):
    pass


class FunctionType(StructuralType):

    def __init__(
            self, generic_parameters=None, domain=None, codomain=None, labels=None):

        super().__init__()

        self.generic_parameters = OrderedDict(generic_parameters or [])
        self.domain = domain or []
        self.codomain = codomain or Nothing
        self.labels = labels or []

    def specialized_parameter(self, generic_name):
        return self.generic_parameters.get(generic_name, self.generic_parameters[generic_name])

    def __eq__(self, other):
        return (type(self) == type(other)
                and (self.generic_parameters == other.generic_parameters)
                and (len(self.domain) == len(other.domain))
                and all(t == u for t, u in zip(self.domain, other.domain))
                and (self.codomain == other.codomain)
                and all(l == m for l, m in zip(self.labels, other.labels)))

    def __str__(self):
        if self.generic_parameters:
            prefix = '<%s> ' % ', '.join(map(str, self.generic_parameters.values()))
        else:
            prefix = ''

        domain = (
            t if not isinstance(t, GenericType) else self.specialized_parameter(t.name)
            for t in self.domain)
        codomain = (
            self.codomain if not isinstance(self.codomain, GenericType)
            else self.specialized_parameter(self.codomain.name))

        return '%s(%s) -> %s' % (
            prefix,
            ', '.join('%s: %s' % (self.labels[i] or '_', t) for i, t in enumerate(domain)),
            codomain)
