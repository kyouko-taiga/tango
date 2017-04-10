class BaseType(object):

    def __eq__(self, other):
        return self is other


class TypeUnion(BaseType):

    def __init__(self, types):
        self.types = [t for t in types]

    def add(self, t):
        if t not in self.types:
            self.types.append(t)

    def replace_content(self, types):
        self.types[:] = [t for t in types]

    def first(self):
        return self.types[0]

    def __iter__(self):
        return iter(self.types)

    def __str__(self):
        return '[' + ' | '.join(map(str, self.types)) + ']'


class NominalType(BaseType):

    def __init__(self, name):
        self.name = name

    def __str__(self):
        return str(self.name)


class StructuralType(BaseType):
    pass


class FunctionType(StructuralType):

    def __init__(
            self, generic_parameters=None, domain=None, codomain=None, labels=None):

        self.generic_parameters = generic_parameters or []
        self.domain = domain or []
        self.codomain = codomain or Nothing
        self.labels = labels or []

    def __eq__(self, other):
        # FIXME
        if self.generic_parameters != other.generic_parameters:
            return False
        if len(self.domain) != len(other.domain):
            return False
        if any(t != u for t, u in zip(self.domain, other.domain)):
            return False
        if self.codomain != other.codomain:
            return False
        if any(l != m for l, m in zip(self.labels, other.labels)):
            return False
        return True

    def __str__(self):
        if self.generic_parameters:
            generic_parameters = '<%s> ' % ', '.join(map(str, self.generic_parameters))
        else:
            generic_parameters = ''

        return '%s(%s) -> %s' % (
            generic_parameters,
            ', '.join('%s: %s' % (self.labels[i] or '_', p) for i, p in enumerate(self.domain)),
            self.codomain)