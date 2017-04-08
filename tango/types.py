class BaseType(object):

    def __eq__(self, other):
        return self is other


class NominalType(BaseType):

    def __init__(self, name):
        self.name = name

    def __str__(self):
        return str(self.name)


class StructuralType(BaseType):

    pass


class FunctionType(StructuralType):

    def __init__(self, generic_parameters=None, domain=None, codomain=None):
        self.generic_parameters = generic_parameters or []
        self.domain = domain or []
        self.codomain = codomain or Nothing

    def __str__(self):
        if self.generic_parameters:
            generic_parameters = '<%s> ' % ', '.join(map(str, self.generic_parameters))
        else:
            generic_parameters = ''

        return '%s(%s) -> %s' % (
            generic_parameters, ', '.join(map(str, self.domain)), self.codomain)
