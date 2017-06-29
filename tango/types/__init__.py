from tango.wrapper import *

__all__ = [
    'TypeFactory', 'TypeBase', 'TypeUnion',
    'ReferenceType',
    'FunctionType',
    'NominalType', 'BuiltinType',
]


class TypeName(TypeBase):

    def __init__(self, name, type):
        self.name = name
        self.type = type

    def __str__(self):
        return self.name


# Following are some helper methods and properties we add by monkeypatching
# the C++ classes, so as to have a nicer API to work with.

def TypeUnion_str(self):
    return '[' + ' | '.join(map(str, self.types)) + ']'

TypeUnion.__str__ = TypeUnion_str


def ReferenceType_str(self):
    return '&' + str(self.referred_type)

ReferenceType.__str__ = ReferenceType_str


def FunctionType_str(self):
    parameters = []
    for i in range(len(self.domain)):
        parameters.append('{}: {}'.format(self.labels[i], self.domain[i]))

    return '(%s) -> %s' % (', '.join(parameters), self.codomain)

FunctionType.__str__ = FunctionType_str


def NominalType_str(self):
    return self.name

NominalType.__str__ = NominalType_str
