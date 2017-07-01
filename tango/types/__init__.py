from tango.wrapper import (
    TypeFactory, TypeBase, TypeVariable, TypeUnion, ReferenceType, FunctionType, NominalType, BuiltinType)


type_factory = TypeFactory()


# Following are some helper methods and properties we add by monkeypatching
# the C++ classes, so as to have a nicer API to work with.

def TypeVariable_str(self):
    return '{}'.format(hex(hash(self)))

TypeVariable.__str__ = TypeVariable_str


def TypeUnion_iter(self):
    return iter(self.types)

def TypeUnion_len(self):
    return len(self.types)

def TypeUnion_str(self):
    return '[' + ' | '.join(map(str, self.types)) + ']'

TypeUnion.__iter__ = TypeUnion_iter
TypeUnion.__len__  = TypeUnion_len
TypeUnion.__str__  = TypeUnion_str


def ReferenceType_str(self):
    return '&' + str(self.referred_type)

ReferenceType.__str__ = ReferenceType_str


def FunctionType_str(self):
    parameters = []
    for i in range(len(self.domain)):
        parameters.append('{}: {}'.format(self.labels[i], self.domain[i]))

    return '(%s) -> %s' % (', '.join(parameters), self.codomain)

FunctionType.__str__  = FunctionType_str
FunctionType.__repr__ = FunctionType_str


def NominalType_str(self):
    return self.name

NominalType.__str__  = NominalType_str
NominalType.__repr__ = NominalType_str
