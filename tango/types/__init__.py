from tango.wrapper import (
    TypeFactory, TypeModifier, TypeBase, TypeName, TypeUnion, TypeVariable,
    FunctionType, NominalType, BuiltinType)


type_factory   = TypeFactory()
type_modifiers = []

for mutability in (TypeModifier.tm_cst, TypeModifier.tm_mut):
    for allocation in (TypeModifier.tm_stk, TypeModifier.tm_shd):
        for store_type in (TypeModifier.tm_val, TypeModifier.tm_ref):
            type_modifiers.append(mutability | allocation | store_type)

def make_type_variants(make_fun, **kwargs):
    result = TypeUnion()
    for m in type_modifiers:
        kwargs['modifiers'] = m
        result.add(make_fun(**kwargs))
    return result


# Following are some helper methods and properties we add by monkeypatching
# the C++ classes, so as to have a nicer API to work with.

def modifiers_to_str(modifiers):
    result = []
    if TypeModifier.tm_cst & modifiers:
        result.append('cst')
    if TypeModifier.tm_mut & modifiers:
        result.append('mut')
    if TypeModifier.tm_stk & modifiers:
        result.append('stk')
    if TypeModifier.tm_shd & modifiers:
        result.append('shd')
    if TypeModifier.tm_val & modifiers:
        result.append('val')
    if TypeModifier.tm_ref & modifiers:
        result.append('ref')
    if TypeModifier.tm_own & modifiers:
        result.append('own')

    return ('@({})'.format(','.join(result)) if len(result) > 1 else
            '@' + result[0] if result else '')


TypeBase.__hash__ = lambda self: 0


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


def FunctionType_str(self):
    parameters = []
    for i in range(len(self.domain)):
        parameters.append('{}: {}'.format(self.labels[i], self.domain[i]))

    return ' '.join(
        (modifiers_to_str(self.modifiers), '(%s) -> %s' % (', '.join(parameters), self.codomain)))

FunctionType.__str__  = FunctionType_str
FunctionType.__repr__ = FunctionType_str


def NominalType_str(self):
    modifiers = modifiers_to_str(self.modifiers)
    if modifiers:
        return modifiers + ' ' + self.name
    return '@* ' + self.name

NominalType.__str__  = NominalType_str
NominalType.__repr__ = NominalType_str


TypeFactory.make_variants = staticmethod(make_type_variants)
