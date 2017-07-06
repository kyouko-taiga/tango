from tango.wrapper import (
    TypeFactory, TypeModifier, TypeBase, TypeName, TypeUnion, TypeVariable,
    FunctionType, NominalType, BuiltinType)


type_factory   = TypeFactory()


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


def TypeFactory_updating(self, ty, **kwargs):
    if isinstance(ty, BuiltinType):
        return self.make_builtin(**{
            'name'     : kwargs.get('name',      ty.name),
            'modifiers': kwargs.get('modifiers', ty.modifiers),
        })

    if isinstance(ty, FunctionType):
        return self.make_function(**{
            'domain'   : kwargs.get('domain',    ty.domain),
            'labels'   : kwargs.get('labels',    ty.labels),
            'modifiers': kwargs.get('modifiers', ty.modifiers),
        })

    assert False, 'cannot update instances of {}'.format(ty.__class__.__name__)

modifiers_combinations = []
for mutability in (TypeModifier.tm_cst, TypeModifier.tm_mut):
    # for allocation in (TypeModifier.tm_stk, TypeModifier.tm_shd):
    for allocation in (TypeModifier.tm_stk,):
        for store_type in (TypeModifier.tm_val, TypeModifier.tm_ref):
            modifiers_combinations.append(mutability | allocation | store_type)

def TypeFactory_make_variants(self, ty):
    result = TypeUnion()
    for modifiers in modifiers_combinations:
        result.add(self.updating(ty, modifiers=modifiers))
    return result

TypeFactory.updating      = TypeFactory_updating
TypeFactory.make_variants = TypeFactory_make_variants
