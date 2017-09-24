from tango.wrapper import (
    TypeModifier, TypeFactory, TypeBase, TypeName, TypeUnion, TypeVariable,
    PlaceholderType, FunctionType, NominalType, BuiltinType)


TM             = TypeModifier
type_factory   = TypeFactory()


# Following are some helper methods and properties we add by monkeypatching
# the C++ classes, so as to have a nicer API to work with.

def modifiers_to_str(modifiers):
    result = []
    if (modifiers & TM.mut) and (modifiers & TM.stk):
        result.append('@mut')
    if modifiers & TM.ref:
        result.append('@ref')
    if modifiers & TM.shd:
        result.append('@shd')
    if modifiers & TM.own:
        result.append('@own')
    return ' '.join(result) if (result != 0) else '@?'


TypeBase.__hash__ = lambda self: 0


TypeUnion_initializer = TypeUnion.__init__
def TypeUnion_init(self, types=None):
    TypeUnion_initializer(self)
    if types:
        self.types = types

def TypeUnion_iter(self):
    return iter(self.types)

def TypeUnion_len(self):
    return len(self.types)

def TypeUnion_str(self):
    return '[' + ' | '.join(map(str, self.types)) + ']'

TypeUnion.__init__ = TypeUnion_init
TypeUnion.__iter__ = TypeUnion_iter
TypeUnion.__len__  = TypeUnion_len
TypeUnion.__str__  = TypeUnion_str


def TypeVariable_str(self):
    modifiers = modifiers_to_str(self.modifiers)
    if modifiers:
        return modifiers + ' ' + hex(hash(self.id))
    return hex(hash(self.id))

TypeVariable.__str__ = TypeVariable_str


def PlaceholderType_str(self):
    modifiers = modifiers_to_str(self.modifiers)
    if modifiers:
        return modifiers + ' ' + self.id
    return self.id

PlaceholderType.__str__ = PlaceholderType_str


def FunctionType_str(self):
    parameters = []
    for i in range(len(self.domain)):
        parameters.append('{}: {}'.format(self.labels[i], self.domain[i]))

    signature = '({}) -> {}'.format(', '.join(parameters), self.codomain)
    modifiers = modifiers_to_str(self.modifiers)
    if modifiers:
        return modifiers + ' ' + signature
    return signature

FunctionType.__str__  = FunctionType_str
FunctionType.__repr__ = FunctionType_str


def NominalType_str(self):
    modifiers = modifiers_to_str(self.modifiers)
    if modifiers:
        return modifiers + ' ' + self.name
    return self.name

NominalType.__str__  = NominalType_str
NominalType.__repr__ = NominalType_str


def TypeFactory_updating(self, ty, **kwargs):
    if isinstance(ty, TypeVariable):
        return self.make_variable(modifiers=kwargs.get('modifiers', ty.modifiers))

    if isinstance(ty, PlaceholderType):
        return self.make_placeholder(
            modifiers = kwargs.get('modifiers', ty.modifiers),
            id        = ty.id)

    if isinstance(ty, BuiltinType):
        return self.make_builtin(**{
            'name'     : kwargs.get('name',      ty.name),
            'modifiers': kwargs.get('modifiers', ty.modifiers),
        })

    if isinstance(ty, FunctionType):
        return self.make_function(**{
            'domain'   : kwargs.get('domain',    list(ty.domain)),
            'labels'   : kwargs.get('labels',    list(ty.labels)),
            'codomain' : kwargs.get('codomain',  ty.codomain),
            'modifiers': kwargs.get('modifiers', ty.modifiers),
        })

    assert False, 'cannot update instances of {}'.format(ty.__class__.__name__)

modifiers_combinations = [
    TM.cst | TM.stk | TM.val,
    TM.cst | TM.stk | TM.ref,
    TM.mut | TM.stk | TM.val,
    TM.mut | TM.stk | TM.ref,
    TM.mut | TM.shd | TM.val,
]

def TypeFactory_make_variants(self, ty):
    result = TypeUnion()
    for modifiers in modifiers_combinations:
        result.add(self.updating(ty, modifiers=modifiers))
    return result

TypeFactory.updating      = TypeFactory_updating
TypeFactory.make_variants = TypeFactory_make_variants
