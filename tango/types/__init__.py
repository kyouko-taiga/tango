from tango.wrapper import (
    TypeModifier, TypeFactory, TypeBase, TypeName, TypeUnion, TypeVariable,
    PlaceholderType, FunctionType, StructType)


TM           = TypeModifier
type_factory = TypeFactory()


def find_placeholders(type_):
    if isinstance(type_, PlaceholderType):
        return set([type_])

    if isinstance(type_, FunctionType):
        result = set()
        for parameter_type in type_.domain:
            result = result | find_placeholders(parameter_type)
        return result | find_placeholders(type_.codomain)

    assert False, 'TODO'


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
    return ' '.join(result) if (modifiers != 0) else '@?'


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


TypeName.__str__ = lambda self: f'TypeName<{self.type}>'


def TypeVariable_str(self):
    modifiers = modifiers_to_str(self.modifiers)
    if modifiers:
        return modifiers + ' ' + hex(hash(self.id))
    return hex(hash(self.id))

TypeVariable.__str__ = TypeVariable_str


def PlaceholderType_str(self):
    if self.specialization is not None:
        return '<{}>'.format(self.specialization)

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


def StructType_str(self):
    modifiers = modifiers_to_str(self.modifiers)
    if modifiers:
        return modifiers + ' ' + self.name
    return self.name

StructType.__str__  = StructType_str
StructType.__repr__ = StructType_str


def TypeFactory_updating(self, ty, **kwargs):
    if isinstance(ty, TypeVariable):
        return self.make_variable(
            modifiers = kwargs.get('modifiers', ty.modifiers),
            id        = ty.id)

    if isinstance(ty, PlaceholderType):
        return self.make_placeholder(
            modifiers = kwargs.get('modifiers', ty.modifiers),
            id        = ty.id)

    if isinstance(ty, FunctionType):
        return self.make_function(**{
            'domain'   : kwargs.get('domain',    list(ty.domain)),
            'labels'   : kwargs.get('labels',    list(ty.labels)),
            'codomain' : kwargs.get('codomain',  ty.codomain),
            'modifiers': kwargs.get('modifiers', ty.modifiers),
        })

    if isinstance(ty, StructType):
        return self.make_struct(**{
            'name'     : kwargs.get('name',      ty.name),
            'modifiers': kwargs.get('modifiers', ty.modifiers),
            'members'  : kwargs.get('members',   {
                name: ty.members[name] for name in ty.members.keys()
            }),
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
