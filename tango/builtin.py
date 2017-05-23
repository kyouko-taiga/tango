from .module import Module, Symbol
from .scope import Scope
from .types import FunctionType, NominalType, TypeName, TypeUnion


builtin_module = Module(name='Tango')
builtin_types  = []

def make_builtin_type(name):
    builtin_type = NominalType(name)
    builtin_module.symbols[name] = Symbol(
        name=name,
        type=TypeName(name=name, type=builtin_type))

    return builtin_type


Type       = make_builtin_type('Type')
Nothing    = make_builtin_type('Nothing')
Anything   = make_builtin_type('Anything')

Int        = make_builtin_type('Int')
Double     = make_builtin_type('Double')
String     = make_builtin_type('String')
Bool       = make_builtin_type('Bool')


builtin_module.symbols['true']  = Symbol(name='true',  type=Bool)
builtin_module.symbols['false'] = Symbol(name='false', type=Bool)
