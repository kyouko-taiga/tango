from .module import Module, Symbol
from .scope import Scope
from .types import FunctionType, NominalType, TypeName, TypeUnion


builtin_module = Module(name='Tango')
builtin_types  = []

def make_builtin_type(name):
    builtin_type = NominalType(name)
    builtin_module.symbols[name] = Symbol(
        name = name,
        type = TypeName(name=name, type=builtin_type))

    return builtin_type


Type       = make_builtin_type('Type')
Nothing    = make_builtin_type('Nothing')
Anything   = make_builtin_type('Anything')

Int        = make_builtin_type('Int')
Double     = make_builtin_type('Double')
String     = make_builtin_type('String')
Bool       = make_builtin_type('Bool')

Int.members = {
    'new': TypeUnion((
           FunctionType(domain=[Int],      codomain=Int),
           FunctionType(domain=[Double],   codomain=Int),
           FunctionType(domain=[String],   codomain=Int, labels=['from_string']))),
    'max': Int,
    'min': Int,
    '+'  : TypeUnion((
           FunctionType(domain=[Int, Int], codomain=Int),
           FunctionType(domain=[Int],      codomain=Int))),
    '-'  : TypeUnion((
           FunctionType(domain=[Int, Int], codomain=Int),
           FunctionType(domain=[Int],      codomain=Int))),
    '*'  : FunctionType(domain=[Int, Int], codomain=Int),
    '/'  : FunctionType(domain=[Int, Int], codomain=Int),
    '%'  : FunctionType(domain=[Int, Int], codomain=Int),
    '<'  : FunctionType(domain=[Int, Int], codomain=Bool),
    '<=' : FunctionType(domain=[Int, Int], codomain=Bool),
    '==' : FunctionType(domain=[Int, Int], codomain=Bool),
    '!=' : FunctionType(domain=[Int, Int], codomain=Bool),
    '>=' : FunctionType(domain=[Int, Int], codomain=Bool),
    '>'  : FunctionType(domain=[Int, Int], codomain=Bool),
}

Double.members = {
    'new': TypeUnion((
           FunctionType(domain=[Double],         codomain=Double),
           FunctionType(domain=[Int],            codomain=Double),
           FunctionType(domain=[String],         codomain=Int, labels=['from_string']))),
    '+'  : TypeUnion((
           FunctionType(domain=[Double, Double], codomain=Double),
           FunctionType(domain=[Double],         codomain=Double))),
    '-'  : TypeUnion((
           FunctionType(domain=[Double, Double], codomain=Double),
           FunctionType(domain=[Double],         codomain=Double))),
    '*'  : FunctionType(domain=[Double, Double], codomain=Double),
    '/'  : FunctionType(domain=[Double, Double], codomain=Double),
    '%'  : FunctionType(domain=[Double, Double], codomain=Double),
    '<'  : FunctionType(domain=[Double, Double], codomain=Bool),
    '<=' : FunctionType(domain=[Double, Double], codomain=Bool),
    '==' : FunctionType(domain=[Double, Double], codomain=Bool),
    '!=' : FunctionType(domain=[Double, Double], codomain=Bool),
    '>=' : FunctionType(domain=[Double, Double], codomain=Bool),
    '>'  : FunctionType(domain=[Double, Double], codomain=Bool),
}

String.members = {
    'new': FunctionType(domain=[String],         codomain=String),
    '+'  : FunctionType(domain=[String, String], codomain=String),
}

Bool.members = {
    'new': FunctionType(domain=[Bool],       codomain=Bool),
    'and': FunctionType(domain=[Bool, Bool], codomain=Bool),
    'or' : FunctionType(domain=[Bool, Bool], codomain=Bool),
}

builtin_module.symbols['true']  = Symbol(name='true',  type=Bool)
builtin_module.symbols['false'] = Symbol(name='false', type=Bool)
