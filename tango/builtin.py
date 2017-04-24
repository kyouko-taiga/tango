from .scope import Scope, Symbol
from .types import FunctionType, NominalType, TypeUnion


builtin_scope = Scope(name='Tango')

def make_builtin_type(name):
    return NominalType(
        name        = name,
        scope       = builtin_scope,
        inner_scope = Scope(name=name, parent=builtin_scope))


Type     = make_builtin_type('Type')
Nothing  = make_builtin_type('Nothing')
Anything = make_builtin_type('Anything')
Self     = make_builtin_type('Self')

Int      = make_builtin_type('Int')
Double   = make_builtin_type('Double')
String   = make_builtin_type('String')
Bool     = make_builtin_type('Bool')

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

builtin_scope.add(Symbol(name='Type',     type=Type))
builtin_scope.add(Symbol(name='Nothing',  type=Nothing))
builtin_scope.add(Symbol(name='Anything', type=Anything))
builtin_scope.add(Symbol(name='Self',     type=Self))
builtin_scope.add(Symbol(name='Int',      type=Int))
builtin_scope.add(Symbol(name='Double',   type=Double))
builtin_scope.add(Symbol(name='String',   type=String))
builtin_scope.add(Symbol(name='Bool',     type=Bool))
builtin_scope.add(Symbol(name='true',     type=Bool))
builtin_scope.add(Symbol(name='false',    type=Bool))

builtin_scope.typenames = {
    'Type',
    'Nothing',
    'Anything',
    'Self',
    'Int',
    'Double',
    'String',
    'Bool',
}
