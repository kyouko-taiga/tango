from .scope import Scope
from .types import FunctionType, NominalType, TypeUnion


builtin_scope = Scope(name='Tango')

Type     = NominalType('Type',     builtin_scope)
Nothing  = NominalType('Nothing',  builtin_scope)
Anything = NominalType('Anything', builtin_scope)
Self     = NominalType('Self',     builtin_scope)

Int      = NominalType('Int',      builtin_scope)
Double   = NominalType('Double',   builtin_scope)
String   = NominalType('String',   builtin_scope)
Bool     = NominalType('Bool',     builtin_scope)

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

builtin_scope.members = {
    'Type'    : Type,
    'Nothing' : Nothing,
    'Anything': Anything,
    'Self'    : Self,
    'Int'     : Int,
    'Double'  : Double,
    'String'  : String,
    'Bool'    : Bool,
    'true'    : Bool,
    'false'   : Bool,
}
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
