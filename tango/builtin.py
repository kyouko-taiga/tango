from .scope import Scope
from .types import FunctionType, NominalType, TypeTag, TypeUnion


Type     = NominalType('Type')
Nothing  = NominalType('Nothing')
Anything = NominalType('Anything')
Self     = NominalType('Self')

Int      = NominalType('Int')
Double   = NominalType('Double')
String   = NominalType('String')

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
}

String.members = {
    'new': FunctionType(domain=[String], codomain=String),
    '+'  : FunctionType(domain=[String, String], codomain=String),
}

builtin_scope = Scope()
builtin_scope.members = {
    'Type'    : TypeTag(Type),
    'Nothing' : TypeTag(Nothing),
    'Anything': TypeTag(Anything),
    'Self'    : TypeTag(Self),
    'Int'     : TypeTag(Int),
    'Double'  : TypeTag(Double),
    'String'  : TypeTag(String),
}
