from .scope import Scope
from .types import FunctionType, NominalType, TypeUnion


Type     = NominalType('Type')
Nothing  = NominalType('Nothing')
Anything = NominalType('Anything')
Self     = NominalType('Self')

Int = NominalType('Int')
Int.members = {
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

Double = NominalType('Double')
Double.members = {
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

String = NominalType('String')
String.members = {
    '+'  : FunctionType(domain=[String, String], codomain=String),
}

builtin_scope = Scope()
builtin_scope.members = {
    'Type'    : Type,
    'Nothing' : Nothing,
    'Anything': Anything,
    'Self'    : Self,
    'Int'     : Int,
    'Double'  : Double,
    'String'  : String,
}
