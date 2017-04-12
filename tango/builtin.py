from .scope import Scope
from .types import NominalType


Type     = NominalType('Type')
Nothing  = NominalType('Nothing')
Anything = NominalType('Anything')
Self     = NominalType('Self')
Int      = NominalType('Int')
Double   = NominalType('Double')
String   = NominalType('String')

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
