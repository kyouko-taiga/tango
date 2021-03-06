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

ModuleType = make_builtin_type('Module')

Int        = make_builtin_type('Int')
Double     = make_builtin_type('Double')
String     = make_builtin_type('String')
Bool       = make_builtin_type('Bool')


builtin_module.symbols['true']  = Symbol(name='true',  type=Bool)
builtin_module.symbols['false'] = Symbol(name='false', type=Bool)

# Int.members = {
#     'new': TypeUnion((
#            FunctionType(domain=[Int],      codomain=Int),
#            FunctionType(domain=[Double],   codomain=Int),
#            FunctionType(domain=[String],   codomain=Int, labels=['from_string']))),
#     'max': Int,
#     'min': Int,
#     '+'  : TypeUnion((
#            FunctionType(domain=[Int, Int], codomain=Int),
#            FunctionType(domain=[Int],      codomain=Int))),
#     '-'  : TypeUnion((
#            FunctionType(domain=[Int, Int], codomain=Int),
#            FunctionType(domain=[Int],      codomain=Int))),
#     '*'  : FunctionType(domain=[Int, Int], codomain=Int),
#     '/'  : FunctionType(domain=[Int, Int], codomain=Int),
#     '%'  : FunctionType(domain=[Int, Int], codomain=Int),
#     '<'  : FunctionType(domain=[Int, Int], codomain=Bool),
#     '<=' : FunctionType(domain=[Int, Int], codomain=Bool),
#     '==' : FunctionType(domain=[Int, Int], codomain=Bool),
#     '!=' : FunctionType(domain=[Int, Int], codomain=Bool),
#     '>=' : FunctionType(domain=[Int, Int], codomain=Bool),
#     '>'  : FunctionType(domain=[Int, Int], codomain=Bool),
# }
# list(map(
#     Int.inner_scope.add,
#     (Symbol(name=name, type=type) for name, type in Int.members.items())))
#
# Double.members = {
#     'new': TypeUnion((
#            FunctionType(domain=[Double],         codomain=Double),
#            FunctionType(domain=[Int],            codomain=Double),
#            FunctionType(domain=[String],         codomain=Int, labels=['from_string']))),
#     '+'  : TypeUnion((
#            FunctionType(domain=[Double, Double], codomain=Double),
#            FunctionType(domain=[Double],         codomain=Double))),
#     '-'  : TypeUnion((
#            FunctionType(domain=[Double, Double], codomain=Double),
#            FunctionType(domain=[Double],         codomain=Double))),
#     '*'  : FunctionType(domain=[Double, Double], codomain=Double),
#     '/'  : FunctionType(domain=[Double, Double], codomain=Double),
#     '%'  : FunctionType(domain=[Double, Double], codomain=Double),
#     '<'  : FunctionType(domain=[Double, Double], codomain=Bool),
#     '<=' : FunctionType(domain=[Double, Double], codomain=Bool),
#     '==' : FunctionType(domain=[Double, Double], codomain=Bool),
#     '!=' : FunctionType(domain=[Double, Double], codomain=Bool),
#     '>=' : FunctionType(domain=[Double, Double], codomain=Bool),
#     '>'  : FunctionType(domain=[Double, Double], codomain=Bool),
# }
# list(map(
#     Double.inner_scope.add,
#     (Symbol(name=name, type=type) for name, type in Double.members.items())))
#
# String.members = {
#     'new': FunctionType(domain=[String],         codomain=String),
#     '+'  : FunctionType(domain=[String, String], codomain=String),
# }
# list(map(
#     String.inner_scope.add,
#     (Symbol(name=name, type=type) for name, type in String.members.items())))
#
# Bool.members = {
#     'new': FunctionType(domain=[Bool],       codomain=Bool),
#     'and': FunctionType(domain=[Bool, Bool], codomain=Bool),
#     'or' : FunctionType(domain=[Bool, Bool], codomain=Bool),
# }
# list(map(
#     Bool.inner_scope.add,
#     (Symbol(name=name, type=type) for name, type in Bool.members.items())))
#
# builtin_scope.add(Symbol(name='Type',     type=Type))
# builtin_scope.add(Symbol(name='Nothing',  type=Nothing))
# builtin_scope.add(Symbol(name='Anything', type=Anything))
# builtin_scope.add(Symbol(name='Int',      type=Int))
# builtin_scope.add(Symbol(name='Double',   type=Double))
# builtin_scope.add(Symbol(name='String',   type=String))
# builtin_scope.add(Symbol(name='Bool',     type=Bool))
# builtin_scope.add(Symbol(name='true',     type=Bool))
# builtin_scope.add(Symbol(name='false',    type=Bool))
#
# builtin_scope.typenames = {
#     'Type',
#     'Nothing',
#     'Anything',
#     'Int',
#     'Double',
#     'String',
#     'Bool',
# }
