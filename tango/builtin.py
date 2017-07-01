from .module import Module, Symbol
from .scope import Scope
from .types import type_factory


builtin_module = Module(name='Tango')
Type           = type_factory.make_builtin('Type')
Nothing        = type_factory.make_builtin('Nothing')
Anything       = type_factory.make_builtin('Anything')
Int            = type_factory.make_builtin('Int')
Double         = type_factory.make_builtin('Double')
String         = type_factory.make_builtin('String')
Bool           = type_factory.make_builtin('Bool')

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
#
# String.members = {
#     'new': FunctionType(domain=[String],         codomain=String),
#     '+'  : FunctionType(domain=[String, String], codomain=String),
# }
#
# Bool.members = {
#     'new': FunctionType(domain=[Bool],       codomain=Bool),
#     'and': FunctionType(domain=[Bool, Bool], codomain=Bool),
#     'or' : FunctionType(domain=[Bool, Bool], codomain=Bool),
# }

builtin_module.symbols['Nothing']  = Symbol(name='Nothing',  type=Type)
builtin_module.symbols['Anything'] = Symbol(name='Anything', type=Type)
builtin_module.symbols['Int']      = Symbol(name='Int',      type=Type)
builtin_module.symbols['Double']   = Symbol(name='Double',   type=Type)
builtin_module.symbols['String']   = Symbol(name='String',   type=Type)
builtin_module.symbols['Bool']     = Symbol(name='Bool',     type=Type)

builtin_module.symbols['true']     = Symbol(name='true',     type=Bool)
builtin_module.symbols['false']    = Symbol(name='false',    type=Bool)
