from .module import Module, Symbol
from .scope import Scope
from .types import type_factory, TypeName


builtin_module = Module(name = 'Tango')
Type           = type_factory.make_builtin(modifiers=0, name='Type')
Nothing        = type_factory.make_builtin(modifiers=0, name='Nothing')
Anything       = type_factory.make_builtin(modifiers=0, name='Anything')
Int            = type_factory.make_builtin(modifiers=0, name='Int')
Double         = type_factory.make_builtin(modifiers=0, name='Double')
String         = type_factory.make_builtin(modifiers=0, name='String')
Bool           = type_factory.make_builtin(modifiers=0, name='Bool')

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

def builtin_symbol(builtin_type):
    return Symbol(
        name = builtin_type.name,
        type = type_factory.make_name(
            name = builtin_type.name,
            type = builtin_type))

builtin_module.symbols['Nothing']  = builtin_symbol(Nothing)
builtin_module.symbols['Anything'] = builtin_symbol(Anything)
builtin_module.symbols['Int']      = builtin_symbol(Int)
builtin_module.symbols['Double']   = builtin_symbol(Double)
builtin_module.symbols['String']   = builtin_symbol(String)
builtin_module.symbols['Bool']     = builtin_symbol(Bool)
