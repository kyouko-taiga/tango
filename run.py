import os
import linecache
import sys

from lark.common import UnexpectedToken

from tango.light import parser, ParseTreeTransformer
from tango.scope_binder import ScopeBinder, SymbolsExtractor
from tango.type_solver import infer_types
from tango.state_checker import CaptureFinder, StateChecker

from tango.wrapper import emit_ir

from tango.ast import Node
from tango.scope import Scope
from tango.types import TypeBase
from tango.type_solver import TypeBase, TypeVariable
from json import dumps, JSONEncoder

class ASTEncoder(JSONEncoder):

    def default(self, obj):
        if isinstance(obj, set):
            return list(obj)
        if isinstance(obj, Node):
            return repr(obj)
        if isinstance(obj, Scope):
            return obj.qualified_name
        if isinstance(obj, TypeBase):
            return str(obj)
        return JSONEncoder.default(self, obj)


if __name__ == '__main__':
    filename = sys.argv[1]
    with open(filename) as f:
        source = f.read()

    # Parse the module declaration and produce an AST.
    try:
        parse_tree = parser.parse(source)
    except UnexpectedToken as e:
        print(
            '{}:{}:{}: syntax error - unexpected token'.format(filename, e.line, e.column),
            file=sys.stderr)
        print(linecache.getline(filename, e.line), file=sys.stderr, end='')
        print(' ' * (e.column - 1) + '^', file=sys.stderr)
        exit(1)

    transformer      = ParseTreeTransformer(filename)
    module_decl      = transformer.transform(parse_tree)
    module_decl.name = os.path.splitext(os.path.basename(filename))[0]

    # Annotate each scope-opening node with the symols it declares.
    symbols_extractor = SymbolsExtractor()
    symbols_extractor.visit(module_decl)

    # Bind all symbols of the module to their respective scope.
    scope_binder = ScopeBinder()
    scope_binder.visit(module_decl)
    # print(dumps(module_decl.to_dict(), indent=2, sort_keys=True, cls=ASTEncoder))

    # Infer the types of all expressions.
    (module, environment) = infer_types(module_decl)

    # environment.print_debug()
    if (len(sys.argv) > 2) and (sys.argv[2] == '-O'):
        emit_ir(module, with_optimizations=True)
    else:
        emit_ir(module, with_optimizations=False)
    exit()

    # Check the correctness of resource access.
    capture_finder = CaptureFinder()
    capture_finder.visit(module_decl)
    state_checker = StateChecker()
    state_checker.visit(module_decl)

    # print(dumps(module_decl.to_dict(), indent=2, sort_keys=True, cls=ASTEncoder))
    # print(module_decl)
