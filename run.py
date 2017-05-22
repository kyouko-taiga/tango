import os
import sys

from tango.light import parser, TangoLightTransformer
from tango.scope_binder import ScopeBinder, SymbolsExtractor

from tango.ast import Node
from tango.scope import Scope
from tango.types import BaseType
from json import dumps, JSONEncoder

class SetEncoder(JSONEncoder):

    def default(self, obj):
        if isinstance(obj, set):
            return list(obj)
        if isinstance(obj, Node):
            return repr(obj)
        if isinstance(obj, Scope):
            return obj.qualified_name
        if isinstance(obj, BaseType):
            return str(obj)
        return JSONEncoder.default(self, obj)


if __name__ == '__main__':
    filename = sys.argv[1]
    with open(filename) as f:
        source = f.read()

    # Parse the module declaration and produce an AST.
    parse_tree = parser.parse(source)
    transformer = TangoLightTransformer()
    module_decl = transformer.transform(parse_tree)
    module_decl.name = os.path.splitext(os.path.basename(filename))[0]

    # Annotate each scope-opening node with the symols it declares.
    symbols_extractor = SymbolsExtractor()
    symbols_extractor.visit(module_decl)

    # Bind all symbols of the module to their respective scope.
    scope_binder = ScopeBinder()
    scope_binder.visit(module_decl)

    print(module_decl)
