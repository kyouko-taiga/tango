import os
import sys

from tango.module_loader import ModuleLoader

# from tango.parser import parse
# from tango.scope_binder import SymbolsExtractor, ScopeBinder
# from tango.type_builder import build_types
# from tango.type_solver import infer_types
# from tango.type_disambiguator import disambiguate_types
#
# from tango.transpilers.python import transpile

from tango.ast import Node
from tango.scope_binder import Scope
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
    module_loader = ModuleLoader(search_paths=[os.path.dirname(filename)])
    module_decl = module_loader.load(os.path.splitext(filename)[0])

    if len(sys.argv) > 2:
        if sys.argv[2] == '--unparse':
            print(module_decl)
        elif sys.argv[2] == '--dump-ast':
            print(dumps(module_decl.to_dict(), indent=2, sort_keys=True, cls=SetEncoder))


if __name__ != '__main__':
    filename = sys.argv[1]
    with open(filename) as f:
        source = f.read()

    # Parse the given source file.
    module_decl = parse(source)
    module_decl.name = '__main__'
    print(module_decl)
    # print(dumps(module.to_dict(), indent=2, sort_keys=True, cls=SetEncoder))
    exit(0)

    # Build the types syntactic definitions.
    module = build_types(module)
    # print(module)
    # exit(0)

    # Extract the symbols declared in each scopes.
    symbols_extractor = SymbolsExtractor()
    module = symbols_extractor.visit(module)
    # print(dumps(module.to_dict(), indent=2, sort_keys=True, cls=SetEncoder))
    # exit(0)

    # Bind all identifiers to their respective scope.
    scope_binder = ScopeBinder()
    scope_binder.visit(module)
    # print(dumps(module.to_dict(), indent=2, sort_keys=True, cls=SetEncoder))
    # exit(0)

    # Infer the types of all expressions.
    (module, environment) = infer_types(module)
    types = environment.storage

    for (symbol, inferred_type) in types.items():
        if isinstance(symbol.id, tuple) and isinstance(symbol.id[0], Scope):
            scope, name = symbol.id
            if scope.name == 'Tango':
                continue
            name = '.'.join(scope.qualified_name.split('.')[2:]) + '.' + name
            print('{:20}{:}'.format(name, inferred_type))

    # Disambiguate the types of each expression.
    (module, ambiguous_nodes) = disambiguate_types(module)
    # print(dumps(module.to_dict(), indent=2, sort_keys=True, cls=SetEncoder))
    # exit(0)

    for node in ambiguous_nodes:
        message = (
            'the type of %s is ambiguous; the following candidates were found: %s' %
            (node, node.__info__['type']))
        print(message + '\n', file=sys.stderr)

    # Transpile the module into python.
    if not os.path.exists('build'):
        os.makedirs('build')
    with open('build/main.py', 'w') as stream:
        transpile(module, stream)
