import os
import sys

from tango.parser import parse
from tango.scope_binder import SymbolsExtractor, ScopeBinder, SelectScopeBinder
from tango.type_solver import infer_types
from tango.type_disambiguator import disambiguate_types

from tango.transpilers.cpp import transpile

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
            return obj.name
        if isinstance(obj, BaseType):
            return str(obj)
        return JSONEncoder.default(self, obj)


if __name__ == '__main__':
    filename = sys.argv[1]
    with open(filename) as f:
        source = f.read()

    # Parse the given source file.
    module = parse(source)
    module.name = '__main__'
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
            print('{:20}{:}'.format(scope.name + '.' + name, inferred_type))

    # Disambiguate the types of each expression.
    (module, ambiguous_nodes) = disambiguate_types(module)
    # print(dumps(module.to_dict(), indent=2, sort_keys=True, cls=SetEncoder))
    # exit(0)

    for node in ambiguous_nodes:
        message = (
            'the type of %s is ambiguous; the following candidates were found: %s' %
            (node, node.__info__['type']))
        print(message + '\n', file=sys.stderr)

    # Transpile the module into c++.
    if not os.path.exists('build'):
        os.makedirs('build')
    with open('build/main.hh', 'w') as header_stream:
        with open('build/main.cc', 'w') as source_stream:
            transpile(module, header_stream, source_stream)
