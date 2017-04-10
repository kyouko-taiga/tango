import sys

from tango.parser import parse
from tango.scope_binder import bind_scopes, SymbolsExtractor, ScopeBinder
from tango.type_checker import infer_types

from tango.ast import Node
from tango.builtin import Scope
from tango.types import BaseType
from json import dumps, JSONEncoder

class SetEncoder(JSONEncoder):

    def default(self, obj):
        if isinstance(obj, set):
            return list(obj)
        if isinstance(obj, Node):
            return repr(obj)
        if isinstance(obj, Scope):
            return obj.uri
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

    # Infer the types of the parsed expressions.
    types = infer_types(module)

    for (symbol, inferred_type) in types.items():
        if isinstance(symbol.id, tuple):
            scope, name = symbol.id
            if scope.id == 0:
                continue
            print(scope.uri + '.' + name + ':', inferred_type)
        # else:
        #     print(symbol, inferred_type)
        # print('%s.%s: %s' % (scope.uri, name, ', '.join(map(str, inferred_type))))
