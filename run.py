import sys

from tango.parser import parse
from tango.scope_binder import bind_scopes, SymbolsExtractor, ScopeBinder
from tango.type_checker import infer_types

from tango.ast import Node
from tango.builtin import Scope, Type
from json import dumps, JSONEncoder

class SetEncoder(JSONEncoder):

    def default(self, obj):
        if isinstance(obj, set):
            return list(obj)
        if isinstance(obj, Node):
            return repr(obj)
        if isinstance(obj, Scope):
            return obj.uri
        if isinstance(obj, Type):
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

    # Bind all identifiers to their respective scope.
    scope_binder = ScopeBinder()
    scope_binder.visit(module)
    # print(dumps(module.to_dict(), indent=2, sort_keys=True, cls=SetEncoder))

    # Infer the types of the parsed expressions.
    types = infer_types(module)

    # import pprint
    # printer = pprint.PrettyPrinter(indent=2)
    # printer.pprint(types)
    # exit(0)

    for (symbol, inferred_type) in types.items():
        (scope, name) = symbol
        if scope.id == 0:
            continue
        print('%s.%s: %s' % (scope.uri, name, ', '.join(map(str, inferred_type))))
