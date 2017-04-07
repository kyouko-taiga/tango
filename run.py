import sys

from tango.parser import parse
from tango.scope_binder import bind_scopes
from tango.type_deducer import infer_types


if __name__ == '__main__':
    filename = sys.argv[1]
    with open(filename) as f:
        source = f.read()

    # Parse the given source file.
    module = parse(source)
    module.name = '__main__'

    # Bind all identifiers to their respective scope.
    bind_scopes(module)

    # Infer the types of the parsed expressions.
    types = infer_types(module)

    # import pprint
    # printer = pprint.PrettyPrinter(indent=2)
    # printer.pprint(types)
    # exit(0)

    for (symbol, inferred_type) in types.items():
        if symbol is None:
            continue

        (scope, name) = symbol
        print('%s.%s: %s' % (scope.name, name, ', '.join(map(str, inferred_type))))
