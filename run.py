import os
import sys

from tango.light import parser, TangoLightTransformer


if __name__ == '__main__':
    filename = sys.argv[1]
    with open(filename) as f:
        source = f.read()

    tree = parser.parse(source)

    transformer = TangoLightTransformer()
    trans = transformer.transform(tree)
    print(trans)
