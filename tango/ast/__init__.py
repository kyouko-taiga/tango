from .ast import *


# Here we define some helper methods to the C++ classes before we re-export
# them, so as to have a nicer API to work with.
__all__ = ['Node', 'Block', 'IntegerLiteral']


def Block_str(self):
    result = '{\n'
    for statement in self.statements:
        result += '\n'.join('  ' + line for line in str(statement).split('\n'))
        result += '\n'
    return result + '}'

Block.__str__ = Block_str

IntegerLiteral.__str__ = lambda self: str(self.value)
