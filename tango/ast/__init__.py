from tango.wrapper import *

__all__ = [
    'Node', 'NodeVisitor', 'NodeTransformer',
    'Module', 'Block',
    'PropDecl', 'ParamDecl', 'FunDecl',
    'Assignment', 'If', 'Return',
    'CallArg', 'Call', 'BinaryExpr', 'Identifier',
    'IntLiteral', 'BoolLiteral',
]


class NodeVisitor(object):

    def visit(self, node):
        method_name = 'visit_' + node.__class__.__name__
        return getattr(self, method_name, self.generic_visit)(node)

    def generic_visit(self, node):
        for attr in node._fields:
            value = getattr(node, attr)
            if isinstance(value, list):
                for item in value:
                    if isinstance(item, Node):
                        self.visit(item)
            elif isinstance(value, Node):
                self.visit(value)


class NodeTransformer(NodeVisitor):

    def generic_visit(self, node):
        for attr in node._fields:
            value = getattr(node, attr)
            if isinstance(value, list):
                new_values = []
                for item in value:
                    if isinstance(item, Node):
                        new_value = self.visit(item)
                        if isinstance(new_value, list):
                            new_values.extend(value)
                        elif isinstance(new_value, Node):
                            new_values.append(new_value)
                    else:
                        new_values.append(item)
                setattr(node, attr, new_values)

            elif isinstance(value, Node):
                new_node = self.visit(value)
                setattr(node, attr, new_node)

        return node


# Following are some helper methods and properties we add by monkeypatching
# the C++ classes, so as to have a nicer API to work with.

NodeList_initializer = NodeList.__init__
def NodeList_init(self, nodes=None):
    NodeList_initializer(self)
    for n in (nodes or []):
        self.append(n)

NodeList.__init__ = NodeList_init


Module._fields = ('body', 'name',)
Module.__str__ = lambda self: str(self.body)


Block_initializer = Block.__init__
def Block_init(self, statements=None):
    Block_initializer(self, NodeList(statements))

def Block_str(self):
    result = '{\n'
    for statement in self.statements:
        result += '\n'.join('  ' + line for line in str(statement).split('\n'))
        result += '\n'
    return result + '}'

Block._fields  = ('statements',)
Block.__init__ = Block_init
Block.__str__  = Block_str


def PropDecl_str(self):
    return '{} {}: {}'.format(self.mutability, self.name, self.type_annotation)

PropDecl._fields = ('name', 'mutability', 'type_annotation',)
PropDecl.__str__ = PropDecl_str


def ParamDecl_str(self):
    return '{} {}: {}'.format(self.mutability, self.name, self.type_annotation)

ParamDecl._fields = ('name', 'mutability', 'type_annotation',)
ParamDecl.__str__ = ParamDecl_str


def FunDecl_str(self):
        return 'fun {} ({}) -> {} {}'.format(
            self.name, self.parameter, self.codomain_annotation, self.body)

FunDecl._fields = ('name', 'parameter', 'codomain_annotation', 'body',)
FunDecl.__str__ = FunDecl_str


def Assignment_str(self):
    return '{} {} {}'.format(self.lvalue, self.operator, self.rvalue)

Assignment._fields = ('lvalue', 'operator', 'rvalue',)
Assignment.__str__ = Assignment_str


def If_str(self):
    return 'if {} {}'.format(self.condition, self.body)

If._fields = ('condition', 'then_block', 'else_block',)
If.__str__ = If_str


def Return_str(self):
    return 'return {}'.format(self.value)

Return._fields = ('value',)
Return.__str__ = Return_str


def BinaryExpr_str(self):
    return '({} {} {})'.format(self.left, self.operator, self.right)

BinaryExpr._fields = ('left', 'operator', 'right',)
BinaryExpr.__str__ = BinaryExpr_str


def CallArg_str(self):
    return '{} {} {}'.format(self.label, self.operator, self.value)

CallArg._fields = ('label', 'operator', 'value',)
CallArg.__str__ = CallArg_str


def Call_str(self):
    return '{}({})'.format(self.callee, ', '.join(map(str, self.arguments)))

Call._fields = ('callee', 'arguments',)
Call.__str__ = Call_str


Identifier._fields = ('name',)
Identifier.__str__ = lambda self: self.name


IntLiteral._fields = ('value',)
IntLiteral.__str__ = lambda self: str(self.value)


BoolLiteral._fields = ('value',)
BoolLiteral.__str__ = lambda self: str(self.value)
