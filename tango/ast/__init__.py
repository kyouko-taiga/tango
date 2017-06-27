from tango.wrapper import *

__all__ = [
    'IdentifierMutability', 'TypeModifier', 'Operator',
    'Node', 'NodeVisitor', 'NodeTransformer',
    'ModuleDecl', 'Block',
    'PropDecl', 'ParamDecl', 'FunDecl',
    'Assignment', 'If', 'Return',
    'CallArg', 'Call', 'BinaryExpr', 'Identifier', 'TypeIdentifier',
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

def IdentifierMutability_str(self):
    if self == IdentifierMutability.im_cst:
        return 'cst'
    if self == IdentifierMutability.im_mut:
        return 'mut'
    return str(IdentifierMutability.im_mut)

IdentifierMutability.__str__ = IdentifierMutability_str


def TypeModifier_str(self):
    if self == TypeModifier.tm_none:
        return ''
    if self == TypeModifier.tm_ref:
        return '&'
    if self == TypeModifier.tm_own:
        return '!'

TypeModifier.__str__ = TypeModifier_str


NodeMetadata_initializer = NodeMetadata.__init__
def NodeMetadata_init(self, **kwargs):
    NodeMetadata_initializer(self)
    self._py_attrs = {}
    for key, value in kwargs.items():
        self[key] = value

def NodeMetadata_contains(self, item):
    try:
        res = hasattr(self, item)
    except TypeError:
        return item in self._py_attrs

def NodeMetadata_getitem(self, item):
    if hasattr(self, item):
        return object.__getattribute__(self, item)
    return self._py_attrs[item]

def NodeMetadata_setitem(self, item, value):
    if hasattr(self, item):
        object.__setattr__(self, item, value)
    else:
        self._py_attrs[item] = value

NodeMetadata.__init__     = NodeMetadata_init
NodeMetadata.__contains__ = NodeMetadata_contains
NodeMetadata.__getitem__  = NodeMetadata_getitem
NodeMetadata.__setitem__  = NodeMetadata_setitem


NodeList_initializer = NodeList.__init__
def NodeList_init(self, nodes=None):
    NodeList_initializer(self)
    for n in (nodes or []):
        self.append(n)

NodeList.__init__ = NodeList_init


def monkeypatch_init(class_):
    class_initializer = class_.__init__
    def new_class_initializer(self, *args, meta=None, **kwargs):
        class_initializer(self, *args, **kwargs)
        self.__meta__ = NodeMetadata(**(meta or {}))
    class_.__init__ = new_class_initializer


monkeypatch_init(ModuleDecl)
ModuleDecl._fields = ('body', 'name',)
ModuleDecl.__str__ = lambda self: str(self.body)


Block_initializer = Block.__init__
def Block_init(self, statements=None, meta=None):
    Block_initializer(self, NodeList(statements))
    self.__meta__ = NodeMetadata(**(meta or {}))

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

monkeypatch_init(PropDecl)
PropDecl._fields  = ('name', 'mutability', 'type_annotation',)
PropDecl.__str__  = PropDecl_str


def ParamDecl_str(self):
    return '{} {}: {}'.format(self.mutability, self.name, self.type_annotation)

monkeypatch_init(ParamDecl)
ParamDecl._fields  = ('name', 'mutability', 'type_annotation',)
ParamDecl.__str__  = ParamDecl_str


def FunDecl_str(self):
        return 'fun {} ({}) -> {} {}'.format(
            self.name, self.parameter, self.codomain_annotation, self.body)

monkeypatch_init(FunDecl)
FunDecl._fields = ('name', 'parameter', 'codomain_annotation', 'body',)
FunDecl.__str__ = FunDecl_str


def Assignment_str(self):
    return '{} {} {}'.format(self.lvalue, self.operator, self.rvalue)

monkeypatch_init(Assignment)
Assignment._fields = ('lvalue', 'operator', 'rvalue',)
Assignment.__str__ = Assignment_str


def If_str(self):
    return 'if {} {}'.format(self.condition, self.body)

monkeypatch_init(If)
If._fields = ('condition', 'then_block', 'else_block',)
If.__str__ = If_str


def Return_str(self):
    return 'return {}'.format(self.value)

monkeypatch_init(Return)
Return._fields = ('value',)
Return.__str__ = Return_str


def BinaryExpr_str(self):
    return '({} {} {})'.format(self.left, self.operator, self.right)

monkeypatch_init(BinaryExpr)
BinaryExpr._fields = ('left', 'operator', 'right',)
BinaryExpr.__str__ = BinaryExpr_str


def CallArg_str(self):
    return '{} {} {}'.format(self.label, self.operator, self.value)

monkeypatch_init(CallArg)
CallArg._fields = ('label', 'operator', 'value',)
CallArg.__str__ = CallArg_str


def Call_str(self):
    return '{}({})'.format(self.callee, ', '.join(map(str, self.arguments)))

monkeypatch_init(Call)
Call._fields = ('callee', 'arguments',)
Call.__str__ = Call_str


monkeypatch_init(Identifier)
Identifier._fields = ('name',)
Identifier.__str__  = lambda self: self.name


def TypeIdentifier_str(self):
    if self.modifier == TypeModifier.tm_ref:
        return '&' + str(self.signature)
    if self.modifier == TypeModifier.tm_own:
        return '!' + str(self.signature)
    return str(self.signature)

monkeypatch_init(TypeIdentifier)
TypeIdentifier._fields = ('signature', 'modifier',)
TypeIdentifier.__str__ = TypeIdentifier_str


monkeypatch_init(IntLiteral)
IntLiteral._fields = ('value',)
IntLiteral.__str__ = lambda self: str(self.value)


monkeypatch_init(BoolLiteral)
BoolLiteral._fields = ('value',)
BoolLiteral.__str__ = lambda self: str(self.value)
