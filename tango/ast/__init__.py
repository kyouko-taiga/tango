from tango.wrapper import (
    TypeModifier, Operator,
    Node, NodeMetadata, NodeList,
    ModuleDecl, Block,
    PropDecl, ParamDecl, FunDecl,
    Assignment, If, Return,
    CallArg, Call, BinaryExpr, Identifier, TypeIdentifier,
    IntLiteral, StringLiteral)


class NodeVisitor(object):

    def visit(self, node):
        method_name = 'visit_' + node.__class__.__name__
        return getattr(self, method_name, self.generic_visit)(node)

    def generic_visit(self, node):
        for attr in node._fields:
            value = getattr(node, attr)
            if isinstance(value, Node):
                self.visit(value)
            elif isinstance(value, NodeList):
                for item in value:
                    if isinstance(item, Node):
                        self.visit(item)


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

operator_str = {
    Operator.o_cpy: '=',
    Operator.o_ref: '&-',
    Operator.o_mov: '<-',
}

def Node_to_dict(self):
    data = {'__meta__': self.__meta__._py_attrs}
    data['__meta__']['type'] = self.__meta__.type

    for attr in self._fields:
        value = getattr(self, attr)
        if isinstance(value, Node):
            data[attr] = value.to_dict()
        elif isinstance(value, NodeList):
            values = []
            for item in value:
                if isinstance(item, Node):
                    values.append(item.to_dict())
                else:
                    values.append(item)
            data[attr] = values
        else:
            data[attr] = value

    return {self.__class__.__name__: data}

Node.to_dict = Node_to_dict


def TypeModifier_str(self):
    return ('@cst' if self == TypeModifier.tm_cst else
            '@mut' if self == TypeModifier.tm_mut else
            '@stk' if self == TypeModifier.tm_stk else
            '@shd' if self == TypeModifier.tm_shd else
            '@val' if self == TypeModifier.tm_val else
            '@ref' if self == TypeModifier.tm_ref else
            '@own' if self == TypeModifier.tm_own else '')

TypeModifier.__str__ = TypeModifier_str


NodeMetadata_initializer = NodeMetadata.__init__
def NodeMetadata_init(self, **kwargs):
    NodeMetadata_initializer(self)
    self._py_attrs = {}
    for key, value in kwargs.items():
        self[key] = value

def NodeMetadata_contains(self, item):
    try:
        return hasattr(self, item)
    except TypeError:
        return item in self._py_attrs

def NodeMetadata_getitem(self, item):
    try:
        return object.__getattribute__(self, item)
    except AttributeError:
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
    result = 'var {}'.format(self.name)
    if self.type_annotation:
        result += ': {}'.format(self.type_annotation)
    if self.initial_value:
        result += ' {} {}'.format(operator_str[self.initial_binding], self.initial_value)
    return result

monkeypatch_init(PropDecl)
PropDecl._fields  = ('name', 'type_annotation', 'initial_value', 'initial_binding',)
PropDecl.__str__  = PropDecl_str


def ParamDecl_str(self):
    result = '{}: {}'.format(self.name, self.type_annotation)
    if self.initial_value:
        result += ' {} {}'.format(operator_str[self.initial_binding], self.initial_value)
    return result

monkeypatch_init(ParamDecl)
ParamDecl._fields  = ('name', 'type_annotation',)
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
    result = str(self.signature)
    if self.modifiers & TypeModifier.tm_own:
        result = '@own ' + result
    if self.modifiers & TypeModifier.tm_ref:
        result = '@ref ' + result
    if self.modifiers & TypeModifier.tm_val:
        result = '@val ' + result
    if self.modifiers & TypeModifier.tm_shd:
        result = '@shd ' + result
    if self.modifiers & TypeModifier.tm_stk:
        result = '@stk ' + result
    if self.modifiers & TypeModifier.tm_mut:
        result = '@mut ' + result
    if self.modifiers & TypeModifier.tm_cst:
        result = '@cst ' + result
    return str(result)

monkeypatch_init(TypeIdentifier)
TypeIdentifier._fields = ('signature', 'modifiers',)
TypeIdentifier.__str__ = TypeIdentifier_str


monkeypatch_init(IntLiteral)
IntLiteral._fields = ('value',)
IntLiteral.__str__ = lambda self: str(self.value)


monkeypatch_init(StringLiteral)
StringLiteral._fields = ('value',)
StringLiteral.__str__ = lambda self: str(self.value)
