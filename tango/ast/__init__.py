from copy import deepcopy

from tango.wrapper import (
    Operator,
    Node, NodeMetadata, NodeList,
    ModuleDecl, Block,
    PropDecl, StructDecl, ParamDecl, FunDecl,
    Assignment, If, Return,
    Argument, Call, Select, BinaryExpr,
    Identifier, TypeIdentifier, FunSignParam, FunSign,
    IntLiteral, DoubleLiteral, StringLiteral, BoolLiteral)
from tango.types import TM


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
            if isinstance(value, Node):
                new_node = self.visit(value)
                setattr(node, attr, new_node)
            elif isinstance(value, NodeList):
                new_values = []
                for item in value:
                    if isinstance(item, Node):
                        new_value = self.visit(item)
                        if isinstance(new_value, Node):
                            new_values.append(new_value)
                        elif isinstance(new_value, NodeList):
                            new_values.extend(new_value)
                setattr(node, attr, NodeList(new_values))

        return node


def make_descriptor(name):

    class Descriptor(object):

        def __get__(self, obj, objtype=None):
            if obj is None:
                return self
            return obj.__meta__[name]

        def __set__(self, obj, value):
            if obj is None:
                return self
            obj.__meta__[name] = value

    return Descriptor()


# Following are some helper methods and properties we add by monkeypatching
# the C++ classes, so as to have a nicer API to work with.

def modifiers_to_str(modifiers):
    result = []
    if (modifiers & TM.mut) and (modifiers & TM.stk):
        result.append('@mut')
    if modifiers & TM.ref:
        result.append('@ref')
    if modifiers & TM.shd:
        result.append('@shd')
    if modifiers & TM.own:
        result.append('@own')
    return ' '.join(result)


operator_str = {
    Operator.add: '+',
    Operator.sub: '-',
    Operator.mul: '*',
    Operator.div: '/',
    Operator.cpy: '=',
    Operator.ref: '&-',
    Operator.mov: '<-',
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

def Node_deepcopy(self, memo):
    new_node = self.__class__(**{
        attr: deepcopy(getattr(self, attr), memo)
        for attr in self._fields
    })

    # Note we don't deepcopy the metadata.
    new_node.__meta__['type'] = self.__meta__['type']
    for attr, value in self.__meta__._py_attrs.items():
        new_node.__meta__[attr] = value
    return new_node

Node.to_dict      = Node_to_dict
Node.__deepcopy__ = Node_deepcopy


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

def NodeList_deepcopy(self, memo):
    return NodeList([deepcopy(it, memo) for it in self])

NodeList.__init__     = NodeList_init
NodeList.__deepcopy__ = NodeList_deepcopy


def monkeypatch_init(class_):
    class_initializer = class_.__init__
    def new_class_initializer(self, *args, meta=None, **kwargs):
        class_initializer(self, *args, **kwargs)
        self.__meta__ = NodeMetadata(**(meta or {}))
    class_.__init__ = new_class_initializer


def ModuleDecl_str(self):
    return '\n'.join(map(str, self.body.statements))

monkeypatch_init(ModuleDecl)
ModuleDecl._fields = ('body', 'name',)
ModuleDecl.__str__ = ModuleDecl_str


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
    result = 'let {}'.format(self.name)
    if self.type_annotation:
        annotation = str(self.type_annotation)
        if annotation:
            result += ': {}'.format(self.type_annotation)
    if self.initial_value:
        result += ' {} {}'.format(operator_str[self.initial_binding], self.initial_value)
    return result

monkeypatch_init(PropDecl)
PropDecl._fields  = ('name', 'type_annotation', 'initial_value', 'initial_binding',)
PropDecl.__str__  = PropDecl_str


def ParamDecl_str(self):
    if self.label == self.name:
        result = f'{self.name}'
    else:
        result = f'{self.label} {self.name}'
    if self.type_annotation:
        result += f': {self.type_annotation}'
    return result

monkeypatch_init(ParamDecl)
ParamDecl._fields  = ('name', 'label', 'type_annotation',)
ParamDecl.__str__  = ParamDecl_str


StructDecl_initializer = StructDecl.__init__
def StructDecl_init(self, placeholders=None, conformances=None, meta=None, **kwargs):
    StructDecl_initializer(self, **kwargs)
    self.__meta__ = NodeMetadata(**(meta or {}))
    self.__meta__['placeholders'] = placeholders or []
    self.__meta__['conformances'] = conformances or []

def StructDecl_str(self):
    placeholders = (
        '<' + ', '.join(map(str, self.placeholders)) + '>'
        if self.placeholders else '')
    conformances = (
        ' conforms ' + ', '.join(map(str, self.conformances)) + '>'
        if self.placeholders else '')

    return f'struct {self.name}{placeholders}{conformances} {self.body}'

StructDecl._fields  = ('name', 'placeholders', 'conformances', 'body',)
StructDecl.placeholders = make_descriptor('placeholders')
StructDecl.conformances = make_descriptor('conformances')
StructDecl.__init__     = StructDecl_init
StructDecl.__str__      = StructDecl_str


FunDecl_initializer = FunDecl.__init__
def FunDecl_init(self, placeholders=None, parameters=None, meta=None, **kwargs):
    FunDecl_initializer(self, parameters=NodeList(parameters), **kwargs)
    self.__meta__ = NodeMetadata(**(meta or {}))
    self.__meta__['placeholders'] = placeholders or []

def FunDecl_str(self):
    placeholders = '<' + ', '.join(map(str, self.placeholders)) + '>' if self.placeholders else ''
    parameters   = ', '.join(map(str, self.parameters))
    codomain     = self.codomain_annotation or 'Nothing'
    return 'function {}{}({}) -> {} {}'.format(
        self.name, placeholders, parameters, codomain, self.body)

FunDecl._fields  = ('name', 'placeholders', 'parameters', 'codomain_annotation', 'body',)
FunDecl.placeholders = make_descriptor('placeholders')
FunDecl.__init__     = FunDecl_init
FunDecl.__str__      = FunDecl_str


def Assignment_str(self):
    return '{} {} {}'.format(self.lvalue, operator_str[self.operator], self.rvalue)

monkeypatch_init(Assignment)
Assignment._fields = ('lvalue', 'operator', 'rvalue',)
Assignment.__str__ = Assignment_str


def If_str(self):
    if self.else_block:
        return 'if {} {} else {}'.format(self.condition, self.then_block, self.else_block)
    return 'if {} {}'.format(self.condition, self.then_block)

monkeypatch_init(If)
If._fields = ('condition', 'then_block', 'else_block',)
If.__str__ = If_str


def Return_str(self):
    return 'return {}'.format(self.value)

monkeypatch_init(Return)
Return._fields = ('value',)
Return.__str__ = Return_str


def BinaryExpr_str(self):
    return '({} {} {})'.format(self.left, operator_str[self.operator], self.right)

monkeypatch_init(BinaryExpr)
BinaryExpr._fields = ('left', 'operator', 'right',)
BinaryExpr.__str__ = BinaryExpr_str


def Argument_str(self):
    return '{} {} {}'.format(self.label, operator_str[self.operator], self.value)

monkeypatch_init(Argument)
Argument._fields = ('label', 'operator', 'value',)
Argument.__str__ = Argument_str


Call_initializer = Call.__init__
def Call_init(self, arguments=None, meta=None, **kwargs):
    Call_initializer(self, arguments=NodeList(arguments), **kwargs)
    self.__meta__ = NodeMetadata(**(meta or {}))

def Call_str(self):
    return '{}({})'.format(self.callee, ', '.join(map(str, self.arguments)))

Call._fields  = ('callee', 'arguments',)
Call.__init__ = Call_init
Call.__str__  = Call_str


monkeypatch_init(Select)
Select._fields = ('owner', 'member',)
Select.__str__  = lambda self: f'{self.owner}.{self.member}'


monkeypatch_init(Identifier)
Identifier._fields = ('name',)
Identifier.__str__  = lambda self: self.name


def TypeIdentifier_str(self):
    modifiers = modifiers_to_str(self.modifiers)
    if modifiers:
        if self.signature:
            return modifiers + ' ' + str(self.signature)
        return modifiers

    if self.signature:
        return str(self.signature)

    return ''

monkeypatch_init(TypeIdentifier)
TypeIdentifier._fields = ('signature', 'modifiers',)
TypeIdentifier.__str__ = TypeIdentifier_str


monkeypatch_init(FunSignParam)
FunSignParam._fields = ('label', 'type_annotation',)
FunSignParam.__str__  = lambda self: '{}: {}'.format(self.label, self.type_annotation)


FunSign_initializer = FunSign.__init__
def FunSign_init(self, parameters=None, meta=None, **kwargs):
    FunSign_initializer(self, parameters=NodeList(parameters), **kwargs)
    self.__meta__ = NodeMetadata(**(meta or {}))

FunSign._fields  = ('parameters', 'codomain_annotation',)
FunSign.__init__ = FunSign_init
FunSign.__str__  = lambda self: '({}) -> {}'.format(
    ', '.join(map(str, self.parameters)), self.codomain_annotation)


monkeypatch_init(IntLiteral)
IntLiteral._fields = ('value',)
IntLiteral.__str__ = lambda self: str(self.value)


monkeypatch_init(DoubleLiteral)
DoubleLiteral._fields = ('value',)
DoubleLiteral.__str__ = lambda self: str(self.value)


monkeypatch_init(StringLiteral)
StringLiteral._fields = ('value',)
StringLiteral.__str__ = lambda self: "'" + str(self.value) + "'"


monkeypatch_init(BoolLiteral)
BoolLiteral._fields = ('value',)
BoolLiteral.__str__ = lambda self: str(self.value)
