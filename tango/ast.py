class Node(object):

    _fields = tuple()

    def __init__(self, meta=None):
        self.__meta__ = meta or {}

    def to_dict(self):
        data = {}
        for attr, value in self.__dict__.items():
            if isinstance(value, list):
                values = []
                for item in value:
                    if isinstance(item, Node):
                        values.append(item.to_dict())
                    else:
                        values.append(item)
                data[attr] = values
            elif isinstance(value, Node):
                data[attr] = value.to_dict()
            else:
                data[attr] = value

        return {self.__class__.__name__: data}


class ModuleDecl(Node):

    _fields = ('body','name',)

    def __init__(self, body, name=None, meta=None):
        super().__init__(meta)
        self.body = body
        self.name = name

    def __str__(self):
        return '\n'.join(map(str, self.body.statements))


class Block(Node):

    _fields = ('statements',)

    def __init__(self, statements=None, meta=None):
        super().__init__(meta)
        self.statements = statements or []

    def __str__(self):
        result = '{\n'
        for statement in self.statements:
            result += '\n'.join('  ' + line for line in str(statement).split('\n'))
            result += '\n'
        return result + '}'


class PropertyDecl(Node):

    _fields = ('name', 'mutability', 'type_annotation',)

    def __init__(self, name, mutability, type_annotation, meta=None):
        super().__init__(meta)
        self.name            = name
        self.mutability      = mutability
        self.type_annotation = type_annotation

    def __str__(self):
        return '{} {}: {}'.format(self.mutability, self.name, self.type_annotation)


class FunctionDecl(Node):

    _fields = ('name', 'parameter', 'return_type', 'body',)

    def __init__(self, name, parameter, return_type, body, meta=None):
        super().__init__(meta)
        self.name         = name
        self.parameter   = parameter
        self.return_type = return_type
        self.body        = body

    def __str__(self):
        return 'fun {} ({}) -> {} {}'.format(
            self.name, self.parameter, self.return_type, self.body)


class FunctionParameter(Node):

    _fields = ('name', 'mutability', 'type_annotation',)

    def __init__(self, name, mutability, type_annotation, meta=None):
        super().__init__(meta)
        self.name            = name
        self.mutability      = mutability
        self.type_annotation = type_annotation

    def __str__(self):
        return '{} {}: {}'.format(self.mutability, self.name, self.type_annotation)


class Assignment(Node):

    _fields = ('lvalue', 'operator', 'rvalue',)

    def __init__(self, lvalue, operator, rvalue, meta=None):
        super().__init__(meta)
        self.lvalue   = lvalue
        self.operator = operator
        self.rvalue   = rvalue

    def __str__(self):
        return '{} {} {}'.format(self.lvalue, self.operator, self.rvalue)


class If(Node):

    _fields = ('condition', 'body',)

    def __init__(self, condition, body, meta=None):
        super().__init__(meta)
        self.condition = condition
        self.body = body

    def __str__(self):
        return 'if {} {}'.format(self.condition, self.body)


class Return(Node):

    _fields = ('value',)

    def __init__(self, value, meta=None):
        super().__init__(meta)
        self.value = value

    def __str__(self):
        return 'return {}'.format(self.value)


class FunctionSignature(Node):

    _fields = ('parameter', 'return_type',)

    def __init__(self, parameter, return_type, meta=None):
        super().__init__(meta)
        self.parameter   = parameter
        self.return_type = return_type

    def __str__(self):
        return '({}) -> {}'.format(self.parameter, self.return_type)


class SignatureParameter(Node):

    _fields = ('label', 'mutability', 'type_annotation',)

    def __init__(self, label, mutability, type_annotation, meta=None):
        super().__init__(meta)
        self.label           = label
        self.mutability      = mutability
        self.type_annotation = type_annotation

    def __str__(self):
        return '{} {}: {}'.format(self.mutability, self.label, self.type_annotation)


class PrefixExpression(Node):

    _fields = ('operator', 'operand',)

    def __init__(self, operator, operand, meta=None):
        super().__init__(meta)
        self.operator = operator
        self.operand  = operand

    def __str__(self):
        return '{} {}'.format(self.operator, self.operand)


class BinaryExpression(Node):

    _fields = ('left', 'operator', 'right')

    def __init__(self, left, operator, right, meta=None):
        super().__init__(meta)
        self.left     = left
        self.operator = operator
        self.right    = right

    def __str__(self):
        return '({} {} {})'.format(self.left, self.operator, self.right)


class Call(Node):

    _fields = ('callee', 'argument',)

    def __init__(self, callee, argument, meta=None):
        super().__init__(meta)
        self.callee = callee
        self.argument = argument

    def __str__(self):
        return '{}({})'.format(self.callee, self.argument)


class CallArgument(Node):

    _fields = ('label', 'operator', 'value',)

    def __init__(self, label, operator, value, meta=None):
        super().__init__(meta)
        self.label    = label
        self.operator = operator
        self.value    = value

    def __str__(self):
        return '{} {} {}'.format(self.label, self.operator, self.value)


class Identifier(Node):

    _fields = ('name',)

    def __init__(self, name, meta=None):
        super().__init__(meta)
        self.name = name

    def __str__(self):
        return self.name


class Literal(Node):

    _fields = ('value',)

    def __init__(self, value, meta=None):
        super().__init__(meta)
        self.value = value

    def __str__(self):
        return str(self.value)


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
