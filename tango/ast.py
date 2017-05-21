class Node(object):

    _fields = tuple()

    def __init__(self):
        self.__info__ = {}

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

    _fields = ('body',)

    def __init__(self, body):
        super().__init__()
        self.body = body

    def __str__(self):
        return '\n'.join(map(str, self.body.statements))


class Block(Node):

    _fields = ('statements',)

    def __init__(self, statements=None):
        super().__init__()
        self.statements = statements or []

    def __str__(self):
        result = '{\n'
        for statement in self.statements:
            result += '\n'.join('  ' + line for line in str(statement).split('\n'))
            result += '\n'
        return result + '}'


class VariableDecl(Node):

    _fields = ('name', 'type_annotation',)

    def __init__(self, name, type_annotation):
        super().__init__()
        self.name            = name
        self.type_annotation = type_annotation

    def __str__(self):
        return 'var {}: {}'.format(self.name, self.type_annotation)


class FunctionDecl(Node):

    _fields = ('name', 'signature', 'body',)

    def __init__(self, name, signature, body):
        super().__init__()
        self.name      = name
        self.signature = signature
        self.body      = body

    def __str__(self):
        return 'fun {} {} {}'.format(self.name, self.signature, self.body)


class FunctionParameter(Node):

    _fields = ('name', 'type_annotation',)

    def __init__(self, name, type_annotation):
        super().__init__()
        self.name            = name
        self.type_annotation = type_annotation

    def __str__(self):
        return 'var {}: {}'.format(self.name, self.type_annotation)


class Assign(Node):

    _fields = ('target', 'operator', 'expression',)

    def __init__(self, target, operator, expression):
        super().__init__()
        self.target     = target
        self.operator   = operator
        self.expression = expression

    def __str__(self):
        return '{} {} {}'.format(self.target, self.operator, self.expression)


class If(Node):

    _fields = ('condition', 'body',)

    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

    def __str__(self):
        return 'if {} {}'.format(self.condition, self.body)


class Return(Node):

    _fields = ('value',)

    def __init__(self, value):
        super().__init__()
        self.value = value

    def __str__(self):
        return 'return {}'.format(self.value)


class FunctionSignature(Node):

    _fields = ('domain', 'codomain',)

    def __init__(self, domain, codomain):
        super().__init__()
        self.domain   = domain
        self.codomain = codomain

    def __str__(self):
        return '({}) -> {}'.format(self.domain, self.codomain)


class PrefixExpression(Node):

    _fields = ('operator', 'operand',)

    def __init__(self, operator, operand):
        super().__init__()
        self.operator = operator
        self.operand  = operand

    def __str__(self):
        return '{} {}'.format(self.operator, self.operand)


class BinaryExpression(Node):

    _fields = ('left', 'operator', 'right')

    def __init__(self, left, operator, right):
        super().__init__()
        self.left     = left
        self.operator = operator
        self.right    = right

    def __str__(self):
        return '({} {} {})'.format(self.left, self.operator, self.right)


class Call(Node):

    _fields = ('callee', 'argument',)

    def __init__(self, callee, argument):
        super().__init__()
        self.callee = callee
        self.argument = argument

    def __str__(self):
        return '{}({})'.format(self.callee, self.argument)


class Identifier(Node):

    _fields = ('name',)

    def __init__(self, name):
        super().__init__()
        self.name = name

    def __str__(self):
        return self.name


class Literal(Node):

    _fields = ('value',)

    def __init__(self, value):
        super().__init__()
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
