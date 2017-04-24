class Node(object):

    _fields = tuple()

    def __init__(self):
        self.__info__ = {}

    def is_ancestor_of(self, other):
        if self is other:
            return True
        for value in self.__dict__.values():
            if isinstance(value, list):
                for item in value:
                    if isinstance(item, Node) and item.is_ancestor_of(other):
                        return True
            elif isinstance(value, Node) and value.is_ancestor_of(other):
                    return True
        return False

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


class Identifier(Node):

    _fields = ('name',)

    def __init__(self, name):
        super().__init__()
        self.name = name

    @property
    def qualname(self):
        if 'scope' in self.__info__:
            return '%s.%s' % (self.__info__['scope'].name, self.name)
        return self.name

    def __str__(self):
        return self.name


class Block(Node):

    _fields = ('statements',)

    def __init__(self, statements=None):
        super().__init__()
        self.statements = statements or []

    def __str__(self):
        return '{%s\n}' % ''.join('\n%s' % statement for statement in self.statements)


class SpecializationParameter(Node):

    _fields = ('name', 'type_annotation',)

    def __init__(self, name, type_annotation):
        super().__init__()
        self.name = name
        self.type_annotation = type_annotation

    def __str__(self):
        return '%s = %s' % (self.name, self.type_annotation)


class TypeIdentifier(Node):

    _fields = ('name', 'specialization_parameters',)

    def __init__(self, name, specialization_parameters=None):
        super().__init__()
        self.name = name
        self.specialization_parameters = specialization_parameters or []

    def __str__(self):
        if self.specialization_parameters:
            return '%s [%s]' % (self.name, ', '.join(map(str, self.specialization_parameters)))
        return str(self.name)

    def __repr__(self):
        return 'TypeIdentifier(%s)' % str(self)


class FunctionParameter(Node):

    _fields = ('name', 'label', 'type_annotation', 'attributes', 'default_value',)

    def __init__(self, name, label, type_annotation, attributes=None, default_value=None):
        super().__init__()
        self.name = name
        self.label = label
        self.type_annotation = type_annotation
        self.attributes = attributes or set()
        self.default_value = default_value

    def __str__(self):
        if 'mutable' in self.attributes:
            mutability_modifier = 'mut'
            attributes = list(filter(lambda attr: attr != 'mutable', self.attributes))
        else:
            mutability_modifier = 'cst'
            attributes = []

        if attributes:
            attributes = ' '.join('@%s' % attribute for attribute in self.attributes) + ' '
        else:
            attributes = ''

        if self.name != self.label:
            label = self.label or '_'
            return '%s %s %s: %s%s' % (
                mutability_modifier, label, self.name, attributes, self.type_annotation)
        return '%s %s: %s%s' % (mutability_modifier, self.name, attributes, self.type_annotation)


class FunctionSignature(Node):

    _fields = ('parameters', 'return_type',)

    def __init__(self, parameters, return_type):
        super().__init__()
        self.parameters = parameters
        self.return_type = return_type

    def __str__(self):
        return '(%s) -> %s' % (', '.join(map(str, self.parameters)), self.return_type)


class Literal(Node):

    _fields = ('value',)

    def __init__(self, value):
        super().__init__()
        self.value = value

    def __str__(self):
        return str(self.value)


class ArrayLiteral(Node):

    _fields = ('items',)

    def __init__(self, items=None):
        super().__init__()
        self.items = items or []

    def __str__(self):
        return '[%s]' % ', '.join(map(str, self.items))


class DictionaryLiteralItem(Node):

    _fields = ('key', 'value',)

    def __init__(self, key, value):
        super().__init__()
        self.key = key
        self.value = value

    def __str__(self):
        return '%s: %s' % (self.key, self.value)


class DictionaryLiteral(Node):

    _fields = ('items',)

    def __init__(self, items=None):
        super().__init__()
        self.items = items or []

    def __str__(self):
        if self.items:
            return '[%s]' % ', '.join(map(str, self.items))
        return '[:]'


class Select(Node):

    _fields = ('owner', 'member',)

    def __init__(self, owner, member):
        super().__init__()
        self.owner = owner
        self.member = member

    def __str__(self):
        return '%s.%s' % (self.owner, self.member)


class ImplicitSelect(Node):

    _fields = ('member',)

    def __init__(self, member):
        super().__init__()
        self.member = member

    def __str__(self):
        return '.%s' % self.member


class PrefixedExpression(Node):

    _fields = ('operator', 'operand',)

    def __init__(self, operator, operand):
        super().__init__()
        self.operator = operator
        self.operand = operand

    def __str__(self):
        return '%s %s' % (self.operator, self.operand)


class BinaryExpression(Node):

    _fields = ('operator', 'left', 'right',)

    def __init__(self, operator, left, right):
        super().__init__()
        self.operator = operator
        self.left = left
        self.right = right

    def __str__(self):
        return '%s %s %s' % (self.left, self.operator, self.right)


class Closure(Node):

    _fields = ('statements', 'parameters',)

    def __init__(self, statements, parameters=None):
        super().__init__()
        self.statements = statements
        self.parameters = parameters or []

    def __str__(self):
        statements = ''.join('\n%s' % statement for statement in self.statements)
        if self.parameters:
            return '{ let %s in %s }' % (', '.join(map(str, self.parameters)), statements)
        return '{ %s }' % statements


class Wildcard(Node):

    def __str__(self):
        return '_'


class Pattern(Node):

    _fields = ('expression', 'parameters',)

    def __init__(self, expression, parameters=None):
        super().__init__()
        self.expression = expression
        self.parameters = parameters or []

    def __str__(self):
        if self.parameters:
            return 'let %s in %s' % (', '.join(map(str, self.parameters)), self.expression or '_')
        return str(self.expression or '_')


class If(Node):

    _fields = ('pattern', 'body', 'else_clause',)

    def __init__(self, pattern, body, else_clause=None):
        super().__init__()
        self.pattern = pattern
        self.body = body
        self.else_clause = else_clause

    def __str__(self):
        if self.else_clause:
            return 'if %s %s else %s' % (self.pattern, self.body, self.else_clause)
        return 'if %s %s' % (self.pattern, self.body)


class SwitchCaseClause(Node):

    _fields = ('pattern', 'body',)

    def __init__(self, pattern, body):
        super().__init__()
        self.pattern = pattern
        self.body = body

    def __str__(self):
        return 'case %s %s' % (self.pattern, self.body)


class Switch(Node):

    _fields = ('expression', 'clauses',)

    def __init__(self, expression, clauses=None):
        super().__init__()
        self.expression = expression
        self.clauses = clauses or []

    def __str__(self):
        return 'switch %s {%s}' % (
            self.expression,
            ''.join('\n%s' % clause for clause in self.clauses))


class CallArgument(Node):

    _fields = ('value', 'name', 'attributes',)

    def __init__(self, value, name=None, attributes=None):
        super().__init__()
        self.value = value
        self.name = name
        self.attributes = attributes or set()

    def __str__(self):
        if self.name:
            return '%s: %s' % (self.name, self.value)
        return str(self.value)


class Call(Node):

    _fields = ('callee', 'arguments',)

    def __init__(self, callee, arguments=None):
        super().__init__()
        self.callee = callee
        self.arguments = arguments or []

    def __str__(self):
        return '%s(%s)' % (self.callee, ', '.join(map(str, self.arguments)))


class Assignment(Node):

    _fields = ('lvalue', 'rvalue',)

    def __init__(self, lvalue, rvalue):
        super().__init__()
        self.lvalue = lvalue
        self.rvalue = rvalue

    def __str__(self):
        return '%s = %s' % (self.lvalue, self.rvalue)


class Return(Node):

    _fields = ('value',)

    def __init__(self, value):
        super().__init__()
        self.value = value

    def __str__(self):
        return 'return %s' % self.value


class Break(Node):

    _fields = ('label',)

    def __init__(self, label=None):
        super().__init__()
        self.label = label

    def __str__(self):
        if self.label:
            return 'break %s' % self.label
        return 'break'


class Continue(Node):

    _fields = ('label',)

    def __init__(self, label=None):
        super().__init__()
        self.label = label

    def __str__(self):
        if self.label:
            return 'continue %s' % self.label
        return 'continue'


class For(Node):

    _fields = ('iterator', 'sequence', 'body', 'label',)

    def __init__(self, iterator, sequence, body, label=None):
        self.iterator = iterator
        self.sequence = sequence
        self.body = body
        self.label = label

    def __str__(self):
        result = 'for %s in %s %s' % (self.iterator or '_', self.sequence, self.body)
        if self.label:
            return ('%s: ' % self.label) + result
        return result


class While(Node):

    _fields = ('pattern', 'body', 'label',)

    def __init__(self, pattern, body, label=None):
        self.pattern = pattern
        self.body = body
        self.label = label

    def __str__(self):
        result = 'while %s %s' % (self.pattern, self.body)
        if self.label:
            return ('%s: ' % self.label) + result
        return result


class ContainerDecl(Node):

    _fields = ('name', 'is_mutable', 'type_annotation', 'initial_value',)

    def __init__(self, name, is_mutable, type_annotation, initial_value):
        super().__init__()
        self.name = name
        self.is_mutable = is_mutable
        self.type_annotation = type_annotation
        self.initial_value = initial_value

    def __str__(self):
        result = ('mut ' if self.is_mutable else 'cst ') + self.name
        if self.type_annotation is not None:
            result += ': %s' % self.type_annotation
        if self.initial_value is not None:
            result += ' = %s' % self.initial_value
        return result


class FunctionDecl(Node):

    _fields = ('name', 'signature', 'body', 'generic_parameters',)

    def __init__(self, name, signature, body, generic_parameters=None):
        super().__init__()
        self.name = name
        self.signature = signature
        self.body = body
        self.generic_parameters = generic_parameters or []

    def __str__(self):
        result = 'fun %s' % self.name
        if self.generic_parameters:
            result += '<%s>' % ', '.join(map(str, self.generic_parameters))
        result += '%s %s' % (self.signature, self.body)
        return result


class EnumCaseParameter(Node):

    _fields = ('label', 'type_annotation',)

    def __init__(self, label, type_annotation):
        super().__init__()
        self.label = label
        self.type_annotation = type_annotation

    def __str__(self):
        return '%s: %s' % (self.label or '_', self.type_annotation)


class EnumCaseDecl(Node):

    _fields = ('name', 'parameters',)

    def __init__(self, name, parameters=None):
        super().__init__()
        self.name = name
        self.parameters = parameters or []

    def __str__(self):
        if self.parameters:
            return 'case %s(%s)' % (self.name, ', '.join(map(str, self.parameters)))
        else:
            return 'case %s' % self.name


class EnumDecl(Node):

    _fields = ('name', 'body', 'import_list', 'conformance_list',)

    def __init__(self, name, body, import_list=None, conformance_list=None):
        super().__init__()
        self.name = name
        self.body = body
        self.import_list = import_list or []
        self.conformance_list = conformance_list or []

    def __str__(self):
        return 'enum %s %s' % (self.name, self.body)


class StructDecl(Node):

    _fields = ('name', 'body', 'import_list', 'conformance_list',)

    def __init__(self, name, body, import_list=None, conformance_list=None):
        super().__init__()
        self.name = name
        self.body = body
        self.import_list = import_list or []
        self.conformance_list = conformance_list or []

    def __str__(self):
        return 'struct %s %s' % (self.name, self.body)


class ModuleDecl(Node):

    _fields = ('name', 'body',)

    def __init__(self, name, body):
        super().__init__()
        self.name = name
        self.body = body

    def __str__(self):
        return '\n'.join(map(str, self.body.statements))


class Visitor(object):

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


class Transformer(Visitor):

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
