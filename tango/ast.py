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


class ModuleDecl(Node):

    _fields = ('name', 'body',)

    def __init__(self, name, body):
        super().__init__()
        self.name = name
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


class PropertyDecl(Node):

    _fields = ('name', 'attributes', 'type_annotation', 'initializer', 'getter', 'setter',)

    def __init__(
            self, name, attributes=None, type_annotation=None, initializer=None,
            getter=None, setter=None):

        super().__init__()
        self.name            = name
        self.attributes      = attributes or []
        self.type_annotation = type_annotation
        self.initializer     = initializer
        self.getter          = getter
        self.setter          = setter

    def __str__(self):
        if 'shared' in self.attributes:
            result = 'shd ' + self.name
            attributes = sorted(filter(lambda attr: attr != 'shared', self.attributes))
        elif 'mutable' in self.attributes:
            result = 'mut ' + self.name
            attributes = sorted(filter(lambda attr: attr != 'mutable', self.attributes))
        else:
            result = 'cst ' + self.name
            attributes = sorted(self.attributes)

        if self.attributes:
            result = ' '.join('@' + str(attr) for attr in attributes) + ' ' + result
        if self.type_annotation:
            result += ': ' + str(self.type_annotation)
        if self.initializer:
            result += ' = ' + str(self.initializer)
        if self.getter or self.setter:
            result += ' {\n'
            if self.getter:
                result += '\n'.join('  ' + line for line in str(self.getter).split('\n'))
                result += '\n'
            if self.setter:
                result += '\n'.join('  ' + line for line in str(self.setter).split('\n'))
                result += '\n'
            result += '}'

        return result


class FunctionDecl(Node):

    _fields = ('name', 'signature', 'body', 'generic_parameters', 'where_clause',)

    def __init__(
            self, name, signature, body=None, attributes=None,
            generic_parameters=None, where_clause=None):

        super().__init__()
        self.name               = name
        self.signature          = signature
        self.body               = body
        self.attributes         = attributes or []
        self.generic_parameters = generic_parameters or []
        self.where_clause       = where_clause

    def __str__(self):
        result = 'fun {}'.format(self.name)
        if self.attributes:
            result = ' '.join('@' + str(attr) for attr in self.attributes) + ' ' + result
        if self.generic_parameters:
            result += '<{}>'.format(', '.join(map(str, self.generic_parameters)))
        result += str(self.signature)
        if self.where_clause:
            result += ' where ' + str(self.where_clause)
        if self.body:
            result += ' ' + str(self.body)
        return result


class FunctionParameterDecl(Node):

    _fields = ('name', 'label', 'type_annotation', 'attributes', 'default_value',)

    def __init__(self, name, label, type_annotation, attributes=None, default_value=None):
        super().__init__()
        self.name = name
        self.label = label
        self.type_annotation = type_annotation
        self.attributes = attributes or []
        self.default_value = default_value

    def __str__(self):
        if 'shared' in self.attributes:
            result = 'shd'
        elif 'mutable' in self.attributes:
            result = 'mut'
        else:
            result = 'cst'

        if self.name != self.label:
            result += ' ' + str(self.label or '_')
        result += ' ' + self.name

        if self.type_annotation:
            result += ': ' + str(self.type_annotation)
        if self.default_value:
            result += ' = ' + str(self.default_value)
        return result


class StructDecl(Node):

    _fields = (
        'name', 'body', 'attributes', 'generic_parameters',
        'conformance_list', 'import_list', 'where_clause',)

    def __init__(
            self, name, body, attributes=None, generic_parameters=None,
            conformance_list=None, import_list=None, where_clause=None):

        super().__init__()
        self.name               = name
        self.body               = body
        self.attributes         = attributes or []
        self.generic_parameters = generic_parameters or []
        self.conformance_list   = conformance_list or []
        self.import_list        = import_list or []
        self.where_clause       = where_clause

    def __str__(self):
        result = 'struct ' + self.name
        if self.attributes:
            result = ' '.join('@' + str(attr) for attr in self.attributes) + ' ' + result
        if self.generic_parameters:
            result += '<{}>'.format(', '.join(map(str, self.generic_parameters)))
        if self.conformance_list:
            result += ': ' + ', '.join(map(str, self.conformance_list))
        if self.import_list:
            result += ' import ' + ', '.join(map(str, self.import_list))
        if self.where_clause:
            result += ' where ' + str(self.where_clause)
        return result + ' ' + str(self.body)


class EnumCaseParameterDecl(Node):

    _fields = ('label', 'type_annotation',)

    def __init__(self, label, type_annotation):
        super().__init__()
        self.label           = label
        self.type_annotation = type_annotation

    def __str__(self):
        return '{}: {}'.format(self.label or '_', self.type_annotation)


class EnumCaseDecl(Node):

    _fields = ('name', 'parameters', 'attributes',)

    def __init__(self, name, parameters=None, attributes=None):
        super().__init__()
        self.name       = name
        self.parameters = parameters or []
        self.attributes = attributes or []

    def __str__(self):
        result = 'case ' + self.name
        if self.attributes:
            result = ' '.join('@' + str(attr) for attr in self.attributes) + ' ' + result
        if self.parameters:
            result += '({})'.format(', '.join(map(str, self.parameters)))
        return result


class EnumDecl(Node):

    _fields = (
        'name', 'body', 'attributes', 'generic_parameters',
        'conformance_list', 'import_list', 'where_clause',)

    def __init__(
            self, name, body, attributes=None, generic_parameters=None,
            conformance_list=None, import_list=None, where_clause=None):

        super().__init__()
        self.name               = name
        self.body               = body
        self.attributes         = attributes or []
        self.generic_parameters = generic_parameters or []
        self.conformance_list   = conformance_list or []
        self.import_list        = import_list or []
        self.where_clause       = where_clause

    def __str__(self):
        result = 'enum ' + self.name
        if self.attributes:
            result = ' '.join('@' + str(attr) for attr in self.attributes) + ' ' + result
        if self.generic_parameters:
            result += '<{}>'.format(', '.join(map(str, self.generic_parameters)))
        if self.conformance_list:
            result += ': ' + ', '.join(map(str, self.conformance_list))
        if self.import_list:
            result += ' import ' + ', '.join(map(str, self.import_list))
        if self.where_clause:
            result += ' where ' + str(self.where_clause)
        return result + ' ' + str(self.body)


class ProtocolDecl(Node):

    _fields = ('name', 'body', 'attributes', 'conformance_list', 'import_list')

    def __init__(self, name, body, attributes=None, conformance_list=None, import_list=None):
        super().__init__()
        self.name             = name
        self.body             = body
        self.attributes       = attributes
        self.conformance_list = conformance_list or []
        self.import_list      = import_list or []

    def __str__(self):
        result = 'protocol ' + self.name
        if self.attributes:
            result = ' '.join('@' + str(attr) for attr in self.attributes) + ' ' + result
        if self.conformance_list:
            result += ': ' + ', '.join(map(str(self.conformance_list)))
        if self.import_list:
            result += ' import ' + ', '.join(map(str, self.import_list))
        return result + ' ' + str(self.body)


class AbstractTypeDecl(Node):

    _fields = ('name', 'conformance_list', 'value', 'attributes',)

    def __init__(self, name, conformance_list=None, value=None, attributes=None):
        super().__init__()
        self.name             = name
        self.conformance_list = conformance_list or []
        self.value            = value
        self.attributes       = attributes or []

    def __str__(self):
        result = 'abs ' + self.name

        if self.attributes:
            result = ' '.join('@' + str(attr) for attr in self.attributes) + ' ' + result
        if self.conformance_list:
            result += ': ' + ', '.join(map(str(self.conformance_list)))
        if self.value:
            result += ' = ' + str(self.value)
        return result


class ExtensionDecl(Node):

    _fields = ('subject', 'declaration', 'where_clause',)

    def __init__(self, subject, declaration, where_clause=None):
        super().__init__()
        self.subject      = subject
        self.declaration  = declaration
        self.where_clause = where_clause

    def __str__(self):
        result = 'extension ' + self.subject
        if self.where_clause:
            result += 'where ' + str(self.where_clause)
        return result + ' -> ' + str(self.declaration)


class SignatureParameter(Node):

    _fields = ('label', 'type_annotation', 'attributes',)

    def __init__(self, label, type_annotation, attributes=None,):
        super().__init__()
        self.label = label
        self.type_annotation = type_annotation
        self.attributes = attributes or []

    def __str__(self):
        if 'shared' in self.attributes:
            result = 'shd'
            attributes = sorted(filter(lambda attr: attr != 'mutable', self.attributes))
        elif 'mutable' in self.attributes:
            result = 'mut'
            attributes = sorted(filter(lambda attr: attr != 'mutable', self.attributes))
        else:
            result = 'cst'
            attributes = sorted(self.attributes)

        result += ' ' + (self.label or '_')

        if self.type_annotation:
            result += ': ' + str(self.type_annotation)
        return result


class FunctionSignature(Node):

    _fields = ('parameters', 'return_type',)

    def __init__(self, return_type, parameters=None):
        super().__init__()
        self.parameters  = parameters
        self.return_type = return_type

    def __str__(self):
        return '({}) -> {}'.format(', '.join(map(str, self.parameters)), self.return_type)


class TupleSignature(Node):

    _fields = ('parameters',)

    def __init__(self, parameters):
        super().__init__()
        self.parameters = parameters

    def __str__(self):
        return '({})'.format(', '.join(map(str, self.parameters)))


class Identifier(Node):

    _fields = ('name', 'specializations',)

    def __init__(self, name, specializations=None):
        super().__init__()
        self.name            = name
        self.specializations = specializations or []

    @property
    def qualname(self):
        if 'scope' in self.__info__:
            return '{}.{}'.format(self.__info__['scope'].name, self.name)
        return self.name

    def __str__(self):
        if self.specializations:
            return '{}<{}>'.format(self.name, ', '.join(map(str, self.specializations)))
        return self.name


class SpecializationArgument(Node):

    _fields = ('name', 'value',)

    def __init__(self, name, value):
        super().__init__()
        self.name  = name
        self.value = value

    def __str__(self):
        return '{} = {}'.format(self.name, self.value)

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
        return '[{}]'.format(', '.join(map(str, self.items)))


class DictionaryLiteralItem(Node):

    _fields = ('key', 'value',)

    def __init__(self, key, value):
        super().__init__()
        self.key   = key
        self.value = value

    def __str__(self):
        return '{}: {}'.format(self.key, self.value)


class DictionaryLiteral(Node):

    _fields = ('items',)

    def __init__(self, items=None):
        super().__init__()
        self.items = items or []

    def __str__(self):
        if self.items:
            return '[{}]'.format(', '.join(map(str, self.items)))
        return '[:]'


class TupleItemDecl(Node):

    _fields = ('label', 'attributes', 'type_signature', 'initializer')

    def __init__(self, label, initializer, attributes=None, type_annotation=None):

        super().__init__()
        self.label           = label
        self.initializer     = initializer
        self.attributes      = attributes or []
        self.type_annotation = type_annotation

    def __str__(self):
        if 'shared' in self.attributes:
            result = 'shd ' + (self.label or '_')
            attributes = sorted(filter(lambda attr: attr != 'shared', self.attributes))
        elif 'mutable' in self.attributes:
            result = 'mut ' + (self.label or '_')
            attributes = sorted(filter(lambda attr: attr != 'mutable', self.attributes))
        else:
            result = 'cst ' + (self.label or '_')
            attributes = sorted(self.attributes)

        if self.attributes:
            result = ' '.join('@' + str(attr) for attr in attributes) + ' ' + result
        if self.type_annotation:
            result += ': ' + str(self.type_annotation)
        return result + ' = ' + str(self.initializer)


class Tuple(Node):

    _fields = ('items',)

    def __init__(self, items):
        super().__init__()
        self.items = items

    def __str__(self):
        return '({})'.format(', '.join(map(str, self.items)))


class Closure(Node):

    _fields = ('statements', 'parameters',)

    def __init__(self, statements, parameters=None):
        super().__init__()
        self.statements = statements
        self.parameters = parameters or []

    def __str__(self):
        statements = ''
        for statement in self.statements:
            statements += '\n'.join('  ' + line for line in str(statement).split('\n'))
            statements += '\n'

        if self.parameters:
            return '{{ {} in\n{}}}'.format(', '.join(map(str, self.parameters)), statements)
        return '{{\n{}}}'.format(statements)


class CallArgument(Node):

    _fields = ('value', 'label', 'attributes',)

    def __init__(self, value, label=None, attributes=None):
        super().__init__()
        self.value      = value
        self.label      = label
        self.attributes = attributes or []

    def __str__(self):
        if self.label:
            result = self.label + ' = '
        else:
            result = ''
        if self.attributes:
            result += ' '.join('@' + str(attr) for attr in self.attributes) + ' '
        return result + str(self.value)


class Call(Node):

    _fields = ('callee', 'arguments',)

    def __init__(self, callee, arguments=None):
        super().__init__()
        self.callee    = callee
        self.arguments = arguments or []

    def __str__(self):
        return '{}({})'.format(self.callee, ', '.join(map(str, self.arguments)))


class Subscript(Node):

    _fields = ('callee', 'arguments',)

    def __init__(self, callee, arguments=None):
        super().__init__()
        self.callee    = callee
        self.arguments = arguments or []

    def __str__(self):
        return '{}[{}]'.format(self.callee, ', '.join(map(str, self.arguments)))


class Select(Node):

    _fields = ('owner', 'member',)

    def __init__(self, owner, member):
        super().__init__()
        self.owner  = owner
        self.member = member

    def __str__(self):
        return '{}.{}'.format(self.owner, self.member)


class ImplicitSelect(Node):

    _fields = ('member',)

    def __init__(self, member):
        super().__init__()
        self.member = member

    def __str__(self):
        return '.' + str(self.member)


class PrefixExpression(Node):

    _fields = ('operator', 'operand',)

    def __init__(self, operator, operand):
        super().__init__()
        self.operator = operator
        self.operand  = operand

    def __str__(self):
        return '{} {}'.format(self.operator, self.operand)


class PostfixExpression(Node):

    _fields = ('operator', 'operand',)

    def __init__(self, operator, operand):
        super().__init__()
        self.operator = operator
        self.operand  = operand

    def __str__(self):
        return '{} {}'.format(self.operand, self.operator)


class BinaryExpression(Node):

    _fields = ('operator', 'left', 'right',)

    def __init__(self, operator, left, right):
        super().__init__()
        self.operator = operator
        self.left     = left
        self.right    = right

    def __str__(self):
        return '{} {} {}'.format(self.left, self.operator, self.right)


class ValueBindingPattern(Node):

    _fields = ('name', 'type_annotation', 'attributes',)

    def __init__(self, name, type_annotation=None, attributes=None):
        super().__init__()
        self.name            = name
        self.type_annotation = type_annotation
        self.attributes      = attributes or []

    def __str__(self):
        if 'shared' in self.attributes:
            result = 'shd ' + self.name
            attributes = sorted(filter(lambda attr: attr != 'mutable', self.attributes))
        elif 'mutable' in self.attributes:
            result = 'mut ' + self.name
            attributes = sorted(filter(lambda attr: attr != 'mutable', self.attributes))
        else:
            result = 'cst ' + self.name
            attributes = sorted(self.attributes)

        if self.type_annotation:
            result += ': ' + str(self.type_annotation)
        return result


class PatternArgument(Node):

    _fields = ('label', 'value',)

    def __init__(self, value, label=None):
        super().__init__()
        self.label = label
        self.value = value

    def __str__(self):
        if self.label:
            return '{} = {}'.format(self.label, self.value)
        return str(self.value)


class TuplePattern(Node):

    _fields = ('items',)

    def __init__(self, items=None):
        super().__init__()
        self.items = items or []

    def __str__(self):
        return '({})'.format(', '.join(map(str, self.items)))


class EnumCasePattern(Node):

    _fields = ('case', 'arguments',)

    def __init__(self, case, arguments=None):
        super().__init__()
        self.case      = case
        self.arguments = arguments or []

    def __str__(self):
        if self.arguments:
            return '{}({})'.format(self.case, ''.join(map(str, self.arguments)))
        return str(self.case)


class WildcardPattern(Node):

    def __str__(self):
        return '_'


class Pattern(Node):

    _fields = ('expression', 'where_clause',)

    def __init__(self, expression, where_clause=None):
        super().__init__()
        self.expression   = expression
        self.where_clause = where_clause

    def __str__(self):
        if self.where_clause:
            return '{} where {}'.format(self.expression, self.where_clause)
        return str(self.expression)


class MatchingPattern(Node):

    _fields = ('value', 'pattern',)

    def __init__(self, value, pattern):
        super().__init__()
        self.value   = value
        self.pattern = pattern

    def __str__(self):
        return '{} ~= {}'.format(self.value, self.pattern)


class If(Node):

    _fields = ('condition', 'body', 'else_clause',)

    def __init__(self, condition, body, else_clause=None):
        super().__init__()
        self.condition   = condition
        self.body        = body
        self.else_clause = else_clause

    def __str__(self):
        if self.else_clause:
            return 'if {} {} else {}'.format(self.condition, self.body, self.else_clause)
        return 'if {} {}'.format(self.condition, self.body)


class SwitchCaseClause(Node):

    _fields = ('pattern', 'body',)

    def __init__(self, pattern, body):
        super().__init__()
        self.pattern = pattern
        self.body    = body

    def __str__(self):
        return 'case {} {}'.format(self.pattern, self.body)


class Switch(Node):

    _fields = ('expression', 'clauses',)

    def __init__(self, expression, clauses=None):
        super().__init__()
        self.expression = expression
        self.clauses = clauses or []

    def __str__(self):
        clauses = ''
        for clause in self.clauses:
            clauses += '\n'.join('  ' + line for line in str(clause).split('\n'))
            clauses += '\n'

        return 'switch {} {{\n{}}}'.format(self.expression, clauses)


class Assignment(Node):

    _fields = ('lvalue', 'rvalue',)

    def __init__(self, lvalue, rvalue):
        super().__init__()
        self.lvalue = lvalue
        self.rvalue = rvalue

    def __str__(self):
        return '{} = {}'.format(self.lvalue, self.rvalue)


class SelfAssignement(Node):

    _fields = ('operator', 'lvalue', 'rvalue',)

    def __init__(self, operator, lvalue, rvalue):
        super().__init__()
        self.operator = operator
        self.lvalue   = lvalue
        self.rvalue    = rvalue

    def __str__(self):
        return '{} {} {}'.format(self.lvalue, self.operator, self.rvalue)


class Return(Node):

    _fields = ('value',)

    def __init__(self, value):
        super().__init__()
        self.value = value

    def __str__(self):
        return 'return ' + str(self.value)


class Break(Node):

    _fields = ('label',)

    def __init__(self, label=None):
        super().__init__()
        self.label = label

    def __str__(self):
        if self.label:
            return 'break ' + self.label
        return 'break'


class Continue(Node):

    _fields = ('label',)

    def __init__(self, label=None):
        super().__init__()
        self.label = label

    def __str__(self):
        if self.label:
            return 'continue ' + self.label
        return 'continue'


class For(Node):

    _fields = ('iterator', 'sequence', 'body', 'label',)

    def __init__(self, iterator, sequence, body, label=None):
        self.iterator = iterator
        self.sequence = sequence
        self.body     = body
        self.label    = label

    def __str__(self):
        result = 'for {} in {} {}'.format(self.iterator, self.sequence, self.body)
        if self.label:
            return self.label + ': ' + result
        return result


class While(Node):

    _fields = ('condition', 'body', 'label',)

    def __init__(self, condition, body, label=None):
        self.condition = condition
        self.body      = body
        self.label     = label

    def __str__(self):
        result = 'while {} {}'.format(self.condition, self.body)
        if self.label:
            return self.label + ': ' + result
        return result


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
