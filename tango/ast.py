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


class Block(Node):

    _fields = ('statements',)

    def __init__(self, statements=None):
        super().__init__()
        self.statements = statements or []

    def __str__(self):
        result = '{\n'
        for statement in self.statements:
            result += '  ' + str(statement) + '\n'
        return result + '}'


class SpecializationArgument(Node):

    _fields = ('name', 'type_annotation',)

    def __init__(self, name, type_annotation):
        super().__init__()
        self.name            = name
        self.type_annotation = type_annotation

    def __str__(self):
        return '{} = {}'.format(self.name, self.type_annotation)


class TypeIdentifier(Node):

    _fields = ('name', 'specializations',)

    def __init__(self, name, specializations=None):
        super().__init__()
        self.name            = name
        self.specializations = specializations or []

    def __str__(self):
        if self.specializations:
            return '{}<{}>'.format(self.name, ', '.join(map(str, self.specializations)))
        return self.name

    def __repr__(self):
        return 'TypeIdentifier({})'.format(self)


class Wildcard(Node):

    def __str__(self):
        return '_'


class ValueBindingPattern(Node):

    _fields = ('name', 'is_mutable', 'is_shared', 'type_annotation',)

    def __init__(self, name, is_mutable=False, is_shared=False, type_annotation=None):
        super().__init__()
        self.name            = name
        self.is_mutable      = is_mutable
        self.is_shared       = is_shared
        self.type_annotation = type_annotation

    def __str__(self):
        if self.is_shared:
            mutability_modifier = 'shd'
        elif self.is_mutable:
            mutability_modifier = 'mut'
        else:
            mutability_modifier = 'cst'

        if self.type_annotation:
            return '{} {}: {}'.format(mutability_modifier, self.name, self.type_annotation)
        return '{} {}'.format(mutability_modifier, self.name)


class ArgumentPattern(Node):

    _fields = ('label', 'pattern',)

    def __init__(self, pattern, label=None):
        super().__init__()
        self.label   = label
        self.pattern = pattern

    def __str__(self):
        if self.label:
            return '{} = {}'.format(self.label, self.pattern)
        return str(self.pattern)


class TuplePattern(Node):

    _fields = ('elements',)

    def __init__(self, elements=None):
        super().__init__()
        self.elements = elements or []

    def __str__(self):
        return '({})'.format(', '.join(map(str, self.elements)))


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


class Parameter(Node):

    _fields = ('name', 'label', 'type_annotation', 'attributes', 'default_value',)

    def __init__(self, name, label, type_annotation, attributes=None, default_value=None):
        super().__init__()
        self.name = name
        self.label = label
        self.type_annotation = type_annotation
        self.attributes = attributes or set()
        self.default_value = default_value

    def __str__(self):
        if 'shared' in self.attributes:
            result = 'mut '
            attributes = sorted(filter(lambda attr: attr != 'mutable', self.attributes))
        elif 'mutable' in self.attributes:
            result = 'mut '
            attributes = sorted(filter(lambda attr: attr != 'mutable', self.attributes))
        else:
            result = 'cst '
            attributes = sorted(self.attributes)

        if self.name != self.label:
            result += str(self.label or '_') + ' '
        result += self.name + ' '

        if attributes:
            result += ' '.join('@' + str(attribute) for attribute in self.attributes) + ' '

        return result + str(self.type_annotation)


class TupleSignature(Node):

    _fields = ('parameters',)

    def __init__(self, parameters):
        super().__init__()
        self.parameters = parameters

    def __str__(self):
        return '({})'.format(', '.join(map(str, self.parameters)))


class FunctionSignature(Node):

    _fields = ('parameters', 'return_type',)

    def __init__(self, return_type, parameters=None):
        super().__init__()
        self.parameters  = parameters
        self.return_type = return_type

    def __str__(self):
        return '({}) -> {}'.format(', '.join(map(str, self.parameters)), self.return_type)


class Literal(Node):

    _fields = ('value',)

    def __init__(self, value):
        super().__init__()
        self.value = value

    def __str__(self):
        return str(self.value)


class ArrayLiteral(Node):

    _fields = ('elements',)

    def __init__(self, elements=None):
        super().__init__()
        self.elements = elements or []

    def __str__(self):
        return '[{}]'.format(', '.join(map(str, self.elements)))


class DictionaryLiteralElement(Node):

    _fields = ('key', 'value',)

    def __init__(self, key, value):
        super().__init__()
        self.key   = key
        self.value = value

    def __str__(self):
        return '{}: {}'.format(self.key, self.value)


class DictionaryLiteral(Node):

    _fields = ('elements',)

    def __init__(self, elements=None):
        super().__init__()
        self.items = elements or []

    def __str__(self):
        if self.elements:
            return '[{}]'.format(', '.join(map(str, self.elements)))
        return '[:]'


class Identifier(Node):

    _fields = ('name',)

    def __init__(self, name):
        super().__init__()
        self.name = name

    @property
    def qualname(self):
        if 'scope' in self.__info__:
            return '{}.{}'.format(self.__info__['scope'].name, self.name)
        return self.name

    def __str__(self):
        return self.name


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


class Closure(Node):

    _fields = ('statements', 'parameters',)

    def __init__(self, statements, parameters=None):
        super().__init__()
        self.statements = statements
        self.parameters = parameters or []

    def __str__(self):
        statements = ''
        for statement in self.statements:
            result += '  ' + str(statement) + '\n'

        if self.parameters:
            return '{{ {} in\n{}}}'.format(', '.join(map(str, self.parameters)), statements)
        return '{{\n{}}}'.format(statements)


class If(Node):

    _fields = ('pattern', 'body', 'else_clause',)

    def __init__(self, pattern, body, else_clause=None):
        super().__init__()
        self.pattern     = pattern
        self.body        = body
        self.else_clause = else_clause

    def __str__(self):
        if self.else_clause:
            return 'if {} {} else {}'.format(self.pattern, self.body, self.else_clause)
        return 'if {} {}'.format(self.pattern, self.body)


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
            clauses += '  ' + str(clause) + '\n'

        return 'switch {} {{\n{}}}'.format(self.expression, clauses)


class CallArgument(Node):

    _fields = ('value', 'name', 'attributes',)

    def __init__(self, value, name=None, attributes=None):
        super().__init__()
        self.value      = value
        self.name       = name
        self.attributes = attributes or set()

    def __str__(self):
        if self.name:
            result = self.name + ' = '
        else:
            result = ''
        if self.attributes:
            result += ' '.join('@' + str(attribute) for attribute in self.attributes) + ' '
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

    _fields = ('pattern', 'body', 'label',)

    def __init__(self, pattern, body, label=None):
        self.pattern = pattern
        self.body    = body
        self.label   = label

    def __str__(self):
        result = 'while {} {}'.format(self.pattern, self.body)
        if self.label:
            return self.label + ': ' + result
        return result


class ContainerDecl(Node):

    _fields = ('name', 'is_mutable', 'is_shared', 'type_annotation',)

    def __init__(self, name, is_mutable, is_shared, type_annotation=None, initial_value=None):
        super().__init__()
        self.name            = name
        self.is_mutable      = is_mutable
        self.is_shared       = is_shared
        self.type_annotation = type_annotation
        self.initial_value   = initial_value

    def __str__(self):
        if self.is_shared:
            result = 'shd '
        elif self.is_mutable:
            result = 'mut '
        else:
            result = 'cst '

        result += self.name

        if self.type_annotation:
            result += ': ' + str(self.type_annotation)
        if self.initial_value:
            result += ' = ' + str(self.initial_value)

        return result


class FunctionDecl(Node):

    _fields = ('name', 'signature', 'body', 'generic_parameters', 'where_clause',)

    def __init__(self, name, signature, body=None, generic_parameters=None, where_clause=None):
        super().__init__()
        self.name               = name
        self.signature          = signature
        self.body               = body
        self.generic_parameters = generic_parameters or []
        self.where_clause       = where_clause

    def __str__(self):
        result = 'fun {}'.format(self.name)
        if self.generic_parameters:
            result += '<{}>'.format(', '.join(map(str, self.generic_parameters)))
        result += str(self.signature)
        if self.where_clause:
            result += ' where ' + str(self.where_clause)
        if self.body:
            result += ' ' + str(self.body)
        return result


class EnumCaseParameter(Node):

    _fields = ('label', 'type_annotation',)

    def __init__(self, label, type_annotation):
        super().__init__()
        self.label           = label
        self.type_annotation = type_annotation

    def __str__(self):
        return '{}: {}'.format(self.label or '_', self.type_annotation)


class EnumCaseDecl(Node):

    _fields = ('name', 'parameters',)

    def __init__(self, name, parameters=None):
        super().__init__()
        self.name       = name
        self.parameters = parameters or []

    def __str__(self):
        if self.parameters:
            return 'case {}({})'.format(self.name, ', '.join(map(str, self.parameters)))
        else:
            return 'case ' + self.name


class EnumDecl(Node):

    _fields = (
        'name', 'body', 'generic_parameters', 'conformance_list', 'import_list', 'where_clause',)

    def __init__(
            self, name, body, generic_parameters=None, conformance_list=None, import_list=None,
            where_clause=None):

        super().__init__()
        self.name               = name
        self.body               = body
        self.generic_parameters = generic_parameters or []
        self.conformance_list   = conformance_list or []
        self.import_list        = import_list or []
        self.where_clause       = where_clause

    def __str__(self):
        result = 'enum ' + self.name
        if self.generic_parameters:
            result += '<{}> '.format(', '.join(map(str, self.generic_parameters)))
        if self.conformance_list:
            result += ': ' + ', '.join(map(str(self.conformance_list)))
        if self.import_list:
            result += ' import ' + ', '.join(map(str(self.import_list)))
        if self.where_clause:
            result += ' where ' + str(self.where_clause)
        return result + ' ' + str(self.body)


class StructDecl(Node):

    _fields = (
        'name', 'body', 'generic_parameters', 'conformance_list', 'import_list', 'where_clause',)

    def __init__(
            self, name, body, generic_parameters=None, conformance_list=None, import_list=None,
            where_clause=None):

        super().__init__()
        self.name               = name
        self.body               = body
        self.generic_parameters = generic_parameters or []
        self.conformance_list   = conformance_list or []
        self.import_list        = import_list or []
        self.where_clause       = where_clause

    def __str__(self):
        result = 'struct ' + self.name
        if self.generic_parameters:
            result += '<{}> '.format(', '.join(map(str, self.generic_parameters)))
        if self.conformance_list:
            result += ': ' + ', '.join(map(str(self.conformance_list)))
        if self.import_list:
            result += ' import ' + ', '.join(map(str(self.import_list)))
        if self.where_clause:
            result += ' where ' + str(self.where_clause)
        return result + ' ' + str(self.body)


class AbstractTypeDecl(Node):

    _fields = ('name', 'conformance_list', 'value',)

    def __init__(self, name, conformance_list=None, value=None):
        super().__init__()
        self.name             = name
        self.conformance_list = conformance_list or []
        self.value            = value

    def __str__(self):
        result = 'abs ' + self.name
        if self.conformance_list:
            result += ': ' + ', '.join(map(str(self.conformance_list)))
        if self.value:
            result += ' = ' + str(self.value)
        return result


class ProtocolDecl(Node):

    _fields = ('name', 'body', 'conformance_list')

    def __init__(self, name, body, conformance_list=None):
        super().__init__()
        self.name             = name
        self.body             = body
        self.conformance_list = conformance_list or []

    def __str__(self):
        result = 'protocol ' + self.name
        if self.conformance_list:
            result += ': ' + ', '.join(map(str(self.conformance_list)))
        return result + ' ' + str(self.body)


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
