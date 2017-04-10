class Node(object):

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

    def __init__(self, name):
        super().__init__()
        self.name = name

    @property
    def qualname(self):
        if 'scope' in self.__info__:
            return '%s.%s' % (self.__info__['scope'].uri, self.name)
        return self.name

    def __str__(self):
        return self.name


class Block(Node):

    def __init__(self, statements=None):
        super().__init__()
        self.statements = statements or []

    def __str__(self):
        if self.statements:
            return '{%s\n}' % ''.join('\n%s' % statement for statement in self.statements)
        return '{}'


class SpecializationParameter(Node):

    def __init__(self, name, type_annotation):
        super().__init__()
        self.name = name
        self.type_annotation = type_annotation

    def __str__(self):
        return '%s = %s' % (self.name, self.type_annotation)


class TypeIdentifier(Node):

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

    def __init__(
            self, name, label, type_annotation, attributes, default_value=None):

        super().__init__()
        self.name = name
        self.label = label
        self.type_annotation = type_annotation
        self.attributes = attributes
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

    def __init__(self, parameters, return_type):
        super().__init__()
        self.parameters = parameters
        self.return_type = return_type

    def __str__(self):
        return '(%s) -> %s' % (', '.join(map(str, self.parameters)), self.return_type)


class Literal(Node):

    def __init__(self, value):
        super().__init__()
        self.value = value

    def __str__(self):
        return str(self.value)


class PrefixedExpression(Node):

    def __init__(self, operator, operand):
        super().__init__()
        self.operator = operator
        self.operand = operand

    def __str__(self):
        return '%s%s' % (self.operator, self.operand)


class BinaryExpression(Node):

    def __init__(self, operator, left, right):
        super().__init__()
        self.operator = operator
        self.left = left
        self.right = right

    def __str__(self):
        return '%s %s %s' % (self.left, self.operator, self.right)


class Assignment(Node):

    def __init__(self, lvalue, rvalue):
        super().__init__()
        self.lvalue = lvalue
        self.rvalue = rvalue

    def __str__(self):
        return '%s = %s' % (self.lvalue, self.rvalue)


class Return(Node):

    def __init__(self, value):
        super().__init__()
        self.value = value

    def __str__(self):
        return 'return %s' % self.value


class CallArgument(Node):

    def __init__(self, value, name=None, attributes=None):
        super().__init__()
        self.value = value
        self.name = name
        self.attributes = attributes or []

    def __str__(self):
        if self.name:
            return '%s: %s' % (self.name, self.value)
        return str(self.value)


class Call(Node):

    def __init__(self, callee, arguments=None):
        super().__init__()
        self.callee = callee
        self.arguments = arguments or []

    def __str__(self):
        return '%s(%s)'


class ConstantDecl(Node):

    def __init__(self, name, type_annotation, initial_value):
        super().__init__()
        self.name = name
        self.type_annotation = type_annotation
        self.initial_value = initial_value

    def __str__(self):
        result = 'cst %s' % self.name
        if self.type_annotation is not None:
            result += ': %s' % self.type_annotation
        if self.initial_value is not None:
            result += ' = %s' % self.initial_value
        return result


class VariableDecl(Node):

    def __init__(self, name, type_annotation, initial_value):
        super().__init__()
        self.name = name
        self.type_annotation = type_annotation
        self.initial_value = initial_value

    def __str__(self):
        result = 'mut %s' % self.name
        if self.type_annotation is not None:
            result += ': %s' % self.type_annotation
        if self.initial_value is not None:
            result += ' = %s' % self.initial_value
        return result


class FunctionDecl(Node):

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

    def __init__(self, name, type_annotation):
        super().__init__()
        self.name = name
        self.type_annotation = type_annotation

    def __str__(self):
        return '%s: %s' % (self.name, self.type_annotation)


class EnumCaseDecl(Node):

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

    def __init__(
            self, name, body, import_list=None, conformance_list=None):

        super().__init__()
        self.name = name
        self.body = body
        self.import_list = import_list or []
        self.conformance_list = conformance_list or []

    def __str__(self):
        return 'enum %s %s' % (self.name, self.body)


class StructDecl(Node):

    def __init__(self, name, body, import_list=None, conformance_list=None):
        super().__init__()
        self.name = name
        self.body = body
        self.import_list = import_list or []
        self.conformance_list = conformance_list or []

    def __str__(self):
        return 'struct %s %s' % (self.name, self.body)


class ModuleDecl(Node):

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
        for attr, value in node.__dict__.items():
            if isinstance(value, list):
                for item in value:
                    if isinstance(item, Node):
                        self.visit(item)
            elif isinstance(value, Node):
                self.visit(value)


class Transformer(Visitor):

    def generic_visit(self, node):
        for attr, value in node.__dict__.items():
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
