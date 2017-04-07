import re

from funcparserlib.lexer import make_tokenizer, Token
from funcparserlib.parser import some, a, many, maybe, finished, skip, forward_decl


def tokenize(s):
    tokenizer = make_tokenizer([
        ('comment',    (r'#.*',)),
        ('newline',    (r'[\r\n]+',)),
        ('space',      (r'[ \t\v]+',)),
        ('operator',   (r'(\->)|(not)|[=\+\-\*/:,&@<>{}\[\]\(\)]',)),
        ('identifier', (r'[^\W\d][\w]*',)),
        ('integer',    (r'-?(0|([1-9][0-9]*))([Ee][+-][0-9]+)?',)),
        ('decimal',       (r'-?(0|([1-9][0-9]*))(\.[0-9]+)([Ee][+-][0-9]+)?',)),
        ('string',     (r'\'[^\']*\'',)),
    ])

    # Ignore whitespaces and comments.
    return [token for token in tokenizer(s) if token.type not in ['space', 'comment']]

def token_of_type(type):
    return some(lambda token: token.type == type)

def flatten(args):
    return [args[0]] + args[1]

def token_value(token):
    return token.value

op     = lambda value: a(Token('operator', value))
kw     = lambda value: a(Token('identifier', value))

op_    = lambda value: skip(a(Token('operator', value)))
kw_    = lambda value: skip(a(Token('identifier', value)))
nl_    = skip(some(lambda token: token.type == 'newline'))
nl_opt = skip(maybe(some(lambda token: token.type == 'newline')))

type_signature = forward_decl()
expression     = forward_decl()
statement      = forward_decl()

class Identifier(object):

    def __init__(self, name, scope=None, type=None):
        self.name = name
        self.scope = scope
        self.type = type

    @property
    def qualname(self):
        return '%s.%s' (self.scope.name or '__unbound__', self.name)

    @staticmethod
    def from_parser(token):
        return Identifier(name=token.value)

    def __str__(self):
        return self.name

identifier = some(lambda token: token.type == 'identifier') >> Identifier.from_parser

class Block(object):

    def __init__(self, statements=None):
        self.statements = statements or []

    @staticmethod
    def from_parser(args):
        return Block(statements = args[0])

    def __str__(self):
        if self.statements:
            return '{%s\n}' % ''.join('\n%s' % statement for statement in self.statements)
        return '{}'

statement_list = (
    maybe(statement) + many(nl_ + statement)
    >> flatten)

block = (
    op_('{') + statement_list + nl_opt + op_('}')
    >> Block.from_parser)

class SpecializationParameter(object):

    def __init__(self, name, type_annotation):
        self.name = name
        self.type_annotation = type_annotation

    @staticmethod
    def from_parser(args):
        return SpecializationParameter(
            name = args[0],
            type_annotation = args[1])

    def __str__(self):
        return '%s = %s' % (self.name, self.type_annotation)

specialization_parameter = (
    identifier + op_('=') + type_signature
    >> SpecializationParameter.from_parser)

specialization_parameter_list = (
    specialization_parameter + many(op_(',') + specialization_parameter)
    >> flatten)

class TypeIdentifier(object):

    def __init__(self, name, specialization_parameters=None):
        self.name = name
        self.specialization_parameters = specialization_parameters or []

    @staticmethod
    def from_parser(args):
        return TypeIdentifier(
            name = args[0],
            specialization_parameters = args[1])

    def __str__(self):
        if self.specialization_parameters:
            return '%s [%s]' % (self.name, ', '.join(map(str, self.specialization_parameters)))
        return '%s' % self.name

    def __repr__(self):
        return 'TypeIdentifier(%s)' % str(self)

type_identifier = (
    identifier + maybe(op_('[') + specialization_parameter_list + op_(']'))
    >> TypeIdentifier.from_parser)

class FunctionParameter(object):

    def __init__(self, name, api_name, type_annotation, is_mutable, attributes):
        self.is_mutable = is_mutable
        self.name = name
        self.api_name = api_name
        self.attributes = attributes
        self.type_annotation = type_annotation

    @staticmethod
    def from_parser(args):
        return FunctionParameter(
            is_mutable = args[0].value == 'mut',
            name = args[2] or args[1],
            api_name = args[1],
            attributes = args[3],
            type_annotation = args[4])

    def __str__(self):
        if self.attributes:
            attributes = ' '.join('@%s' % attribute for attribute in self.attributes) + ' '
        else:
            attributes = ''

        if self.name != self.api_name:
            return '%s %s: %s%s' % (self.api_name, self.name, attributes, self.type_annotation)
        return '%s: %s%s' % (self.name, attributes, self.type_annotation)

function_parameter = (
    (kw('cst') | kw('mut')) +
    identifier + maybe(identifier) + op_(':') +
    many(op_('@') + identifier) + type_signature
    >> FunctionParameter.from_parser)

function_parameter_list = (
    function_parameter + many(op_(',') + function_parameter)
    >> flatten)

class FunctionType(object):

    def __init__(self, parameters, return_type):
        self.parameters = parameters
        self.return_type = return_type

    @staticmethod
    def from_parser(args):
        return FunctionType(
            parameters = args[0] or [],
            return_type = args[1])

    def __str__(self):
        return '(%s) -> %s' % (', '.join(map(str, self.parameters)), self.return_type)

function_type = (
    op_('(') + maybe(function_parameter_list) + op_(')') +
    op_('->') + type_signature
    >> FunctionType.from_parser)

type_signature.define(function_type | type_identifier)

class Literal(object):

    def __init__(self, value, type):
        self.value = value
        self.type = type

    @staticmethod
    def make_integer(token):
        return Literal(
            value=token.value,
            type=TypeIdentifier(name=Identifier(name='Int')))

    @staticmethod
    def make_double(token):
        return Literal(
            value=token.value,
            type=TypeIdentifier(name=Identifier(name='Double')))

    @staticmethod
    def make_string(token):
        return Literal(
            value=token.value,
            type=TypeIdentifier(name=Identifier(name='String')))

    def __str__(self):
        return str(self.value)

pfx_op = op('not') >> token_value

mul_op = (op('*') | op('/')) >> token_value
add_op = (op('+') | op('-')) >> token_value

integer_literal = token_of_type('integer') >> Literal.make_integer
double_literal = token_of_type('decimal') >> Literal.make_double
string_literal = token_of_type('string') >> Literal.make_string

constant = double_literal | integer_literal | string_literal
variable = identifier

citizen = constant | variable

class PrefixedExpression(object):

    def __init__(self, operator, operand):
        self.operator = operator
        self.operand = operand

    @staticmethod
    def from_parser(args):
        return PrefixedExpression(
            operator = args[0],
            operand = args[1])

    def __str__(self):
        return '%s%s' % (self.operator, self.operand)

pfx_expr = (
    pfx_op + expression
    >> PrefixedExpression.from_parser)

class BinaryExpression(object):

    def __init__(self, operator, left, right):
        self.operator = operator
        self.left = left
        self.right = right

    @staticmethod
    def from_parser(args):

        # Implementation note:
        # We receive a tuple (operand, [(operator, operand)]) from the parser.
        # Would the parsed expression be `0 + 1 - 2`, `args` would be
        # equivalent to (Literal(0), [('+', Literal(1)), ('-', Literal(2))]).

        if args[1]:
            return BinaryExpression(
                left = args[0],
                operator = args[1][0][0],
                right = BinaryExpression.from_parser((args[1][0][1], args[1][1:])))

        # Note that `from_parser` will be called for the last operand, even if
        # there isn't any binary expression left to create. Hence we can
        # simply return the operand "as is".
        return args[0]

    def __str__(self):
        return '%s %s %s' % (self.left, self.operator, self.right)

mul_expr = citizen + many(mul_op + citizen) >> BinaryExpression.from_parser
add_expr = mul_expr + many(add_op + mul_expr) >> BinaryExpression.from_parser
bin_expr = add_expr

expression.define(bin_expr)

class Assignment(object):

    def __init__(self, target, value):
        self.target = target
        self.value = value

    @staticmethod
    def from_parser(args):
        return Assignment(
            target = args[0],
            value = args[1])

    def __str__(self):
        return '%s = %s' % (self.target, self.value)

assignment = (
    expression + op_('=') + expression
    >> Assignment.from_parser)

class FunctionDecl(object):

    def __init__(self, name, signature, body, generic_parameters=None):
        self.name = name
        self.signature = signature
        self.body = body
        self.generic_parameters = generic_parameters or []

    @staticmethod
    def from_parser(args):
        return FunctionDecl(
            name = args[0],
            generic_parameters = args[1],
            signature = FunctionType(
                parameters = args[2],
                return_type = args[3] or TypeIdentifier(name=Identifier(name='Void'))),
            body = args[4])

    def __str__(self):
        result = 'fun %s' % self.name
        if self.generic_parameters:
            result += '<%s>' % ', '.join(map(str, self.generic_parameters))
        result += '%s %s' % (self.signature, self.body)
        return result

generic_parameters = (
    type_identifier + many(op_(',') + type_identifier) >> flatten)

function_decl = (
    kw_('fun') + identifier +
    maybe(op_('<') + generic_parameters + op_('>')) +
    op_('(') + maybe(function_parameter_list) + op_(')') +
    maybe(op_('->') + type_signature) +
    block
    >> FunctionDecl.from_parser)

class ConstantDecl(object):

    def __init__(self, name, type_annotation, initial_value, type=None):
        self.name = name
        self.type_annotation = type_annotation
        self.initial_value = initial_value
        self.type = type

    @staticmethod
    def from_parser(args):
        return ConstantDecl(
            name = args[0],
            type_annotation = args[1],
            initial_value = args[2])

    def __str__(self):
        result = 'cst %s' % self.name
        if self.type_annotation is not None:
            result += ': %s' % self.type_annotation
        if self.initial_value is not None:
            result += ' = %s' % self.initial_value
        return result

constant_decl = (
    kw_('cst') + identifier +
    maybe(op_(':') + type_signature) +
    maybe(op_('=') + expression)
    >> ConstantDecl.from_parser)

class VariableDecl(object):

    def __init__(self, name, type_annotation, initial_value, type=None):
        self.name = name
        self.type_annotation = type_annotation
        self.initial_value = initial_value
        self.type = type

    @staticmethod
    def from_parser(args):
        return VariableDecl(
            name = args[0],
            type_annotation = args[1],
            initial_value = args[2])

    def __str__(self):
        result = 'mut %s' % self.name
        if self.type_annotation is not None:
            result += ': %s' % self.type_annotation
        if self.initial_value is not None:
            result += ' = %s' % self.initial_value
        return result

variable_decl = (
    kw_('mut') + identifier +
    maybe(op_(':') + type_signature) +
    maybe(op_('=') + expression)
    >> VariableDecl.from_parser)

type_import_list = (
    type_identifier + many(op_(',') + type_identifier)
    >> flatten)

type_conformance_list = (
    type_identifier + many(op_('&') + type_identifier)
    >> flatten)

class EnumCaseParameter(object):

    def __init__(self, name, type_annotation):
        self.name = name
        self.type_annotation = type_annotation

    @staticmethod
    def from_parser(args):
        return EnumCaseParameter(
            name = args[0],
            type_annotation = args[1])

    def __str__(self):
        return '%s: %s' % (self.name, self.type_annotation)

enum_case_parameter = (
    identifier + op_(':') + type_signature
    >> EnumCaseParameter.from_parser)

enum_case_parameter_list = (
    enum_case_parameter + many(op_(',') + enum_case_parameter)
    >> flatten)

class EnumCase(object):

    def __init__(self, name, parameters):
        self.name = name
        self.parameters = parameters

    @staticmethod
    def from_parser(args):
        return EnumCase(
            name = args[0],
            parameters = args[1])

    def __str__(self):
        if self.parameters:
            return 'case %s(%s)' % (self.name, ', '.join(map(str, self.parameters)))
        else:
            return 'case %s' % self.name

enum_case = (
    kw_('case') + identifier + maybe(op_('(') + enum_case_parameter_list + op_(')'))
    >> EnumCase.from_parser)

class EnumDecl(object):

    def __init__(
            self, name, cases=None, methods=None,
            import_list=None, conformance_list=None):

        self.name = name
        self.cases = cases or []
        self.methods = methods or []
        self.import_list = import_list or []
        self.conformance_list = conformance_list or []

    @staticmethod
    def from_parser(args):
        return EnumDecl(
            name = args[0],
            import_list = args[1],
            conformance_list = args[2],
            cases = list(filter(lambda e: isinstance(e, EnumCase), args[3])),
            methods = list(filter(lambda e: isinstance(e, FunctionDecl),args[3])))

    def __str__(self):
        cases = ''.join('\t%s\n' % case for case in self.cases)
        return 'enum %s {\n%s}' % (self.name, cases)

enum_member = enum_case | function_decl

enum_body = (
    maybe(enum_member) + many(nl_ + enum_member)
    >> flatten)

enum_decl = (
    kw_('enum') + identifier +
    maybe(kw_('import') + type_import_list) +
    maybe(op_(':') + type_conformance_list) +
    op_('{') + enum_body + nl_opt + op_('}')
    >> EnumDecl.from_parser)

class StructDecl(object):

    def __init__(
            self, name, stored_properties=None, methods=None,
            import_list=None, conformance_list=None):

        self.name = name
        self.stored_properties = stored_properties or []
        self.methods = methods or []
        self.import_list = import_list or []
        self.conformance_list = conformance_list or []

    @staticmethod
    def from_parser(args):
        return StructDecl(
            name = args[0],
            import_list = args[1],
            conformance_list = args[2],
            stored_properties = list(filter(
                lambda e: isinstance(e, VariableDecl) or isinstance(e, ConstantDecl), args[3])),
            methods = list(filter(lambda e: isinstance(e, FunctionDecl), args[3])))

    def __str__(self):
        pass

struct_member = variable_decl | constant_decl | function_decl

struct_body = (
    maybe(struct_member) + many(nl_ + struct_member)
    >> flatten)

struct_decl = (
    kw_('struct') + identifier +
    maybe(kw_('import') + type_import_list) +
    maybe(op_(':') + type_conformance_list) +
    op_('{') + struct_body + nl_opt + op_('}')
    >> StructDecl.from_parser)

statement.define(
    constant_decl |
    variable_decl |
    function_decl |
    enum_decl |
    struct_decl |
    assignment)

class ModuleDecl(object):

    def __init__(self, name, statements):
        self.name = name
        self.statements = statements

    @staticmethod
    def from_parser(args):
        return ModuleDecl(
            name = '',
            statements = args)

    def __str__(self):
        return '\n'.join(map(str, self.statements))

module_decl = (
    statement_list + nl_opt
    >> ModuleDecl.from_parser)

parser = module_decl + skip(finished)

def parse(s):
    return parser.parse(tokenize(s))
