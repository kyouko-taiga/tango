import re

from funcparserlib.lexer import make_tokenizer, Token
from funcparserlib.parser import some, a, many, maybe, finished, skip, forward_decl

from .ast import *
from .builtin import Nothing, Int, Double, String


def tokenize(s):
    tokenizer = make_tokenizer([
        ('comment',    (r'#.*',)),
        ('newline',    (r'[\r\n]+',)),
        ('space',      (r'[ \t\v]+',)),
        ('operator',   (r'(\->)|(not)|[=\+\-\*/:,&@<>{}\[\]\(\)]',)),
        ('identifier', (r'[^\W\d][\w]*',)),
        ('number',     (r'[-+]?(0|([1-9][0-9]*))(\.[0-9]+)?([Ee][+-]?[0-9]+)?',)),
        ('string',     (r'\'[^\']*\'',)),
    ])

    # Ignore whitespaces and comments.
    return [token for token in tokenizer(s) if token.type not in ['space', 'comment']]

def token_of_type(type):
    return some(lambda token: token.type == type)

def flatten(args):
    return [args[0]] + args[1]

op     = lambda value: a(Token('operator', value))
kw     = lambda value: a(Token('identifier', value))

op_    = lambda value: skip(a(Token('operator', value)))
kw_    = lambda value: skip(a(Token('identifier', value)))
nl_    = skip(some(lambda token: token.type == 'newline'))
nl_opt = skip(maybe(some(lambda token: token.type == 'newline')))

type_signature = forward_decl()
expression     = forward_decl()
statement      = forward_decl()

def make_identifier(token):
    return Identifier(name = token.value)

identifier = (
    some(lambda token: token.type == 'identifier')
    >> make_identifier)

def make_operator_identifier(token):
    return OperatorIdentifier(name = token.value)

pfx_op = op('not') >> make_operator_identifier

mul_op = (op('*') | op('/')) >> make_operator_identifier
add_op = (op('+') | op('-')) >> make_operator_identifier

operator_identifier = pfx_op | mul_op | add_op

def make_statement_list(args):
    if args[0] is None:
        return args[1]
    return flatten(args)

statement_list = (
    maybe(statement) + many(nl_ + statement)
    >> make_statement_list)

def make_block(statements):
    return Block(statements = statements)

block = (
    op_('{') + statement_list + nl_opt + op_('}')
    >> make_block)

def make_specialization_parameter(args):
    return SpecializationParameter(
        name = args[0],
        type_annotation = args[1])

specialization_parameter = (
    identifier + op_('=') + type_signature
    >> make_specialization_parameter)

specialization_parameter_list = (
    specialization_parameter + many(op_(',') + specialization_parameter)
    >> flatten)

def make_type_identifier(args):
    return TypeIdentifier(
        name = args[0],
        specialization_parameters = args[1])

type_identifier = (
    identifier + maybe(op_('[') + specialization_parameter_list + op_(']'))
    >> make_type_identifier)

def make_function_parameter(args):
    attributes = args[3] or []
    if args[0].value == 'mut':
        attributes.append('mutable')

    return FunctionParameter(
        name = args[2] or args[1],
        api_name = args[1],
        attributes = attributes,
        type_annotation = args[4],
        default_value = args[5])

function_parameter = (
    (kw('cst') | kw('mut')) +
    identifier + maybe(identifier) + op_(':') +
    many(op_('@') + identifier) + type_signature +
    maybe(op_('=') + expression)
    >> make_function_parameter)

function_parameter_list = (
    function_parameter + many(op_(',') + function_parameter)
    >> flatten)

def make_function_signature(args):
    return FunctionSignature(
        parameters = args[0] or [],
        return_type = args[1])

function_signature = (
    op_('(') + maybe(function_parameter_list) + op_(')') +
    op_('->') + type_signature
    >> make_function_signature)

type_signature.define(function_signature | type_identifier)

def make_number_literal(token):
    result = Literal(value = token.value)
    if ('.' in token.value) or ('e' in token.value) or ('E' in token.value):
        result.__info__['type'] = Double
    else:
        result.__info__['type'] = Int
    return result

def make_string_literal(token):
    result = Literal(value = token.value)
    result.__info__['type'] = String
    return result

number_literal = token_of_type('number') >> make_number_literal
string_literal = token_of_type('string') >> make_string_literal

constant = number_literal | string_literal
variable = identifier | operator_identifier

citizen = constant | variable

def make_prefixed_expression(args):
    return PrefixedExpression(
        operator = args[0],
        operand = args[1])

pfx_expr = (
    pfx_op + expression
    >> make_prefixed_expression)

def make_binary_expression(args):

    # Implementation note:
    # We receive a tuple (operand, [(operator, operand)]) from the parser.
    # Would the parsed expression be `0 + 1 - 2`, `args` would be
    # equivalent to (Literal(0), [('+', Literal(1)), ('-', Literal(2))]).

    if args[1]:
        return BinaryExpression(
            left = args[0],
            operator = args[1][0][0],
            right = make_binary_expression((args[1][0][1], args[1][1:])))

    # Note that `make_binary_expression` will be called for the last operand,
    # even if there isn't any binary expression left to create. Hence we can
    # simply return the operand "as is".
    return args[0]

mul_expr = citizen + many(mul_op + citizen) >> make_binary_expression
add_expr = mul_expr + many(add_op + mul_expr) >> make_binary_expression
bin_expr = add_expr

expression.define(bin_expr)

def make_assignment(args):
    return Assignment(
        target = args[0],
        value = args[1])

assignment = (
    expression + op_('=') + expression
    >> make_assignment)

def make_constant_decl(args):
    return ConstantDecl(
        name = args[0],
        type_annotation = args[1],
        initial_value = args[2])

constant_decl = (
    kw_('cst') + identifier +
    maybe(op_(':') + type_signature) +
    maybe(op_('=') + expression)
    >> make_constant_decl)

def make_variable_decl(args):
    return VariableDecl(
        name = args[0],
        type_annotation = args[1],
        initial_value = args[2])

variable_decl = (
    kw_('mut') + identifier +
    maybe(op_(':') + type_signature) +
    maybe(op_('=') + expression)
    >> make_variable_decl)

def make_function_decl(args):
    return FunctionDecl(
        name = args[0],
        generic_parameters = args[1],
        signature = FunctionSignature(
            parameters = args[2] or [],
            return_type = args[3] or Nothing),
        body = args[4])

generic_parameters = (
    type_identifier + many(op_(',') + type_identifier) >> flatten)

function_decl = (
    kw_('fun') + (identifier | operator_identifier) +
    maybe(op_('<') + generic_parameters + op_('>')) +
    op_('(') + maybe(function_parameter_list) + op_(')') +
    maybe(op_('->') + type_signature) +
    block
    >> make_function_decl)

type_import_list = (
    type_identifier + many(op_(',') + type_identifier)
    >> flatten)

type_conformance_list = (
    type_identifier + many(op_('&') + type_identifier)
    >> flatten)

def make_enum_case_parameter(args):
    return EnumCaseParameter(
        name = args[0],
        type_annotation = args[1])

enum_case_parameter = (
    identifier + op_(':') + type_signature
    >> make_enum_case_parameter)

enum_case_parameter_list = (
    enum_case_parameter + many(op_(',') + enum_case_parameter)
    >> flatten)

def make_enum_case(args):
    return EnumCase(
        name = args[0],
        parameters = args[1])

enum_case = (
    kw_('case') + identifier + maybe(op_('(') + enum_case_parameter_list + op_(')'))
    >> make_enum_case)

def make_enum_decl(args):
    return EnumDecl(
        name = args[0],
        import_list = args[1],
        conformance_list = args[2],
        cases = list(filter(lambda e: isinstance(e, EnumCase), args[3])),
        methods = list(filter(lambda e: isinstance(e, FunctionDecl),args[3])))

enum_member = enum_case | function_decl

enum_body = (
    maybe(enum_member) + many(nl_ + enum_member)
    >> flatten)

enum_decl = (
    kw_('enum') + identifier +
    maybe(kw_('import') + type_import_list) +
    maybe(op_(':') + type_conformance_list) +
    op_('{') + enum_body + nl_opt + op_('}')
    >> make_enum_decl)

def make_struct_decl(args):
    return StructDecl(
        name = args[0],
        import_list = args[1],
        conformance_list = args[2],
        stored_properties = list(filter(
            lambda e: isinstance(e, VariableDecl) or isinstance(e, ConstantDecl), args[3])),
        methods = list(filter(lambda e: isinstance(e, FunctionDecl), args[3])))

struct_member = variable_decl | constant_decl | function_decl

struct_body = (
    maybe(struct_member) + many(nl_ + struct_member)
    >> flatten)

struct_decl = (
    kw_('struct') + identifier +
    maybe(kw_('import') + type_import_list) +
    maybe(op_(':') + type_conformance_list) +
    op_('{') + struct_body + nl_opt + op_('}')
    >> make_struct_decl)

statement.define(
    constant_decl |
    variable_decl |
    function_decl |
    enum_decl |
    struct_decl |
    assignment)

def make_module_decl(args):
    return ModuleDecl(
        name = '',
        body = Block(statements = args))

module_decl = (
    statement_list + nl_opt
    >> make_module_decl)

parser = module_decl + skip(finished)

def parse(s):
    return parser.parse(tokenize(s))
