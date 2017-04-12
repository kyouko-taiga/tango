import re

from funcparserlib.lexer import make_tokenizer, Token
from funcparserlib.parser import some, a, many, maybe, finished, skip, forward_decl

from .ast import *
from .builtin import Nothing, Int, Double, String


def tokenize(s):
    tokenizer = make_tokenizer([
        ('comment',    (r'#.*\n',)),
        ('newline',    (r'[\r\n]+',)),
        ('space',      (r'[ \t\v]+',)),
        ('operator',   (r'(\->)|(not)|[=\+\-\*\/%\.:,&@<>{}\[\]\(\)]',)),
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
nl_opt = skip(many(some(lambda token: token.type == 'newline')))

type_signature = forward_decl()
expression     = forward_decl()
statement      = forward_decl()

def make_identifier(token):
    return token.value

identifier = (
    some(lambda token: token.type == 'identifier')
    >> make_identifier)

pfx_op = op('not') >> make_identifier

mul_op = (op('*') | op('/') | op('%')) >> make_identifier
add_op = (op('+') | op('-')) >> make_identifier

operator_identifier = pfx_op | mul_op | add_op

def make_statement_list(args):
    if args[0] is None:
        return args[1]
    return flatten(args)

statement_list = (
    maybe(statement) + many(nl_ + nl_opt + statement) + nl_opt
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
    identifier + op_(':') + type_signature
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
    attributes = args[2] or []
    if args[0].value == 'mut':
        attributes.append('mutable')

    return FunctionParameter(
        name = args[1],
        label = args[1],
        attributes = attributes,
        type_annotation = args[3])

function_parameter = (
    (kw('cst') | kw('mut')) +
    identifier + op_(':') +
    many(op_('@') + identifier) + type_signature
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

def make_variable_identifier(name):
    return Identifier(name = name)

variable_identifier = (
    (identifier | operator_identifier)
    >> make_variable_identifier)

def make_select_expression(args):
    if args[1]:
        return Select(
            owner = make_select_expression((args[0], args[1][:-1])),
            member = args[1][-1])
    return args[0]

select_expression = (
    variable_identifier + many(op_('.') + variable_identifier)
    >> make_select_expression)

citizen = constant | select_expression

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

def make_call_positional_argument(args):
    return CallArgument(
        attributes = args[0],
        value = args[1])

call_position_argument = (
    many(op_('@') + identifier) + expression
    >> make_call_positional_argument)

def make_call_named_argument(args):
    return CallArgument(
        name = args[0],
        attributes = args[1],
        value = args[2])

call_named_argument = (
    identifier + op_(':') + many(op_('@') + identifier) + expression
    >> make_call_named_argument)

call_argument = call_named_argument | call_position_argument

call_argument_list = (
    call_argument + many(op_(',') + call_argument)
    >> flatten)

def make_call(args):
    return Call(callee = args[0], arguments = args[1])

function_identifier = select_expression | variable_identifier

call_expression = (
    function_identifier + op_('(') + maybe(call_argument_list) + op_(')')
    >> make_call)

expression.define(call_expression | bin_expr | pfx_expr)

def make_assignment(args):
    return Assignment(
        lvalue = args[0],
        rvalue = args[1])

lvalue = variable_identifier

assignment = (
    lvalue + op_('=') + expression
    >> make_assignment)

def make_return_statement(args):
    return Return(value = args)

return_statement = (
    kw_('return') + expression
    >> make_return_statement)

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

def make_function_decl_parameter(args):
    attributes = args[3] or []
    if args[0].value == 'mut':
        attributes.append('mutable')

    name = args[2] or args[1]
    if name == '_':
        raise SyntaxError("'_' is not a valid parameter name")
    label = args[1] if (args[1] != '_') else None

    return FunctionParameter(
        name = name,
        label = label,
        attributes = attributes,
        type_annotation = args[4],
        default_value = args[5])

function_decl_parameter = (
    (kw('cst') | kw('mut')) +
    identifier + maybe(identifier) + op_(':') +
    many(op_('@') + identifier) + type_signature +
    maybe(op_('=') + expression)
    >> make_function_decl_parameter)

function_decl_parameter_list = (
    function_decl_parameter + many(op_(',') + function_decl_parameter)
    >> flatten)

def make_function_decl(args):
    return FunctionDecl(
        name = args[0],
        generic_parameters = args[1],
        signature = FunctionSignature(
            parameters = args[2] or [],
            return_type = args[3] or Nothing),
        body = args[4])

generic_parameters = (
    identifier + many(op_(',') + identifier) >> flatten)

function_decl = (
    kw_('fun') + (identifier | operator_identifier) +
    maybe(op_('<') + generic_parameters + op_('>')) +
    op_('(') + maybe(function_decl_parameter_list) + op_(')') +
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

def make_enum_case_decl(args):
    return EnumCaseDecl(
        name = args[0],
        parameters = args[1])

enum_case_decl = (
    kw_('case') + identifier +
    maybe(op_('(') + enum_case_parameter_list + op_(')'))
    >> make_enum_case_decl)

def make_enum_decl(args):
    return EnumDecl(
        name = args[0],
        import_list = args[1],
        conformance_list = args[2],
        body = Block(args[3]))

enum_member = enum_case_decl | function_decl

enum_member_list = (
    maybe(enum_member) + many(nl_ + nl_opt + enum_member) + nl_opt
    >> make_statement_list)

enum_decl = (
    kw_('enum') + identifier +
    maybe(kw_('import') + type_import_list) +
    maybe(op_(':') + type_conformance_list) +
    op_('{') + enum_member_list + op_('}')
    >> make_enum_decl)

def make_struct_decl(args):
    return StructDecl(
        name = args[0],
        import_list = args[1],
        conformance_list = args[2],
        body = Block(args[3]))

struct_member = variable_decl | constant_decl | function_decl

struct_members = (
    maybe(struct_member) + many(nl_ + nl_opt + struct_member) + nl_opt
    >> make_statement_list)

struct_decl = (
    kw_('struct') + identifier +
    maybe(kw_('import') + type_import_list) +
    maybe(op_(':') + type_conformance_list) +
    op_('{') + struct_members + op_('}')
    >> make_struct_decl)

statement.define(
    constant_decl |
    variable_decl |
    function_decl |
    enum_decl |
    struct_decl |
    assignment |
    return_statement |
    call_expression)

def make_module_decl(args):
    return ModuleDecl(
        name = '',
        body = Block(statements = args))

module_decl = (
    statement_list
    >> make_module_decl)

parser = module_decl + skip(finished)

def parse(s):
    return parser.parse(tokenize(s))
