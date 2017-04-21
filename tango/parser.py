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
        ('operator',   (r'\->|not|and|or|[><=!]=|[=\+\-\*\/%\.:,&@<>{}\[\]\(\)]',)),
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

type_signature    = forward_decl()
expression        = forward_decl()
call_expression   = forward_decl()
if_expression     = forward_decl()
switch_expression = forward_decl()
container_decl    = forward_decl()
struct_decl       = forward_decl()
enum_decl         = forward_decl()
statement         = forward_decl()

wildcard = kw_('_') >> (lambda _: Wildcard())

def make_identifier(token):
    return token.value

identifier = (
    some(lambda token: token.type == 'identifier')
    >> make_identifier)

pfx_op = (op('+') | op('-') | op('not')) >> make_identifier

mul_op = (op('*') | op('/') | op('%')) >> make_identifier
add_op = (op('+') | op('-')) >> make_identifier
cmp_op = (op('<') | op('<=') | op('>=') | op('>')) >> make_identifier
eq_op  = (op('==') | op('!=')) >> make_identifier
and_op = op('and') >> make_identifier
or_op  = op('or') >> make_identifier

operator_identifier = pfx_op | or_op | and_op | eq_op | cmp_op | mul_op | add_op

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

def make_nested_type_identifier(args):
    if args[1]:
        return Select(
            owner = make_select_expression((args[0], args[1][:-1])),
            member = args[1][-1])
    return args[0]

nested_type_identifier = (
    type_identifier + many(op_('.') + type_identifier)
    >> make_nested_type_identifier)

def make_function_parameter(args):
    attributes = set(args[2])
    if args[0].value == 'mut':
        attributes.add('mutable')

    return FunctionParameter(
        name = args[1],
        label = args[1] if (args[1] != '_') else None,
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

type_signature.define(function_signature | nested_type_identifier)

def make_number_literal(token):
    result = Literal(value = token.value)
    if ('.' in token.value) or ('e' in token.value) or ('E' in token.value):
        result.__info__['type'] = Double
    else:
        result.__info__['type'] = Int
    return result

number_literal = token_of_type('number') >> make_number_literal

def make_string_literal(token):
    result = Literal(value = token.value[1:-1])
    result.__info__['type'] = String
    return result

string_literal = token_of_type('string') >> make_string_literal

def make_array_literal(args):
    return ArrayLiteral(items = args)

array_literal_items = (
    expression + many(op_(',') + expression) + maybe(op_(','))
    >> flatten)

array_literal = (
    op_('[') + maybe(array_literal_items) + op_(']')
    >> make_array_literal)

def make_dictionary_literal_item(args):
    return DictionaryLiteralItem(
        key   = args[0],
        value = args[1])

dictionary_literal_item = (
    expression + op_(':') + expression
    >> make_dictionary_literal_item)

dictionary_literal_items = (
    dictionary_literal_item + many(op_(',') + dictionary_literal_item) + maybe(op_(','))
    >> flatten)

def make_dictionary_literal(args):
    return DictionaryLiteral(items = args if isinstance(args, list) else None)

dictionary_literal = (
    op_('[') + (dictionary_literal_items | op(':')) + op_(']')
    >> make_dictionary_literal)

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

def make_implicit_select_expression(args):
    return ImplicitSelect(member = args)

implicit_select_expression = (
    op_('.') + identifier
    >> make_implicit_select_expression)

def make_closure(args):
    return Closure(
        parameters = args[0],
        statements = args[1])

closure_parameter_list = (
    kw_('let') + container_decl + many(op_(',') + container_decl) + kw_('in')
    >> flatten)

closure = (
    op_('{') + nl_opt + maybe(closure_parameter_list) + statement_list + op_('}')
    >> make_closure)

def make_prefixed_expression(args):
    if args[0]:
        return PrefixedExpression(
            operator = args[0],
            operand = args[1])
    return args[1]

literal = dictionary_literal | array_literal | number_literal | string_literal
primary = closure | literal | identifier | op_('(') + expression + op_(')')

sfx_expr = (
    call_expression | if_expression | switch_expression |
    select_expression | implicit_select_expression |
    primary)

pfx_expr = (
    maybe(pfx_op) + sfx_expr
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

operand = pfx_expr | (op_('(') + expression + op_(')'))
expr_50 = operand + many(mul_op + operand) >> make_binary_expression
expr_40 = expr_50 + many(add_op + expr_50) >> make_binary_expression
expr_30 = expr_40 + many(cmp_op + expr_40) >> make_binary_expression
expr_20 = expr_30 + many(eq_op  + expr_30) >> make_binary_expression
expr_10 = expr_20 + many(and_op + expr_20) >> make_binary_expression
expr_00 = expr_10 + many(or_op  + expr_10) >> make_binary_expression

bin_expr = expr_00

expression.define(bin_expr)

def make_call_positional_argument(args):
    return CallArgument(
        attributes = set(args[0]),
        value = args[1])

call_position_argument = (
    many(op_('@') + identifier) + expression
    >> make_call_positional_argument)

def make_call_named_argument(args):
    return CallArgument(
        name = args[0],
        attributes = set(args[1]),
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

function_identifier = select_expression | implicit_select_expression | variable_identifier

call_expression.define(
    function_identifier + op_('(') + maybe(call_argument_list) + op_(')')
    >> make_call)

def make_pattern(args):
    return Pattern(
        parameters = args[0],
        expression = args[1])

pattern_parameter_list = (
    kw_('let') + container_decl + many(op_(',') + container_decl) + kw_('in')
    >> flatten)

pattern = (
    maybe(pattern_parameter_list) + (wildcard | expression)
    >> make_pattern)

def make_if_expression(args):
    return If(
        pattern     = args[0],
        body        = args[1],
        else_clause = args[2])

else_clause = (
    (kw_('else') + if_expression) | (kw_('else') + block))

if_expression.define(
    kw_('if') + pattern + block + maybe(else_clause)
    >> make_if_expression)

def make_switch_case_clause(args):
    return SwitchCaseClause(
        pattern = args[0],
        body    = args[1])

switch_case_clause = (
    kw_('case') + pattern + block
    >> make_switch_case_clause)

switch_case_clause_list = (
    many(nl_opt + switch_case_clause) + nl_opt)

def make_switch_expression(args):
    return Switch(
        expression = args[0],
        clauses =    args[1])

switch_expression.define(
    kw_('switch') + expression + op_('{') + switch_case_clause_list + op_('}')
    >> make_switch_expression)

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

def make_break_statement(args):
    return Break(label = args)

break_statement = (
    kw_('break') + maybe(identifier)
    >> make_break_statement)

def make_continue_statement(args):
    return Continue(label = args)

continue_statement = (
    kw_('continue') + maybe(identifier)
    >> make_continue_statement)

def make_for_loop(args):
    return For(
        label    = args[0],
        iterator = args[1] if isinstance(args[1], ContainerDecl) else None,
        sequence = args[2],
        body     = args[3])

for_loop = (
    maybe(identifier + op_(':')) +
    kw_('for') + (container_decl | kw('_')) + kw_('in') + expression + block
    >> make_for_loop)

def make_while_loop(args):
    return While(
        label   = args[0],
        pattern = args[1],
        body    = args[2])

while_loop = (
    maybe(identifier + op_(':')) +
    kw_('while') + pattern + block
    >> make_while_loop)

def make_container_decl(args):
    return ContainerDecl(
        is_constant = args[0].value == 'cst',
        name = args[1],
        type_annotation = args[2],
        initial_value = args[3])

container_decl.define(
    (kw('cst') | kw('mut')) + identifier +
    maybe(op_(':') + type_signature) +
    maybe(op_('=') + expression)
    >> make_container_decl)

def make_function_decl_parameter(args):
    attributes = set(args[3])
    if args[0].value == 'mut':
        attributes.add('mutable')

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
    nested_type_identifier + many(op_(',') + nested_type_identifier)
    >> flatten)

type_conformance_list = (
    nested_type_identifier + many(op_('&') + nested_type_identifier)
    >> flatten)

def make_enum_case_parameter(args):
    return EnumCaseParameter(
        label = args[0] if (args[0] != '_') else None,
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

enum_member = enum_decl | struct_decl | enum_case_decl | function_decl

enum_member_list = (
    maybe(enum_member) + many(nl_ + nl_opt + enum_member) + nl_opt
    >> make_statement_list)

enum_decl.define(
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

struct_member = enum_decl | struct_decl | function_decl | container_decl

struct_member_list = (
    maybe(struct_member) + many(nl_ + nl_opt + struct_member) + nl_opt
    >> make_statement_list)

struct_decl.define(
    kw_('struct') + identifier +
    maybe(kw_('import') + type_import_list) +
    maybe(op_(':') + type_conformance_list) +
    op_('{') + struct_member_list + op_('}')
    >> make_struct_decl)

statement.define(
    container_decl |
    function_decl |
    enum_decl |
    struct_decl |
    assignment |
    for_loop |
    while_loop |
    return_statement |
    break_statement |
    continue_statement |
    call_expression |
    if_expression |
    switch_expression)

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
