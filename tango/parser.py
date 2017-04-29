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
        ('operator',   (r'\->|not|and|or|is|as[\?!]|[&\|\+\-\*\/%~><=!]=|===|>>=?|<<=?|'
                        r'[=\+\-\*\/%~\.:,\?!@<>{}\[\]\(\)]',)),
        ('name',       (r'[^\W\d][\w]*',)),
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
kw     = lambda value: a(Token('name', value))

op_    = lambda value: skip(a(Token('operator', value)))
kw_    = lambda value: skip(a(Token('name', value)))
nl_    = skip(some(lambda token: token.type == 'newline'))
nl_opt = skip(many(some(lambda token: token.type == 'newline')))

type_signature = forward_decl()
expression     = forward_decl()
select_expr    = forward_decl()
call_expr      = forward_decl()
subscript_expr = forward_decl()
if_expr        = forward_decl()
switch_expr    = forward_decl()
pattern        = forward_decl()
container_decl = forward_decl()
struct_decl    = forward_decl()
enum_decl      = forward_decl()
protocol_decl  = forward_decl()
statement      = forward_decl()

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

def make_name(token):
    return token.value

name = (
    some(lambda token: token.type == 'name')
    >> make_name)

self_assignment_op = (
    op('&=') | op('|=') | op('+=') | op('-=') | op('*=') | op('/=') | op('%=') |
    op('>>=') | op('<<=')) >> make_name

prefix_op  = (op('+') | op('-') | op('~') | op('not')) >> make_name
postfix_op = (op('?') | op('!')) >> make_name

shf_op     = (op('>>') | op('<<')) >> make_name
mul_op     = (op('*') | op('/') | op('%')) >> make_name
add_op     = (op('+') | op('-')) >> make_name
cmp_op     = (op('<') | op('<=') | op('>=') | op('>')) >> make_name
cast_op    = (op('as?') | op('as!')) >> make_name
eq_op      = (op('is') | op('==') | op('!=') | op('~=') | op('===')) >> make_name
and_op     = op('and') >> make_name
or_op      = op('or') >> make_name
infix_op   = cast_op | or_op | and_op | eq_op | cmp_op | add_op | mul_op

operator_name = self_assignment_op | prefix_op | postfix_op | infix_op

def make_specialization_argument(args):
    return SpecializationArgument(
        name            = args[0],
        type_annotation = args[1])

specialization_argument = (
    name + op_('=') + type_signature
    >> make_specialization_argument)

specialization_argument_list = (
    specialization_argument + many(op_(',') + specialization_argument)
    >> flatten)

def make_type_identifier(args):
    return TypeIdentifier(
        name            = args[0],
        specializations = args[1])

type_identifier = (
    name + maybe(op_('<') + specialization_argument_list + op_('>'))
    >> make_type_identifier)

type_expr = type_identifier + op_('.') + kw_('self')

def make_nested_type_identifier(args):
    if args[1]:
        return Select(
            owner = make_explicit_select_expr((args[0], args[1][:-1])),
            member = args[1][-1])
    return args[0]

nested_type_identifier = (
    type_identifier + many(op_('.') + type_identifier)
    >> make_nested_type_identifier)

wildcard = kw_('_') >> (lambda _: Wildcard())

def make_value_binding_pattern(args):
    return ValueBindingPattern(
        is_mutable      = args[0].value != 'cst',
        is_shared       = args[0].value == 'shd',
        name            = args[1],
        type_annotation = args[2])

value_binding_pattern = (
    (kw('cst') | kw('mut') | kw('shd')) + name +
    maybe(op_(':') + type_signature)
    >> make_value_binding_pattern)

def make_argument_pattern(args):
    return ArgumentPattern(
        label   = args[0],
        pattern = args[1])

argument_pattern = (
    maybe(name + op_('=')) + pattern
    >> make_argument_pattern)

argument_pattern_list = (
    argument_pattern + many(op_(',') + argument_pattern)
    >> flatten)

def make_tuple_pattern(args):
    return TuplePattern(elements = args)

tuple_pattern = (
    op_('(') + argument_pattern_list + op_(')')
    >> make_tuple_pattern)

def make_enum_case_pattern(args):
    return EnumCasePattern(
        case      = args[0],
        arguments = args[1])

enum_case_pattern = (
    select_expr +
    maybe(op_('(') + argument_pattern_list + op_(')'))
    >> make_enum_case_pattern)

def make_pattern(args):
    return Pattern(
        expression   = args[0],
        where_clause = args[1])

pattern.define(
    (wildcard | value_binding_pattern | tuple_pattern | enum_case_pattern | expression) +
    maybe(kw_('where') + expression)
    >> make_pattern)

def make_parameter(args):
    attributes = set(args[2])
    if args[0].value == 'mut':
        attributes.add('mutable')
    if args[0].value == 'shd':
        attributes.add('shared')

    return Parameter(
        name            = args[1],
        label           = args[1] if (args[1] != '_') else None,
        attributes      = attributes,
        type_annotation = args[3])

parameter = (
    (kw('cst') | kw('mut') | kw('shd')) +
    name + op_(':') +
    many(op_('@') + name) + type_signature
    >> make_parameter)

parameter_list = (
    parameter + many(op_(',') + parameter)
    >> flatten)

def make_tuple_signature(args):
    return TupleSignature(parameters = args[0])

tuple_signature = (
    op_('(') + parameter_list + op_(')')
    >> make_tuple_signature)

def make_function_signature(args):
    return FunctionSignature(
        parameters  = args[0],
        return_type = args[1])

function_signature = (
    op_('(') + maybe(parameter_list) + op_(')') +
    op_('->') + type_signature
    >> make_function_signature)

type_signature.define(function_signature | tuple_signature | nested_type_identifier)

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
    return ArrayLiteral(elements = args)

array_literal_element_list = (
    expression + many(op_(',') + expression) + maybe(op_(','))
    >> flatten)

array_literal = (
    op_('[') + maybe(array_literal_element_list) + op_(']')
    >> make_array_literal)

def make_dictionary_literal_element(args):
    return DictionaryLiteralElement(
        key   = args[0],
        value = args[1])

dictionary_literal_element = (
    expression + op_(':') + expression
    >> make_dictionary_literal_element)

dictionary_literal_element_list = (
    dictionary_literal_element + many(op_(',') + dictionary_literal_element) + maybe(op_(','))
    >> flatten)

def make_dictionary_literal(args):
    return DictionaryLiteral(elements = args if isinstance(args, list) else None)

dictionary_literal = (
    op_('[') + (dictionary_literal_element_list | op(':')) + op_(']')
    >> make_dictionary_literal)

def make_variable_identifier(name):
    return Identifier(name = name)

variable_identifier = (
    (name | operator_name)
    >> make_variable_identifier)

def make_closure(args):
    return Closure(
        parameters = args[0],
        statements = args[1])

closure_parameter_list = (
    container_decl + many(op_(',') + container_decl) + kw_('in')
    >> flatten)

closure = (
    op_('{') + nl_opt + maybe(closure_parameter_list) + statement_list + op_('}')
    >> make_closure)

literal = dictionary_literal | array_literal | number_literal | string_literal
primary = closure | literal | variable_identifier | op_('(') + expression + op_(')')
operand = call_expr | subscript_expr | type_expr | select_expr | primary

def make_postfix_expr(args):
    if args[1]:
        return PostfixExpression(
            operand  = args[1],
            operator = args[0])
    return args[0]

postfix_expr = (
    operand + maybe(postfix_op)
    >> make_postfix_expr)

def make_prefix_expr(args):
    if args[0]:
        return PrefixExpression(
            operator = args[0],
            operand  = args[1])
    return args[1]

prefix_expr = (
    maybe(prefix_op) + postfix_expr
    >> make_prefix_expr)

def make_binary_expr(args):

    # Implementation note:
    # We receive a tuple (operand, [(operator, operand)]) from the parser.
    # Would the parsed expression be `0 + 1 - 2`, `args` would be
    # equivalent to (Literal(0), [('+', Literal(1)), ('-', Literal(2))]).

    if args[1]:
        return BinaryExpression(
            left     = args[0],
            operator = args[1][0][0],
            right    = make_binary_expr((args[1][0][1], args[1][1:])))

    # Note that `make_binary_expr` will be called for the last operand,
    # even if there isn't any binary expression left to create. Hence we can
    # simply return the operand "as is".
    return args[0]

expr_80 = prefix_expr | if_expr | switch_expr | (op_('(') + expression + op_(')'))
expr_70 = expr_80 + many(shf_op  + expr_80) >> make_binary_expr
expr_60 = expr_70 + many(mul_op  + expr_70) >> make_binary_expr
expr_50 = expr_60 + many(add_op  + expr_60) >> make_binary_expr
expr_40 = expr_50 + many(cast_op + expr_50) >> make_binary_expr
expr_30 = expr_40 + many(cmp_op  + expr_40) >> make_binary_expr
expr_20 = expr_30 + many(eq_op   + expr_30) >> make_binary_expr
expr_10 = expr_20 + many(and_op  + expr_20) >> make_binary_expr
expr_00 = expr_10 + many(or_op   + expr_10) >> make_binary_expr

bin_expr = expr_00

expression.define(bin_expr)

def make_implicit_select_expr(args):
    return ImplicitSelect(member = args)

implicit_select_expr = (
    op_('.') + name
    >> make_implicit_select_expr)

def make_explicit_select_expr(args):
    return Select(
        owner  = args[0],
        member = args[1])

# FIXME Because of left recursion, parsing the owner as a postfix expression
# would trigger an infinite recursion. Hence we force such expressions to be
# enclosed within parenthesis, via the production rule of `primary`.
explicit_select_owner = (
    primary + maybe(postfix_op)
    >> make_postfix_expr)

explicit_select_expr = (
    explicit_select_owner + op_('.') + variable_identifier
    >> make_explicit_select_expr)

select_expr.define(explicit_select_expr | implicit_select_expr)

def make_call_positional_argument(args):
    return CallArgument(
        attributes = set(args[0]),
        value      = args[1])

call_positional_argument = (
    many(op_('@') + name) + expression
    >> make_call_positional_argument)

def make_call_named_argument(args):
    return CallArgument(
        name       = args[0],
        attributes = set(args[1]),
        value      = args[2])

call_named_argument = (
    name + op_('=') + many(op_('@') + name) + expression
    >> make_call_named_argument)

call_argument = call_named_argument | call_positional_argument

call_argument_list = (
    call_argument + many(op_(',') + call_argument)
    >> flatten)

def make_call(args):
    return Call(callee = args[0], arguments = args[1])

# FIXME Because of left recursion, parsing the callee as a postfix expression
# would trigger an infinite recursion. Hence we force such expressions to be
# enclosed within parenthesis, via the production rule of `primary`.
call_callee = (
    primary + maybe(postfix_op)
    >> make_postfix_expr)

call_expr.define(
    call_callee + op_('(') + maybe(call_argument_list) + op_(')')
    >> make_call)

def make_subscript(args):
    return Subscript(callee = args[0], arguments = args[1])

# FIXME Because of left recursion, parsing the callee as a postfix expression
# would trigger an infinite recursion. Hence we force such expressions to be
# enclosed within parenthesis, via the production rule of `primary`.
subscript_callee = (
    primary + maybe(postfix_op)
    >> make_postfix_expr)

subscript_expr.define(
    subscript_callee + op_('[') + call_argument_list + op_(']')
    >> make_subscript)

def make_if_expr(args):
    return If(
        pattern     = args[0],
        body        = args[1],
        else_clause = args[2])

else_clause = (
    (kw_('else') + if_expr) | (kw_('else') + block))

if_expr.define(
    kw_('if') + pattern + block + maybe(else_clause)
    >> make_if_expr)

def make_switch_case_clause(args):
    return SwitchCaseClause(
        pattern = args[0],
        body    = args[1])

switch_case_clause = (
    kw_('case') + pattern + block
    >> make_switch_case_clause)

switch_case_clause_list = (
    many(nl_opt + switch_case_clause) + nl_opt)

def make_switch_expr(args):
    return Switch(
        expression = args[0],
        clauses    = args[1])

switch_expr.define(
    kw_('switch') + expression + op_('{') + switch_case_clause_list + op_('}')
    >> make_switch_expr)

def make_assignment(args):
    return Assignment(
        lvalue = args[0],
        rvalue = args[1])

assignment = (
    pattern + op_('=') + expression
    >> make_assignment)

def make_self_assignement(args):
    return SelfAssignement(
        lvalue   = args[0],
        operator = args[1],
        rvalue   = args[2])

self_assignment = (
    expression + self_assignment_op + expression
    >> make_self_assignement)

def make_return_statement(args):
    return Return(value = args)

return_statement = (
    kw_('return') + expression
    >> make_return_statement)

def make_break_statement(args):
    return Break(label = args)

break_statement = (
    kw_('break') + maybe(name)
    >> make_break_statement)

def make_continue_statement(args):
    return Continue(label = args)

continue_statement = (
    kw_('continue') + maybe(name)
    >> make_continue_statement)

def make_for_loop(args):
    return For(
        label    = args[0],
        iterator = args[1],
        sequence = args[2],
        body     = args[3])

    # TODO Reject invalid patterns

for_loop = (
    maybe(name + op_(':')) +
    kw_('for') + pattern + kw_('in') + expression + block
    >> make_for_loop)

def make_while_loop(args):
    return While(
        label   = args[0],
        pattern = args[1],
        body    = args[2])

while_loop = (
    maybe(name + op_(':')) +
    kw_('while') + pattern + block
    >> make_while_loop)

def make_container_decl(args):
    return ContainerDecl(
        is_mutable      = args[0].value != 'cst',
        is_shared       = args[0].value == 'shd',
        name            = args[1],
        type_annotation = args[2],
        initial_value   = args[3])

container_decl.define(
    (kw('cst') | kw('mut') | kw('shd')) + name +
    maybe(op_(':') + type_signature) +
    maybe(op_('=') + expression)
    >> make_container_decl)

def make_function_decl_parameter(args):
    attributes = set(args[3])
    if args[0].value == 'mut':
        attributes.add('mutable')
    if args[0].value == 'shd':
        attributes.add('shared')

    name = args[2] or args[1]
    if name == '_':
        raise SyntaxError("'_' is not a valid parameter name")
    label = args[1] if (args[1] != '_') else None

    return Parameter(
        name            = name,
        label           = label,
        attributes      = attributes,
        type_annotation = args[4],
        default_value   = args[5])

function_decl_parameter = (
    (kw('cst') | kw('mut') | kw('shd')) +
    name + maybe(name) + op_(':') +
    many(op_('@') + name) + type_signature +
    maybe(op_('=') + expression)
    >> make_function_decl_parameter)

function_decl_parameter_list = (
    function_decl_parameter + many(op_(',') + function_decl_parameter)
    >> flatten)

def make_function_decl(args):
    return FunctionDecl(
        name               = args[0],
        generic_parameters = args[1],
        signature          = FunctionSignature(
            parameters  = args[2] or [],
            return_type = args[3] or Nothing),
        where_clause       = args[4],
        body               = args[5])

generic_parameters = (
    name + many(op_(',') + name) >> flatten)

function_decl = (
    kw_('fun') + (name | operator_name) +
    maybe(op_('<') + generic_parameters + op_('>')) +
    op_('(') + maybe(function_decl_parameter_list) + op_(')') +
    maybe(op_('->') + type_signature) +
    maybe(kw_('where') + expression) +
    block
    >> make_function_decl)

type_import_list = (
    nested_type_identifier + many(op_(',') + nested_type_identifier)
    >> flatten)

type_conformance_list = (
    nested_type_identifier + many(op_(',') + nested_type_identifier)
    >> flatten)

def make_enum_case_parameter(args):
    return EnumCaseParameter(
        label           = args[0] if (args[0] != '_') else None,
        type_annotation = args[1])

enum_case_parameter = (
    name + op_(':') + type_signature
    >> make_enum_case_parameter)

enum_case_parameter_list = (
    enum_case_parameter + many(op_(',') + enum_case_parameter)
    >> flatten)

def make_enum_case_decl(args):
    return EnumCaseDecl(
        name       = args[0],
        parameters = args[1])

enum_case_decl = (
    kw_('case') + name +
    maybe(op_('(') + enum_case_parameter_list + op_(')'))
    >> make_enum_case_decl)

def make_enum_decl(args):
    return EnumDecl(
        name               = args[0],
        generic_parameters = args[1],
        conformance_list   = args[2],
        import_list        = args[3],
        where_clause       = args[4],
        body               = Block(args[4]))

enum_member = enum_decl | struct_decl | protocol_decl | enum_case_decl | function_decl

enum_member_list = (
    maybe(enum_member) + many(nl_ + nl_opt + enum_member) + nl_opt
    >> make_statement_list)

enum_decl.define(
    kw_('enum') + name +
    maybe(op_('<') + generic_parameters + op_('>')) +
    maybe(op_(':') + type_conformance_list) +
    maybe(kw_('import') + type_import_list) +
    maybe(kw_('where') + expression) +
    op_('{') + enum_member_list + op_('}')
    >> make_enum_decl)

def make_struct_decl(args):
    return StructDecl(
        name               = args[0],
        generic_parameters = args[1],
        conformance_list   = args[2],
        import_list        = args[3],
        where_clause       = args[4],
        body               = Block(args[5]))

struct_member = enum_decl | struct_decl | protocol_decl | function_decl | container_decl

struct_member_list = (
    maybe(struct_member) + many(nl_ + nl_opt + struct_member) + nl_opt
    >> make_statement_list)

struct_decl.define(
    kw_('struct') + name +
    maybe(op_('<') + generic_parameters + op_('>')) +
    maybe(op_(':') + type_conformance_list) +
    maybe(kw_('import') + type_import_list) +
    maybe(kw_('where') + expression) +
    op_('{') + struct_member_list + op_('}')
    >> make_struct_decl)

def make_protocol_property_decl(args):
    return ContainerDecl(
        is_mutable      = args[0].value != 'cst',
        is_shared       = args[0].value == 'shd',
        name            = args[1],
        type_annotation = args[2])

protocol_property_decl = (
    (kw('cst') | kw('mut') | kw('shd')) + name +
    maybe(op_(':') + type_signature)
    >> make_protocol_property_decl)

def make_protocol_function_decl(args):
    return FunctionDecl(
        name               = args[0],
        generic_parameters = args[1],
        signature          = FunctionSignature(
            parameters  = args[2] or [],
            return_type = args[3] or Nothing),
        where_clause       = args[4],
        body               = None)

protocol_function_decl = (
    kw_('fun') + (name | operator_name) +
    maybe(op_('<') + generic_parameters + op_('>')) +
    op_('(') + maybe(function_decl_parameter_list) + op_(')') +
    maybe(op_('->') + type_signature) +
    maybe(kw_('where') + expression)
    >> make_protocol_function_decl)

def make_abstract_type_decl(args):
    return AbstractTypeDecl(
        name             = args[0],
        conformance_list = args[1])

abstract_type_decl = (
    kw_('abs') + name +
    maybe(op_(':') + type_conformance_list)
    >> make_abstract_type_decl)

def make_protocol_decl(args):
    return ProtocolDecl(
        name             = args[0],
        conformance_list = args[1],
        body             = Block(args[2]))

protocol_member = protocol_function_decl | protocol_property_decl | abstract_type_decl

protocol_member_list = (
    maybe(protocol_member) + many(nl_ + nl_opt + protocol_member) + nl_opt
    >> make_statement_list)

protocol_decl.define(
    kw_('protocol') + name +
    maybe(op_(':') + type_conformance_list) +
    op_('{') + protocol_member_list + op_('}')
    >> make_protocol_decl)

def make_extension_decl(args):
    return ExtensionDecl(
        subject      = args[0],
        where_clause = args[1],
        declaration  = args[2])

extension_decl = (
    kw_('extension') + name +
    maybe(kw_('where') + expression) +
    op_('->') + nl_opt + (struct_decl | enum_decl)
    >> make_extension_decl)

statement.define(
    container_decl |
    function_decl |
    enum_decl |
    struct_decl |
    protocol_decl |
    extension_decl |
    assignment |
    self_assignment |
    for_loop |
    while_loop |
    return_statement |
    break_statement |
    continue_statement |
    call_expr |
    if_expr |
    switch_expr)

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
