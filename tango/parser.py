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
        ('operator',   (r'\->|not|and|or|is|as[\?!]|===|>>=?|<<=?|[&\^\|\+\-\*\/%~><!=]=?|'
                        r'[\.:,\?@{}\[\]\(\)]',)),
        ('name',       (r'[^\W\d][\w]*',)),
        ('number',     (r'[-+]?(0|([1-9][0-9]*))(\.[0-9]+)?([Ee][+-]?[0-9]+)?',)),
        ('string',     (r'\'[^\']*\'',)),
    ])

    # Ignore whitespaces and comments.
    return [token for token in tokenizer(s) if token.type not in ['space', 'newline', 'comment']]

def token_of_type(type):
    return some(lambda token: token.type == type)

def flatten(args):
    return [args[0]] + args[1]


# Terminal symbols.

op     = lambda value: a(Token('operator', value))
kw     = lambda value: a(Token('name', value))

op_    = lambda value: skip(a(Token('operator', value)))
kw_    = lambda value: skip(a(Token('name', value)))
nl_    = skip(some(lambda token: token.type == 'newline'))
nl_opt = skip(many(some(lambda token: token.type == 'newline')))

def make_name(token):
    return token.value

name     = some(lambda token: token.type == 'name') >> make_name


# Operators.

self_assignment_op = (
              op('>>=') | op('<<=') |
              op('*=')  | op('/=')  | op('%=')  | op('+=')  | op('-=')  |
              op('&=')  | op('^=')  | op('|=')                              ) >> make_name

prefix_op  = (op('+')   | op('-')   | op('~')   | op('not')                 ) >> make_name
postfix_op = (op('?')   | op('!')                                           ) >> make_name

shf_op     = (op('>>')  | op('<<')                                          ) >> make_name
mul_op     = (op('*')   | op('/')   | op('%')                               ) >> make_name
add_op     = (op('+')   | op('-')                                           ) >> make_name
cmp_op     = (op('<')   | op('<=')  | op('>=')  | op('>')                   ) >> make_name
cast_op    = (op('as?') | op('as!')                                         ) >> make_name
eq_op      = (op('is')  | op('==')  | op('!=')  | op('===')                 ) >> make_name
and_op     =  op('&')                                                         >> make_name
xor_op     =  op('^')                                                         >> make_name
or_op      =  op('|')                                                         >> make_name
land_op    =  op('and')                                                       >> make_name
lor_op     =  op('or')                                                        >> make_name

operator_name = (
              op('>>=') | op('<<=') |
              op('*=')  | op('/=')  | op('%=')  | op('+=')  | op('-=')  |
              op('&=')  | op('|=')  |
              op('+')   | op('-')   | op('~')   | op('not') |
              op('?')   | op('!')   |
              op('>>')  | op('<<')  |
              op('*')   | op('/')   | op('%')   |
              op('<')   | op('<=')  | op('>=')  | op('>')   |
              op('as?') | op('as!') |
              op('is')  | op('==')  | op('!=')  | op('~=')  | op('===') |
              op('&')   | op('^')   | op('|')   |
              op('and') | op('or')                                          ) >> make_name


# Nonterminal symbols.

fun_decl       = forward_decl()
struct_decl    = forward_decl()
enum_decl      = forward_decl()
protocol_decl  = forward_decl()
expr           = forward_decl()
select_expr    = forward_decl()
operand        = forward_decl()
if_expr        = forward_decl()
switch_expr    = forward_decl()
type_signature = forward_decl()
type_name      = forward_decl()
pattern        = forward_decl()
stmt           = forward_decl()

def make_stmt_list(args):
    if args[0] is None:
        return args[1]
    return flatten(args)

stmt_list = (
    stmt + many(stmt)
    >> make_stmt_list)

def make_module_decl(args):
    return ModuleDecl(
        name = '',
        body = Block(statements = args))

module_decl = maybe(stmt_list) >> make_module_decl

def make_block(statements):
    return Block(statements = statements)

block = (
    op_('{') + maybe(stmt_list) + op_('}')
    >> make_block)

attribute = (
    (op_('@') + kw('mut')) |
    (op_('@') + kw('shd')) |
    (op_('@') + kw('override')))

mutability_modifier = kw('cst') | kw('mut') | kw('shd')

initializer = op_('=') + expr

def make_prop_decl(args):
    attributes = set(token.value for token in args[0])
    for attr in attributes:
        if attr != 'override':
            raise SyntaxError("invalid attribute '{}' on property declaration".format(attr))

    if args[1].value == 'mut':
        attributes.add('mutable')
    elif args[1].value == 'shd':
        attributes.add('shared')

    result = PropertyDecl(
        attributes      = attributes,
        name            = args[2],
        type_annotation = args[3])

    if isinstance(args[4], tuple):
        for decl in args[4]:
            # Because of the parser we defined, we can assume that `decl` is
            # either `None` or an instance of FunctionDecl.
            if decl is None:
                continue
            if decl.name == 'get':
                if result.getter is not None:
                    raise SyntaxError('duplicate property getter declaration')
                result.getter = decl
            elif decl.name == 'set':
                if result.setter is not None:
                    raise SyntaxError('duplicate property getter declaration')
                result.setter = decl
            else:
                raise SyntaxError("property getter/setter should be called 'get'/'set'")
    else:
        result.initializer = args[4]

    return result

prop_decl = (
    many(attribute) +
    mutability_modifier + name +
    maybe(op_(':') + type_signature) +
    maybe(initializer | (op_('{') + fun_decl + maybe(fun_decl) + op_('}')))
    >> make_prop_decl)

def make_fun_decl_param(args):
    if args[0].value == 'mut':
        attributes = {'mutable'}
    elif args[0].value == 'shd':
        attributes = {'mutable'}
    else:
        attributes = set()

    name = args[2] or args[1]
    if name == '_':
        raise SyntaxError("invalid parameter name: '_'")
    label = args[1] if (args[1] != '_') else None

    return FunctionParameterDecl(
        attributes      = attributes,
        name            = name,
        label           = label,
        type_annotation = args[3],
        default_value   = args[4])

fun_decl_param = (
    mutability_modifier + name + maybe(name) +
    op_(':') + type_signature +
    maybe(initializer)
    >> make_fun_decl_param)

fun_decl_param_list = (
    fun_decl_param + many(op_(',') + fun_decl_param)
    >> flatten)

generic_clause = (
    op_('<') + name + many(op_(',') + name) + op_('>')
    >> flatten)

where_clause = kw_('where') + expr

def make_fun_decl(args):
    attributes = set(token.value for token in args[0])
    for attr in attributes:
        if attr != 'override':
            raise SyntaxError("invalid attribute '{}' on function declaration".format(attr))

    return FunctionDecl(
        attributes         = attributes,
        name               = args[1],
        generic_parameters = args[2],
        signature          = FunctionSignature(
            parameters  = args[3] or [],
            return_type = args[4] or Nothing),
        where_clause       = args[5],
        body               = args[6])

fun_decl.define(
    many(attribute) +
    kw_('fun') + name +
    maybe(generic_clause) +
    op_('(') + maybe(fun_decl_param_list) + op_(')') +
    maybe(op_('->') + type_signature) +
    maybe(where_clause) +
    maybe(block)
    >> make_fun_decl)

def make_struct_decl(args):
    attributes = set(token.value for token in args[0])
    for attr in attributes:
        if attr != 'override':
            raise SyntaxError("invalid attribute '{}' on struct declaration".format(attr))

    return StructDecl(
        attributes         = attributes,
        name               = args[1],
        generic_parameters = args[2],
        conformance_list   = args[3],
        import_list        = args[4],
        where_clause       = args[5],
        body               = Block(args[6]))

conformance_clause = (
    op_(':') + type_name + many(op_(',') + type_name)
    >> flatten)

import_clause = (
    kw_('import') + type_name + many(op_(',') + type_name)
    >> flatten)

struct_member = prop_decl | fun_decl | struct_decl | enum_decl | protocol_decl

struct_member_list = (
    maybe(struct_member) + many(struct_member)
    >> make_stmt_list)

struct_decl.define(
    many(attribute) +
    kw_('struct') + name +
    maybe(generic_clause) +
    maybe(conformance_clause) +
    maybe(import_clause) +
    maybe(where_clause) +
    op_('{') + struct_member_list + op_('}')
    >> make_struct_decl)

def make_enum_case_param(args):
    return EnumCaseParameterDecl(
        label           = args[0] if (args[0] != '_') else None,
        type_annotation = args[1])

enum_case_param = (
    name + op_(':') + type_signature
    >> make_enum_case_param)

enum_case_param_list = (
    enum_case_param + many(op_(',') + enum_case_param)
    >> flatten)

def make_enum_case_decl(args):
    attributes = set(token.value for token in args[0])
    for attr in attributes:
        if attr != 'override':
            raise SyntaxError("invalid attribute '{}' on enum case declaration".format(attr))

    return EnumCaseDecl(
        attributes = attributes,
        name       = args[1],
        parameters = args[2])

enum_case_decl = (
    many(attribute) +
    kw_('case') + name +
    maybe(op_('(') + enum_case_param_list + op_(')'))
    >> make_enum_case_decl)

def make_enum_decl(args):
    attributes = set(token.value for token in args[0])
    for attr in attributes:
        if attr != 'override':
            raise SyntaxError("invalid attribute '{}' on enum declaration".format(attr))

    return EnumDecl(
        attributes         = attributes,
        name               = args[1],
        generic_parameters = args[2],
        conformance_list   = args[3],
        import_list        = args[4],
        where_clause       = args[5],
        body               = Block(args[6]))

enum_member = prop_decl | fun_decl | struct_decl | enum_decl | enum_case_decl | protocol_decl

enum_member_list = (
    maybe(enum_member) + many(enum_member)
    >> make_stmt_list)

enum_decl.define(
    many(attribute) +
    kw_('enum') + name +
    maybe(generic_clause) +
    maybe(conformance_clause) +
    maybe(import_clause) +
    maybe(where_clause) +
    op_('{') + enum_member_list + op_('}')
    >> make_enum_decl)

def make_abstract_type_decl(args):
    attributes = set(token.value for token in args[0])
    for attr in attributes:
        if attr != 'override':
            raise SyntaxError("invalid attribute '{}' on abstract type declaration".format(attr))

    return AbstractTypeDecl(
        attributes       = attributes,
        name             = args[0],
        conformance_list = args[1])

abstract_type_decl = (
    many(attribute) +
    kw_('abs') + name +
    maybe(conformance_clause)
    >> make_abstract_type_decl)

def make_protocol_decl(args):
    attributes = set(token.value for token in args[0])
    for attr in attributes:
        if attr != 'override':
            raise SyntaxError("invalid attribute '{}' on protocol declaration".format(attr))

    return ProtocolDecl(
        attributes       = attributes,
        name             = args[1],
        conformance_list = args[2],
        import_list      = args[3],
        body             = Block(args[4]))

protocol_member = prop_decl | fun_decl | abstract_type_decl

protocol_member_list = (
    maybe(protocol_member) + many(protocol_member)
    >> make_stmt_list)

protocol_decl.define(
    many(attribute) +
    kw_('protocol') + name +
    maybe(conformance_clause) +
    maybe(import_clause) +
    op_('{') + protocol_member_list + op_('}')
    >> make_protocol_decl)

def make_extension_decl(args):
    return ExtensionDecl(
        subject      = args[0],
        where_clause = args[1],
        declaration  = args[2])

extension_decl = (
    kw_('extension') + type_name +
    maybe(where_clause) +
    op_('->') + (struct_decl | enum_decl)
    >> make_extension_decl)

def make_signature_param(args):
    if args[0].value == 'mut':
        attributes = {'mutable'}
    elif args[0].value == 'shd':
        attributes = {'shared'}
    else:
        attributes = set()

    return SignatureParameter(
        attributes      = attributes,
        label           = args[1] if (args[1] != '_') else None,
        type_annotation = args[2])

signature_param = (
    mutability_modifier + name +
    op_(':') + type_signature
    >> make_signature_param)

signature_param_list = (
    signature_param + many(op_(',') + signature_param)
    >> flatten)

def make_fun_signature(args):
    return FunctionSignature(
        parameters  = args[0],
        return_type = args[1])

fun_signature = (
    op_('(') + maybe(signature_param_list) + op_(')') +
    op_('->') + type_signature
    >> make_fun_signature)

def make_tuple_signature(args):
    return TupleSignature(parameters = args)

tuple_signature = (
    op_('(') + signature_param_list + op_(')')
    >> make_tuple_signature)

def make_specialization_argument(args):
    return SpecializationArgument(
        name  = args[0],
        value = args[1])

specialization_argument = (
    name + op_('=') + type_signature
    >> make_specialization_argument)

specialization_argument_list = (
    specialization_argument + many(op_(',') + specialization_argument)
    >> flatten)

def make_identifier(args):
    return Identifier(
        name            = args[0],
        specializations = args[1])

# FIXME Because of the way the lexer parses operator tokens, it will read `>>`
# in the string `A<T = A<T = B>>` as a right shift operator. To avoid this
# issue, one has to add a space between any consecutive `<` or `>` that
# shouldn't be parsed as a shift operator (e.g. `A<T = A<T = B> >`).
identifier = (
    (name | operator_name) +
    maybe(op_('<') + specialization_argument_list + op_('>'))
    >> make_identifier)

type_name.define(identifier)

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

array_literal_item_list = (
    expr + many(op_(',') + expr) + maybe(op_(','))
    >> flatten)

array_literal = (
    op_('[') + maybe(array_literal_item_list) + op_(']')
    >> make_array_literal)

def make_dict_literal_item(args):
    return DictionaryLiteralItem(
        key   = args[0],
        value = args[1])

dict_literal_item = (
    expr + op_(':') + expr
    >> make_dict_literal_item)

dict_literal_item_list = (
    dict_literal_item + many(op_(',') + dict_literal_item) + maybe(op_(','))
    >> flatten)

def make_dict_literal(args):
    return DictionaryLiteral(items = args if isinstance(args, list) else None)

dict_literal = (
    op_('[') + (dict_literal_item_list | op(':')) + op_(']')
    >> make_dict_literal)

literal = dict_literal | array_literal | string_literal | number_literal

def make_tuple_item(args):
    if args[0].value == 'mut':
        attributes = {'mutable'}
    elif args[0].value == 'shd':
        attributes = {'mutable'}
    else:
        attributes = set()

    return TupleItemDecl(
        attributes      = attributes,
        label           = args[1] if (args[1] != '_') else None,
        type_annotation = args[2],
        initializer     = args[3])

tuple_item = (
    mutability_modifier + name +
    maybe(op_(':') + type_signature) +
    initializer
    >> make_tuple_item)

tuple_item_list = (
    tuple_item + many(op_(',') + tuple_item)
    >> flatten)

def make_tuple_expr(args):
    return Tuple(items = args)

tuple_expr = (
    op_('(') + tuple_item_list + op_(')')
    >> make_tuple_expr)

def make_value_binding_pattern(args):
    if args[0].value == 'mut':
        attributes = {'mutable'}
    elif args[0].value == 'shd':
        attributes = {'mutable'}
    else:
        attributes = set()

    return ValueBindingPattern(
        attributes      = attributes,
        name            = args[1],
        type_annotation = args[2])

value_binding_pattern = (
    mutability_modifier + name +
    maybe(op_(':') + type_signature)
    >> make_value_binding_pattern)

def make_closure(args):
    return Closure(
        parameters = args[0],
        statements = args[1])

closure_param_list = (
    value_binding_pattern + many(op_(',') + value_binding_pattern) + kw_('in')
    >> flatten)

closure_expr = (
    op_('{') + maybe(closure_param_list) + stmt_list + op_('}')
    >> make_closure)

primary = tuple_expr | closure_expr | literal | identifier | op_('(') + expr + op_(')')

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

    # We receive a tuple (operand, [(operator, operand)]) from the parser.
    # Would the parsed expression be `0 + 1 - 2`, `args` would be equivalent
    # to (Literal(0), [('+', Literal(1)), ('-', Literal(2))]).

    if args[1]:
        return BinaryExpression(
            left     = args[0],
            operator = args[1][0][0],
            right    = make_binary_expr((args[1][0][1], args[1][1:])))

    # Note that `make_binary_expr` will be called for the last operand,
    # even if there isn't any binary expression left to create. Hence we can
    # simply return the operand "as is".
    return args[0]

expr_10  = prefix_expr | (op_('(') + expr + op_(')'))
expr_09  = expr_10 + many(shf_op  + expr_10) >> make_binary_expr
expr_08  = expr_09 + many(mul_op  + expr_09) >> make_binary_expr
expr_07  = expr_08 + many(add_op  + expr_08) >> make_binary_expr
expr_06  = expr_07 + many(cmp_op  + expr_07) >> make_binary_expr
expr_05  = expr_06 + many(cast_op + expr_06) >> make_binary_expr
expr_04  = expr_05 + many(eq_op   + expr_05) >> make_binary_expr
expr_03  = expr_04 + many(and_op  + expr_04) >> make_binary_expr
expr_02  = expr_03 + many(xor_op  + expr_03) >> make_binary_expr
expr_01  = expr_02 + many(or_op   + expr_02) >> make_binary_expr
expr_00  = expr_01 + many(land_op + expr_01) >> make_binary_expr
bin_expr = expr_00 + many(lor_op  + expr_00) >> make_binary_expr

expr.define(if_expr | switch_expr | bin_expr)

def make_call_arg(args):
    attributes = set()
    for attr in args[1]:
        if attr.value == 'mut':
            attributes.add('mutable')
        elif attr.value == 'shd':
            attributes.add('shared')
        else:
            raise SyntaxError("invalid attribute '{}' on call argument".format(attr))

    return CallArgument(
        label      = args[0],
        attributes = attributes,
        value      = args[2])

call_arg = (
    maybe(name + op_('=')) + many(attribute) + expr
    >> make_call_arg)

call_arg_list = (
    call_arg + many(op_(',') + call_arg)
    >> flatten)

def make_call_expr(args):
    return Call(callee = args[0], arguments = args[1])

# FIXME Because of left recursion, parsing the owner as a postfix expression
# would trigger an infinite recursion. Hence, we restrict it to "select" and
# "primary" expressions. The former is the most current case we'll encounter
# (after the single identifier), and the latter will break the left recursion
# since its production rule requires expressions to be enclosed within
# parenthesis.
call_callee = (
    maybe(prefix_op) + (select_expr | primary)
    >> make_prefix_expr)

call_expr = (
    call_callee + op_('(') + maybe(call_arg_list) + op_(')')
    >> make_call_expr)

def make_subscript(args):
    return Subscript(callee = args[0], arguments = args[1])

# FIXME Because of left recursion, parsing the owner as a postfix expression
# would trigger an infinite recursion. Hence, we restrict it to "select" and
# "primary" expressions. The former is the most current case we'll encounter
# (after the single identifier), and the latter will break the left recursion
# since its production rule requires expressions to be enclosed within
# parenthesis.
subscript_callee = (
    maybe(prefix_op) + (select_expr | primary)
    >> make_prefix_expr)

subscript_expr = (
    subscript_callee + op_('[') + call_arg_list + op_(']')
    >> make_subscript)

def make_implicit_select_expr(args):
    return ImplicitSelect(member = args)

implicit_select_expr = (
    op_('.') + identifier
    >> make_implicit_select_expr)

def make_explicit_select_expr(args):
    return Select(
        owner  = args[0],
        member = args[1])

# FIXME Because of left recursion, parsing the owner as a postfix expression
# would trigger an infinite recursion. Hence, we restrict it to "primary"
# expressions, which will break the left recursion since its production rule
# requires expressions to be enclosed within parenthesis.
explicit_select_owner = (
    maybe(prefix_op) + primary
    >> make_prefix_expr)

explicit_select_expr = (
    explicit_select_owner + op_('.') + identifier
    >> make_explicit_select_expr)

select_expr.define(explicit_select_expr | implicit_select_expr)

type_signature.define(fun_signature | tuple_signature | select_expr | type_name)

operand.define(call_expr | subscript_expr | select_expr | primary)

wildcard_pattern = kw_('_') >> (lambda _: WildcardPattern())

def make_pattern_arg(args):
    return PatternArgument(
        label = args[0],
        value = args[1])

pattern_arg = (
    maybe(name + op_('=')) + pattern
    >> make_pattern_arg)

pattern_arg_list = (
    pattern_arg + many(op_(',') + pattern_arg)
    >> flatten)

def make_tuple_pattern(args):
    return TuplePattern(items = args)

tuple_pattern = (
    op_('(') + pattern_arg_list + op_(')')
    >> make_tuple_pattern)

def make_enum_case_pattern(args):
    return EnumCasePattern(
        case      = args[0],
        arguments = args[1])

enum_case_pattern = (
    select_expr +
    op_('(') + pattern_arg_list + op_(')')
    >> make_enum_case_pattern)

def make_pattern(args):
    return Pattern(
        expression   = args[0],
        where_clause = args[1])

pattern.define(
    (wildcard_pattern | value_binding_pattern | tuple_pattern | enum_case_pattern | expr) +
    maybe(where_clause)
    >> make_pattern)

def make_matching_pattern(args):
    return MatchingPattern(
        value        = args[0],
        pattern      = args[1])

matching_pattern = (
    expr + op_('~=') + pattern
    >> make_matching_pattern)

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

def make_switch_expr(args):
    return Switch(
        expression = args[0],
        clauses    = args[1])

switch_expr.define(
    kw_('switch') + expr + op_('{') + many(switch_case_clause) + op_('}')
    >> make_switch_expr)

def make_assignment(args):
    return Assignment(
        lvalue = args[0],
        rvalue = args[1])

assignment = (
    pattern + op_('=') + expr
    >> make_assignment)

def make_self_assignement(args):
    return SelfAssignement(
        lvalue   = args[0],
        operator = args[1],
        rvalue   = args[2])

self_assignment = (
    expr + self_assignment_op + expr
    >> make_self_assignement)

def make_return_stmt(args):
    return Return(value = args)

return_stmt = (
    kw_('return') + expr
    >> make_return_stmt)

def make_break_stmt(args):
    return Break(label = args)

break_stmt = (
    kw_('break') + maybe(name)
    >> make_break_stmt)

def make_continue_stmt(args):
    return Continue(label = args)

continue_stmt = (
    kw_('continue') + maybe(name)
    >> make_continue_stmt)

def make_for_loop(args):
    return For(
        label    = args[0],
        iterator = args[1],
        sequence = args[2],
        body     = args[3])

    # TODO Reject invalid patterns

for_loop = (
    maybe(name + op_(':')) +
    kw_('for') + pattern + kw_('in') + expr + block
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

stmt.define(
    prop_decl       |
    fun_decl        |
    struct_decl     |
    enum_decl       |
    protocol_decl   |
    extension_decl  |
    call_expr       |
    if_expr         |
    switch_expr     |
    assignment      |
    self_assignment |
    for_loop        |
    while_loop      |
    return_stmt     |
    break_stmt      |
    continue_stmt   )

parser = module_decl + skip(finished)

def parse(s):
    return parser.parse(tokenize(s))
