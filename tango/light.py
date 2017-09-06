import os

from lark import Lark, Transformer

from . import ast
from .builtin import Int, Double, String, Nothing
from .types import TypeModifier as TM


operator_table = {
    '+'  : ast.Operator.o_add,
    '-'  : ast.Operator.o_sub,
    '*'  : ast.Operator.o_mul,
    '/'  : ast.Operator.o_div,
    '='  : ast.Operator.o_cpy,
    '&-' : ast.Operator.o_ref,
    '<-' : ast.Operator.o_mov,
}

type_modifier_table = {
    'CST': TM.tm_cst,
    'MUT': TM.tm_mut,
    'STK': TM.tm_stk,
    'SHD': TM.tm_shd,
    'VAL': TM.tm_val,
    'REF': TM.tm_ref,
    'OWN': TM.tm_own,
}


grammar_filename = os.path.join(os.path.dirname(__file__), 'light.g')
with open(grammar_filename) as f:
    parser = Lark(f, lexer='standard', start='module')


class TangoLightTransformer(Transformer):

    def module(self, items):
        return ast.ModuleDecl(
            body = ast.Block(statements=items[0]),
            name = '')

    def block(self, items):
        return ast.Block(
            statements = items[1],
            meta       = {
                'start': (items[0].line, items[0].column),
                'end'  : (items[2].line, items[2].column)
            })

    def stmt_list(self, items):
        return items

    def prop_decl(self, items):
        tail            = items[2:]
        type_annotation = None
        initial_value   = None
        initial_binding = ast.Operator.o_cpy
        end             = 0

        # If the next item after the property's name is an AST node, it means
        # the declation comes with a type annotation.
        if tail and isinstance(tail[0], ast.Node):
            type_annotation = tail[0]
            end             = tail[0].__meta__['end']
            tail.pop(0)

        # If the next (next) item after the property's name starts with an
        # assignment operator, it means the declaration comes with an initial
        # value expression.
        if tail:
            initial_binding = operator_table[tail[0].children[0].value]
            initial_value   = tail[1]
            end             = tail[1].__meta__['end']

        return ast.PropDecl(
            name            = items[1].value,
            type_annotation = type_annotation,
            initial_binding = initial_binding,
            initial_value   = initial_value,
            meta            = {
                'start': (items[0].line, items[0].column),
                'end'  : end
            })

    def fun_decl(self, items):
        parameters          = items[2]  if isinstance(items[2], list) else None
        codomain_annotation = items[-2] if isinstance(items[-2], ast.TypeIdentifier) else None

        return ast.FunDecl(
            name                = items[1].value,
            parameters          = parameters,
            codomain_annotation = codomain_annotation,
            body                = items[-1],
            meta                = {
                'start': (items[0].line, items[0].column),
                'end'  : items[-1].__meta__['end']
            })

    def param_decls(self, items):
        return items

    def param_decl(self, items):
        return ast.ParamDecl(
            name            = items[0].value,
            type_annotation = items[1],
            meta            = {
                'start': (items[0].line, items[0].column),
                'end'  : items[1].__meta__['end']
            })

    def assign_stmt(self, items):
        return ast.Assignment(
            lvalue   = items[0],
            operator = operator_table[items[1].children[0].value],
            rvalue   = items[2],
            meta     = {
                'start': items[0].__meta__['start'],
                'end'  : items[2].__meta__['end']
            })

    def if_stmt(self, items):
        return ast.If(
            condition = items[1],
            body      = items[2],
            meta      = {
                'start': (items[0].line, items[0].column),
                'end'  : items[2].__meta__['end']
            })

    def return_stmt(self, items):
        return ast.Return(
            value = items[1],
            meta  = {
                'start': (items[0].line, items[0].column),
                'end'  : items[1].__meta__['end']
            })

    def binary_expr(self, items):
        if len(items) == 1:
            return items[0]
        return ast.BinaryExpr(
            left     = items[0],
            operator = operator_table[items[1].children[0].value],
            right    = self.binary_expr(items[2:]),
            meta     = {
                'start': items[0].__meta__['start'],
                'end'  : items[2].__meta__['end']
            })

    def prefix_expr(self, items):
        if len(items) == 1:
            return items[0]
        return ast.PrefixExpression(
            operator = items[0].children[0].value,
            operand  = items[1],
            meta     = {
                'start': (items[0].children[0].line, items[0].children[0].column),
                'end'  : items[1].__meta__['end']
            })

    def call_expr(self, items):
        return ast.Call(
            callee    = items[0],
            arguments = items[2] if isinstance(items[2], list) else None,
            meta      = {
                'start': items[0].__meta__['start'],
                'end'  : (items[-1].line, items[-1].column),
            })

    def call_args(self, items):
        return items

    def call_arg(self, items):
        return ast.CallArg(
            label    = items[0].value,
            operator = operator_table[items[1].children[0].value],
            value    = items[2],
            meta     = {
                'start': (items[0].line, items[0].column),
                'end'  : items[2].__meta__['end']
            })

    def type_ident(self, items):
        # Check if there's a type signature.
        if isinstance(items[-1], ast.Node):
            signature = items[-1]

            modifiers = 0
            for it in items[:-1]:
                modifiers |= it[0]

            meta = {
                'start': items[0][1]['start'] if len(items) > 1 else signature.__meta__['start'],
                'end'  : signature.__meta__['end'],
            }

        # Otherwise we only combine modifiers.
        else:
            signature = None
            modifiers = 0
            for it in items:
                modifiers |= it[0]

            meta = {
                'start': items[ 0][1]['start'],
                'end'  : items[-1][1]['end'],
            }

        # Check type modifiers combinations.
        if (modifiers & TM.tm_cst) and (modifiers & TM.tm_mut):
            raise SyntaxError("'@cst' modifier cannot be used together with '@mut'")
        if (modifiers & TM.tm_cst) and (modifiers & TM.tm_shd):
            raise SyntaxError("'@cst' modifier cannot be used together with '@shd'")
        if (modifiers & TM.tm_stk) and (modifiers & TM.tm_shd):
            raise SyntaxError("'@stk' modifier cannot be used together with '@shd'")
        if (modifiers & TM.tm_shd) and (modifiers & TM.tm_ref):
            raise SyntaxError("'@shd' modifier cannot be used together with '@ref'")
        if (modifiers & TM.tm_val) and (modifiers & TM.tm_ref):
            raise SyntaxError("'@val' modifier cannot be used together with '@ref'")

        # Set implicit type modifiers.
        if not (modifiers & TM.tm_mut):
            modifiers |= TM.tm_cst if not (modifiers & TM.tm_shd) else TM.tm_mut
        if not (modifiers & TM.tm_shd):
            modifiers |= TM.tm_stk
        if not (modifiers & TM.tm_ref):
            modifiers |= TM.tm_val

        return ast.TypeIdentifier(
            modifiers = modifiers,
            signature = signature,
            meta      = meta)

    def type_modifier(self, items):
        modifiers = 0
        for token in items:
            modifiers |= type_modifier_table[token.type]
        return (modifiers, {
                'start': (items[0].line,  items[0].column),
                'end':   (items[-1].line, items[-1].column + len(items[-1].value))
            })

    def ident(self, items):
        return ast.Identifier(
            name = items[0].value,
            meta = {
                'start': (items[0].line, items[0].column),
                'end'  : (items[0].line, items[0].column + len(items[0].value))
            })

    def fun_sign(self, items):
        parameters = items[1] if isinstance(items[1], list) else None
        return ast.FunSign(
            parameters          = parameters,
            codomain_annotation = items[-1],
            meta                = {
                'start': (items[0].line, items[0].column),
                'end'  : items[-1].__meta__['end']
            })

    def sign_params(sefl, items):
        return items

    def sign_param(self, items):
        return ast.FunSignParam(
            label           = items[0].value,
            type_annotation = items[1],
            meta = {
                'start': (items[0].line, items[0].column),
                'end'  : items[1].__meta__['end']
            })

    def literal(self, items):
        if items[0].type == 'NUMBER':
            value = items[0].value
            if ('.' in value) or ('e' in value) or ('E' in value):
                value      = float(value)
                node_class = ast.DoubleLiteral
                node_type  = Double
            else:
                value      = int(value)
                node_class = ast.IntLiteral
                node_type  = Int
        elif items[0].type == 'STRING':
            value      = (items[0].value[1:-1])
            node_class = ast.StringLiteral
            node_type  = String
        else:
            assert False, 'unknown literal type: {}'.format(items[0].type)

        return node_class(
            value = value,
            meta  = {
                'start': (items[0].line, items[0].column),
                'end'  : (items[0].line, items[0].column + len(items[0].value)),
                'type' : node_type,
            })
