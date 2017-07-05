import os

from lark import Lark, Transformer

from . import ast
from .builtin import Int, Double, String


operator_table = {
    '=' : ast.Operator.o_cpy,
    '&-': ast.Operator.o_ref,
    '<-': ast.Operator.o_mov,
}

type_modifier_table = {
    'CST': ast.TypeModifier.tm_cst,
    'MUT': ast.TypeModifier.tm_mut,
    'STK': ast.TypeModifier.tm_stk,
    'SHD': ast.TypeModifier.tm_shd,
    'VAL': ast.TypeModifier.tm_val,
    'REF': ast.TypeModifier.tm_ref,
    'OWN': ast.TypeModifier.tm_own,
}

default_type_modifier = ast.TypeModifier.tm_cst | ast.TypeModifier.tm_stk | ast.TypeModifier.tm_val


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
        return ast.FunDecl(
            name        = items[1].value,
            parameter   = items[2],
            return_type = items[3],
            body        = items[4],
            meta        = {
                'start': (items[0].line, items[0].column),
                'end'  : items[4].__meta__['end']
            })

    def param_decl(self, items):
        return ast.FunctionParameter(
            name            = items[0].value,
            type_annotation = items[1],
            meta            = {
                'start': (items[0].line, items[0].column),
                'end'  : items[1].__meta__['end']
            })

    def assign_stmt(self, items):
        return ast.Assignment(
            lvalue   = items[0],
            operator = items[1].children[0].value,
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
        return ast.BinaryExpression(
            left     = items[0],
            operator = items[1].children[0].value,
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
            arguments = [items[2]],
            meta      = {
                'start': items[0].__meta__['start'],
                'end'  : (items[3].line, items[3].column),
            })

    def call_arg(self, items):
        return ast.CallArgument(
            label    = items[0].value,
            operator = items[1].children[0].value,
            value    = items[2],
            meta     = {
                'start': (items[0].line, items[0].column),
                'end'  : items[2].__meta__['end']
            })

    def type_ident(self, items):
        if len(items) > 1:
            modifiers = items[0]
            signature = items[1]
        else:
            modifiers = default_type_modifier
            signature = items[0]

        return ast.TypeIdentifier(
            modifiers = modifiers,
            signature = signature,
            meta      = {
                'start': signature.__meta__['start'],
                'end'  : signature.__meta__['end']
            })

    def type_modifier(self, items):
        modifiers = 0
        for token in items:
            modifiers |= type_modifier_table[token.type]
        return modifiers

    def ident(self, items):
        return ast.Identifier(
            name = items[0].value,
            meta = {
                'start': (items[0].line, items[0].column),
                'end'  : (items[0].line, items[0].column + len(items[0].value))
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

    def fun_sign(self, items):
        return ast.FunctionSignature(
            parameter   = items[1],
            return_type = items[4],
            meta        = {
                'start': (items[0].line, items[0].column),
                'end'  : items[4].__meta__['end']
            })

    def sign_param(self, items):
        return ast.SignatureParameter(
            mutability      = items[0].value,
            label           = items[1].value,
            type_annotation = items[2],
            meta = {
                'start': (items[0].line, items[0].column),
                'end'  : items[2].__meta__['end']
            })


def _mutability_modifier(token):
    if token.type == 'CST':
        return ast.IdentifierMutability.im_cst
    if token.type == 'MUT':
        return IdentifierMutability.im_mut
    assert False, 'unsupported identifier mutability {}'.format(token.type)
