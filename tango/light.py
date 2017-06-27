import os

from lark import Lark, Transformer

from . import ast
from .builtin import Int, Double, String


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
        return ast.PropDecl(
            mutability      = _mutability_modifier(items[0]),
            name            = items[1].value,
            type_annotation = items[2],
            meta            = {
                'start': (items[0].line, items[0].column),
                'end'  : items[2].__meta__['end']
            })

    def fun_decl(self, items):
        return ast.FunctionDecl(
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
            mutability      = items[0].value,
            name            = items[1].value,
            type_annotation = items[2],
            meta            = {
                'start': (items[0].line, items[0].column),
                'end'  : items[2].__meta__['end']
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
        modifier = ast.TypeModifier.tm_none
        if len(items) > 1:
            signature = items[1]
            modifier  = (
                ast.TypeModifier.tm_ref if items[0].children[0] == '&' else
                ast.TypeModifier.tm_own if items[0].children[0] == '!' else
                ast.TypeModifier.tm_none)
            start     = (items[0].children[0].line, items[0].children[0].column)
            end       = items[1].__meta__['end']
        else:
            signature = items[0]
            modifier  = ast.TypeModifier.tm_none
            start     = items[0].__meta__['start']
            end       = items[0].__meta__['end']

        return ast.TypeIdentifier(
            signature = signature,
            modifier  = modifier,
            meta      = {
                'start': start,
                'end'  : end
            })

    def ident(self, items):
        return ast.Identifier(
            name = items[0].value,
            meta = {
                'start': (items[0].line, items[0].column),
                'end'  : (items[0].line, items[0].column + len(items[0].value))
            })

    def literal(self, items):
        value = items[0].value
        if items[0].type == 'NUMBER':
            if ('.' in value) or ('e' in value) or ('E' in value):
                node_class = ast.DoubleLiteral
            else:
                node_class = ast.IntLiteral
        elif items[0].type == 'STRING':
            node_class = ast.StringLiteral
            value = value[1:-1]
        else:
            assert False, 'unknown literal type: {}'.format(items[0].type)

        return node_class(
            value = value,
            meta  = {
                'start': (items[0].line, items[0].column),
                'end'  : (items[0].line, items[0].column + len(items[0].value)),
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
