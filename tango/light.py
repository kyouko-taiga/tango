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
            body = ast.Block(statements=items[0]))

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
        return ast.PropertyDecl(
            mutability      = items[0].value,
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
            type = Double if ('.' in value) or ('e' in value) or ('E' in value) else Int
        elif items[0].type == 'STRING':
            value = value[1:-1]
            type  = String

        return ast.Literal(
            value = value,
            meta  = {
                'start': (items[0].line, items[0].column),
                'end'  : (items[0].line, items[0].column + len(items[0].value)),
                'type' : type
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
