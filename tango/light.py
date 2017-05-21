import os

from lark import Lark, Transformer

from . import ast


grammar_filename = os.path.join(os.path.dirname(__file__), 'light.g')
with open(grammar_filename) as f:
    parser = Lark(f, lexer='standard', start='module')


class TangoLightTransformer(Transformer):

    def module(self, items):
        return ast.ModuleDecl(
            body = ast.Block(statements=items[0]))

    def block(self, items):
        return ast.Block(statements=items[0])

    def stmt_list(self, items):
        return list(items)

    def var_decl(self, items):
        return ast.VariableDecl(
            name            = items[0].value,
            type_annotation = items[1])

    def fun_decl(self, items):
        return ast.FunctionDecl(
            name      = items[0].value,
            signature = ast.FunctionSignature(
                domain   = items[1],
                codomain = items[2]),
            body      = items[3])

    def param_decl(self, items):
        return ast.FunctionParameter(
            name            = items[0].value,
            type_annotation = items[1])

    def assign_stmt(self, items):
        return ast.Assign(
            target     = items[0],
            operator   = items[1].children[0].value,
            expression = items[2])

    def if_stmt(self, items):
        return ast.If(
            condition = items[0],
            body      = items[1])

    def return_stmt(self, items):
        return ast.Return(value=items[0])

    def binary_expr(self, items):
        if len(items) == 1:
            return items[0]
        return ast.BinaryExpression(
            left     = items[0],
            operator = items[1].children[0].value,
            right    = self.binary_expr(items[2:]))

    def prefix_expr(self, items):
        if len(items) == 1:
            return items[0]
        return ast.PrefixExpression(
            operator = items[0].children[0].value,
            operand  = items[1])

    def call_expr(self, items):
        return ast.Call(
            callee   = items[0],
            argument = items[1])

    def ident(self, items):
        return items[0].value

    def literal(self, items):
        return ast.Literal(value=items[0])

    def fun_sign(self, items):
        return ast.FunctionSignature(
            domain   = ast.FunctionParameter(
                name            = items[0],
                type_annotation = items[1]),
            codomain = items[1])
