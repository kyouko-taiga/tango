import os

from lark import Lark, Transformer
from lark.lexer import Token
from lark.tree import Tree, Transformer_NoRecurse

from . import ast
from .errors import CompilerError, UnknownModifierError
from .builtin import Int, Double, String, Bool
from .types import TypeModifier as TM


operator_table = {
    '+'  : ast.Operator.add,
    '-'  : ast.Operator.sub,
    '*'  : ast.Operator.mul,
    '/'  : ast.Operator.div,
    '='  : ast.Operator.cpy,
    '&-' : ast.Operator.ref,
    '<-' : ast.Operator.mov,
}


grammar_filename = os.path.join(os.path.dirname(__file__), 'light.g')
with open(grammar_filename) as f:
    parser = Lark(f, start='module', parser='lalr', propagate_positions=True)


class ParseTreeTransformer(Transformer_NoRecurse):

    def __init__(self, filename):
        self.filename = filename

    def module(self, tree):
        return ast.ModuleDecl(
            body = ast.Block(
                statements = tree.children,
                meta       = {
                    'start': (tree.line, tree.column),
                    'end'  : (tree.end_line, tree.end_col)
                }),
            name = '',
            meta = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def block(self, tree):
        return ast.Block(
            statements = tree.children,
            meta       = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col)
            })

    def fun_decl(self, tree):
        index = 1
        if isinstance(tree.children[index], list) and isinstance(tree.children[index][0], str):
            placeholders = tree.children[index]
            index += 1
        else:
            placeholders = None

        # TODO: Make sure placeholders don't appear more than once.

        if isinstance(tree.children[index], list):
            parameters = tree.children[index]
            index += 1
        else:
            parameters = None

        if isinstance(tree.children[index], ast.TypeIdentifier):
            codomain_annotation = tree.children[index]
            index += 1
        else:
            codomain_annotation = None

        return ast.FunDecl(
            name                = tree.children[0].value,
            placeholders        = placeholders,
            parameters          = parameters,
            codomain_annotation = codomain_annotation,
            body                = tree.children[index],
            meta                = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def placeholders(self, tree):
        return [token.value for token in tree.children]

    def param_decls(self, tree):
        return tree.children

    def param_decl(self, tree):
        index = 1
        if isinstance(tree.children[1], Token) and (tree.children[1].type == 'NAME'):
            label = tree.children[0].value
            name  = tree.children[1].value
            index += 1
        else:
            label = tree.children[0].value
            name  = label

        if isinstance(tree.children[index], ast.TypeIdentifier):
            type_annotation = tree.children[index]
            index += 1
        else:
            type_annotation = None

        return ast.ParamDecl(
            name            = name,
            label           = label,
            type_annotation = type_annotation,
            meta            = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

        # TODO: Handle default values.

    def prop_decl(self, tree):
        index = 1
        if (index < len(tree.children)) and isinstance(tree.children[index], ast.TypeIdentifier):
            type_annotation = tree.children[index]
            index += 1
        else:
            type_annotation = None

        if (index < len(tree.children)) and isinstance(tree.children[index], Token):
            initial_binding = operator_table[tree.children[index].value]
            initial_value   = tree.children[index + 1]
            index += 1
        else:
            initial_binding = ast.Operator.cpy
            initial_value   = None
            index += 2

        # TODO: Handle descriptors.

        return ast.PropDecl(
            name            = tree.children[0].value,
            type_annotation = type_annotation,
            initial_binding = initial_binding,
            initial_value   = initial_value,
            meta            = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def struct_decl(self, tree):
        index = 1
        if isinstance(tree.children[index], list) and isinstance(tree.children[index][0], str):
            placeholders = tree.children[index]
            index += 1
        else:
            placeholders = None

        # TODO: Make sure placeholders don't appear more than once.

        if isinstance(tree.children[index], list) and isinstance(tree.children[index][0], str):
            conformances = tree.children[index]
            index += 1
        else:
            conformances = None

        # TODO: Make sure conformances don't appear more than once.
        # TODO: Check for unexpected statements.
        # TODO: Disallow struct declarations for builtin types (e.g. Int).

        return ast.StructDecl(
            name         = tree.children[0].value,
            placeholders = placeholders,
            conformances = conformances,
            body         = tree.children[index],
            meta         = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def assign_stmt(self, tree):
        return ast.Assignment(
            lvalue   = tree.children[0],
            operator = operator_table[tree.children[1].value],
            rvalue   = tree.children[2],
            meta     = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def return_stmt(self, tree):
        return ast.Return(
            value = tree.children[0],
            meta  = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

        # TODO: Handle return labels.

    def binary_expr(self, tree):
        left = tree.children.pop(0)
        while tree.children:
            op    = operator_table[tree.children.pop(0).value]
            right = tree.children.pop(0)

            left = ast.BinaryExpr(
                left     = left,
                operator = op,
                right    = right,
                meta  = {
                    'start': (tree.line, tree.column),
                    'end'  : right.__meta__['end'],
                })

        return left


    def prefix_expr(self, tree):
        return ast.PrefixExpression(
            operator = operator_table[tree.children[0].value],
            operand  = tree.children[1],
            meta     = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def call_expr(self, tree):
        return ast.Call(
            callee    = tree.children[0],
            arguments = tree.children[1] if len(tree.children) > 1 else None,
            meta      = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def select_expr(self, tree):
        return ast.Select(
            owner  = tree.children[0],
            member = tree.children[1],
            meta   = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def arguments(self, tree):
        return tree.children

    def argument(self, tree):
        return ast.Argument(
            label    = tree.children[0].value,
            operator = operator_table[tree.children[1].value],
            value    = tree.children[2],
            meta     = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def if_expr(self, tree):
        return ast.If(
            condition  = tree.children[0],
            then_block = tree.children[1],
            else_block = tree.children[2] if len(tree.children) > 2 else None,
            meta       = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def type_ident(self, tree):
        signature = tree.children[-1] if not isinstance(tree.children[-1], TM) else None
        modifiers = 0
        for tm in tree.children:
            if not isinstance(tm, TM):
                break
            modifiers |= tm

        # Check type modifiers combinations.
        if (modifiers & TM.cst) and (modifiers & TM.mut):
            raise CompilerError("error - '@cst' modifier cannot be used together with '@mut'")
        if (modifiers & TM.cst) and (modifiers & TM.shd):
            raise CompilerError("error - '@cst' modifier cannot be used together with '@shd'")
        if (modifiers & TM.stk) and (modifiers & TM.shd):
            raise CompilerError("error - '@stk' modifier cannot be used together with '@shd'")
        if (modifiers & TM.shd) and (modifiers & TM.ref):
            raise CompilerError("error - '@shd' modifier cannot be used together with '@ref'")
        if (modifiers & TM.val) and (modifiers & TM.ref):
            raise CompilerError("error - '@val' modifier cannot be used together with '@ref'")

        # Set implicit type modifiers.
        if not (modifiers & TM.mut):
            modifiers |= TM.cst if not (modifiers & TM.shd) else TM.mut
        if not (modifiers & TM.shd):
            modifiers |= TM.stk
        if not (modifiers & TM.ref):
            modifiers |= TM.val

        return ast.TypeIdentifier(
            modifiers = modifiers,
            signature = signature,
            meta = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def fun_sign(self, tree):
        index = 0
        if isinstance(tree.children[index], list):
            parameters = tree.children[index]
            index += 1
        else:
            parameters = None

        return ast.FunSign(
            parameters          = parameters,
            codomain_annotation = tree.children[index],
            meta                = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def sign_params(self, tree):
        return tree.children

    def sign_param(self, tree):
        return ast.FunSignParam(
            label           = tree.children[0].value,
            type_annotation = tree.children[1],
            meta            = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

    def type_modifier(self, tree):
        try:
            return getattr(TM, tree.children[0].value)
        except AttributeError:
            raise UnknownModifierError(tree.children[0].value, filename=self.filename)

    def ident(self, tree):
        return ast.Identifier(
            name = tree.children[0].value,
            meta = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
            })

        # TODO: Handle generic specializtions.

    def int_literal(self, tree):
        return ast.IntLiteral(
            value = int(tree.children[0].value),
            meta  = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
                'type' : Int,
            })

    def double_literal(self, tree):
        return ast.DoubleLiteral(
            value = float(tree.children[0].value),
            meta  = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
                'type' : Double,
            })

    def string_literal(self, tree):
        return ast.StringLiteral(
            value = tree.children[0].value[1:-1],
            meta  = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
                'type' : String,
            })

    def true_literal(self, tree):
        return ast.BoolLiteral(
            value = True,
            meta  = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
                'type' : Bool,
            })

    def false_literal(self, tree):
        return ast.BoolLiteral(
            value = False,
            meta  = {
                'start': (tree.line, tree.column),
                'end'  : (tree.end_line, tree.end_col),
                'type' : Bool,
            })


class TangoLightTransformer(Transformer):

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
