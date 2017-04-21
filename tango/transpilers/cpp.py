from tango.ast import *
from tango.builtin import Int, Double, String
from tango.types import NominalType


def transpile(module, header_stream, source_stream):
    transpiler = Transpiler(header_stream, source_stream)
    transpiler.visit(module)


def translate_type(type_instance):
    if isinstance(type_instance, NominalType):
        return compatibilize(type_instance.scope.name + '_' + type_instance.name)

    assert False, 'cannot translate %s' % type_instance


def translate_op(operator):
    if operator in ('+', '-', '*', '/'):
        return operator
    assert False, 'cannot translate %s' % operator


def translate_expr(node):
    if isinstance(node, Literal):
        if node.__info__['type'] == String:
            return '"' + node.value + '"'
        return node.value

    if isinstance(node, Identifier):
        return compatibilize(node.__info__['scope'].name + '_' + node.name)

    if isinstance(node, BinaryExpression):
        return '{} {} {}'.format(
            translate_expr(node.left),
            translate_op(node.operator),
            translate_expr(node.right))

    assert False, 'cannot translate %s' % node


def compatibilize(name):
    result = str(str(name.encode())[2:-1]).replace('\\', '')
    result = name.replace('.', '_')
    if result[0].isdigit():
        result = '_' + result

    return result


class Transpiler(Visitor):

    def __init__(self, header_stream, source_stream):
        self.header_stream = header_stream
        self.source_stream = source_stream

        self.indent = 0

        self.write_source('#include "tango.hh"')
        self.write_source('')

        self.containers = {}
        self.functions = {}
        self.types = {}

    def write_header(self, data):
        print(' ' * self.indent + data, file=self.header_stream)

    def write_source(self, data):
        print(' ' * self.indent + data, file=self.source_stream)

    def visit_ContainerDecl(self, node):
        # Write a new variable declaration.
        var_type = translate_type(node.__info__['type'])
        var_name = compatibilize(node.__info__['scope'].name + '_' + node.name)
        declaration = var_type + ' ' + var_name

        # If the container's has an initial value, write it as well.
        if node.initial_value:
            declaration += ' = ' + translate_expr(node.initial_value)

        self.write_source(declaration + ';')
