import hashlib

from tango.ast import *
from tango.builtin import Int, Double, String
from tango.types import FunctionType, NominalType, TypeUnion


def transpile(module, header_stream, source_stream):
    transpiler = Transpiler(header_stream, source_stream)
    transpiler.visit(module)


def compatibilize(name):
    result = str(str(name.encode())[2:-1]).replace('\\', '')
    for punct in '. ()[]<>-:':
        result = result.replace(punct, '')

    if result[0].isdigit():
        result = '_' + result

    return result


operator_translations = {
    '+': '__add__',
    '-': '__sub__',
    '*': '__mul__',
    '/': '__div__',
}


class Functor(object):

    def __init__(self, function_type):
        self.function_type = function_type

    @property
    def type_signature(self):
        # FIXME This discriminator isn't good enough, as different signatures
        # may have the same string representation, since their `__str__`
        # implementation doesn't use full names.
        discriminator = hashlib.sha1(str(self.function_type).encode()).hexdigest()[-8:]
        return compatibilize('Sig' + str(self.function_type) + discriminator)


class Transpiler(Visitor):

    def __init__(self, header_stream, source_stream):
        self.header_stream = header_stream
        self.source_stream = source_stream

        self.indent = 0

        self.containers = {}
        self.functions = {}
        self.functors = {}
        self.types = {}

    def write_header(self, data, end='\n'):
        print(' ' * self.indent + data, file=self.header_stream, end=end)

    def write_source(self, data, end='\n'):
        print(' ' * self.indent + data, file=self.source_stream, end=end)

    def visit_ModuleDecl(self, node):
        self.write_source('#include "tango.hh"')
        self.write_source('')
        self.write_source('int main(int argc, char* argv[]) {')
        self.indent += 4

        self.generic_visit(node)

        self.write_source('return 0;')
        self.indent -= 4
        self.write_source('}')

    def visit_ContainerDecl(self, node):
        # Write a new variable declaration.
        var_type = self.translate_type(node.__info__['type'])
        var_name = compatibilize(node.__info__['scope'].name + '_' + node.name)
        declaration = var_type + ' ' + var_name

        # If the container's has an initial value, write it as well.
        if node.initial_value:
            declaration += ' = ' + self.translate_expr(node.initial_value)

        self.write_source(declaration + ';')

    def visit_Call(self, node):
        self.write_source(self.translate_expr(node) + ';')

    def visit_If(self, node):
        assert not node.pattern.parameters, 'TODO pattern matching in if expressions'
        condition = self.translate_expr(node.pattern.expression)
        self.write_source('if (' + condition + ') {')
        self.indent += 4
        self.visit(node.body)
        self.indent -= 4
        self.write_source('}')

        if isinstance(node.else_clause, Block):
            self.write_source('else {')
            self.indent += 4
            self.visit(node.else_clause)
            self.indent -= 4
            self.write_source('}')

        elif isinstance(node.else_clause, If):
            self.write_source('else')
            self.visit(node.else_clause)

    def translate_type(self, type_instance):
        if isinstance(type_instance, NominalType):
            return compatibilize(type_instance.scope.name + '_' + type_instance.name)

        if isinstance(type_instance, FunctionType):
            # Register a new functor for the parsed function type.
            functor = self.functors.get(type_instance)
            if functor is None:
                functor = Functor(type_instance)
                self.functors[type_instance] = functor

            return 'std::shared_ptr<' + functor.type_signature + '>'

        assert False, 'cannot translate {}'.format(type_instance)


    def translate_expr(self, node):
        if isinstance(node, Literal):
            if node.__info__['type'] == String:
                return '"' + node.value + '"'
            return node.value

        if isinstance(node, Identifier):
            # If the identifier is `true` or `false`, we write it as is.
            if node.name in ['true', 'false']:
                return node.name

            # If the identifier isn't a keyword, first, we retrive the entity
            # the identifier is denoting.
            decls = node.__info__['scope'][node.name]

            # If the identifier denotes a simple container, we return its full
            # name (i.e. scope + name).
            if isinstance(decls[0], ContainerDecl):
                return compatibilize(node.__info__['scope'].name + '_' + node.name)

            # If the identifier denotes a function declaration, we have to
            # know which overload and/or specialization it refers to, so as to
            # create a different full name for each case.
            if isinstance(decls[0], FunctionDecl):
                # If the identifier has a single type non generic type, we can
                # use it as is to discriminate the identifier.
                node_type = node.__info__['type']
                if not isinstance(node_type, TypeUnion) and not node_type.is_generic:
                    discriminating_type = node_type

                # If the identifier was used as the callee of a function call,
                # we can expect the type solver to add a `specialized_type`
                # key in the node's metadata.
                elif 'specialized_type' in node.__info__:
                    discriminating_type = node.__info__['specialized_type']

                # It should be illegal to use an overloaded or generic
                # identifier outside of a function call.
                else:
                    assert False, (
                        "ambiguous use of '{}' wasn't handled by the type disambiguator"
                        .format(node))

                # FIXME This discriminator isn't good enough, as different
                # signatures may have the same string representation, since
                # their `__str__` implementation doesn't use full names.
                discriminator = hashlib.sha1(str(discriminating_type).encode()).hexdigest()[-8:]
                return compatibilize(node.__info__['scope'].name + '_' + node.name + discriminator)

        if isinstance(node, PrefixedExpression):
            return '{}.{}({})'.format(
                self.translate_type(node.operand.__info__['type']),
                operator_translations[node.operator],
                self.translate_expr(node.operand))

        if isinstance(node, BinaryExpression):
            return '{}.{}({}, {})'.format(
                self.translate_type(node.left.__info__['type']),
                operator_translations[node.operator],
                self.translate_expr(node.left),
                self.translate_expr(node.right))

        if isinstance(node, Call):
            callee_name = self.translate_expr(node.callee)
            return '(*({}))({})'.format(
                callee_name,
                ', '.join(map(self.translate_expr, node.arguments)))

        if isinstance(node, CallArgument):
            return self.translate_expr(node.value)

        assert False, 'cannot translate {}'.format(node)


def find_function_implementation(node):
    scope = node.callee.__info__['scope']
    while scope is not None:
        for decl in node.callee.__info__['scope'][node.callee.name]:
            # When the object denoted by the identifier is a declaration, it
            # means we have to instantiate that declaration.
            if isinstance(decl, FunctionDecl):
                function_type = decl.__info__['type']

                # We select the first non-generic function declaration that
                # that matches the signature candidate of the call node.
                if function_type == node.__info__['signature_candidate']:
                    return decl

                assert not function_type.is_generic, 'TODO: {} is generic'.format(function_type)

            # When the object denoted by the identifier is a type, it means
            # it's been declared in another module. Hence, we should refer to
            # the symbol of this other module.
            else:
                assert False, 'TODO: {} is declared in another module'.format(node.callee)

        # Move to the enclosing scope if we couldn't find any match.
        scope = scope.parent

    # We should always find at least one valid implementation, unless
    # something went wrong with the type solver.
    assert False, 'could not find the implementation of {}'.format(node.callee)
