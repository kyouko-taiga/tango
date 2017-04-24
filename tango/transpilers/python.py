import hashlib

from tango.ast import *
from tango.builtin import Int, Double, String
from tango.scope import Scope
from tango.types import FunctionType, NominalType, TypeUnion


indent = '    '


def transpile(module, source_stream):
    # 1st step: AST transformation
    # In a first step, we have to transform the AST to ease the translation.
    # We'll make all function calls static, and remove constructions that
    # can't be easily translated into python easily (e.g. if expressions as
    # rvalues).
    preparator = Preparator()
    module = preparator.visit(module)

    # 3rd step: Transpilation
    # Now we can translate the AST to python code.
    translator = Translator(source_stream)
    translator.visit(module)


operator_translations = {
    '+' : '__add__',
    '-' : '__sub__',
    '*' : '__mul__',
    '/' : '__div__',
    '<' : '__lt__',
    '<=': '__le__',
    '==': '__eq__',
    '!=': '__ne__',
    '>=': '__ge__',
    '>' : '__gt__',
}


class Preparator(Transformer):

    class TypeNode(Node):
        # A dummy AST node whose sole purpose is to specify type information for
        # some internally created nodes.

        def __init__(self, type):
            super().__init__()
            self.__info__['type'] = type

    def __init__(self):
        self.scope = Scope(name='__tango__')

    def visit_PrefixedExpression(self, node):
        # Look for the method that should be applied.
        operand_type = node.operand.__info__['type']
        assert isinstance(operand_type, NominalType)

        callee = Select(
            owner  = Preparator.TypeNode(type=operand_type),
            member = Identifier(name=node.operator))

        callee.__info__['type'] = node.__info__['type']
        callee.member.__info__['scope'] = operand_type.inner_scope
        print(operand_type.inner_scope.name)
        exit()

        # Replace the current node with a call to the operator's function.
        call = Call(
            callee    = Identifier(name=str(operand_type) + operator_translations[node.operator]),
            arguments = [CallArgument(node.operand)])

        call.callee.__info__['type'] = operand_type.members[node.operator]
        call.callee.__info__['scope'] = self.scope

        call.__info__['type'] = node.__info__['type']
        call.__info__['function_type'] = node.__info__['function_type']

        return self.visit(call)

    def visit_BinaryExpression(self, node):
        # Look for the method that should be applied.
        left_type = node.left.__info__['type']

        # Replace the current node with a call to the operator's function.
        call = Call(
            callee    = Identifier(name=str(left_type) + operator_translations[node.operator]),
            arguments = [CallArgument(node.left), CallArgument(node.right)])

        call.callee.__info__['type'] = left_type.members[node.operator]
        call.callee.__info__['scope'] = self.scope

        call.__info__['type'] = node.__info__['type']
        call.__info__['function_type'] = node.__info__['function_type']

        return self.visit(call)

    def visit_Call(self, node):
        # We choose the most specialized function among the overloads of the
        # operand's type.
        specialized_function_type = node.__info__['function_type']
        actual_function_type = None
        if not isinstance(node.callee.__info__['type'], TypeUnion):
            actual_function_type = node.callee.__info__['type']
        else:
            for candidate in node.callee.__info__['type']:
                if candidate == specialized_function_type:
                    actual_function_type = candidate
                    break
                if candidate.is_compatible_with(specialized_function_type):
                    actual_function_type = candidate

        # We can expect the type solver to heve detected if there wasn't any
        # compatible candidate.
        assert actual_function_type is not None
        # node.__info__['function_type'] = actual_function_type

        # Visit the node's arguments.
        node = self.generic_visit(node)
        return node


def compatibilize(name):
    result = 'v' + str(str(name.encode())[2:-1]).replace('\\', '')
    for punct in '. ()[]<>-:':
        result = result.replace(punct, '')
    return result


def signature_hash(signature):
    # FIXME There's a risk of collision using string representations. Because
    # the `__str__` implementation of types doesn't use full names, different
    # signatures may have the same representations.
    return hashlib.sha1(str(signature).encode()).hexdigest()[-8:]


def identifier_name(node):
    name = node.__info__['scope'].name + node.name

    # We have to add a discriminator suffix to identifiers that represent
    # functions, as they might be overloaded.
    if isinstance(node.__info__['type'], FunctionType):
        name += signature_hash(node.__info__['type'])

    return compatibilize(name)


class Translator(Visitor):

    def __init__(self, source_stream):
        self.source_stream = source_stream

        self.level = 0

        self.containers = {}
        self.functions = {}
        self.functors = {}
        self.types = {}

    def write(self, data, end='\n', level=None):
        level = level or self.level
        print(indent * self.level + data, file=self.source_stream, end=end)

    def visit_ModuleDecl(self, node):
        self.write('import tango.py')
        self.write('')
        self.generic_visit(node)

    def visit_ContainerDecl(self, node):
        # Write a new variable declaration.
        declaration = identifier_name(node)

        # If the container's has an initial value, write it as well.
        if node.initial_value:
            declaration += ' = ' + self.translate_expr(node.initial_value)

        self.write(declaration)

    def visit_FunctionDecl(self, node):
        assert isinstance(node.__info__['type'], FunctionType), (
            "FunctionDecl doesn't have function type")

        name = identifier_name(node)
        parameters = []
        for parameter in node.signature.parameters:
            parameter_string = identifier_name(parameter)
            if parameter.default_value:
                parameter_string += '=' + self.translate_expr(parameter.default_value)
            parameters.append(parameter_string)

        self.write('def {}({}):'.format(name, ', '.join(parameters)))
        self.level += 1
        self.generic_visit(node)
        self.level -= 1
        self.write('', level=0)

    def visit_Return(self, node):
        self.write('return {}'.format(self.translate_expr(node.value)))

    # def visit_Call(self, node):
    #     self.write_source(self.translate_expr(node) + ';')
    #
    # def visit_If(self, node):
    #     assert not node.pattern.parameters, 'TODO pattern matching in if expressions'
    #     condition = self.translate_expr(node.pattern.expression)
    #     self.write_source('if (' + condition + ') {')
    #     self.indent += 4
    #     self.visit(node.body)
    #     self.indent -= 4
    #     self.write_source('}')
    #
    #     if isinstance(node.else_clause, Block):
    #         self.write_source('else {')
    #         self.indent += 4
    #         self.visit(node.else_clause)
    #         self.indent -= 4
    #         self.write_source('}')
    #
    #     elif isinstance(node.else_clause, If):
    #         self.write_source('else')
    #         self.visit(node.else_clause)

    def translate_expr(self, node):
        if isinstance(node, Literal):
            if node.__info__['type'] == String:
                return '"' + node.value + '"'
            return node.value

        if isinstance(node, Identifier):
            # If the identifier is `true` or `false`, we write it as is.
            if node.name == 'true':
                return 'True'
            elif node.name == 'false':
                return 'False'

            # If the identifier isn't a keyword, first, we retrive the entity
            # the identifier is denoting.
            decls = node.__info__['scope'][node.name]

            # If the identifier denotes a simple container, we return its full
            # name (i.e. scope + name).
            if isinstance(decls[0], (ContainerDecl, FunctionParameter)):
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

        if isinstance(node, Call):
            return '{}({})'.format(
                self.translate_expr(node.callee),
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
