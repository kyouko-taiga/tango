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

        operator_type = operand_type.members[node.operator]
        assert isinstance(operator_type, (FunctionType, TypeUnion))

        # Replace the current node with a call to the operator's function.
        callee = Select(
            owner  = Identifier(name=operand_type.name),
            member = Identifier(name=node.operator))
        callee.__info__['type'] = node.__info__['type']
        callee.owner.__info__['type'] = operand_type
        callee.owner.__info__['scope'] = operand_type.scope
        callee.member.__info__['type'] = operator_type
        callee.member.__info__['scope'] = operand_type.inner_scope

        call = Call(callee=callee, arguments=[CallArgument(node.operand)])
        call.__info__['type'] = node.__info__['type']
        call.__info__['function_call_type'] = node.__info__['function_call_type']

        return self.visit(call)

    def visit_BinaryExpression(self, node):
        # Look for the method that should be applied.
        operand_type = node.left.__info__['type']
        assert isinstance(operand_type, NominalType)

        operator_type = operand_type.members[node.operator]
        assert isinstance(operator_type, (FunctionType, TypeUnion))

        # Replace the current node with a call to the operator's function.
        callee = Select(
            owner  = Identifier(name=operand_type.name),
            member = Identifier(name=node.operator))
        callee.__info__['type'] = node.__info__['type']
        callee.owner.__info__['type'] = operand_type
        callee.owner.__info__['scope'] = operand_type.scope
        callee.member.__info__['type'] = operator_type
        callee.member.__info__['scope'] = operand_type.inner_scope

        call = Call(callee=callee, arguments=[CallArgument(node.left), CallArgument(node.right)])
        call.__info__['type'] = node.__info__['type']
        call.__info__['function_call_type'] = node.__info__['function_call_type']

        return self.visit(call)

    def visit_ContainerDecl(self, node):
        if isinstance(node.__info__['type'], FunctionType):
            node.__info__['discriminator'] = signature_hash(node.__info__['type'])
        return self.generic_visit(node)

    def visit_FunctionDecl(self, node):
        node.__info__['discriminator'] = signature_hash(node.__info__['type'])
        return self.generic_visit(node)

    def visit_Call(self, node):
        discriminator = signature_hash(node.__info__['function_call_type'])
        if isinstance(node.callee, Identifier):
            node.callee.__info__['discriminator'] = discriminator
        elif isinstance(node.callee, Select):
            node.callee.member.__info__['discriminator'] = discriminator
        return self.generic_visit(node)

    def visit_Identifier(self, node):
        if isinstance(node.__info__['type'], FunctionType):
            node.__info__['discriminator'] = signature_hash(node.__info__['type'])
        return self.generic_visit(node)


def compatibilize(name):
    result = name
    for punct in '. ()[]<>-:':
        result = result.replace(punct, '')
    return result


def signature_hash(signature):
    # FIXME There's a risk of collision using string representations. Because
    # the `__str__` implementation of types doesn't use full names, different
    # signatures may have the same representations.
    assert isinstance(signature, FunctionType)
    return hashlib.sha1(str(signature).encode()).hexdigest()[-8:]


def identifier_name(node):
    symbol = node.__info__['scope'][node.name]
    result = compatibilize(symbol.scope.qualified_name + '.' + symbol.name)
    if 'discriminator' in node.__info__:
        result += node.__info__['discriminator']
    return result


def translate_expr(node, function_call_type=None):
    if isinstance(node, Literal):
        if node.__info__['type'] == String:
            return '"' + node.value + '"'
        return node.value

    if isinstance(node, Identifier):
        # If the identifier is the keyword `true` or `false`, we return
        # its python's equivalent.
        if node.name == 'true':
            return 'True'
        elif node.name == 'false':
            return 'False'

        return identifier_name(node)

    if isinstance(node, Select):
        result = translate_expr(node.owner)
        result += operator_translations.get(node.member.name, compatibilize(node.member.name))
        if 'discriminator' in node.member.__info__:
            result += node.member.__info__['discriminator']
        return result

    if isinstance(node, Call):
        # Function name.
        function_call_type = node.__info__['function_call_type']
        result = translate_expr(node.callee, function_call_type)

        # Function arguments.
        result += '('
        for i, argument in enumerate(node.arguments):
            if not 'mutable' in function_call_type.attributes[i]:
                result += 'deepcopy({})'.format(translate_expr(argument))
            else:
                result += translate_expr(argument)
            result += ', '
        return result.rstrip(', ') + ')'

    if isinstance(node, CallArgument):
        return translate_expr(node.value)

    assert False, 'cannot translate {}'.format(node)


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
        self.write('import tango')
        self.write('from copy import deepcopy')
        self.write('')
        self.generic_visit(node)

    def visit_ContainerDecl(self, node):
        # Write a new variable declaration.
        declaration = identifier_name(node)

        # If the container's has an initial value, write it as well.
        if node.initial_value:
            declaration += ' = ' + translate_expr(node.initial_value)

        self.write(declaration)

    def visit_FunctionDecl(self, node):
        assert isinstance(node.__info__['type'], FunctionType), (
            "FunctionDecl doesn't have function type")

        name = identifier_name(node)
        parameters = []
        for parameter in node.signature.parameters:
            parameter_string = identifier_name(parameter)
            if parameter.default_value:
                parameter_string += '=' + translate_expr(parameter.default_value)
            parameters.append(parameter_string)

        self.write('def {}({}):'.format(name, ', '.join(parameters)))
        self.level += 1
        self.generic_visit(node)
        self.level -= 1
        self.write('', level=0)

    def visit_Return(self, node):
        self.write('return {}'.format(translate_expr(node.value)))

    def visit_Call(self, node):
        self.write(translate_expr(node))

    def visit_If(self, node):
        assert not node.pattern.parameters, 'TODO pattern matching in if expressions'
        condition = translate_expr(node.pattern.expression)
        self.write('if ' + condition + ':')
        self.level += 1
        self.visit(node.body)
        self.level -= 1

        if node.else_clause:
            self.write('else:')
            self.level += 1
            self.visit(node.else_clause)
            self.level -= 1


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
