from . import ast
from .types import BuiltinType, PlaceholderType, TypeName, find_placeholders


class Mangler(ast.NodeTransformer):

    def __init__(self):
        self.module_name = None

    def visit_ModuleDecl(self, node):
        self.module_name = node.name
        return self.generic_visit(node)

    def visit_FunDecl(self, node):
        # A function named `main` is a special case that shouldn't be mangled.
        if node.name == 'main':
            return node

        # If the function is not generic, it should be emitted as is, and we
        # only have to mangle its name.
        function_type = node.__meta__['type']
        if not node.__meta__['type'].is_generic:
            node.name = mangle_function_name(self.module_name, node.name, function_type)
            return self.generic_visit(node)

        # If it is generic, we should emit one version for each of its
        # specializations.
        results = []
        for function_type in node.__meta__['symbol'].specializations:
            # Copy the function declaration.
            new_node = ast.FunDecl(**{attr: getattr(node, attr) for attr in node._fields})

            # TODO: We should perform a deep copy here.

            # Update the function declaration to match its specialization.
            new_node.__meta__['type'] = function_type
            new_node.placeholders = []

            # FIXME
            # specializer = Specializer(function_type)
            # new_node = specializer.visit(new_node)

            new_node.name = mangle_function_name(self.module_name, node.name, function_type)

            results.append(new_node)

            # QUESTION: We don't rewrite type annotations here. Instead, we
            # only update their metadata to reflect their specialized type.
            # This means the code we produce here will probably not compile.
            # However, this shouldn't matter for IR generation, because it
            # only relies on the metadata to properly type declarations.
            # Should we transform type annotation anyway to produce compilable
            # code? Is there any reason to do it beside the correctness of
            # this homomorphism?

        return ast.NodeList(results)


class Specializer(ast.NodeTransformer):

    def __init__(self, specialization_type):
        self.specialization_type = specialization_type
        self.specializations = {
            placeholder.id: placeholder.specialization
            for placeholder in find_placeholders(specialization_type)
        }

    def visit_Identifier(self, node):
        node_type = node.__meta__['type']
        if isinstance(node_type, TypeName):
            if isinstance(node_type.type, PlaceholderType):
                # FIXME
                new_node = ast.Identifier(name=str(self.specializations[node_type.type.id]))
                new_node.__meta__['type'] = self.specializations[node_type.type.id]
                return new_node

        return node


def mangle_function_name(module_name, function_name, function_type):
    result = '_Z'

    # Mangle the module name.
    result += f'm{len(module_name)}{module_name}'

    # Mangle the function name.
    result += f'f{len(function_name)}{function_name}'

    # Mangle the function type.
    for label, parameter_type in zip(function_type.labels, function_type.domain):
        # Mangle the parameter label.
        if label == '_':
            result += '0'
        else:
            result += f'{len(label)}{label}'

        # Mangle the parameter type.
        result += mangle_type(parameter_type)

    result += '_' + mangle_type(function_type.codomain)

    return result


def mangle_type(type_):
    if isinstance(type_, PlaceholderType):
        assert type_.specialization is not None, 'unexpected unspecialized placeholder'
        return mangle_type(type_.specialization)

    result = ''

    # Mangle the type modifiers.
    result += hex(type_.modifiers)[2:]

    if isinstance(type_, BuiltinType):
        result += type_.name[0].lower()

    # TODO: Handle function and custom types.

    return result
