from copy import deepcopy

from . import ast
from . import types


class Mangler(ast.NodeTransformer):

    def __init__(self):
        self.module_name = None

    def visit_ModuleDecl(self, node):
        self.module_name = node.name
        return self.generic_visit(node)

    def visit_FunDecl(self, node):
        # A function named `main` is a special case that shouldn't be mangled.
        if node.name == 'main':
            return self.generic_visit(node)

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
            new_node = deepcopy(node)

            # Update the function declaration to match its specialization.
            specializer = Specializer(function_type)
            new_node    = specializer.visit(new_node)

            # Store the mangled specialization.
            new_node.name = mangle_function_name(self.module_name, node.name, function_type)
            results.append(self.generic_visit(new_node))

            # QUESTION: We don't rewrite type annotations here. Instead, we
            # only update their metadata to reflect their specialized type.
            # This means the code we produce here will probably not compile.
            # However, this shouldn't matter for IR generation, because it
            # only relies on the metadata to properly type declarations.
            # Should we transform type annotation anyway to produce compilable
            # code? Is there any reason to do it beside the correctness of
            # this homomorphism?

        return ast.NodeList(results)

    def visit_Identifier(self, node):
        # Check if the identifier's associated with a function declaration, in
        # which case it should be mangled.
        if isinstance(node.__meta__['scope'][node.name].code, ast.FunDecl):

            # NOTE: In the above test, we're assuming identifiers whose symbol
            # is associated with a function declaration to always represent
            # the function itself. This should be right as long as we only
            # associate function declarations to symbols that are used in
            # expressions.

            node.name = mangle_function_name(self.module_name, node.name, node.__meta__['type'])

        return node


class Specializer(ast.NodeTransformer):

    def __init__(self, specialization_type):
        self.specialization_type = specialization_type
        self.specializations = {
            placeholder.id: placeholder.specialization
            for placeholder in types.find_placeholders(specialization_type)
        }

    def generic_visit(self, node):
        if node.__meta__['type'] is not None:
            node.__meta__['type'] = self.specialize_type(node.__meta__['type'])
        return super(Specializer, self).generic_visit(node)

    def specialize_type(self, type_):
        if not type_.is_generic:
            return type_

        if isinstance(type_, types.TypeName):
            return types.type_factory.make_name(
                name = type_.name,
                type = self.specialize_type(gentype_eric_type.type))

        if isinstance(type_, types.PlaceholderType):
            return self.specializations[type_.id]

        if isinstance(type_, types.FunctionType):
            return types.type_factory.make_function(
                modifiers = type_.modifiers,
                domain    = [self.specialize_type(p) for p in type_.domain],
                codomain  = self.specialize_type(type_.codomain),
                labels    = list(type_.labels))

        assert isinstance(generic_type, types.StructType)
        return generic_type

        # TODO: Handle structural types


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
    if isinstance(type_, types.PlaceholderType):
        assert type_.specialization is not None, 'unexpected unspecialized placeholder'
        return mangle_type(type_.specialization)

    result = ''

    # Mangle the type modifiers.
    result += hex(type_.modifiers)[2:]

    if isinstance(type_, types.StructType):
        result += type_.name[0].lower()

    # TODO: Handle function and custom types.

    return result
