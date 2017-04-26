from .ast import *


def build_types(node):
    type_builder = TypeBuilder()
    return type_builder.visit(node)


class TypeBuilder(Transformer):

    def __init__(self):
        # Methods, properties and enum cases may use `Self` as a placeholder
        # to denote the "final" type they're defined in. This stack will serve
        # us to keep track of the actual type `Self` represents.
        self.self_type_identifiers = []

    def visit_nominal_type(self, node):
        # Create the type identifier that should replace `Self` in the type
        # declaration.
        self.self_type_identifiers.append(TypeIdentifier(
            name = node.name,
            specialization_arguments = [
                SpecializationArgument(
                    name            = parameter,
                    type_annotation = TypeIdentifier(name=parameter))
                for parameter in node.generic_parameters
            ]))

        result = self.generic_visit(node)
        self.self_type_identifiers.pop()
        return result

    def visit_StructDecl(self, node):
        return self.visit_nominal_type(node)

    def visit_EnumDecl(self, node):
        return self.visit_nominal_type(node)

    def visit_TypeIdentifier(self, node):
        if node.name == 'Self':
            try:
                return self.self_type_identifiers[-1]
            except IndexError:
                raise SyntaxError("invalid use of 'Self' outside of a type declaration")
        return node
