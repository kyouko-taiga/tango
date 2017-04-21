from .ast import *
from .types import FunctionType, TypeUnion


def disambiguate_types(node):
    type_disambiguator = TypeDisambiguator()
    type_disambiguator.visit(node)
    return (node, type_disambiguator.ambiguous_nodes)


class TypeDisambiguator(Visitor):

    def __init__(self):
        self.ambiguous_nodes = []

    def visit(self, node):
        if ('type' in node.__info__) and isinstance(node.__info__['type'], TypeUnion):
            # If the current node denotes a function identifier, it's okay for
            # it to be tagged with multiple types.
            if isinstance(node, Identifier):
                # First, we get the object(s) (declaration node(s) or foreign
                # type(s)) that introduced the identifier.
                try:
                    decl = next(iter(node.__info__['scope'][node.name]))
                except TypeError:
                    decl = node.__info__['scope'][node.name]

                is_ambiguous = (
                    # If the identifier was declared in the current module,
                    # its introducing objects must be function declarations.
                    not isinstance(decl, FunctionDecl) and
                    # If it was declared in another module, its introducing
                    # objects must be function types.
                    not isinstance(decl, FunctionType))

                # NOTE We only chec the first introducing object, under the
                # assumption that the scope binder prevents identifiers to be
                # declared twice, unless they denote overloaded functions.

            # Expressions should never have multiple types.
            else:
                is_ambiguous = True

            if is_ambiguous:
                self.ambiguous_nodes.append(node)

        self.generic_visit(node)
