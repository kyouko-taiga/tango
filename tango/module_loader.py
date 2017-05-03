import os

from . import ast
from .builtin import builtin_module
from .errors import ModuleImportError
from .parser import parse
from .scope_binder import ScopeBinder


class ModuleLoader(object):

    def __init__(self, search_paths=None):
        self.loaded_modules = {'__builtin__': builtin_module}
        self.search_paths = search_paths or [os.getcwd()]

    def load(self, module_name):
        if module_name in self.loaded_modules:
            return self.loaded_modules[module_name]

        # Search for the module in the search paths.
        rel_path = os.path.join(*module_name.split('.')) + '.tango'
        source = None
        for search_path in self.search_paths:
            abs_path = os.path.join(search_path, rel_path)
            if os.path.exists(abs_path):
                with open(abs_path) as f:
                    source = f.read()
                break

        if source is None:
            raise ModuleImportError(
                "cannot load module '{}': module not found".format(module_name))

        # Parse the module declaration to produce an AST.
        module_decl = parse(source)
        module_decl.name = module_name

        # Replace the `Self` placeholder.
        self_replacer = SelfReplacer()
        module_decl = self_replacer.visit(module_decl)

        # Annotate each scope-creating node with the symbols it declares.
        symbols_extractor = SymbolsExtractor()
        symbols_extractor.visit(module_decl)

        # Bind all symbols of the module to their respective scope.
        scope_binder = ScopeBinder()
        scope_binder.visit(module_decl)

        return module_decl


class SelfReplacer(ast.Transformer):
    '''
    Replace occurences of the placeholder `Self` in type declarations.

    Members of a type may use the identifier `Self` as a placeholder for the
    the full name of the type they are declared in. Not only this can serve as
    a syntactic sugar to avoid repeating generic parameters, this placeholder
    is also required for protocols and imported types to be able to refer to
    their actual "final" type.
    '''

    def __init__(self):
        # This stack will serve the latest type declaration we encountered.
        self.self_identifiers = []

    def visit_nominal_type(self, node):
        # Create the identifier that should replace the placeholder.
        self.self_identifiers.append(ast.Identifier(name = node.name))
        if hasattr(node, 'generic_parameters'):
            self.self_identifiers[-1].specializations = [
                ast.SpecializationArgument(
                    name  = parameter,
                    value = ast.Identifier(name=parameter))
                for parameter in node.generic_parameters
            ]

        # Visit the type declaration.
        result = self.generic_visit(node)

        self.self_identifiers.pop()
        return result

    def visit_StructDecl(self, node):
        return self.visit_nominal_type(node)

    def visit_EnumDecl(self, node):
        return self.visit_nominal_type(node)

    def visit_ProtocolDecl(self, node):
        return self.visit_nominal_type(node)

    def visit_Identifier(self, node):
        if node.name == 'Self':
            try:
                return self.self_identifiers[-1]
            except IndexError:
                raise SyntaxError("invalid use of 'Self' outside of a type declaration")
        return node


class SymbolsExtractor(ast.Visitor):
    '''Annotate every Block node with the symbols it declares.'''

    symbol_node_classes = (
        ast.PropertyDecl, ast.FunctionDecl, ast.AbstractTypeDecl,
        ast.StructDecl, ast.EnumDecl, ast.EnumCaseDecl, ast.ProtocolDecl)

    def __init__(self):
        self.blocks = []

    def visit(self, node):
        if isinstance(node, ast.Block):
            self.blocks.append(node)
            node.__info__['symbols'] = set()
        elif isinstance(node, SymbolsExtractor.symbol_node_classes):
            self.blocks[-1].__info__['symbols'].add(node.name)

        self.generic_visit(node)

        if isinstance(node, ast.Block):
            self.blocks.pop()
