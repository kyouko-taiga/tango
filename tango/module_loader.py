import os

from .builtin import builtin_module
from .errors import ModuleImportError
from .parser import parse
from .scope_binder import ScopeBinder, SymbolsExtractor


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

        # Annotate each scope-opening node with the symbols it declares.
        symbols_extractor = SymbolsExtractor()
        symbols_extractor.visit(module_decl)

        # Bind all symbols of the module to their respective scope.
        scope_binder = ScopeBinder()
        scope_binder.visit(module_decl)

        return module_decl
