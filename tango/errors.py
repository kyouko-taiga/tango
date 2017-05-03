class ModuleImportError(Exception):
    pass


class DuplicateDeclaration(Exception):

    def __init__(self, name):
        super().__init__(name)


class UndefinedSymbol(Exception):
    pass


class InferenceError(Exception):
    pass
