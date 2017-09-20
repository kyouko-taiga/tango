class CompilerError(Exception):

    def __init__(self, message='unknown error', filename='', line=0, column=0):
        self.line   = line
        self.column = column

        super(CompilerError, self).__init__(f"{filename}:{line}:{column}: {message}")


class UnknownModifierError(CompilerError):

    def __init__(self, value, filename='', line=0, column=0):
        super(UnknownModifierError, self).__init__(
            message  = f"error - unknown modifier '{value}'",
            filename = filename,
            line     = line,
            column   = column)


class ModuleImportError(Exception):
    pass


class DuplicateDeclaration(Exception):

    def __init__(self, name):
        super().__init__(name)


class UndefinedSymbol(Exception):
    pass


class InferenceError(Exception):
    pass


class AssignmentError(Exception):
    pass


class ResourceAccessError(Exception):
    pass
