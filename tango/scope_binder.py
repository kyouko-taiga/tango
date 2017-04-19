from .ast import Transformer, Visitor
from .builtin import builtin_scope
from .errors import DuplicateDeclaration, UndefinedSymbol
from .parser import *
from .scope import Scope
from .types import BaseType


def bind_scopes(ast):
    # Extract the symbols declared in each scopes.
    symbols_extractor = SymbolsExtractor()
    result = symbols_extractor.visit(ast)

    # Bind all identifiers to their respective scope.
    scope_binder = ScopeBinder()
    scope_binder.visit(result)

    select_scope_binder = SelectScopeBinder()
    select_scope_binder.visit(result)

    return result


class SymbolsExtractor(Transformer):

    def visit_Block(self, node):
        symbols = set()

        for i, statement in enumerate(node.statements):
            if isinstance(statement, (EnumCaseDecl, ContainerDecl)):
                # Add the container's identifier to the current scope.
                symbols.add(statement.name)

            if isinstance(statement, (FunctionDecl, EnumDecl, StructDecl)):
                # Extract the symbols from the body of the type declaration.
                node.statements[i] = self.visit(statement)

                # Add the name of the type declaration to the current scope.
                symbols.add(statement.name)

            if isinstance(statement, If):
                # Extract the symbols from the body of the if expression.
                node.statements[i] = self.visit(statement)

        node.__info__['symbols'] = symbols
        return node


class ScopeBinder(Visitor):

    def __init__(self):
        # This list represent the scope stack we'll manipulate as we'll walk
        # down the AST.

        # Array[Scope]
        self.scopes = [builtin_scope]

        # This mapping will help us keep track of what the identifier being
        # declared when visiting its declaration, which is necessary to
        # properly map the scopes of declaration expressions that refer to the
        # same name as the identifier under declaration, but from an enclosing
        # scope.
        # For example, consider the following snippet in which `x` declared
        # within the function `f` should be a new variable, but inialized with
        # the value of the constant `x` defined in the global scope:
        # >>> cst x = 0
        # >>> fun f() { mut x = x }

        # (Scope) -> Set[String]
        self.under_declaration = {}

    @property
    def current_scope(self):
        return self.scopes[-1]

    def push_scope(self):
        # Push a new scope on the stack.
        self.current_scope.children.append(Scope(parent=self.scopes[-1]))
        self.scopes.append(self.current_scope.children[-1])

        # Initialize the set that keeps track of what identifier is being
        # declared when visiting its declaration.
        self.under_declaration[self.current_scope] = set()

    def visit_If(self, node):
        # Push a new scope on the stack before visiting the node's, pre-filled
        # with the symbols declared within the node's pattern parameters.
        self.push_scope()
        for parameter in node.pattern.parameters:
            self.current_scope[parameter.name] = []
        node.body.__info__['scope'] = self.current_scope

        # Visit the node's pattern.
        self.visit(node.pattern)

        # Insert the symbols declared within the function's block into the
        # current scope. Note that we do that *after* we visited the default
        # values and the return type, so as to avoid binding ayny of them to
        # the symbols of the function's scope.
        for symbol in node.body.__info__['symbols']:
            # Note that we could detect whether a symbol collides with a the
            # name of a parameter or generic parameter here. But letting the
            # scope binder find about the error while visiting the duplicate
            # declaration makes for better error messages.
            if symbol not in self.current_scope:
                self.current_scope[symbol] = []

        # Visit the node's body.
        self.visit(node.body)
        self.scopes.pop()

        # If the node's else clause is a simple block, we have to push a new
        # scope on the stack before visiting it.
        if isinstance(node.else_clause, Block):
            self.push_scope()
            for symbol in node.else_clause.__info__['symbols']:
                self.current_scope[symbol] = []
            node.else_clause.__info__['scope'] = self.current_scope

            self.visit(node.else_clause)
            self.scopes.pop()

        # If the node's else clause is another if expression, we can let its
        # visitor create a new scope for it.
        elif isinstance(node.else_clause, If):
            self.visit(node.else_clause)

    def visit_ModuleDecl(self, node):
        # Push a new scope on the stack before visiting the node's block, pre-
        # filled with the symbols declared within the latter.
        self.push_scope()
        for symbol in node.body.__info__['symbols']:
            self.current_scope[symbol] = []
        node.body.__info__['scope'] = self.current_scope

        self.visit(node.body)
        self.scopes.pop()

    def visit_ContainerDecl(self, node):
        # Make sure the container's name wasn't already declared within the
        # current scope.
        if self.current_scope[node.name]:
            raise DuplicateDeclaration(node.name)

        # Bind the scopes of the container's type annotation and initializing
        # expression (if any).
        self.under_declaration[self.current_scope].add(node.name)

        if node.initial_value:
            self.visit(node.initial_value)
        if node.type_annotation:
            self.visit(node.type_annotation)

        self.under_declaration[self.current_scope].remove(node.name)

        # Finally, insert the container's name in the current scope.
        self.current_scope.add(node.name, node)
        node.__info__['scope'] = self.current_scope

    def visit_FunctionDecl(self, node):
        # If the function's identifer was already declared within the current
        # scope, make sure it is associated with other function declarations
        # only (overloading).
        if self.current_scope[node.name]:
            decls = self.current_scope[node.name]
            if decls and (not isinstance(decls[0], FunctionDecl)):
                raise DuplicateDeclaration(node.name)

        # Insert the function's name in the current scope, so that we can
        # properly refer to it in nested scopes.
        self.current_scope.add(node.name, node)
        node.__info__['scope'] = self.current_scope

        # Push a new scope on the stack before visiting the function's
        # declaration, pre-filled with the symbols declared within the
        # function's generic parameters.
        self.push_scope()
        for symbol in node.generic_parameters:
            self.current_scope[symbol] = [Identifier(name=symbol)]
        node.body.__info__['scope'] = self.current_scope

        # Visit the default values of the function's parameters (if any). Note
        # that we allow such default values to refer to the function itself,
        # so as to match the behaviour of container declarations.
        for parameter in node.signature.parameters:
            self.visit(parameter.type_annotation)
            if parameter.default_value:
                self.under_declaration[self.current_scope].add(parameter.name)
                self.visit(parameter.default_value)
                self.under_declaration[self.current_scope].remove(parameter.name)

        # Visit the return type of the function (unless it's been inferred as
        # an actual type by the parser).
        if not isinstance(node.signature.return_type, BaseType):
            self.visit(node.signature.return_type)

        # Add the parameter names to the function's scope. Note that we do
        # that *after* we visited the default values and the return type, so
        # as to avoid binding any of them to the parameter names.
        for parameter in node.signature.parameters:
            # Make sure the parameter's name doesn't collide with the name of
            # a generic parameter.
            if parameter.name in self.current_scope:
                raise DuplicateDeclaration(parameter.name)

            self.current_scope[parameter.name] = [parameter]
            parameter.__info__['scope'] = self.current_scope

        # Insert the symbols declared within the function's block into the
        # current scope. Note that we do that *after* we visited the default
        # values and the return type, so as to avoid binding any of them to
        # the symbols of the function's scope.
        for symbol in node.body.__info__['symbols']:
            # Note that we could detect whether a symbol collides with a the
            # name of a parameter or generic parameter here. But letting the
            # scope binder find about the error while visiting the duplicate
            # declaration makes for better error messages.
            if symbol not in self.current_scope:
                self.current_scope[symbol] = []

        # Finally, visit the function's body.
        self.visit(node.body)
        self.scopes.pop()

    def visit_EnumDecl(self, node):
        # Make sure the function's identifier wasn't already declared within
        # the current scope.
        if self.current_scope[node.name]:
            raise DuplicateDeclaration(node.name)

        # Insert the enum's name in the current scope, so that we can
        # properly refer to it in nested scopes.
        self.current_scope.add(node.name, node)
        node.__info__['scope'] = self.current_scope

        # Insert the enum's name in the typenames of the current scope.
        self.current_scope.typenames.add(node.name)

        # Push a new scope on the stack before visiting the enum's body, pre-
        # filled with the symbols it declares.
        self.push_scope()
        for symbol in node.body.__info__['symbols']:
            self.current_scope[symbol] = []
        node.body.__info__['scope'] = self.current_scope

        # Finally, visit the enum's body.
        self.visit(node.body)
        self.scopes.pop()

    def visit_EnumCaseDecl(self, node):
        # Make sure the case's identifier wasn't already declared within the
        # current scope.
        if self.current_scope[node.name]:
            raise DuplicateDeclaration(node.name)

        # Visit the case's parameters (if any).
        for parameter in node.parameters:
            self.visit(parameter)

        # Finally, insert the case's name in the current scope.
        self.current_scope.add(node.name, node)
        node.__info__['scope'] = self.current_scope

    def visit_StructDecl(self, node):
        self.visit_EnumDecl(node)

    def visit_Identifier(self, node):
        # If we're currently visiting the declaration of the identifier, we
        # should necessarily bind it to an enclosing scope.
        if node.name in self.under_declaration[self.current_scope]:
            if self.current_scope.parent is None:
                raise UndefinedSymbol(node.name)
            defining_scope = self.current_scope.parent.defining_scope(node.name)
        else:
            defining_scope = self.current_scope.defining_scope(node.name)

        if defining_scope is None:
            raise UndefinedSymbol(node.name)
        node.__info__['scope'] = defining_scope

    def visit_TypeIdentifier(self, node):
        self.visit_Identifier(node)

    def visit_Select(self, node):
        # Bind the scopes of the symbols in the owning expression.
        self.visit(node.owner)

        # Unfortunately, we can't bind the symbols of a select expression yet,
        # because it will depend on the kind of declaration the owner's
        # identifier is refencing. We'll perform that task in another pass.


class SelectScopeBinder(Visitor):

    def nested_scope(self, node):
        # If the node is an identifier, we have to walk to the node that
        # declares it. If it turns out to be a nominal type, we can return the
        # scope of the type's body.
        if isinstance(node, (Identifier, TypeIdentifier)):
            decls = node.__info__['scope'][node.name]
            if isinstance(decls, list) and decls and isinstance(decls[0], (StructDecl, EnumDecl)):
                return decls[0].body.__info__['scope']

        # If the node is a select expression, we first have to get the nested
        # scope of the owner, in order to walk to the node that declares the
        # member. If it turns out to be a nominal type, we can return the
        # scope of the type's body.
        elif isinstance(node, Select):
            owner_nested_scope = self.nested_scope(node.owner)
            if owner_nested_scope is not None:
                decls = owner_nested_scope[node.member.name]
                if decls and isinstance(decls[0], (StructDecl, EnumDecl)):
                    return decls[0].body.__info__['scope']

        # In any other case, either the identifier refers to a dynamic type or
        # a variable, so we can't decide what's its nested scope.
        return None

    def visit_Select(self, node):
        member_scope = self.nested_scope(node.owner)
        if member_scope:
            node.member.__info__['scope'] = member_scope
        self.visit(node.owner)
