from .ast import Transformer, Visitor
from .builtin import Scope, builtin_scope
from .errors import DuplicateDeclaration, UndefinedSymbol
from .parser import *
from .types import BaseType


def bind_scopes(node):
    binder = ScopeBinder()
    binder.visit(node)


class SymbolsExtractor(Transformer):

    def visit_Block(self, node):
        symbols = set()

        for i, statement in enumerate(node.statements):
            if isinstance(statement, (ConstantDecl, VariableDecl)):
                # Add the container's identifier to the current scope.
                symbols.add(statement.name)

            if isinstance(statement, (FunctionDecl, EnumDecl, StructDecl)):
                # Extract the symbols from the body of the type declaration.
                node.statements[i] = self.visit(statement)

                # Add the name of the type declaration to the current scope.
                symbols.add(statement.name)

        node.__info__['symbols'] = symbols
        return node

    def visit_EnumDecl(self, node):
        symbols = set([c.name for c in node.cases])
        for i, method in enumerate(node.methods):
            node.methods[i] = self.visit(method)
            symbols.add(method.name)

        node.__info__['symbols'] = symbols
        return node

    def visit_StructDecl(self, node):
        symbols = set([p.name for p in node.stored_properties])
        for i, method in enumerate(node.methods):
            node.methods[i] = self.visit(method)
            symbols.add(method.name)

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

        # Some nodes (e.g. function declarations) implicitly declare some
        # symbols in their associated block. As new scopes are created by
        # `visit_Block`, the latter should have a way to store the implicit
        # declarations before its statements are visited.

        # Dictionary[Key: String, Value: Node]
        self.implicit_declarations = {}

    @property
    def current_scope(self):
        return self.scopes[-1]

    def visit_Block(self, node):
        # Push a new scope on the stack before visitng the node's statements,
        # pre-filled with the symbols declared within the current block.
        self.current_scope.children.append(Scope(parent=self.scopes[-1]))
        self.scopes.append(self.current_scope.children[-1])
        for symbol in node.__info__['symbols']:
            self.current_scope[symbol] = []

        # Store the optional implicit declarations of the block.
        for name, decl_node in self.implicit_declarations.items():
            self.current_scope.add(name, decl_node)
            decl_node.__info__['scope'] = self.current_scope

        # Initialize the set that keeps track of what identifier is being
        # declared when visiting its declaration.
        self.under_declaration[self.current_scope] = set()

        for statement in node.statements:
            self.visit(statement)

        self.scopes.pop()

    def visit_ConstantDecl(self, node):
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

    def visit_VariableDecl(self, node):
        self.visit_ConstantDecl(node)

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

        # Visit the default values of the function's parameters (if any). Note
        # that we allow such default values to refer to the function itself,
        # so as to match the behaviour of container declarations.
        for parameter in node.signature.parameters:
            self.visit(parameter.type_annotation)
            if parameter.default_value:
                self.visit(parameter.default_value)

        # Visit the return type of the function (unless it's been inferred as
        # an actual type by the parser).
        if not isinstance(node.signature.return_type, BaseType):
            self.visit(node.signature.return_type)

        # Define the parameters of the function as the implicit declarations
        # of its block before visiting it.
        self.implicit_declarations = {p.name: p for p in node.signature.parameters}
        self.visit(node.body)

    def visit_EnumDecl(self, node):
        # Make sure the function's identifier wasn't already declared within
        # the current scope.
        if self.current_scope[node.name]:
            raise DuplicateDeclaration(node.name)

        # Push a new scope on the stack before visitng the enum's body, pre-
        # filled with the symbols it declares.
        self.scopes.append(Scope(parent=self.scopes[-1]))
        for symbol in node.__info__['symbols']:
            self.current_scope[symbol] = []

        # Initialize the set that keeps track of what identifier is being
        # declared when visiting its declaration.
        self.under_declaration[self.current_scope] = set()

        for child_node in node.cases + node.methods:
            self.visit(child_node)

        self.scopes.pop()

        # Finally, insert the enum's name in the current scope.
        self.current_scope.add(node.name, node)
        node.__info__['scope'] = self.current_scope

    def visit_EnumCaseDecl(self, node):
        # Make sure the case's identifier wasn't already decalred within the
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
        # Make sure the function's identifier wasn't already declared within
        # the current scope.
        if self.current_scope[node.name]:
            raise DuplicateDeclaration(node.name)

        # Push a new scope on the stack before visitng the enum's body, pre-
        # filled with the symbols it declares.
        self.scopes.append(Scope(parent=self.scopes[-1]))
        for symbol in node.__info__['symbols']:
            self.current_scope[symbol] = []

        # Initialize the set that keeps track of what identifier is being
        # declared when visiting its declaration.
        self.under_declaration[self.current_scope] = set()

        for child_node in node.stored_properties + node.methods:
            self.visit(child_node)

        self.scopes.pop()

        # Finally, insert the struct's name in the current scope.
        self.current_scope.add(node.name, node)
        node.__info__['scope'] = self.current_scope

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
