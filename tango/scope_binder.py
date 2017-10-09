from .ast import *
from .builtin import builtin_module
from .errors import DuplicateDeclaration, UndefinedSymbol
from .scope import Scope, Symbol
from .types import TypeBase, FunctionType


class ScopeBinder(NodeVisitor):

    def __init__(self):
        # This list represent the scope stack we'll manipulate as we'll walk
        # down the AST.
        self.scopes = []

        self.next_scope_id = 0

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
        self.under_declaration = {}

    @property
    def current_scope(self):
        return self.scopes[-1]

    def push_scope(self, name=None):
        if name is None:
            name = str(self.next_scope_id)
            self.next_scope_id += 1

        # Push a new scope on the stack.
        new_scope = Scope(name=name)
        if self.scopes:
            new_scope.parent = self.current_scope
            self.current_scope.children.append(new_scope)
        self.scopes.append(new_scope)

        # Initialize the set that keeps track of what identifier is being
        # declared when visiting its declaration.
        self.under_declaration[self.current_scope] = set()

    def visit_ModuleDecl(self, node):
        # Create a scope for the module under declaration, pre-filled with the
        # the symbols of the builtin module.
        self.push_scope(name=node.name)
        for symbol in builtin_module.symbols.values():
            self.current_scope.add(symbol)

        # TODO Register a symbol for each imported module.

        # NOTE We can choose here how we want to handle shadowing of imported
        # symbols. With the current implementation, redeclaring a symbol that
        # can't be overloaded will raise a DuplicateDeclaration error. That's
        # because we don't replace imported symbols with unbound symbols in
        # the module scope.

        # Add all the symbols declared within the node's block.
        for name in node.body.__meta__['symbols']:
            if name not in self.current_scope:
                self.current_scope.add(Symbol(name=name))
        node.body.__meta__['scope'] = self.current_scope

        self.visit(node.body)
        self.scopes.pop()

    def visit_PropDecl(self, node):
        # Make sure the property's name wasn't already declared.
        symbol = self.current_scope[node.name]
        if symbol.code is not None:
            raise DuplicateDeclaration(
                "{}:{}: duplicate declaration of '{}'".format(
                    node.__meta__['start'][0],
                    node.__meta__['start'][1],
                    node.name
                ))

        # Bind the symbol to the current node.
        symbol.code = node
        node.__meta__['scope'] = self.current_scope

        # Bind the scopes of the container's type annotation and initializer.
        self.under_declaration[self.current_scope].add(node.name)
        self.generic_visit(node)
        self.under_declaration[self.current_scope].remove(node.name)

    def visit_FunDecl(self, node):
        # If the function's identifer was already declared within the current
        # scope, make sure it is associated with other function declarations
        # only (overloading).
        symbol = self.current_scope[node.name]
        if (symbol.code is not None):
            if not (isinstance(symbol.code, FunDecl) or
                    isinstance(symbol.type, FunctionType)):
                raise DuplicateDeclaration(
                    "{}:{}: duplicate declaration of '{}'".format(
                        node.__meta__['start'][0],
                        node.__meta__['start'][1],
                        node.name
                    ))

        # Bind the symbol to the current node.
        if symbol.code is None:
            symbol.code = node
        else:
            self.current_scope.add(Symbol(name=node.name, code=node))
        node.__meta__['scope'] = self.current_scope

        # Push a new scope on the stack before visiting the function's
        # declaration, pre-filled with the generic placeholders.
        self.push_scope()
        for placeholder in node.placeholders:
            self.current_scope.add(Symbol(name=placeholder, code=node))
        node.body.__meta__['scope'] = self.current_scope

        # Visit the type annotation of the function parameters.
        for parameter in node.parameters:
            self.visit(parameter.type_annotation)

        # Visit the codomain annotation of the function.
        self.visit(node.codomain_annotation)

        # Add the parameter names to the function's scope.
        for parameter in node.parameters:
            if parameter.name in self.current_scope:
                raise DuplicateDeclaration(
                    "{}:{}: duplicate declaration of '{}'".format(
                        parameter.__meta__['start'][0],
                        parameter.__meta__['start'][1],
                        parameter.name
                    ))

            self.current_scope.add(Symbol(name=parameter.name, code=parameter))
            parameter.__meta__['scope'] = self.current_scope

        # Insert the symbols declared within the function's block into the
        # current scope.
        for symbol in node.body.__meta__['symbols']:
            # We could detect whether a symbol collides with a the name of a
            # parameter or generic parameter here. But letting the scope
            # binder find about the error while visiting the duplicate
            # declaration makes for better error messages.
            if symbol not in self.current_scope:
                self.current_scope.add(Symbol(name=symbol))

        # Finally, visit the function's body.
        self.visit(node.body)
        self.scopes.pop()

    def visit_Identifier(self, node):
        # If we're currently visiting the declaration of the identifier, we
        # should necessarily bind it to an enclosing scope.
        defining_scope = self.current_scope.defining_scope(node.name)
        if defining_scope is not None:
            if node.name in self.under_declaration.get(defining_scope, {}):
                if defining_scope.parent is None:
                    raise UndefinedSymbol(
                        "{}:{}: undefined symbol '{}'".format(
                            node.__meta__['start'][0],
                            node.__meta__['start'][1],
                            node.name
                        ))
                defining_scope = defining_scope.parent.defining_scope(node.name)

        if defining_scope is None:
            raise UndefinedSymbol(
                "{}:{}: undefined symbol '{}'".format(
                    node.__meta__['start'][0],
                    node.__meta__['start'][1],
                    node.name
                ))
        node.__meta__['scope'] = defining_scope

    def visit_If(self, node):
        # Bind the symbols in the node's condition.
        self.visit(node.condition)

        # Push a new scope on the stack before visiting the node's then block,
        # pre-filled with the symbols it declares.
        self.push_scope()
        for name in node.then_block.__meta__['symbols']:
            self.current_scope.add(Symbol(name=name))
        node.then_block.__meta__['scope'] = self.current_scope
        self.visit(node.then_block)

        self.scopes.pop()

        # Visit the node's else block (if it exists) after popping the scope
        # of the then block so that symbols of the latter can't collide with
        # that of the former.
        if node.else_block:
            node.else_block.__meta__['scope'] = self.current_scope
            self.visit(node.else_block)


class SymbolsExtractor(NodeVisitor):
    '''Annotate every Block node with the symbols it declares.'''

    scope_opening_node_classes = (Block, If,)
    symbol_node_classes        = (PropDecl, FunDecl,)

    def __init__(self):
        self.blocks = []

    def visit(self, node):
        if isinstance(node, SymbolsExtractor.scope_opening_node_classes):
            self.blocks.append(node)
            node.__meta__['symbols'] = set()
        elif isinstance(node, SymbolsExtractor.symbol_node_classes):
            self.blocks[-1].__meta__['symbols'].add(node.name)

        self.generic_visit(node)

        if isinstance(node, SymbolsExtractor.scope_opening_node_classes):
            self.blocks.pop()
