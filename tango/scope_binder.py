from .ast import *
from .builtin import builtin_module, ModuleType
from .errors import DuplicateDeclaration, UndefinedSymbol
from .module import Symbol
# from .scope import Scope
from .types import BaseType


class ScopeBinder(Visitor):

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

        # Add `Tango` itself as a symbol of the module scope, so we can also
        # refer to builtin symbols by their fully qualified names (e.g.
        # `Tango.Int`).
        self.current_scope.add(
            Symbol(name='Tango', type=ModuleType, nested=builtin_module.symbols))

        # TODO Register a symbol for each imported module.

        # NOTE We can choose here how we want to handle shadowing of imported
        # symbols. With the current implementation, redeclaring a symbol that
        # can't be overloaded will raise a DuplicateDeclaration error. That's
        # because we don't replace imported symbols with unbound symbols in
        # the module scope.

        # Add all the symbols declared within the node's block.
        for name in node.body.__info__['symbols']:
            if name not in self.current_scope:
                self.current_scope.add(Symbol(name=name))
        node.body.__info__['scope'] = self.current_scope

        self.visit(node.body)
        self.scopes.pop()

    def visit_PropertyDecl(self, node):
        # Make sure the property's name wasn't already declared.
        symbol = self.current_scope[node.name]
        if symbol.code is not None:
            raise DuplicateDeclaration(node.name)

        # Bind the symbol to the current node.
        symbol.code = node

        # Bind the scopes of the container's type annotation and initializer.
        self.under_declaration[self.current_scope].add(node.name)
        self.generic_visit(node)
        self.under_declaration[self.current_scope].remove(node.name)

        # Bind the node to the current scope.
        node.__info__['scope'] = self.current_scope

    def visit_pattern_matching_node(self, node):
        # Push a new scope on the stack before visiting the node's pattern,
        # pre-filled with the symbols declared as pattern parameters.
        self.push_scope()
        for parameter in node.pattern.parameters:
            self.current_scope.add(Symbol(name=parameter.name))
        self.visit(node.pattern)

        # Insert the symbols declared within the node's body into the current
        # scope. Note that we do that *after* we visited the declarations of
        # pattern parameters, so as to avoid binding any of them to the
        # symbols of the node's scope.
        node.body.__info__['scope'] = self.current_scope
        for symbol in node.body.__info__['symbols']:
            # Note that we could detect whether a symbol collides with a the
            # name of a parameter here. But letting the scope binder find
            # about the error while visiting the duplicate declaration makes
            # for better error messages.
            if symbol not in self.current_scope:
                self.current_scope.add(Symbol(name=symbol))

        # Visit the node's body.
        self.visit(node.body)
        self.scopes.pop()

    def visit_If(self, node):
        self.visit_pattern_matching_node(node)

        # If the node's else clause is a simple block, we have to push a new
        # scope on the stack before visiting it.
        if isinstance(node.else_clause, Block):
            self.push_scope()
            for symbol in node.else_clause.__info__['symbols']:
                self.current_scope.add(Symbol(name=symbol))
            node.else_clause.__info__['scope'] = self.current_scope

            self.visit(node.else_clause)
            self.scopes.pop()

        # If the node's else clause is another if expression, we can let its
        # visitor create a new scope for it.
        elif isinstance(node.else_clause, If):
            self.visit(node.else_clause)

    def visit_SwitchCaseClause(self, node):
        self.visit_pattern_matching_node(node)

    def visit_FunctionDecl(self, node):
        # If the function's identifer was already declared within the current
        # scope, make sure it is associated with other function declarations
        # only (overloading).
        symbol = self.current_scope[node.name]
        if symbol.decl:
            if not isinstance(symbol.decl, list) or not isinstance(symbol.decl[0], FunctionDecl):
                raise DuplicateDeclaration(node.name)

        # Set the symbol declaration to the current node, so that we can
        # properly refer to it in nested scopes.
        symbol.decl            = [node] if symbol.decl is None else symbol.decl + [node]
        node.__info__['scope'] = self.current_scope

        # Push a new scope on the stack before visiting the function's
        # declaration, pre-filled with the symbols declared within the
        # function's generic parameters.
        self.push_scope()
        for symbol in node.generic_parameters:
            self.current_scope.add(Symbol(name=symbol, decl=Identifier(name=symbol)))
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

            self.current_scope.add(Symbol(name=parameter.name, decl=parameter))
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
                self.current_scope.add(Symbol(name=symbol))

        # Finally, visit the function's body.
        self.visit(node.body)
        self.scopes.pop()

    def visit_EnumCaseDecl(self, node):
        # Make sure the case's identifier wasn't already declared within the
        # current scope.
        symbol = self.current_scope[node.name]
        if symbol.decl is not None:
            raise DuplicateDeclaration(node.name)

        # Visit the case's parameters (if any).
        for parameter in node.parameters:
            self.visit(parameter)

        # Finally, set the symbol declaration.
        symbol.decl            = node
        node.__info__['scope'] = self.current_scope

    def visit_nominal_type(self, node):
        # Make sure the type's identifier wasn't already declared within the
        # current scope.
        symbol = self.current_scope[node.name]
        if symbol.decl is not None:
            raise DuplicateDeclaration(node.name)

        # Set the symbol declaration, so that we can properly refer to it in
        # nested scopes.
        symbol.decl            = node
        node.__info__['scope'] = self.current_scope

        # Insert the type's name in the typenames of the current scope.
        self.current_scope.typenames.add(node.name)

        # Push a new scope on the stack before visiting the type's body, pre-
        # filled with the symbols it declares, as well as those declared as
        # its generic parameters.
        self.push_scope(name=node.name)
        for symbol in node.body.__info__['symbols']:
            self.current_scope.add(Symbol(name=symbol))
        for symbol in node.generic_parameters:
            self.current_scope.add(Symbol(name=symbol, decl=Identifier(name=symbol)))
        node.body.__info__['scope'] = self.current_scope

        # Finally, visit the type's body.
        self.visit(node.body)
        self.scopes.pop()

    def visit_EnumDecl(self, node):
        self.visit_nominal_type(node)

    def visit_StructDecl(self, node):
        self.visit_nominal_type(node)

    def visit_Identifier(self, node):
        # If we're currently visiting the declaration of the identifier, we
        # should necessarily bind it to an enclosing scope.
        defining_scope = self.current_scope.defining_scope(node.name)
        if defining_scope is not None:
            if node.name in self.under_declaration.get(defining_scope, {}):
                if defining_scope.parent is None:
                    raise UndefinedSymbol(node.name)
                defining_scope = defining_scope.parent.defining_scope(node.name)

        if defining_scope is None:
            raise UndefinedSymbol(node.name)
        node.__info__['scope'] = defining_scope

    def visit_TypeIdentifier(self, node):
        self.visit_Identifier(node)
        for argument in node.specialization_arguments:
            self.visit(argument)

    def visit_Select(self, node):
        # Bind the scopes of the symbols in the owning expression.
        self.visit(node.owner)

        # Unfortunately, we can't bind the symbols of a select expression yet,
        # because it will depend on the kind of declaration the owner's
        # identifier is refencing.


class Scope(object):

    next_id = 0

    def __init__(self, name, parent=None):
        self.id = Scope.next_id
        Scope.next_id += 1

        self.name = name
        self.parent = parent
        self.children = []

        # (String) -> Symbol
        self.symbols = {}

        # Types are first-class citizen (typed with `Type`), but there're many
        # instances where we need the type name to refer to the type's itself
        # rather than the first-class type value (e.g. in annotations).
        # Whether the type name should be interpreted as a first-class value
        # or a reference to its own type depends on where the name is used. As
        # a result, we've to know what symbols refer to a type name in a given
        # scope, so that we can decide what semantics give those identifiers.
        self.typenames = set()

    @property
    def qualified_name(self):
        if self.parent is None:
            return self.name
        return self.parent.qualified_name + '.' + self.name

    def defining_scope(self, name):
        if name in self.symbols:
            return self
        if self.parent:
            return self.parent.defining_scope(name)
        return None

    def add(self, symbol):
        self.symbols[symbol.name] = symbol
        # symbol.scope = self

    def get(self, name, default=None):
        return self.symbols.get(name, default)

    def __iter__(self):
        return iter(self.symbols)

    def __contains__(self, name):
        return name in self.symbols

    def __getitem__(self, name):
        return self.symbols[name]

    def __hash__(self):
        return hash(self.id)

    def __eq__(self, other):
        return self.id == other.id

    def __str__(self):
        return self.qualified_name
