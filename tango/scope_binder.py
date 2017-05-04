from .ast import *
from .builtin import builtin_module, ModuleType
from .errors import DuplicateDeclaration, UndefinedSymbol
from .module import Symbol
from .scope import Scope
from .types import BaseType, FunctionType


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
        node.__info__['scope'] = self.current_scope

        # Push a new scope on the stack before visiting them, pre-filled with
        # the `get` and `set` symbols.
        self.push_scope()
        self.current_scope.add(Symbol(name='get'))
        self.current_scope.add(Symbol(name='set'))

        # Bind the scopes of the container's type annotation and initializer.
        self.under_declaration[self.current_scope].add(node.name)
        self.generic_visit(node)
        self.under_declaration[self.current_scope].remove(node.name)
        self.scopes.pop()

    def visit_ValueBindingPattern(self, node):
        # Make sure the name of the pattern wasn't already declared.
        symbol = self.current_scope[node.name]
        if symbol.code is not None:
            raise DuplicateDeclaration(node.name)

        # Bind the symbol to the current node.
        symbol.code = node
        node.__info__['scope'] = self.current_scope

        # Bind the scopes of the container's type annotation and initializer.
        self.under_declaration[self.current_scope].add(node.name)
        self.generic_visit(node)
        self.under_declaration[self.current_scope].remove(node.name)

    def visit_FunctionDecl(self, node):
        # If the function's identifer was already declared within the current
        # scope, make sure it is associated with other function declarations
        # only (overloading).
        symbol = self.current_scope[node.name]
        if (symbol.code is not None):
            if not (isinstance(symbol.code, FunctionDecl) or
                    isinstance(symbol.type, FunctionType)):
                raise DuplicateDeclaration(node.name)

        # Bind the symbol to the current node.
        if symbol.code is None:
            symbol.code = node
        else:
            self.current_scope.add(Symbol(name=node.name, code=node))
        node.__info__['scope'] = self.current_scope

        # Push a new scope on the stack before visiting the function's
        # declaration, pre-filled with the symbols declared within the
        # function's generic parameters.
        self.push_scope()
        for name in node.generic_parameters:
            self.current_scope.add(Symbol(name=name, code=Identifier(name=name)))
        node.body.__info__['scope'] = self.current_scope

        # Visit the default values of the function's parameters (if any). Note
        # that we allow such default values to refer to the function itself,
        # as if it were defined in an outer scope.
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

        # Visit the where clause of the function (if any).
        if node.where_clause:
            self.visit(node.where_clause)

        # Add the parameter names to the function's scope. Note that we do
        # that *after* we visited the default values and the return type, so
        # as to avoid binding any of them to be bound to any parameter name.
        for parameter in node.signature.parameters:
            # Make sure the parameter's name doesn't collide with the name of
            # a generic parameter.
            if parameter.name in self.current_scope:
                raise DuplicateDeclaration(parameter.name)

            self.current_scope.add(Symbol(name=parameter.name, code=parameter))
            parameter.__info__['scope'] = self.current_scope

        # Insert the symbols declared within the function's block into the
        # current scope. Note that we do that *after* we visited the default
        # values and the return type, so as to avoid binding any of them to
        # the symbols of the function's scope.
        for symbol in node.body.__info__['symbols']:
            # We could detect whether a symbol collides with a the name of a
            # parameter or generic parameter here. But letting the scope
            # binder find about the error while visiting the duplicate
            # declaration makes for better error messages.
            if symbol not in self.current_scope:
                self.current_scope.add(Symbol(name=symbol))

        # Finally, visit the function's body.
        self.visit(node.body)
        self.scopes.pop()

    def visit_AbstractTypeDecl(self, node):
        # Make sure the abstract type's name wasn't already declared.
        symbol = self.current_scope[node.name]
        if symbol.code is not None:
            raise DuplicateDeclaration(node.name)

        # Bind the symbol to the current node.
        symbol.code = node
        node.__info__['scope'] = self.current_scope

        # Visit the node's initializer (if any).
        self.generic_visit(node)

    def visit_EnumCaseDecl(self, node):
        # Make sure the case's identifier wasn't already declared within the
        # current scope.
        symbol = self.current_scope[node.name]
        if symbol.code is not None:
            raise DuplicateDeclaration(node.name)

        # Bind the symbol to the current node.
        symbol.code = node
        node.__info__['scope'] = self.current_scope

        # Visit the case's parameters (if any).
        for parameter in node.parameters:
            self.visit(parameter)

    def visit_nominal_type(self, node):
        # Make sure the type's identifier wasn't already declared within the
        # current scope.
        symbol = self.current_scope[node.name]
        if symbol.code is not None:
            raise DuplicateDeclaration(node.name)

        # Bind the symbol to the current node.
        symbol.code = node
        node.__info__['scope'] = self.current_scope

        # Insert the type's name in the typenames of the current scope.
        self.current_scope.typenames.add(node.name)

        # Push a new scope on the stack before visiting the type's body, pre-
        # filled with the symbols it declares, as well as those declared as
        # its generic parameters.
        self.push_scope(name=node.name)
        for name in node.body.__info__['symbols']:
            self.current_scope.add(Symbol(name=name))
        for name in node.generic_parameters:
            self.current_scope.add(Symbol(name=name, code=Identifier(name=symbol)))
        node.body.__info__['scope'] = self.current_scope

        # Introduces a `Self` symbol in the type's scope, to bind references
        # to the placeholder.
        self.current_scope.add(Symbol(name='Self', code=node))

        # Visit the type's declaration.
        self.generic_visit(node.body)
        self.scopes.pop()

    def visit_StructDecl(self, node):
        self.visit_nominal_type(node)

    def visit_EnumDecl(self, node):
        self.visit_nominal_type(node)

    def visit_ProtocolDecl(self, node):
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

        for argument in node.specializations:
            self.visit(argument)

    def visit_Select(self, node):
        # Bind the scopes of the symbols in the owning expression.
        self.visit(node.owner)

        # Unfortunately, we can't bind the symbols of a select expression yet,
        # because it will depend on the kind of declaration the owner's
        # identifier is refencing.

    def visit_ImplicitSelect(self, node):
        pass

    def visit_If(self, node):
        # Push a new scope on the stack before visiting the node's condition,
        # pre-filled with the symbols it declares.
        self.push_scope()
        for name in node.__info__['symbols']:
            self.current_scope.add(Symbol(name=name))
        self.visit(node.condition)

        # Push yet another scope on the stack before visiting the node's body,
        # pre-filled with the symbols it declares. Note how we nest this
        # second scope inside that of the node's condition, so that we can
        # bind symbols declared within patterns.
        self.push_scope()
        for name in node.body.__info__['symbols']:
            self.current_scope.add(Symbol(name=name))
        node.body.__info__['scope'] = self.current_scope
        self.visit(node.body)

        # Pop both scopes.
        self.scopes.pop()
        self.scopes.pop()

        # If the node's else clause is a simple block, we have to push a new
        # scope on the stack before visiting it.
        if isinstance(node.else_clause, Block):
            self.push_scope()
            for name in node.else_clause.__info__['symbols']:
                self.current_scope.add(Symbol(name=name))
            node.else_clause.__info__['scope'] = self.current_scope

            self.visit(node.else_clause)
            self.scopes.pop()

        # If the node's else clause is another if expression, we can let its
        # visitor create take it from there.
        elif isinstance(node.else_clause, If):
            self.visit(node.else_clause)

    def visit_SwitchCaseClause(self, node):
        # Push a new scope on the stack before visiting the node's pattern,
        # pre-filled with the symbols it declares.
        self.push_scope()
        for name in node.__info__['symbols']:
            self.current_scope.add(Symbol(name=name))
        self.visit(node.pattern)

        # Push yet another scope on the stack before visiting the node's body,
        # pre-filled with the symbols it declares. Note how we nest this
        # second scope inside that of the node's condition, so that we can
        # bind symbols declared within patterns.
        self.push_scope()
        for name in node.body.__info__['symbols']:
            self.current_scope.add(Symbol(name=name))
        node.body.__info__['scope'] = self.current_scope
        self.visit(node.body)

        # Pop both scopes.
        self.scopes.pop()
        self.scopes.pop()

    def visit_For(self, node):
        # Push a new scope on the stack before visiting the node's iterator,
        # pre-filled with the symbols it declares.
        self.push_scope()
        for name in node.__info__['symbols']:
            self.current_scope.add(Symbol(name=name))
        self.visit(node.iterator)

        # Push yet another scope on the stack before visiting the node's body,
        # pre-filled with the symbols it declares. Note how we nest this
        # second scope inside that of the node's condition, so that we can
        # bind symbols declared within patterns.
        self.push_scope()
        for name in node.body.__info__['symbols']:
            self.current_scope.add(Symbol(name=name))
        node.body.__info__['scope'] = self.current_scope
        self.visit(node.body)

        # Pop both scopes.
        self.scopes.pop()
        self.scopes.pop()

    def visit_While(self, node):
        # Push a new scope on the stack before visiting the node's condition,
        # pre-filled with the symbols it declares.
        self.push_scope()
        for name in node.__info__['symbols']:
            self.current_scope.add(Symbol(name=name))
        self.visit(node.condition)

        # Push yet another scope on the stack before visiting the node's body,
        # pre-filled with the symbols it declares. Note how we nest this
        # second scope inside that of the node's condition, so that we can
        # bind symbols declared within patterns.
        self.push_scope()
        for name in node.body.__info__['symbols']:
            self.current_scope.add(Symbol(name=name))
        node.body.__info__['scope'] = self.current_scope
        self.visit(node.body)

        # Pop both scopes.
        self.scopes.pop()
        self.scopes.pop()

    def visit_Closure(self, node):
        # Push a new scope on the stack before visiting the node's parameters
        # and statements, pre-filled with the symbols they declare.
        self.push_scope()
        for name in node.__info__['symbols']:
            self.current_scope.add(Symbol(name=name))
        node.__info__['scope'] = self.current_scope

        self.generic_visit(node)
        self.scopes.pop()


class SymbolsExtractor(Visitor):
    '''Annotate every Block node with the symbols it declares.'''

    scope_opening_node_classes = (Block, If, SwitchCaseClause, For, While, Closure,)

    symbol_node_classes = (
        PropertyDecl, FunctionDecl, AbstractTypeDecl,
        StructDecl, EnumDecl, EnumCaseDecl, ProtocolDecl,
        ValueBindingPattern,)

    def __init__(self):
        self.blocks = []

    def visit(self, node):
        if isinstance(node, SymbolsExtractor.scope_opening_node_classes):
            self.blocks.append(node)
            node.__info__['symbols'] = set()
        elif isinstance(node, SymbolsExtractor.symbol_node_classes):
            self.blocks[-1].__info__['symbols'].add(node.name)

        self.generic_visit(node)

        if isinstance(node, SymbolsExtractor.scope_opening_node_classes):
            self.blocks.pop()
