from .ast import Transformer, Visitor
from .parser import *


def bind_scopes(node):
    binder = ScopeBinder()
    binder.visit(node)


class DuplicateDeclaration(Exception):

    def __init__(self, name):
        super().__init__(name)


class Scope(object):

    next_id = 0

    def __init__(self, name, parent=None):
        self.id = Scope.next_id
        Scope.next_id += 1

        self.name = name
        self.members = {}

        self.parent = parent
        self.children = []

    @property
    def uri(self):
        if self.parent is None:
            return str(self.id)
        return '%s.%s' % (self.parent.uri, self.id)

    def defining_scope(self, name):
        if name in self.members:
            return self
        if parent:
            return parent.defining_scope(name)
        return None

    def __contains__(self, name):
        return name in self.members

    def __getitem__(self, name):
        return self.members[name]
        # if name in self.members:
        #     return self.members[name]
        # if self.parent is not None:
        #     return self.parent[name]
        # raise KeyError(name)

    def __setitem__(self, name, value):
        self.members[name] = value

    def __hash__(self):
        return hash(self.id)

    def __eq__(self, other):
        return self.id == other.id


class SymbolExtractor(Transformer):

    def search_for_symbols(self, node):
        symbols = []
        if isinstance(node, (ModuleDecl, Block)):
            for i, statement in enumerate(node.statements):
                if isinstance(statement, (ConstantDecl, VariableDecl)):
                    symbols.append(statement.name)

                    # If the statement is a container's declaration, we should
                    # enclose all remaining statements in a new block, so that
                    # we can properly bind symbols that shadow a parent scope.
                    if i < len(node.statements) - 1:
                        processed = node.statements[:i]
                        remaining = node.statements[i + 1:]
                        new_statements = processed + [
                            Block(statements = [self.visit(s) for s in remaining])
                        ]

                        new_node = node.copy()
                        new_node.statements = new_statements
                        new_node.__symbols = symbols
                        return new_node

                elif isinstance(statement, (FunctionDecl, StructDecl, EnumDecl)):
                    result.append(statement.name)

        new_node = node.copy()
        new_node.symbols = symbols
        return new_node


    def visit_ModuleDecl(self, node):
        symbols, new_node = self.search_for_symbols(node)





class ScopeBinder(Visitor):

    def __init__(self):
        # We put None here to handle names that aren't defined within a scope.
        self.scopes = [None]

    @property
    def current_scope(self):
        return self.scopes[-1]

    def visit_ModuleDecl(self, node):
        # Push a new scope for the current module.
        self.scopes.append(Scope(name=node.name, parent=self.scopes[-1]))

        for statement in node.statements:
            self.visit(statement)

        self.scopes.pop()

    def visit_ConstantDecl(self, node):
        # Make sure the container's identifier wasn't already declared within
        # the current scope.
        if ((node.name.name in self.current_scope) and
            (not isinstance(self.current_scope[node.name.name], ForwardIdentifier))):
            raise DuplicateDeclaration(node.name.name)

        # Bind the scopes of the container's initializing expression (if any).
        # Note that we do that first, so we don't improperly bind the
        # container's identifier to the current scope.
        if node.initial_value:
            self.visit(node.initial_value)

        # Bind the scopes of the container's type annotation (if any).
        if node.type_annotation:
            self.visit(node.type_annotation)

        # If the container's identifier was forward declared, we should bind
        # its instance to the current node now. Otherwise, we should insert
        # the node in the current scope.
        if node.name.name in self.current_scope:
            decl = self.current_scope[node.name.name]
            decl.instance = node
        else:
            self.current_scope[node.name.name] = node

        # Finally, we associated the scope of the current node.
        node.name.scope = self.current_scope

    def visit_VariableDecl(self, node):
        self.visit_ConstantDecl(node)

    def visit_Identifier(self, node):
        # Check if the identifier was already defined in the visible scopes.
        defining_scope = self.current_scope.defining_scope(node.name)
        if defining_scope is not None:


        # Insert the identifier into the current scope if it wasn't already.
        if node.name not in self.current_scope.members:
            self.current_scope.members[node.name] = node
        node.scope = self.current_scope

    def visit_BinaryExpression(self, node):
        pass
