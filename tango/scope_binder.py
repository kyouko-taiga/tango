from .ast import Visitor
from .parser import *


def bind_scopes(node):
    binder = ScopeBinder()
    binder.visit(node)


class DuplicateDeclaration(Exception):

    def __init__(self, name):
        super().__init__(name)


class Scope(object):

    def __init__(self, id, name):
        self.id = id
        self.name = name
        self.members = set()

    def __hash__(self):
        return hash(self.id)

    def __eq__(self, other):
        return self.id == other.id


class ScopeBinder(Visitor):

    def __init__(self):
        # We put None here to handle names that aren't defined within a scope.
        self.scopes = [None]
        self.next_scope_id = 0

    @property
    def current_scope(self):
        return self.scopes[-1]

    def visit_ModuleDecl(self, node):
        # Push a new scope for the current module.
        self.scopes.append(Scope(id=self.next_scope_id, name=node.name))
        self.next_scope_id += 1

        for statement in node.statements:
            self.visit(statement)

        self.scopes.pop()

    def visit_ConstantDecl(self, node):
        # Make sure the container's identifier doesn't already exists within
        # the current scope.
        if node.name.name in self.current_scope.members:
            raise DuplicateDeclaration(node.name.name)

        # Insert the container's identifier into the current scope.
        self.current_scope.members.add(node.name.name)
        node.name.scope = self.current_scope

        # Bind the scopes in the container's initial value (if any).
        if node.initial_value:
            self.visit(node.initial_value)

    def visit_VariableDecl(self, node):
        self.visit_ConstantDecl(node)
