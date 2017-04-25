class Symbol(object):

    def __init__(self, name, is_mutable=False, decl=None, type=None):
        self.name = name
        self.is_mutable = is_mutable
        self.decl = decl
        self.type = type

        self.scope = None


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
        symbol.scope = self

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
