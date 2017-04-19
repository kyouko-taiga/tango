class Scope(object):

    next_id = 0

    def __init__(self, parent=None):
        self.id = Scope.next_id
        Scope.next_id += 1
        self.parent = parent
        self.children = []

        # (String) -> [Node]
        self.members = {}

        # Types are first-class citizen (typed with `Type`), but there're many
        # instances where we need the type name to refer to the type's itself
        # rather than the first-class type value (e.g. in annotations).
        # Whether the type name should be interpreted as a first-class value
        # or a reference to its own type depends on where the name is used. As
        # a result, we've to know what symbols refer to a type name in a given
        # scope, so that we can decide what semantics give those identifiers.
        self.typenames = set()

    @property
    def uri(self):
        if self.parent is None:
            return str(self.id)
        return '%s.%s' % (self.parent.uri, self.id)

    def defining_scope(self, name):
        if name in self.members:
            return self
        if self.parent:
            return self.parent.defining_scope(name)
        return None

    def add(self, name, value):
        self.members[name] = self.members.get(name, []) + [value]

    def __contains__(self, name):
        return name in self.members

    def __getitem__(self, name):
        return self.members[name]

        # TODO Provide the overloads of function types whose signature are not
        # shadowed from the enclosing scopes.

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
