from .types import NominalType


Nothing  = NominalType('Nothing')
Anything = NominalType('Anything')
Int      = NominalType('Int')
Double   = NominalType('Double')
String   = NominalType('String')


class Scope(object):

    next_id = 0

    def __init__(self, parent=None):
        self.id = Scope.next_id
        Scope.next_id += 1
        self.parent = parent
        self.children = []

        # (String) -> [Node]
        self.members = {}

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


builtin_scope = Scope()
builtin_scope.members = {
    'Nothing' : Nothing,
    'Anything': Anything,
    'Int'     : Int,
    'Double'  : Double,
    'String'  : String,
}