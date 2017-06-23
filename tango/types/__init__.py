from tango.wrapper import *

__all__ = ['TypeBase', 'ReferenceType', 'FunctionType', 'NominalType', 'BuiltinType', 'TypeUnion',]


class TypeUnion(TypeBase):

    def __init__(self, types=None):
        super().__init__()
        self.types = []

        # We can't use a set to store the types of the union, as not all types
        # are hashable yet. There isn't a total order relationship on them
        # neither, so our only option is a O(n^2) algorithm.
        if types is not None:
            for t in types:
                self.add(t)

    @property
    def is_generic(self):
        return any(t.is_generic for t in self.types)

    def add(self, t):
        if t not in self.types:
            self.types.append(t)

    def replace_content(self, types):
        self.types[:] = []
        for t in types:
            self.add(t)

    def copy(self):
        return TypeUnion(t.copy() if isinstance(t, TypeUnion) else t for t in self.types)

    def first(self):
        return self.types[0]

    def __iter__(self):
        return iter(self.types)

    def __len__(self):
        return len(self.types)

    def __eq__(self, other):
        return (type(self) == type(other)
                and (len(self.types) == len(other.types))
                and all(t in other.types for t in self.types))

    def __hash__(self):
        # We can't hash a type union without a total order on all types.
        return 0

    def __str__(self):
        return '[' + ' | '.join(map(str, self.types)) + ']'


# Following are some helper methods and properties we add by monkeypatching
# the C++ classes, so as to have a nicer API to work with.

TypeList_initializer = TypeList.__init__
def TypeList_init(self, types=None):
    TypeList_initializer(self)
    for t in (types or []):
        self.append(t)

TypeList.__init__ = TypeList_init


def ReferenceType_str(self):
    return '&' + str(self.referred_type)

ReferenceType.__str__ = ReferenceType_str


FunctionType_initializer = FunctionType.__init__
def FunctionType_init(self, domain=None, labels=None, codomain=None):
    sl = StringList()
    for l in (labels or []):
        sl.append(l)

    FunctionType_initializer(self, TypeList(domain), sl, codomain)

def FunctionType_str(self):
    parameters = []
    for i in range(len(self.domain)):
        parameters.append('{}: {}'.format(self.labels[i], self.domain[i]))

    return '(%s) -> %s' % (', '.join(parameters), self.codomain)

FunctionType.__init__ = FunctionType_init
FunctionType.__str__  = FunctionType_str


def NominalType_str(self):
    return self.name

NominalType.__str__ = NominalType_str
