from .ast import *
from .builtin import builtin_module
from .errors import AssignmentError, ResourceAccessError


class CaptureFinder(NodeVisitor):

    def __init__(self):
        self.current_scope = []
        self.captures      = []

    def visit_Block(self, node):
        self.captures.append(set())
        self.current_scope.append(node.__meta__['scope'])

        self.generic_visit(node)

        node.__meta__['captures'] = self.captures[-1]
        self.captures.pop()
        self.current_scope.pop()

    def visit_Identifier(self, node):
        # We do nothing if we're not in a scope.
        if not self.current_scope:
            return

        if node.name not in self.current_scope[-1]:
            self.captures[-1].add((node.__meta__['scope'], node.name))


class _Resource(object):

    def __init__(self, state, ownership, pointee=None):
        # 0: uninitialized
        # 1: initialized
        # 2: moved
        self.state = state

        # True or False
        self.ownership = ownership

        # Identifier
        self.pointee = pointee

    def __hash__(self):
        return hash((self.state, self.ownership, self.pointee))

    def __eq__(self):
        return (type(self) == type(other)
                and self.state == other.state
                and self.ownership == other.ownership
                and self.pointee == other.pointee)


class StateChecker(NodeVisitor):

    def __init__(self):
        self.variables = {}

    def __getitem__(self, node):
        assert isinstance(node, Identifier)
        return self.variables[(node.__meta__['scope'], node.name)]

    def __setitem__(self, node, resource):
        assert isinstance(node, Identifier)
        self.variables[(node.__meta__['scope'], node.name)] = resource

    def visit_ModuleDecl(self, node):
        # We consider all builtin symbols to be initialized.
        module_scope = node.body.__meta__['scope']
        for symbol in builtin_module.symbols:
            self.variables[(module_scope, symbol)] = _Resource(1, True)

        # Visit the module's statements.
        self.visit(node.body)

    def visit_Block(self, node):
        # Mark all symbols declared within the block uninitialized.
        for symbol in node.__meta__['symbols']:
            self.variables[(node.__meta__['scope'], symbol)] = _Resource(0, False)

        # Visit the block's statements.
        self.generic_visit(node)

    def visit_Assignment(self, node):
        lvalue = node.lvalue
        rvalue = node.rvalue
        assert isinstance(lvalue, Identifier)

        # Make sure the lvalue can be assigned.
        if lvalue.__meta__.get('mutability') == 'cst':
            if self[lvalue].state != 0:
                raise AssignmentError(
                    "{}:{}: cannot assign to value: '{}' is a constant".format(
                        node.__meta__['start'][0],
                        node.__meta__['start'][1],
                        lvalue.name
                    ))

        # Check if the preconditions of the rvalue are satisfied.
        if self.check_preconditions(rvalue):
            # If the node is a copy assignment, we can simply mark the lvalue
            # as initialized, and owner of its value.
            if node.operator == '=':
                self[lvalue] = _Resource(1, True, lvalue)

            if node.operator == '&-':
                # The rvalue of a reference assignment has to be an identifier.
                if not isinstance(rvalue, Identifier):
                    raise AssignmentError(
                        "{}:{}: cannot create reference: the rvalue is not a property".format(
                            node.__meta__['start'][0],
                            node.__meta__['start'][1]
                        ))

                # Make sure the rvalue isn't a reference to the lvalue.
                if self[lvalue].ownership:
                    pointee = self[rvalue].pointee
                    if (pointee.name == lvalue.name
                        and pointee.__meta__['scope'] == lvalue.__meta__['scope']):
                        raise AssignmentError(
                            "{}:{}: cannot create reference: '{}' is a reference cycle".format(
                                node.__meta__['start'][0],
                                node.__meta__['start'][1],
                                node
                            ))

                # Make sure the scope of the lvalue doesn't live longer than
                # that of the rvalue.
                cursor = node.lvalue.__meta__['scope']
                valid  = False
                while cursor is not None:
                    if cursor == node.rvalue.__meta__['scope']:
                        valid = True
                        break
                    cursor = cursor.parent

                if not valid:
                    raise AssignmentError(
                        "{}:{}: cannot create reference: '{}' outlives '{}'".format(
                            node.__meta__['start'][0],
                            node.__meta__['start'][1],
                            node.lvalue,
                            node.rvalue
                        ))

                # If all checks passed, we can mark th lvalue as initialized,
                # but not owner of its value.
                self[lvalue] = _Resource(1, False, rvalue)

                # TODO Check for escapes by closure capture.

            if node.operator == '<-':
                if isinstance(rvalue, Identifier):
                    # If the rvalue is an identifier, it should hold ownership
                    # of its value.
                    if not self[rvalue].ownership:
                        raise AssignmentError(
                            "{}:{}: cannot move: '{}' does not hold ownership".format(
                                node.__meta__['start'][0],
                                node.__meta__['start'][1],
                                rvalue.name
                            ))

                    # Mark the lvalue as initialized, and the rvalue as moved.
                    self[lvalue] = _Resource(1, True, lvalue)
                    self[rvalue] = _Resource(2, False)

                else:
                    # If the rvalue is an expression, we simply mark the
                    # lvalue as initialized, and owner of its value.
                    self[lvalue] = _Resource(1, True, lvalue)


    def check_preconditions(self, node):
        if isinstance(node, Literal):
            return True

        if isinstance(node, Identifier):
            resource = self.variables.get((node.__meta__['scope'], node.name), _Resource(0, False))
            if resource.state == 0:
                raise AssignmentError(
                    "{}:{}: symbol '{}' used before being initialized".format(
                        node.__meta__['start'][0],
                        node.__meta__['start'][1],
                        node.name
                    ))
            return True

        assert False, "no rule for node '{}'".format(node.__class__.__name__)

    def _var_id(self, node):
        return (node.__meta__['scope'], node.name)
