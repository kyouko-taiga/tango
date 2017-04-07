from itertools import product

from .ast import Visitor
from .parser import *


class MissingTypeAnnotation(Exception):
    pass

class InferenceError(Exception):
    pass

class UndefinedSymbol(Exception):
    pass


class TypeVariable(object):

    next_id = 0

    def __init__(self):
        self.id = TypeVariable.next_id
        TypeVariable.next_id += 1

        # The type with which the variable will be eventually instanciated.
        self.instance = None


def infer_types(node):
    # We first visit all nodes to infer the type of the expressions.
    type_deducer = TypeDeducer()
    type_deducer.visit(node)

    return type_deducer.environment


class TypeDeducer(Visitor):

    def __init__(self):
        # (Scope, String) -> [Type]
        self.environment = {
            None: {},
        }

    def visit_ConstantDecl(self, node):
        inferred = analyse(node, self.environment)
        self.environment[(node.name.scope, node.name.name)] = inferred

    def visit_VariableDecl(self, node):
        inferred = analyse(node, self.environment)
        self.environment[(node.name.scope, node.name.name)] = inferred

    def visit_Assignment(self, node):
        analyse(node, self.environment)


def analyse(node, environment):

    # If the node is an identifier, we should simply try to get its type from
    # the environment.
    if isinstance(node, Identifier):
        try:
            return environment[(node.scope, node.name)]
        except KeyError:
            raise UndefinedSymbol(node.name)

    # If the node is a container declaration, we either can infer its type
    # from the definition (and the environment), or we introduce a new type
    # variable.
    if isinstance(node, (ConstantDecl, VariableDecl)):
        # If there isn't neither a type annotation, nor an initializing value,
        # we have no choice but to introduce a new type variable.
        if not (node.type_annotation or node.initial_value):
            identifier = node.name
            return [TypeVariable()]

        # If there's an initializing value, we have to infer its type first.
        if node.initial_value:
            initial_value_types = analyse(node.initial_value, environment)

            # If there's a type annotation as well, we should unify the type
            # it denotes with the one that was inferred from the initializing
            # value. This will not only check that the types match, but will
            # also try to infer specialization arguments of abstract types.
            if node.type_annotation:
                return unify([node.type_annotation], initial_value_types)

            return initial_value_types

        # If there isn't an initializing value, the we should return the type
        # annotation.
        return [node.type_annotation]

    # If the node is a literal, its type was already inferred by the parser.
    if isinstance(node, Literal):
        return [node.type]

    # If the node is an assignment, we should infer and unify the type of both
    # sides' expressions.
    if isinstance(node, Assignment):
        target_types = analyse(node.target, environment)
        value_types = analyse(node.value, environment)
        return unify(target_types, value_types)

    if instance(node, BinaryExpression):
        operator_signatures = environment[]

    assert False, "no type inference for node '%s'" % node.__class__.__name__


def unify(lhs, rhs):
    results = []
    for (t, u) in product(lhs, rhs):
        try:
            unify_single(t, u)
            results.append(t)
        except InferenceError:
            pass

    if not results:
        raise InferenceError(
            'no match found for [%s] x [%s]' %
            (', '.join(map(str, lhs)), ', '.join(map(str, rhs))))

    return results


def unify_single(lhs, rhs):
    a = prune(lhs)
    b = prune(rhs)

    # If the left operand if a type variable, then we instanciate it with the
    # right operand.
    if isinstance(a, TypeVariable):
        if a != b:
            # TODO Check for recursive unifications
            a.instance = b

    # If the right operand is a type variable (and the left one is not), we
    # instanciate it with the left operand.
    elif isinstance(b, TypeVariable):
        unify(b, a)

    # If both left and right operands aren't type variables, we have to unify
    # their specialization parameters (if any), and check if they match.
    elif isinstance(a, TypeIdentifier) and isinstance(b, TypeIdentifier):
        if (a.name.name != b.name.name) or (a.name.scope != b.name.scope):
            raise InferenceError("type '%s' doesn't match type '%s'" % (a, b))

        # TODO unify abstract types and generic functions

    else:
        assert False, 'not unified'


def prune(type_object):
    """
    Returns the actual instance of `type_object`.

    If the given `type_object` is a type variable, this function will walk the
    `TypeVariable.instance` property until it finds a type instance (or None).
    As a side effect, it will also cut unecessary indirections.
    """
    if isinstance(type_object, TypeVariable):
        if type_object.instance is not None:
            type_object.instance = prune(type_object.instance)
            return type_object

    return type_object
