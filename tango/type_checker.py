from itertools import product

from .ast import Visitor
from .builtin import Type, builtin_scope
from .errors import UndefinedSymbol, InferenceError
from .parser import *


class TypeVariable(object):

    next_id = 0

    def __init__(self):
        self.id = TypeVariable.next_id
        TypeVariable.next_id += 1

        # The type with which the variable will be eventually instanciated.
        self.instance = None

    def __str__(self):
        if self.instance is None:
            return '$%i' % self.id
        else:
            return str(self.instance)


def infer_types(node):
    # We first visit all nodes to infer the type of the expressions.
    type_deducer = TypeDeducer()
    type_deducer.visit(node)

    return type_deducer.environment


class TypeDeducer(Visitor):

    def __init__(self):
        # (Scope, String) -> [Type]
        self.environment = {
            (builtin_scope, symbol): types
            for symbol, types in builtin_scope.members.items()
        }

    def visit_ConstantDecl(self, node):
        inferred = analyse(node, self.environment)
        self.environment[(node.name.__info__['scope'], node.name.name)] = inferred

    def visit_VariableDecl(self, node):
        inferred = analyse(node, self.environment)
        self.environment[(node.name.__info__['scope'], node.name.name)] = inferred

    def visit_Assignment(self, node):
        analyse(node, self.environment)


def analyse(node, environment):

    # If the node is an identifier, we should simply try to get its type from
    # the environment.
    if isinstance(node, Identifier):
        try:
            return environment[(node.__info__['scope'], node.name)]
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
        return [node.__info__['type']]

    # If the node is an assignment, we should infer and unify the type of both
    # sides' expressions.
    if isinstance(node, Assignment):
        target_types = analyse(node.target, environment)
        value_types = analyse(node.value, environment)
        return unify(target_types, value_types)

    if instance(node, BinaryExpression):
        operator_signatures = environment

    assert False, "no type inference for node '%s'" % node.__class__.__name__


def unify(lhs, rhs):
    results = []
    for (t, u) in product(lhs, rhs):
        try:
            results.extend(unify_single(t, u))
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

    # If the left operand is a type variable, then we instanciate it with the
    # right operand.
    if isinstance(a, TypeVariable):
        if a != b:
            # TODO Check for recursive unifications
            a.instance = b
        return [a]

    if isinstance(b, TypeVariable):
        return unify_single(b, a)

    # If only one operand is a type identifier, then have to unify the type
    # it refers to with the other operand. Note that since an identifier may
    # refer to multiple types (because of overloading), we have to unify all
    # possible referred types.
    if isinstance(a, TypeIdentifier):
        walked = a.name.__info__['scope'][a.name.name]
        return unify(walked, [b])

    if isinstance(b, TypeIdentifier):
        return unify_single(b, a)

    # If both operands are resolved types, then we simply have to make sure
    # they are equivalent.
    if isinstance(a, Type) and isinstance(b, Type):
        if a != b:
            raise InferenceError("types '%s' and '%s' doesn't match" % (a. b))
        return [a]

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
