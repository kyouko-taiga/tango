from itertools import product

from .ast import Visitor
from .builtin import builtin_scope
from .errors import UndefinedSymbol, InferenceError
from .parser import *
from .types import BaseType, FunctionType


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

    def visit_FunctionDecl(self, node):
        # We first need to infer the types of the function parameters.
        parameter_types = []
        for parameter in node.signature.parameters:
            inferred = analyse(parameter, self.environment)
            self.environment[(parameter.name.__info__['scope'], parameter.name.name)] = inferred
            parameter_types.append(inferred)

        # Once we've inferred the types of the function signature, we can
        # create a type for the function itself.
        return_types = [analyse(node.signature.return_type, self.environment)]
        function_types = []
        for types in product(*(parameter_types + return_types)):
            function_types.append(FunctionType(
                domain = types[0:-1],
                codomain = types[-1]))
            # TODO Handle generic parameters

        key = (node.name.__info__['scope'], node.name.name)
        if not key in self.environment:
            self.environment[key] = []
        self.environment[key].extend(function_types)

        # Finally, we should continue the type inference in the function body.
        self.visit(node.body)

    def visit_Assignment(self, node):
        analyse(node, self.environment)


def analyse(node, environment):
    # If the node is a literal, its type was already inferred by the parser.
    if isinstance(node, Literal):
        return [node.__info__['type']]

    # If the node is an identifier, we should simply try to get its type from
    # the environment.
    if isinstance(node, Identifier):
        try:
            return environment[(node.__info__['scope'], node.name)]
        except KeyError:
            raise UndefinedSymbol(node.name)

    # If the node is a type identifier, we should try to get the type it
    # refers to from the its associated scope.
    if isinstance(node, TypeIdentifier):
        try:
            return environment[(node.name.__info__['scope'], node.name.name)]
            # TODO Handle generic parameters
        except KeyError:
            raise UndefinedSymbol(node.name)

    # If the node is a container declaration, we either can infer its type
    # from the definition (and the environment), or we introduce a new type
    # variable.
    if isinstance(node, (ConstantDecl, VariableDecl)):
        # If there isn't neither a type annotation, nor an initializing value,
        # we have no choice but to introduce a new type variable.
        if not (node.type_annotation or node.initial_value):
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

        # If there isn't an initializing value, we should simply return the
        # type annotation.
        return analyse(node.type_annotation, environment)

    # Function parameters can be seen as a container declaration within the
    # function's scope. As such, if the node is a function parameter, we can
    # use a similar strategy as for the the case of container declarations.
    if isinstance(node, FunctionParameter):
        # If there's a default value, we have to infer its type first, and
        # unify it with the parameter's type annotation.
        if node.default_value:
            default_value_types = analyse(node.default_value, environment)
            return unify([node.type_annotation], default_value_types)

        # If there isn't any default value, we should simply return the type
        # annotation.
        return [node.type_annotation]

    # If the node is an assignment, we should infer and unify the type of both
    # sides' expressions.
    if isinstance(node, Assignment):
        target_types = analyse(node.target, environment)
        value_types = analyse(node.value, environment)
        return unify(target_types, value_types)

    if isinstance(node, BinaryExpression):
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
    if isinstance(a, BaseType) and isinstance(b, BaseType):
        if a != b:
            raise InferenceError("types '%s' and '%s' doesn't match" % (a, b))
        return [a]

    # TODO unify abstract types and generic functions
    # TODO (?) check for types mutability

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
