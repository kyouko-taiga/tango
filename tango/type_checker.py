from itertools import product

from .ast import *
from .builtin import builtin_scope
from .errors import UndefinedSymbol, InferenceError
from .types import BaseType, FunctionType, TypeUnion


class Substitution(object):

    def __init__(self, storage=None):
        self.storage = storage or {}

    def __getitem__(self, t):
        if not isinstance(t, TypeVariable):
            return t
        if t in self.storage:
            return self[self.storage[t]]
        return t

    def __setitem__(self, variable, value):
        self.storage[variable] = value

    def __contains__(self, variable):
        return variable in self.storage

    def unify(self, t, u):
        a = self[t]
        b = self[u]

        if isinstance(a, TypeVariable):
            self.storage[a] = b
        elif isinstance(b, TypeVariable):
            self.storage[b] = a

        elif isinstance(a, TypeUnion) and isinstance(b, TypeUnion):
            results = []
            for ita, itb in product(a, b):
                if matches(ita, itb):
                    self.unify(ita, itb)
                    results.append(ita)

            if not results:
                raise InferenceError("no types in '%s' matches a type in '%s'" % (a, b))
            a.replace_content(results)
            b.replace_content(results)

        elif isinstance(a, TypeUnion) and isinstance(b, BaseType):
            for it in a:
                if matches(it, b):
                    self.unify(it, b)
                    a.replace_content((it,))
                    break
            else:
                raise InferenceError("no type in '%s' matches '%s'" % (a, b))

            # TODO When we'll implement abstract types and/or protocols, we
            # might have to to unify multiple result instead of simply the
            # first type that matches in the union.

        elif isinstance(b, TypeUnion) and isinstance(a, BaseType):
            self.unify(b, a)

        elif isinstance(a, BaseType) and isinstance(b, BaseType):
            if a != b:
                raise InferenceError("type '%s' does not match '%s'" % (a, b))

        # TODO unify abstract types and generic functions
        # TODO (?) check for types mutability

        else:
            assert False, 'cannot unify %r and %r' % (a, b)

    def reified(self):
        result = Substitution()
        for variable in self.storage:
            walked = self[variable]
            result[variable] = walked
        return result


class TypeVariable(object):

    next_id = 0

    def __init__(self, id=None):
        if id is None:
            self.id = TypeVariable.next_id
            TypeVariable.next_id += 1
        elif isinstance(id, Node):
            self.id = (id.__info__['scope'], id.name)
        else:
            self.id = id

    def __hash__(self):
        return hash(self.id)

    def __eq__(self, other):
        return self.id == other.id

    def __str__(self):
        return '$%i' % hash(self)


def infer_types(node):
    # We first visit all nodes to infer the type of the expressions.
    type_deducer = TypeSolver()

    # FIXME We should compute a fixed point here.
    type_deducer.visit(node)
    type_deducer.visit(node)

    return type_deducer.environment.reified().storage


class TypeSolver(Visitor):

    def __init__(self):
        self.environment = Substitution({
            TypeVariable(id=(builtin_scope, symbol)): type
            for symbol, type in builtin_scope.members.items()
        })

    def eval_type_signature(self, signature):
        if isinstance(signature, BaseType):
            return signature

        if isinstance(signature, TypeIdentifier):
            try:
                return self.environment[TypeVariable(signature)]
                # TODO Handle generic parameters
            except KeyError:
                raise UndefinedSymbol(signature.name)

        assert False, 'refine_type_signature(%s)' % signature.__class__.__name__

    def visit_Block(self, node):
        # We introduce a new type variable for each symbol declared within the
        # current block before visiting its statements, so we can handle cases
        # where a variable refers a type that has yet to be defined.
        scope = node.__info__['scope']
        for symbol in node.__info__['symbols']:
            var = TypeVariable((scope, symbol))
            if var not in self.environment:
                self.environment[var] = TypeVariable()

        for statement in node.statements:
            self.visit(statement)

    def visit_ConstantDecl(self, node):
        # If there isn't neither a type annotation, nor an initializing value,
        # we can't infer any type information.
        if not (node.type_annotation or node.initial_value):
            return node

        inferred = None

        # If there's an initializing value, we have to infer its type first.
        if node.initial_value:
            initial_value_type = analyse(node.initial_value, self.environment)

            # If there's a type annotation as well, we should unify the type
            # it denotes with the one that was inferred from the initializing
            # value. This will not only check that the types match, but will
            # also try to infer specialization arguments of abstract types.
            if node.type_annotation:
                type_annotation = self.eval_type_signature(node.type_annotation)
                self.environment.unify(type_annotation, initial_value_type)

            inferred = initial_value_type

        # If there isn't an initializing value, we should simply use the type
        # annotation.
        else:
            inferred = self.eval_type_signature(node.type_annotation)

        self.environment.unify(TypeVariable(node), inferred)
        return node

    def visit_VariableDecl(self, node):
        return self.visit_ConstantDecl(node)

    def visit_FunctionDecl(self, node):
        # The return type is simply a type signature we've to evaluate.
        return_type = self.eval_type_signature(node.signature.return_type)

        # Function parameters can be seen as a container declaration within the
        # function's scope. As such, if we can use a similar strategy as the
        # one we use for the the case of container declarations.
        parameter_types = []
        for parameter in node.signature.parameters:
            # Unlike container declarations, type parameters always have a
            # type annotation.
            type_annotation = self.eval_type_signature(parameter.type_annotation)

            # If the parameter also have a default value, we should unify it
            # with the type we've evaluated from the annotation.
            if parameter.default_value:
                default_value_type = analyse(parameter.default_value, self.environment)
                self.environment.unify(type_annotation, default_value_type)

            self.environment.unify(TypeVariable(parameter), type_annotation)
            parameter_types.append(type_annotation)

        # Once we've inferred the types of the function signature, we can
        # create a type for the function itself.
        function_type = FunctionType(
            domain = parameter_types,
            codomain = return_type,
            labels = [parameter.label for parameter in node.signature.parameters])

        # As functions may be overloaded, we can't unify the function type
        # we've just created with the function's name directly. Instead, we
        # should create a TypeUnion to potentially include the signature of
        # the function's overloads as we find them.
        function_name = TypeVariable(node)
        walked = self.environment[function_name]
        if isinstance(walked, TypeVariable):
            overload_set = TypeUnion((function_type,))
            self.environment[walked] = overload_set
        elif isinstance(walked, TypeUnion) and isinstance(walked.first(), FunctionType):
            overload_set = walked
            overload_set.add(function_type)
        else:
            raise InferenceError("cannot overload '%s' with '%s'" % (walked, function_type))

        # If the function name is also associated with function types in the
        # enclosing scopes, we should add the latter as overloads.
        for overload in find_overload_decls(node.name, node.__info__['scope']):
            self.visit(overload)
            overload_set.add(self.environment[TypeVariable(overload)])

        # Finally, we should continue the type inference in the function body.
        self.visit(node.body)

    def visit_Assignment(self, node):
        # First, we infer and unify the types of the lvalue and rvalue.
        lvalue_type = analyse(node.lvalue, self.environment)
        rvalue_type = analyse(node.rvalue, self.environment)
        self.environment.unify(lvalue_type, rvalue_type)

        # If the lvalue is an identifer, we unify its type with that of the
        # rvalue in our environment.
        if isinstance(node.lvalue, Identifier):
            self.environment.unify(TypeVariable(node.lvalue), rvalue_type)
            return node

        # Note that for now, only identifiers are valid lvalues.
        assert False, '%s is not a valid lvalue' % node.target.__class__.__name__


def analyse(node, environment):
    # If the node is a literal, its type was already inferred by the parser.
    if isinstance(node, Literal):
        return node.__info__['type']

    # If the node is an identifier, we should simply try to get its type from
    # the environment.
    if isinstance(node, Identifier):
        try:
            return environment[TypeVariable(node)]
        except KeyError:
            raise UndefinedSymbol(node.name)

    if isinstance(node, BinaryExpression):
        operator_signatures = environment

    if isinstance(node, Call):
        # First, we have to get the available signatures of for the callee.
        callee_signatures = analyse(node.callee, environment)

        # As functions may be overloaded, we group their different signatures
        # in a TypeUnion. As a result, we expect `callee_signatures` to be
        # iterable. But since it might be possible that we try to infer the
        # type of a function call before its signature has been inferred, we
        # should handle cases where `callee_signatures` is still a variable.
        if isinstance(callee_signatures, TypeVariable):
            return TypeVariable()

        if not isinstance(callee_signatures, TypeUnion):
            assert False, 'expected TypeUnion of FunctionType'
        if not isinstance(callee_signatures.first(), FunctionType):
            raise InferenceError("'%s' is not a function type" % callee_signatures.first())

        # Then, we have to infer the type of each argument.
        argument_types = []
        for argument in node.arguments:
            argument_types.append(analyse(argument.value, environment))

        # Then, we have to find which signatures agree with the types we've
        # inferred for the function arguments.
        candidates = []
        for signature in callee_signatures:
            # Check the number of parameters.
            if len(signature.domain) != len(node.arguments):
                continue

            # Check the labels of parameters.
            valid = True
            for expected_label, argument in zip(signature.labels, node.arguments):
                if (expected_label == '_') and (argument.name is not None):
                    valid = False
                    break
                elif expected_label != argument.name:
                    valid = False
                    break
            if not valid:
                continue

            # Check the types of the parameters.
            for expected_type, argument_type in zip(signature.domain, argument_types):
                if not matches(expected_type, argument_type):
                    valid = False
                    break
            if not valid:
                continue

            candidates.append(signature)

        # TODO Handle multiple candidates
        if len(candidates) == 0:
            raise InferenceError(
                "function call do not match any candidate for types '%s'" % callee_signatures)
        elif len(candidates) == 1:
            return candidates[0].codomain
        else:
            return TypeUnion(candidate.codomain for candidate in candidates)

        # TODO Handle variadic arguments
        # A possible approach would be to transform the Call nodes of the AST
        # whenever we visit a variadic parameter, so that we regroup the
        # arguments that can be considered part of the variadic argument.
        # Depending on the definitive syntax and restrictions we'll adopt for
        # variadics parameters, we might have to check multiple configurations
        # of argument groups.

    assert False, "no type inference for node '%s'" % node.__class__.__name__


def find_overload_decls(name, scope):
    if scope.parent and (name in scope.parent):
        if any(not isinstance(decl, FunctionDecl) for decl in scope.parent[name]):
            return []
        return scope.parent[name] + find_overload_decls(name, scope.parent.parent)
    return []

def matches(t, u):
    if isinstance(t, TypeVariable):
        return True
    if isinstance(u, TypeVariable):
        return True
    if isinstance(t, TypeUnion):
        return any(matches(it, u) for it in t)
    if isinstance(u, TypeUnion):
        return matches(u, t)
    return t == u
