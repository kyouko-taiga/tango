from collections import OrderedDict
from itertools import product
from warnings import warn

from .ast import *
from .builtin import Type, builtin_scope
from .errors import UndefinedSymbol, InferenceError
from .types import BaseType, FunctionType, GenericType, StructType, TypeUnion


def infer_types(node, max_iter=100):
    type_solver = TypeSolver()
    type_solver.visit(node)
    types_finder = TypesFinder(type_solver.environment)
    types_finder.visit(node)

    i = 0
    while True:
        type_solver.visit(node)
        previous = types_finder.types
        types_finder.types = {}
        types_finder.visit(node)
        if types_finder.types == previous:
            break

        i += 1
        if i > max_iter:
            raise InferenceError('could not reach a fixed point after %s iterations' % max_iter)

    return type_solver.environment.reified().storage


class TypesFinder(Visitor):

    def __init__(self, environment):
        self.types = {}
        self.environment = environment

    def visit(self, node):
        if 'type' in node.__info__:
            # NOTE If we give the type solver the ability to transform the
            # AST, we might have to find another way to "hash" the nodes.
            self.types[id(node)] = self.environment.deepwalk(node.__info__['type'])

        self.generic_visit(node)


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
        return (type(self) == type(other)) and (self.id == other.id)

    def __str__(self):
        return '$%i' % hash(self)


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

        if a == b:
            pass

        elif isinstance(a, TypeVariable):
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

        elif isinstance(a, StructType) and isinstance(b, StructType):
            if (a.name != b.name) or (a.members.keys() ^ b.members.keys()):
                raise InferenceError("type '%s' does not match '%s'" % (a, b))
            for it in a.members:
                self.unify(a.members[it], b.members[it])

        elif isinstance(a, BaseType) and isinstance(b, BaseType):
            if a != b:
                raise InferenceError("type '%s' does not match '%s'" % (a, b))

        # TODO unify abstract types and generic functions
        # TODO take conformance into account

        else:
            assert False, 'cannot unify %r and %r' % (a, b)

    def deepwalk(self, t):
        if isinstance(t, TypeUnion):
            result = TypeUnion(self.deepwalk(it) for it in t)
            # Avoid creating singletons when there's only one candidate.
            if len(result) == 1:
                return result.first()
            return result

        if isinstance(t, FunctionType):
            return FunctionType(
                domain = [self.deepwalk(p) for p in t.domain],
                codomain = self.deepwalk(t.codomain),
                labels = t.labels,
                generic_parameters = [
                    (name, self.deepwalk(value))
                    for name, value in t.generic_parameters.items()
                ])

        if not isinstance(t, TypeVariable):
            return t

        if t in self.storage:
            return self.deepwalk(self.storage[t])

        return t

    def reified(self):
        result = Substitution()
        for variable in self.storage:
            walked = self.deepwalk(variable)
            result[variable] = walked
        return result

    def print_debug(self):
        for (symbol, inferred_type) in self.storage.items():
            if isinstance(symbol.id, tuple):
                scope, name = symbol.id
                if scope.id == 0:
                    continue
                print('{:25}{:15}{:}'.format(
                    str(symbol),
                    scope.uri + '.' + name,
                    hex(id(inferred_type)) + ' ' + str(inferred_type)))
            else:
                print('{:25}{:15}{:}'.format(
                    str(symbol),
                    '-',
                    hex(id(inferred_type)) + ' ' + str(inferred_type)))


class TypeSolver(Visitor):

    def __init__(self):
        # A simple substitution map: (TypeVariable) -> Type.
        self.environment = Substitution({
            TypeVariable(id=(builtin_scope, symbol)): Type
            for symbol, obj in builtin_scope.members.items() if isinstance(obj, BaseType)
        })

        # The store of nominal types: (Scope, String) -> Type.
        self.nominal_types = {
            (builtin_scope, symbol): obj
            for symbol, obj in builtin_scope.members.items() if isinstance(obj, BaseType)
        }

        # Methods, properties and enum cases may use `Self` as a placeholder
        # to denote the "final" type they're defined in. This stack will serve
        # us to keep track of the actual type `Self` represents, depending on
        # the context we'll be evaluation typing informations.
        self.current_self_type = []

    def type_or_container_type(self, identifier):
        # If a nominal type in the identifer's scope is named after the
        # identifier, we treat the identifier as a type name.
        if identifier.name == 'Self':
            try:
                return self.current_self_type[-1]
            except IndexError:
                raise InferenceError("invalid use of 'Self' outside of a type declaration")

        key = (identifier.__info__['scope'], identifier.name)
        if key in self.nominal_types:
            return self.nominal_types[key]

        # Otherwise, we treat the identifier as a usual container's name.
        return self.environment[TypeVariable(identifier)]

    def eval_type_signature(self, signature):
        if isinstance(signature, BaseType):
            return signature

        # If the signature is an AST node, maybe we already parsed it.
        if isinstance(signature, Node) and ('type' in signature.__info__):
            return signature.__info__['type']

        # If the signature is a type identifier, we should get it from the
        # store of nominal types.
        if isinstance(signature, TypeIdentifier):
            if signature.name == 'Self':
                try:
                    result = self.current_self_type[-1]
                except IndexError:
                    raise InferenceError("invalid use of 'Self' outside of a type declaration")
            else:
                try:
                    result = self.nominal_types[(signature.__info__['scope'], signature.name)]
                except KeyError:
                    raise UndefinedSymbol(signature.name)

            signature.__info__['type'] = result
            return result

            # TODO Handle abstract types

        # If the signature is a type function, we should build a FunctionType.
        if isinstance(signature, FunctionSignature):
            parameter_types = [
                self.eval_type_signature(p.type_annotation)
                for p in signature.parameters
            ]
            return_type = self.eval_type_signature(signature.return_type)

            function_type = FunctionType(
                domain = parameter_types,
                codomain = return_type,
                labels = [p.label for p in signature.parameters])

            signature.__info__['type'] = function_type
            return function_type

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
            return

        # If there's an initializing value, we have to infer its type first.
        if node.initial_value:
            initial_value_type = self.analyse(node.initial_value)

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

        # Finally, we should unify the inferred type with the type variable
        # corresponding to the symbol under declaration.
        self.environment.unify(TypeVariable(node), inferred)

    def visit_VariableDecl(self, node):
        return self.visit_ConstantDecl(node)

    def visit_FunctionDecl(self, node):
        # First, we should create (unless we already did) a generic type for
        # each of the function's generic parameters (if any), to populate the
        # nominal types store.
        member_scope = node.body.__info__['scope']
        generic_parameters = OrderedDict()
        for symbol in node.generic_parameters:
            if (member_scope, symbol) not in self.nominal_types:
                self.nominal_types[(member_scope, symbol)] = GenericType(symbol)
            generic_parameters[symbol] = self.nominal_types[(member_scope, symbol)]

        # Unlike container declarations, function parameters always have a
        # type annotation, so we can type them directly.
        parameter_types = []
        for parameter in node.signature.parameters:
            type_annotation = self.eval_type_signature(parameter.type_annotation)
            parameter_types.append(type_annotation)

            # We should unify the type we read from the annotations with the
            # type variable corresponding to the parameter name, so that it'll
            # be typed in the function's body.
            self.environment.unify(TypeVariable(parameter), type_annotation)

            # Function parameters may be associated with a default value. In
            # those instances, we should infer the type of the initializing
            # expression and unify it with that of the parameter's annotation.
            if parameter.default_value:
                default_value_type = self.analyse(parameter.default_value)
                self.environment.unify(type_annotation, default_value_type)

        # The return type is simply a type signature we've to evaluate.
        return_type = self.eval_type_signature(node.signature.return_type)

        # Once we've computed the function signature, we can create a type
        # for the function itself.
        function_type = FunctionType(
            domain = parameter_types,
            codomain = return_type,
            labels = [parameter.label for parameter in node.signature.parameters],
            generic_parameters = generic_parameters)

        # As functions may be overloaded, we can't unify the function type
        # we've created with the function's name directly. Instead, we should
        # put it in a TypeUnion to potentially include the signature of the
        # function's overloads, as we find them.
        walked = self.environment[TypeVariable(node)]
        if isinstance(walked, TypeVariable):
            overload_set = TypeUnion((function_type,))
            self.environment.unify(walked, overload_set)
        elif isinstance(walked, TypeUnion) and isinstance(walked.first(), FunctionType):
            overload_set = walked
            overload_set.add(function_type)
        else:
            raise InferenceError("cannot overload '%s' with '%s'" % (walked, function_type))

        # If the function name is also associated with function types in the
        # enclosing scopes, we should add the latter as overloads.
        for overload in find_overload_decls(node.name, node.__info__['scope']):
            overload_set.add(TypeVariable(overload))

        # Finally, we should continue the type inference in the function body.
        self.visit(node.body)

    def visit_StructDecl(self, node):
        # First, we should create a new type for the struct, using fresh type
        # variables for each of the symbols it defines, and keep it in the
        # nominal types store.
        member_scope = node.body.__info__['scope']
        struct_type = StructType(
            name = node.name,
            members = {
                name: self.environment[TypeVariable((member_scope, name))]
                for name in node.body.__info__['symbols']
            })
        self.nominal_types[(node.__info__['scope'], node.name)] = struct_type

        # The struct type itself should be typed with `Type`.
        self.environment.unify(TypeVariable(node), Type)

        # The body of a struct can be visited as a normal satement block, as
        # long as we push a variable on the `current_self_type` stack before,
        # to properly type references to `Self`.
        self.current_self_type.append(struct_type)
        self.visit(node.body)
        self.current_self_type.pop()

    def visit_Assignment(self, node):
        # First, we infer and unify the types of the lvalue and rvalue.
        lvalue_type = self.analyse(node.lvalue)
        rvalue_type = self.analyse(node.rvalue)
        self.environment.unify(lvalue_type, rvalue_type)

        # If the lvalue is an identifer, we unify its name with the type of
        # the rvalue.
        if isinstance(node.lvalue, Identifier):
            self.environment.unify(TypeVariable(node.lvalue), rvalue_type)
            return

        # Note that for now, only identifiers are valid lvalues.
        assert False, '%s is not a valid lvalue' % node.target.__class__.__name__

    def analyse(self, node):
        # If the node is a literal, its type should already have been inferred
        # by the parser.
        if isinstance(node, Literal):
            return node.__info__['type']

        # If the node is an identifier, we should simply try to get its type
        # from the environment.
        if isinstance(node, Identifier):
            try:
                node.__info__['type'] = self.environment[TypeVariable(node)]
                return node.__info__['type']
            except KeyError:
                raise UndefinedSymbol(node.name)

        if isinstance(node, Select):
            # If the owner is an identifier, we might have to treat it as a
            # type name rather than a container's name.
            if isinstance(node.owner, Identifier):
                owner_types = self.type_or_container_type(node.owner)

            # If it's any other kind of expression, we have to infer its type.
            else:
                owner_types = self.analyse(node.owner)

            # If the result we got is a type variable, we have no choice but
            # to return another type variable.
            if isinstance(owner_types, TypeVariable):
                return TypeVariable()

            # If the result we got is an actual type (or union of), we should
            # look for a member with the requested name in its definition.
            if not isinstance(owner_types, TypeUnion):
                owner_types = (owner_types,)

            candidates = []
            for owner_type in owner_types:
                if isinstance(owner_type, TypeVariable):
                    candidates.append(TypeVariable())
                elif isinstance(owner_type, BaseType) and (node.member.name in owner_type.members):
                    candidates.append(owner_type.members[node.member.name])

            if len(candidates) == 0:
                raise InferenceError(
                    "no candidates in %s has a member '%s'" % (owner_types, node.member.name))

            if len(candidates) == 1:
                result = self.environment[candidates[0]]
            else:
                result = TypeUnion(self.environment[candidate.codomain] for candidate in candidates)

            node.__info__['type'] = result
            return result

        if isinstance(node, BinaryExpression):
            operator_signatures = self.environment

        if isinstance(node, Call):
            # First, we have to get the available signatures for the callee.
            callee_signatures = self.analyse(node.callee)

            # Since it might be possible that we try to infer the type of a
            # function call before its signature has been inferred, we should
            # handle cases where `callee_signatures` is still a variable.
            if isinstance(callee_signatures, TypeVariable):
                return TypeVariable()

            # Since functions may be overloaded, we group their different
            # signatures in a TypeUnion. As a result, we can expect
            # `callee_signatures` to be iterable.
            assert isinstance(callee_signatures, TypeUnion), 'expected TypeUnion of FunctionType'
            if not isinstance(callee_signatures.first(), FunctionType):
                raise InferenceError("'%s' is not a function type" % callee_signatures.first())

            # Then, we have to infer the type of each argument.
            argument_types = []
            for argument in node.arguments:
                argument_types.append(self.analyse(argument.value))

            # Then, we have to find which signatures agree with the types
            # we've inferred for the function arguments.
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

                # NOTE Using profiling, we might determine that it could s be
                # more efficient to already eliminate candidates whose domain
                # or codomain doesn't match the argument and return types of
                # our node here, before they are specialized.

                candidates.append(signature)

            # Specialize the generic arguments of the candidates with the
            # argument types we inferred.
            specialized_candidates = (specialize(c, argument_types) for c in candidates)
            candidates = (c for specialized in specialized_candidates for c in specialized)

            # Check the specialized candidate to filter out those whose
            # signature doesn't match the node's arguments or return type.
            selected_candidates = []
            for signature in candidates:
                for expected_type, proposed_type in zip(signature.domain, argument_types):
                    if isinstance(expected_type, GenericType):
                        expected_type = signature.specialized_parameter(expected_type.name)
                    if not matches(expected_type, proposed_type):
                        valid = False
                        break
                if not valid:
                    continue

                if 'type' in node.__info__:
                    expected_type = signature.codomain
                    proposed_type = node.__info__['type']
                    if isinstance(expected_type, GenericType):
                        expected_type = signature.specialized_parameter(expected_type.name)
                    if not matches(expected_type, proposed_type):
                        continue

                selected_candidates.append(signature)

            if len(selected_candidates) == 0:
                raise InferenceError(
                    "function call do not match any candidate for types '%s'" % callee_signatures)

            # Unify the argument types of the node with the domain of the
            # selected candidates, to propagate type constraints.
            for i, argument_type in enumerate(argument_types):
                candidate_domains = TypeUnion()
                for candidate in selected_candidates:
                    # If the candidate argument type is generic, we should try
                    # to find its replacement in the candidate's
                    # specialization. Otherwise we simply use the type of the
                    # candidate argument.
                    if isinstance(candidate.domain[i], GenericType):
                        candidate_domains.add(
                            candidate.specialized_parameter(candidate.domain[i].name))
                    else:
                        candidate_domains.add(candidate.domain[i])

                # We have to make a copy of the type union we created here to
                # avoid introducing circular substitutions in the environment.
                # This could happen if `cnadidate_domains` references
                # `argument_type` somewhere in the hierarchy.
                candidate_domains = candidate_domains.copy()
                self.environment.unify(candidate_domains, argument_type)

            result = TypeUnion()
            for candidate in selected_candidates:
                codomain = candidate.codomain
                if isinstance(codomain, GenericType):
                    codomain = candidate.specialized_parameter(codomain.name)
                result.add(self.environment[codomain])

            # Avoid creating singletons when there's only one candidate.
            if len(result) == 1:
                result = result.first()

            node.__info__['type'] = result
            return result

            # TODO Handle variadic arguments
            # A possible approach would be to transform the Call nodes of the
            # AST whenever we visit a variadic parameter, so that we regroup
            # the arguments that can be considered part of the variadic
            # argument. Depending on the definitive syntax and restrictions
            # we'll adopt for variadics parameters, we might have to check
            # multiple configurations of argument groups.

        assert False, "no type inference for node '%s'" % node.__class__.__name__


def find_overload_decls(name, scope):
    if scope.parent and (name in scope.parent):
        if any(not isinstance(decl, FunctionDecl) for decl in scope.parent[name]):
            return []
        return scope.parent[name] + find_overload_decls(name, scope.parent.parent)
    return []


def specialize(signature, specialized_argument_types):
    # There's noting to do if the signature doesn't have generic parameters.
    if not signature.generic_parameters:
        yield signature
        return

    generics = signature.generic_parameters

    # Create the set of all possible combination of arguments, that is the
    # cartesian product `a0 x a1 x ... an`, considering all `ai` that aren't
    # type unions to be singleton of themselves.
    choices = [ai if isinstance(ai, TypeUnion) else (ai,) for ai in specialized_argument_types]
    argument_types_combinations = product(*choices)

    # Specialize the generic types used in the function parameters for each
    # possible combination of arguments.
    specialized_signatures = []
    for combination in argument_types_combinations:
        specializations = {}
        for original, replacement in zip(signature.domain, combination):
            if isinstance(original, GenericType):
                assert specializations.get(original.name) in (None, replacement)
                specializations[original.name] = replacement

        yield FunctionType(
            domain             = signature.domain,
            codomain           = signature.codomain,
            labels             = signature.labels,
            generic_parameters = OrderedDict([
                (name, specializations.get(name, generic))
                for name, generic in signature.generic_parameters.items()
            ]))

    # TODO Handle abstract types


def matches(t, u):
    if isinstance(t, TypeVariable):
        return True
    if isinstance(u, TypeVariable):
        return True
    if isinstance(t, TypeUnion):
        return any(matches(it, u) for it in t)
    if isinstance(u, TypeUnion):
        return matches(u, t)
    if isinstance(t, StructType) and isinstance(u, StructType):
        return ((t.name == u.name)
                and not (t.members.keys() ^ u.members.keys())
                and all(matches(t.members[it], u.members[it]) for it in t.members))

    return t == u
