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


class TypeTag(BaseType):
    # Types themselves should be typed with `Type`. But there're many
    # instances where we need the type name to refer to the type's itself and
    # not that of the first-class type value (e.g. `Int.+`). Whether the type
    # name should be interpreted as a first-class value or a reference to its
    # own type depends on where the name is used. As a result, it is simpler
    # to store a `TypeTag` object in the solver's environment, so that we can
    # choose what type we want depending on the context.

    def __init__(self, instance_type):
        self.instance_type = instance_type

    def __eq__(self, other):
        return (type(self) == type(other)) and (self.instance_type == other.instance_type)

    def __str__(self):
        return 'TypeTag<%s>' % self.instance_type


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

        elif isinstance(a, TypeTag) and isinstance(b, TypeTag):
            self.unify(a.instance_type, b.instance_type)

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
                domain             = [self.deepwalk(p) for p in t.domain],
                codomain           = self.deepwalk(t.codomain),
                labels             = t.labels,
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

    class TypeNode(Node):
        # A dummy AST node whose sole purpose is to specify type information for
        # some internally created nodes.

        def __init__(self, type):
            self.type = type

    def __init__(self):
        # A simple substitution map: (TypeVariable) -> Type.
        self.environment = Substitution({
            TypeVariable(id=(builtin_scope, symbol)): TypeTag(obj)
            for symbol, obj in builtin_scope.members.items() if isinstance(obj, BaseType)
        })

        # Methods, properties and enum cases may use `Self` as a placeholder
        # to denote the "final" type they're defined in. This stack will serve
        # us to keep track of the actual type `Self` represents, depending on
        # the context we'll be evaluation typing informations.
        self.current_self_type = []

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
            if isinstance(initial_value_type, TypeTag):
                initial_value_type = Type

            # If there's a type annotation as well, we should unify the type
            # it denotes with the one that was inferred from the initializing
            # value. This will not only check that the types match, but will
            # also try to infer specialization arguments of abstract types.
            if node.type_annotation:
                type_annotation = self.analyse(node.type_annotation)
                if isinstance(type_annotation, TypeTag):
                    type_annotation = type_annotation.instance_type
                self.environment.unify(type_annotation, initial_value_type)

            inferred = initial_value_type

        # If there isn't an initializing value, we should simply use the type
        # annotation.
        else:
            inferred = self.analyse(node.type_annotation)
            if isinstance(inferred, TypeTag):
                inferred = inferred.instance_type

        # Finally, we should unify the inferred type with the type variable
        # corresponding to the symbol under declaration.
        self.environment.unify(TypeVariable(node), inferred)

    def visit_VariableDecl(self, node):
        return self.visit_ConstantDecl(node)

    def visit_FunctionDecl(self, node):
        # First, we should create (unless we already did) a generic type for
        # each of the function's generic parameters (if any), to populate the
        # environment.
        member_scope = node.body.__info__['scope']
        generic_parameters = OrderedDict()
        for symbol in node.generic_parameters:
            var = TypeVariable(id=(member_scope, symbol))
            if var not in self.environment:
                self.environment.unify(var, GenericType(symbol))
                generic_parameters[symbol] = var
            else:
                generic_parameters[symbol] = self.environment[var]

        # Unlike container declarations, function parameters always have a
        # type annotation, so we can type them directly.
        parameter_types = []
        for parameter in node.signature.parameters:
            type_annotation = self.analyse(parameter.type_annotation)
            if isinstance(type_annotation, TypeTag):
                type_annotation = type_annotation.instance_type
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
                if isinstance(default_value_type, TypeTag):
                    default_value_type = Type
                self.environment.unify(type_annotation, default_value_type)

        # The return type is simply a type signature we've to evaluate.
        return_type = self.analyse(node.signature.return_type)
        if isinstance(return_type, TypeTag):
            return_type = return_type.instance_type

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

        # FIXME Avoid adding duplicates to the set of overloads.

        # Finally, we should continue the type inference in the function body.
        self.visit(node.body)

    def visit_StructDecl(self, node):
        # First, we should create a new type for the struct, using fresh type
        # variables for each of the symbols it defines.
        member_scope = node.body.__info__['scope']

        # Then, we create a new struct type (unless we already did) that we
        # enclose it within a type tag that we unify with the struct's name.
        walked = self.environment[TypeVariable(node)]
        if isinstance(walked, TypeVariable):
            struct_type = StructType(
                name    = node.name,
                members = {
                    name: self.environment[TypeVariable((member_scope, name))]
                    for name in node.body.__info__['symbols']
                })

            type_tag = TypeTag(struct_type)
            self.environment.unify(TypeVariable(node), type_tag)

        else:
            type_tag = walked
            assert isinstance(type_tag, TypeTag)

        # The body of a struct can be visited as a normal satement block, as
        # long as we push a variable on the `current_self_type` stack before,
        # to properly catch references to `Self`.
        self.current_self_type.append(type_tag)
        self.visit(node.body)
        self.current_self_type.pop()

    def visit_Assignment(self, node):
        # First, we infer and unify the types of the lvalue and rvalue.
        lvalue_type = self.analyse(node.lvalue)
        rvalue_type = self.analyse(node.rvalue)
        if isinstance(rvalue_type, TypeTag):
            rvalue_type = Type
        self.environment.unify(lvalue_type, rvalue_type)

        # If the lvalue is an identifer, we unify its name with the type of
        # the rvalue.
        if isinstance(node.lvalue, Identifier):
            self.environment.unify(TypeVariable(node.lvalue), rvalue_type)
            return

        # Note that for now, only identifiers are valid lvalues.
        assert False, '%s is not a valid lvalue' % node.target.__class__.__name__

    def analyse(self, node):
        if isinstance(node, BaseType):
            return node

        # If the node is a dummy TypeNode, we should simply return the types
        # it represents.
        if isinstance(node, TypeSolver.TypeNode):
            return node.type

        # If the node is a literal, its type should already have been inferred
        # by the parser.
        if isinstance(node, Literal):
            return node.__info__['type']

        # If the node is an identifier, we should simply try to get its type
        # from the environment.
        if isinstance(node, (Identifier, TypeIdentifier)):
            # If the identifier's name is the special `Self` keyword, we
            # should the tag of the type under declaration.
            if node.name == 'Self':
                try:
                    result = self.current_self_type[-1]
                except IndexError:
                    raise InferenceError("invalid use of 'Self' outside of a type declaration")
            else:
                try:
                    result = self.environment[TypeVariable(node)]
                except KeyError:
                    raise UndefinedSymbol(node.name)

            node.__info__['type'] = result
            return result

        if isinstance(node, Select):
            # First, we need to infer the type of the owner.
            owner_types = self.analyse(node.owner)
            if isinstance(owner_types, TypeTag):
                owner_types = owner_types.instance_type

            # If the result we got is a type variable, we have no choice but
            # to return another type variable.
            if isinstance(owner_types, TypeVariable):
                return TypeVariable()

            # If the result we got is an actual type (or union of), we should
            # look for a member with the requested name in its (or their)
            # definition(s).
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

        # If the node is a function signature, we should build a FunctionType.
        if isinstance(node, FunctionSignature):
            domain = []
            for parameter in node.parameters:
                type_annotation = self.analyse(parameter.type_annotation)
                if isinstance(type_annotation, TypeTag):
                    type_annotation = type_annotation.instance_type
                domain.append(type_annotation)

            codomain = self.analyse(node.return_type)
            if isinstance(codomain, TypeTag):
                codomain = codomain.instance_type

            function_type = FunctionType(
                domain   = domain,
                codomain = codomain,
                labels   = [p.label for p in node.parameters])

            node.__info__['type'] = function_type
            return function_type

        if isinstance(node, PrefixedExpression):
            # First, we have to infer the type of the operand.
            operand_type = self.analyse(node.operand)
            if isinstance(operand_type, TypeTag):
                operand_type = Type

            # Then, we can get the available signatures for the operator.
            candidates = find_operator_candidates(operand_type, node.operator)
            if len(candidates) == 0:
                raise InferenceError(
                    "%s has a no member '%s'" % (operand_type, node.operator))

            # Once we've got those operator signatures, we should create a
            # temporary Call node to fallback on the usual analysis of
            # function call return types.
            call_node = Call(
                callee = TypeSolver.TypeNode(TypeUnion(candidates)),
                arguments = [CallArgument(node.operand)])
            result = self.analyse(call_node)

            node.__info__['type'] = result
            return result

        if isinstance(node, BinaryExpression):
            # First, we have to infer the type of the left operand.
            left_type = self.analyse(node.left)
            if isinstance(left_type, TypeTag):
                left_type = Type

            # Then, we can get the available signatures for the operator.
            candidates = find_operator_candidates(left_type, node.operator)
            if len(candidates) == 0:
                raise InferenceError(
                    "%s has a no member '%s'" % (left_type, node.operator))

            # Once we've got those operator signatures, we should create a
            # temporary Call node to fallback on the usual analysis of
            # function call return types.
            call_node = Call(
                callee = TypeSolver.TypeNode(TypeUnion(candidates)),
                arguments = [CallArgument(node.left), CallArgument(node.right)])
            result = self.analyse(call_node)

            node.__info__['type'] = result
            return result

        if isinstance(node, Call):
            # First, we have to get the available signatures for the callee.
            callee_signatures = self.analyse(node.callee)
            if isinstance(callee_signatures, TypeTag):
                callee_signatures = callee_signatures.instance_type

            # We make `callee_signatures` an iterable if it's not, so we can
            # use it as a TypeUnion even if it isn't.
            if not isinstance(callee_signatures, TypeUnion):
                callee_signatures = (callee_signatures,)

            eligible_signatures = []
            selected_codomains = []

            for signature in callee_signatures:
                # We should list all function signatures as candidates.
                if isinstance(signature, FunctionType):
                    eligible_signatures.append(signature)
                    continue

                # When the signature is a variable, we should add a fresh
                # variable to the list of pre-selected codomains.
                if isinstance(signature, TypeVariable):
                    selected_codomains.append(TypeVariable())
                    continue

                # The callee may also be a non-function type, for calls that
                # apply a type constructor (e.g. Int(0)). In those cases, we
                # should list all the constructors of the type as candidates.
                if not isinstance(signature, FunctionType):
                    type_constructors = signature.members.get('new', [])
                    if isinstance(type_constructors, TypeVariable):
                        selected_codomains.append(signature)
                    elif isinstance(type_constructors, TypeUnion):
                        eligible_signatures.extend(type_constructors)
                    else:
                        eligible_signatures.append(type_constructors)

            # If we couldn't find any eligible signature, we should return the
            # type variables we already selected as codomains (if any).
            if not eligible_signatures:
                if not selected_codomains:
                    raise InferenceError(
                        "function call do not match any candidate in '%s'" % callee_signatures)

                # Avoid creating singletons when there's only one candidate.
                if len(selected_codomains) == 1:
                    result = selected_codomains[0]
                else:
                    result = TypeUnion(selected_codomains)
                node.__info__['type'] = result
                return result

            # Then, we have to infer the type of each argument.
            argument_types = []
            for argument in node.arguments:
                argument_type = self.analyse(argument.value)
                if isinstance(argument_type, TypeTag):
                    argument_type = Type
                argument_types.append(argument_type)

            # Then, we have to find which signatures agree with the types
            # we've inferred for the function arguments.
            candidates = []
            for signature in eligible_signatures:
                # Make sure the signature is a function type.
                assert isinstance(signature, FunctionType)

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
            candidates = list(c for specialized in specialized_candidates for c in specialized)

            # Check the specialized candidate to filter out those whose
            # signature doesn't match the node's arguments or return type.
            selected_candidates = []
            for signature in candidates:
                valid = True
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
                    "function call do not match any candidate in '%s'" % eligible_signatures)

            # Unify the argument types of the node with the domain of the
            # selected candidates, to propagate type constraints.
            for i, argument_type in enumerate(argument_types):
                candidate_domains = TypeUnion()
                overloads = []
                for candidate in selected_candidates:
                    # If the candidate argument type is generic, we should try
                    # to find its replacement in the candidate's
                    # specialization. Otherwise we simply use the type of the
                    # candidate argument.
                    if isinstance(candidate.domain[i], GenericType):
                        arg = candidate.specialized_parameter(candidate.domain[i].name)
                    else:
                        arg = candidate.domain[i]

                    # We don't unify function type arguments when they're used
                    # as parameters, to preserve their overloads.
                    if isinstance(arg, FunctionType):
                        overloads.append(arg)
                    else:
                        candidate_domains.add(arg)

                if candidate_domains.types:
                    # We have to make a copy of the type union we created here
                    # to avoid introducing circular substitutions in the
                    # environment. This could happen if `candidate_domains`
                    # references `argument_type` somewhere in the hierarchy.
                    candidate_domains = candidate_domains.copy()
                    self.environment.unify(candidate_domains, argument_type)

                for overload in overloads:
                    argument_type.add(overload)

            result = TypeUnion()
            for candidate in selected_candidates:
                codomain = candidate.codomain
                if isinstance(codomain, GenericType):
                    codomain = candidate.specialized_parameter(codomain.name)
                result.add(self.environment[codomain])

            for codomain in selected_codomains:
                result.add(selected_codomains)

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


def find_operator_candidates(operand_type, operator):
    # If the operand's type is a variable, we have no choice but to return
    # another type variable.
    if isinstance(operand_type, TypeVariable):
        return TypeVariable()

    # Otherwise, we can simply search its members to find signature candidates
    # for the given operator.
    if not isinstance(operand_type, TypeUnion):
        operand_type = (operand_type,)

    candidates = []
    for expr_type in operand_type:
        if isinstance(expr_type, TypeVariable):
            candidates.append(TypeVariable())
        elif isinstance(expr_type, BaseType) and (operator in expr_type.members):
            candidate = expr_type.members[operator]
            if isinstance(candidate, TypeUnion):
                candidates.extend(candidate)
            else:
                candidates.append(candidate)

    return candidates


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
