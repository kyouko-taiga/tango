from collections import OrderedDict
from itertools import chain, product

from .ast import *
from .builtin import Bool, Type, builtin_scope
from .errors import UndefinedSymbol, InferenceError
from .scope import Scope
from .types import BaseType, EnumType, FunctionType, GenericType, StructType, TypeTag, TypeUnion


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

        self.is_generic = False

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
                domain   = [self.deepwalk(p) for p in t.domain],
                codomain = self.deepwalk(t.codomain),
                labels   = t.labels)

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
            if isinstance(symbol.id, tuple) and isinstance(symbol.id[0], Scope):
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
            TypeVariable(id=(builtin_scope, symbol)): obj
            for symbol, obj in builtin_scope.members.items()
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

    def visit_ContainerDecl(self, node):
        # If there isn't neither a type annotation, nor an initializing value,
        # we can't infer any type information.
        if not (node.type_annotation or node.initial_value):
            return

        # If there's an initializing value, we have to infer its type first.
        if node.initial_value:
            initial_value_type = type_instance(self.analyse(node.initial_value))

            # If there's a type annotation as well, we should unify the type
            # it denotes with the one that was inferred from the initializing
            # value. This will not only check that the types match, but will
            # also try to infer specialization arguments of abstract types.
            if node.type_annotation:
                type_annotation = type_reference(self.analyse(node.type_annotation))
                self.environment.unify(type_annotation, initial_value_type)

            inferred = initial_value_type

        # If there isn't an initializing value, we should simply use the type
        # annotation.
        else:
            inferred = type_reference(self.analyse(node.type_annotation))

        # Finally, we should unify the inferred type with the type variable
        # corresponding to the symbol under declaration.
        self.environment.unify(TypeVariable(node), inferred)

    def visit_FunctionDecl(self, node):
        # First, we should create (unless we already did) a generic type for
        # each of the function's generic parameters (if any), to populate the
        # environment.
        member_scope = node.body.__info__['scope']
        for symbol in node.generic_parameters:
            var = TypeVariable(id=(member_scope, symbol))
            if var not in self.environment:
                self.environment.unify(var, GenericType(symbol))

        # Unlike container declarations, function parameters always have a
        # type annotation, so we can type them directly.
        parameter_types = []
        for parameter in node.signature.parameters:
            type_annotation = type_reference(self.analyse(parameter.type_annotation))
            parameter_types.append(type_annotation)

            # We should unify the type we read from the annotations with the
            # type variable corresponding to the parameter name, so that it'll
            # be typed in the function's body.
            self.environment.unify(TypeVariable(parameter), type_annotation)

            # Function parameters may be associated with a default value. In
            # those instances, we should infer the type of the initializing
            # expression and unify it with that of the parameter's annotation.
            if parameter.default_value:
                default_value_type = type_instance(self.analyse(parameter.default_value))
                self.environment.unify(type_annotation, default_value_type)

        # The return type is simply a type signature we've to evaluate.
        return_type = type_reference(self.analyse(node.signature.return_type))

        # Once we've computed the function signature, we can create a type
        # for the function itself.
        function_type = FunctionType(
            domain   = parameter_types,
            codomain = return_type,
            labels   = [parameter.label for parameter in node.signature.parameters])

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

    def visit_nominal_type(self, node, type_class):
        # First, we should create a new type for the nominal, using fresh type
        # variables for each of the symbols it defines.
        member_scope = node.body.__info__['scope']

        # Then, we create a new type (unless we already did) that we enclose
        # within a type tag we unify with the struct's name.
        walked = self.environment[TypeVariable(node)]
        if isinstance(walked, TypeVariable):
            type_tag = TypeTag(type_class(
                name    = node.name,
                members = {
                    name: self.environment[TypeVariable((member_scope, name))]
                    for name in node.body.__info__['symbols']
                }))

            self.environment.unify(TypeVariable(node), type_tag)

        else:
            type_tag = walked
            assert isinstance(type_tag, TypeTag)

        # The body of a type can be visited as a normal satement block, as
        # long as we push a variable on the `current_self_type` stack before,
        # to properly catch references to `Self`.
        self.current_self_type.append(type_tag)
        self.visit(node.body)
        self.current_self_type.pop()

    def visit_StructDecl(self, node):
        self.visit_nominal_type(node, StructType)

    def visit_EnumDecl(self, node):
        self.visit_nominal_type(node, EnumType)

    def visit_EnumCaseDecl(self, node):
        # Enum case declaration should always be visited in the context of an
        # enum declaration. Consequently, the top `current_self_type` should
        # refer to that enum type.
        assert self.current_self_type and isinstance(self.current_self_type[-1], TypeTag)
        enum_type = self.current_self_type[-1].instance_type

        # If the case doesn't have any associated value, we type it as the
        # enum type directly.
        if not node.parameters:
            self.environment.unify(TypeVariable(node), enum_type)
            return

        # If the case does have associated values, we first have to infer
        # their types.
        parameter_types = []
        for parameter in node.parameters:
            type_annotation = type_reference(self.analyse(parameter.type_annotation))
            parameter_types.append(type_annotation)

        # Then, we can type the case as a function taking associated values to
        # return the enum type.
        case_type = FunctionType(
            domain   = parameter_types,
            codomain = enum_type,
            labels   = [parameter.label for parameter in node.parameters])
        self.environment.unify(TypeVariable(node), case_type)

    def visit_Assignment(self, node):
        # First, we infer and unify the types of the lvalue and rvalue.
        lvalue_type = self.analyse(node.lvalue)
        rvalue_type = type_instance(self.analyse(node.rvalue))
        self.environment.unify(lvalue_type, rvalue_type)

        # If the lvalue is an identifer, we unify its name with the type of
        # the rvalue.
        if isinstance(node.lvalue, Identifier):
            self.environment.unify(TypeVariable(node.lvalue), rvalue_type)
            return

        # Note that for now, only identifiers are valid lvalues.
        assert False, '%s is not a valid lvalue' % node.target.__class__.__name__

    def visit_If(self, node):
        # First, we visit the types of the pattern parameters (if any).
        for parameter in node.pattern.parameters:
            self.visit(parameter)

        # Then, we infer the type of the pattern expression.
        condition_type = type_instance(self.analyse(node.pattern.expression))

        # The condition of an if expressions should always be a boolean, so we
        # can unify the type of the pattern expression with Bool.
        self.environment.unify(condition_type, Bool)

        # Finally we can visit the node's body and else clause (if any).
        self.visit(node.body)
        if node.else_clause:
            self.visit(node.else_clause)

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

            # We raise a flag if the owner we're visiting isn't a type tag, to
            # decide whether or not we should perform automatic self binding.
            as_instance_member = not isinstance(owner_types, TypeTag)

            owner_types = type_reference(owner_types)
            if not isinstance(owner_types, TypeUnion):
                owner_types = (owner_types,)

            candidates = []
            for owner_type in owner_types:
                # If the owner's type is a variable, we have no choice but to
                # return another unrelated type variable.
                if isinstance(owner_type, TypeVariable):
                    candidates.append(TypeVariable())

                # If the owner's type could be inferred, we should look for
                # members with the requested name in its definition(s).
                elif isinstance(owner_type, BaseType) and (node.member.name in owner_type.members):
                    members = self.environment[owner_type.members[node.member.name]]
                    if not isinstance(members, TypeUnion):
                        members = (members,)

                    for member in members:
                        # If we're visiting a type's instance, and the member
                        # is a method of the type, we should create a new
                        # function type to apply automatic self binding.
                        if as_instance_member and isinstance(member, FunctionType):
                            candidates.append(FunctionType(
                                domain   = member.domain[1:],
                                codomain = member.codomain,
                                labels   = member.labels[1:]))
                        else:
                            candidates.append(member)

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
                type_annotation = type_reference(self.analyse(parameter.type_annotation))
                domain.append(type_annotation)

            codomain = type_reference(self.analyse(node.return_type))

            function_type = FunctionType(
                domain   = domain,
                codomain = codomain,
                labels   = [p.label for p in node.parameters])

            node.__info__['type'] = function_type
            return function_type

        if isinstance(node, PrefixedExpression):
            # First, we have to infer the type of the operand.
            operand_type = type_instance(self.analyse(node.operand))

            # Then, we can get the available signatures for the operator.
            candidates = self.find_operator_candidates(operand_type, node.operator)
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
            left_type = type_instance(self.analyse(node.left))

            # Then, we can get the available signatures for the operator.
            candidates = self.find_operator_candidates(left_type, node.operator)
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
            # First, we get the possible types of the callee.
            callee_type = type_reference(self.analyse(node.callee))
            if not isinstance(callee_type, TypeUnion):
                callee_type = (callee_type,)

            # For each type the callee can represent, we identify which can be
            # candidates for a function call.
            candidates = []
            selected_codomains = []
            for signature in callee_type:
                # We list all function signatures as candidates.
                if isinstance(signature, FunctionType):
                    candidates.append(signature)
                    continue

                # If the signature is a variable, it means we don't know its
                # codomain yet. In that case, we add a fresh variable to the
                # set of pre-selected codomains.
                if isinstance(signature, TypeVariable):
                    selected_codomains.append(TypeVariable())
                    continue

                # If the signature is a non-function type, it may designate a
                # call to a type constructor. In that case, all constructors
                # of the type become candidates.
                if isinstance(signature, BaseType):
                    if 'new' not in signature.members:
                        continue
                    constructors = self.environment[signature.members['new']]

                    if not isinstance(constructors, TypeUnion):
                        constructors = (constructors,)

                    for constructor in constructors:
                        # If the constructor is a variable, it means we don't
                        # know its signature yet. But we do know its codomain
                        # (as it's a constructor), so we add the type to the
                        # set of pre-selected codomains.
                        if isinstance(constructor, TypeVariable):
                            selected_codomains.append(signature)
                            continue

                        # Otherwise, we'll build a new signature that doesn't
                        # require the first `self` parameter (for automatic
                        # self binding), and we'll list it as a candidate.
                        assert isinstance(constructor, FunctionType)
                        candidates.append(FunctionType(
                            domain   = constructor.domain[1:],
                            codomain = constructor.codomain,
                            labels   = constructor.labels[1:]))

            # If we can't find any candidate, we return the type codomains we
            # already selected (if any).
            if not candidates:
                if not selected_codomains:
                    raise InferenceError(
                        "function call do not match any candidate in '%s'" %
                        ', '.join(map(str, callee_type)))

                # Avoid creating signletons when there's only one result.
                if len(selected_codomains) == 1:
                    result = selected_codomains[0]
                else:
                    result = TypeUnion(selected_codomains)
                node.__info__['type'] = result
                return result

            # Then, we have to infer the type of each argument.
            argument_types = []
            for argument in node.arguments:
                argument_type = type_instance(self.analyse(argument.value))
                argument_types.append(argument_type)

            # Either the return type was already inferred in a previous pass,
            # or create a new variable for it.
            return_type = node.__info__.get('type', TypeVariable())

            # Once we've got signature candidates and argument types, we can
            # filter out signatures that aren't compatible with the argument
            # types we inferred.
            compatible_candidates = []
            for signature in candidates:
                assert(isinstance(signature, FunctionType))

                # We check it the number of parameters match.
                if len(signature.domain) != len(node.arguments):
                    continue

                # We check if the labels match.
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

                compatible_candidates.append(signature)

                # NOTE Using profiling, we might determine that it could s be
                # more efficient to already eliminate candidates whose domain
                # or codomain doesn't match the argument and return types of
                # our node here, before they are specialized.

            # Once we've identified compatible candidates, we can specialize
            # each of them for the argument types we inferred, and possibly
            # the return type we inferred from a previous pass.
            choices = [ai if isinstance(ai, TypeUnion) else (ai,) for ai in argument_types]
            choices.append(return_type if isinstance(return_type, TypeUnion) else (return_type,))

            specialized_candidates = []
            for types in product(*choices):
                specializer = FunctionType(domain=types[0:-1], codomain=types[-1])
                specialized_candidates.extend(flatten(
                    specialize(candidate, specializer, node)
                    for candidate in compatible_candidates))

            # Then we filter out the specialized candidates whose signature
            # doesn't match the node's arguments or return type.
            selected_candidates = []
            for signature in specialized_candidates:
                valid = True
                for expected_type, argument_type in zip(signature.domain, argument_types):
                    if not matches(expected_type, argument_type):
                        valid = False
                        break
                if not valid:
                    continue

                if not matches(signature.codomain, return_type):
                    continue

                selected_candidates.append(signature)

            # If we can't find any candidate, we return the type codomains we
            # already selected (if any).
            if not selected_candidates:
                if not selected_codomains:
                    raise InferenceError(
                        "function call do not match any candidate in '%s'" %
                        ', '.join(map(str, callee_type)))

            # We unify the argument types of the node with the domain of the
            # selected candidates, to propagate type constraints.
            for i, argument_type in enumerate(argument_types):
                candidate_domains = TypeUnion()
                overloads = []
                for candidate in selected_candidates:
                    arg = self.environment[candidate.domain[i]]

                    # We don't unify function type arguments when they're used
                    # as parameters, to preserve their overloads.
                    if isinstance(arg, FunctionType):
                        # We don't unify generic specializations neither, as
                        # it would pollute the type of generic name otherwise.
                        if not argument_type.is_generic:
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
                result.add(self.environment[candidate.codomain])

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

    def find_operator_candidates(self, operand_type, operator):
        # If the operand's type is a variable, we have no choice but to return
        # another type variable.
        if isinstance(operand_type, TypeVariable):
            return [TypeVariable()]

        # Otherwise, we can simply search its members to find signature
        # candidates for the given operator.
        if not isinstance(operand_type, TypeUnion):
            operand_type = (operand_type,)

        candidates = []
        for expr_type in (self.environment[t] for t in operand_type):
            if isinstance(expr_type, TypeVariable):
                candidates.append(TypeVariable())

            elif isinstance(expr_type, BaseType) and (operator in expr_type.members):
                candidate = self.environment[expr_type.members[operator]]
                if isinstance(candidate, TypeUnion):
                    candidates.extend(self.environment[c] for c in candidate)
                else:
                    candidates.append(candidate)

        return candidates


def type_instance(signature):
    assert isinstance(signature, (BaseType, TypeVariable))

    if isinstance(signature, TypeTag):
        return Type
    if isinstance(signature, GenericType):
        if isinstance(signature.signature, str):
            return Type
        return type_instance(signature.signature)
    return signature


def type_reference(signature):
    assert isinstance(signature, (BaseType, TypeVariable))

    if isinstance(signature, TypeTag):
        return signature.instance_type
    if isinstance(signature, GenericType):
        if isinstance(signature.signature, str):
            return signature
        return type_reference(signature.signature)
    return signature


def find_overload_decls(name, scope):
    if scope.parent and (name in scope.parent):
        if any(not isinstance(decl, FunctionDecl) for decl in scope.parent[name]):
            return []
        return scope.parent[name] + find_overload_decls(name, scope.parent.parent)
    return []


def specialize(unspecialized, specializer, call_node, specializations=None):
    if not unspecialized.is_generic:
        yield unspecialized
        return

    specializations = specializations if (specializations is not None) else {}
    if id(unspecialized) in specializations:
        # FIXME Maybe we should check whether the stored specialization result
        # is compatible with the replacements we otherwise would have yielded.
        for t in specializations[id(unspecialized)]:
            yield t
        return

    if isinstance(unspecialized, GenericType):
        if isinstance(specializer, TypeUnion):
            for t in specializer:
                yield t if not t.is_generic else TypeVariable(id=(id(t), id(call_node)))
        if specializer.is_generic:
            yield TypeVariable(id=(id(specializer), id(call_node)))
        else:
            yield specializer
        return

    if isinstance(unspecialized, FunctionType):
        # If the specializer isn't a function type as well, we can't do
        # anything.
        if not isinstance(specializer, FunctionType):
            return

        specialized_domain = []

        for original, replacement in zip(unspecialized.domain, specializer.domain):
            if original.is_generic:
                specialized = list(specialize(original, replacement, call_node, specializations))
                specializations[id(original)] = specialized
                specialized_domain.append(specialized)
            else:
                specialized_domain.append((original,))

        if unspecialized.codomain.is_generic:
            specialized = list(specialize(
                unspecialized.codomain, specializer.codomain, call_node, specializations))
            specializations[id(unspecialized.codomain)] = specialized
            specialized_codomain = specialized
        else:
            specialized_codomain = (unspecialized.codomain,)

        # FIXME Determine if a replacement can be a type union (or `specialize` returned
        # more than one result for the domain or codomain). If it can't, then it that
        # cartesian product will always be a singleton.
        for specialized in product(*chain(specialized_domain, (specialized_codomain,))):
            yield FunctionType(
                domain   = specialized[0:-1],
                codomain = specialized[-1],
                labels   = unspecialized.labels)

        return

    assert False, 'cannot specialize %s' % unspecialized.__class__.__name__


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

    # Note that unification of function types is stricter than what `matches`
    # checks, requiring `t` and `u` to be equal under `__eq__`. Relaxing the
    # constraint here is what lets us matching a generic signature to its
    # specialization when visiting Call nodes. However, we normally only unify
    # generic signatures with themselves in FunctionDecl nodes, so having a
    # more relaxed match function shouldn't cause any issue.
    if isinstance(t, FunctionType) and isinstance(u, FunctionType):
        return (len(t.domain) == len(u.domain)
                and all(matches(t.domain[i], u.domain[i]) for i in range(len(t.domain)))
                and matches(t.codomain, u.codomain)
                and matches(t.labels, u.labels))

    return t == u


def flatten(iterable):
    return (i for it in iterable for i in it)
