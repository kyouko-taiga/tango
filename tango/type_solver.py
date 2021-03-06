from itertools import chain, product

from .ast import *
from .builtin import Bool, Type, builtin_scope
from .errors import InferenceError
from .scope import Scope
from .types import (
    BaseType, EnumType, FunctionType, GenericType, NominalType, StructType, TypeUnion)


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

    return (node, type_solver.environment.reified())


class TypesFinder(Visitor):

    def __init__(self, environment):
        self.types = {}
        self.environment = environment

    def visit(self, node):
        if 'type' in node.__info__:
            # NOTE If we give the type solver the ability to transform the
            # AST, we might have to find another way to "hash" the nodes.
            walked = self.environment.deepwalk(node.__info__['type'])
            node.__info__['type'] = walked
            self.types[id(node)] = walked

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

    def is_generic(self, memo=None):

        # NOTE We chose to always consider type variables non-generic. The
        # consequence of this choice is that whenever we visit a generic type
        # that has yet to be specialized, we have to type the expression that
        # uses it with another fresh variable.
        # Another approach would be to allow type variables to hold a
        # specialization list as well. That way we could express "some type
        # specialized with this". This would reduce the number of variables we
        # have to create, but would also make matching and unification harder.

        return False

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

    def unify(self, t, u, memo=None):
        if memo is None:
            memo = set()
        key = (id(t), id(u))
        if key in memo:
            return
        memo.add(key)

        a = self[t]
        b = self[u]

        if a == b:
            return

        if isinstance(a, TypeVariable):
            self.storage[a] = b
        elif isinstance(b, TypeVariable):
            self.storage[b] = a

        elif isinstance(a, TypeUnion) and isinstance(b, TypeUnion):
            results = []
            for ita, itb in product(a, b):
                if self.matches(ita, itb):
                    self.unify(ita, itb, memo)
                    results.append(ita)

            if not results:
                raise InferenceError("no types in '%s' matches a type in '%s'" % (a, b))
            a.replace_content(results)
            b.replace_content(results)

        elif isinstance(a, TypeUnion) and isinstance(b, BaseType):
            for it in a:
                if self.matches(it, b):
                    self.unify(it, b, memo)
                    a.replace_content((it,))
                    break
            else:
                raise InferenceError("no type in '%s' matches '%s'" % (a, b))

            # TODO When we'll implement abstract types and/or protocols, we
            # might have to to unify multiple result instead of simply the
            # first type that matches in the union.

        elif isinstance(b, TypeUnion) and isinstance(a, BaseType):
            self.unify(b, a, memo)

        elif isinstance(a, NominalType) and isinstance(b, NominalType):
            if (a.name != b.name) or (a.members.keys() ^ b.members.keys()):
                raise InferenceError("type '%s' does not match '%s'" % (a, b))
            for it in a.members:
                self.unify(a.members[it], b.members[it], memo)

        elif isinstance(a, FunctionType) and isinstance(b, FunctionType):
            for ita, itb in zip(a.domain, b.domain):
                self.unify(ita, itb, memo)
            self.unify(a.codomain, b.codomain, memo)

        elif isinstance(a, BaseType) and isinstance(b, BaseType):
            if a != b:
                raise InferenceError("type '%s' does not match '%s'" % (a, b))

        # TODO take conformance into account

        else:
            assert False, 'cannot unify %r and %r' % (a, b)

    def matches(self, t, u):
        a = self[t]
        b = self[u]

        if a == b:
            return True

        if isinstance(t, TypeVariable):
            return True
        if isinstance(u, TypeVariable):
            return True

        if isinstance(t, TypeUnion):
            return any(self.matches(it, u) for it in t)
        if isinstance(u, TypeUnion):
            return self.matches(u, t)

        if isinstance(t, NominalType) and isinstance(u, NominalType):
            return ((t.name == u.name)
                    and (t.scope == u.scope)
                    and not (t.members.keys() ^ u.members.keys())
                    and all(self.matches(t.members[it], u.members[it]) for it in t.members))

        # Note that unification of function types is stricter than what `matches`
        # checks, requiring `t` and `u` to be equal under `__eq__`. Relaxing the
        # constraint here is what lets us matching a generic signature to its
        # specialization when visiting Call nodes. However, we normally only unify
        # generic signatures with themselves in FunctionDecl nodes, so having a
        # more relaxed match function shouldn't cause any issue.
        if isinstance(t, FunctionType) and isinstance(u, FunctionType):
            return (len(t.domain) == len(u.domain)
                    and all(self.matches(t.domain[i], u.domain[i]) for i in range(len(t.domain)))
                    and self.matches(t.codomain, u.codomain)
                    and self.matches(t.labels, u.labels))

        return t == u

    def deepwalk(self, t, memo=None):
        if memo is None:
            memo = {}
        if id(t) in memo:
            return memo[id(t)]

        if isinstance(t, TypeUnion):
            result = TypeUnion()
            memo[id(t)] = result
            result.types = TypeUnion(self.deepwalk(it, memo) for it in t).types

            # Avoid creating singletons when there's only one candidate.
            if len(result) == 1:
                result = result.first()
            return result

        if isinstance(t, NominalType):
            result = t.__class__(
                name               = t.name,
                scope              = t.scope,
                inner_scope        = t.inner_scope,
                generic_parameters = t.generic_parameters,
                specializations    = t.specializations)
            memo[id(t)] = result

            result.members = {
                name: self.deepwalk(member, memo)
                for name, member in t.members.items()
            }
            return result

        if isinstance(t, FunctionType):
            result = FunctionType(
                labels     = t.labels,
                attributes = t.attributes)
            memo[id(t)] = result

            result.domain = [self.deepwalk(p, memo) for p in t.domain]
            result.codomain = self.deepwalk(t.codomain, memo)
            return result

        if not isinstance(t, TypeVariable):
            return t

        if t in self.storage:
            return self.deepwalk(self.storage[t], memo)

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
                if scope.name == 'Tango':
                    continue
                print('{:25}{:15}{:}'.format(
                    str(symbol),
                    scope.name + '.' + name,
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

        def __init__(self, type, is_typename=False):
            super().__init__()
            self.type = type
            self.is_typename = is_typename

    def __init__(self):
        # A simple substitution map: (TypeVariable) -> Type.
        self.environment = Substitution({
            TypeVariable(id=(builtin_scope, symbol.name)): symbol.type
            for symbol in builtin_scope.symbols.values()
        })

        # Enum case declarations typically don't explicitly specify their
        # "return" type, so we need a way to keep track of their type as we
        # visit them.
        self.current_self_type = []

    class ReturnFinder(Visitor):

        def __init__(self):
            self.return_statements = []

        def visit_Block(self, node):
            for statement in node.statements:
                if isinstance(statement, Return):
                    self.return_statements.append(statement)

                if isinstance(statement, If):
                    sub_finder = TypeSolver.ReturnFinder()
                    sub_finder.visit(statement)
                    self.return_statements.extend(sub_finder.return_statements)

    def visit_Block(self, node):
        # We introduce a new type variable for each symbol declared within the
        # current block before visiting its statements, so we can handle cases
        # where a variable refers a type that has yet to be defined.
        scope = node.__info__['scope']
        for symbol in node.__info__['symbols']:
            var = TypeVariable((scope, symbol))
            if var not in self.environment:
                self.environment[var] = TypeVariable()

        # We also store a reference to all return statements within the block,
        # so we can later unify their types (e.g. for functions).
        node.__info__['return_statements'] = []
        for statement in node.statements:
            self.visit(statement)
            if isinstance(statement, Return):
                node.__info__['return_statements'].append(statement)

    def visit_ContainerDecl(self, node):
        # If there isn't neither a type annotation, nor an initializing value,
        # we can't infer any additional type information.
        if not (node.type_annotation or node.initial_value):
            return

        # If there's an initializing value, we have to infer its type first.
        if node.initial_value:
            initial_value_type = self.read_type_instance(node.initial_value)

            # If there's a type annotation as well, we should unify the type
            # it denotes with the one that was inferred from the initializing
            # value. This will not only check that the types match, but will
            # also try to infer specialization arguments of abstract types.
            if node.type_annotation:
                type_annotation = self.read_type_reference(node.type_annotation)

                # If the type of initializing value is generic, we have to
                # first specialize it with the type annotation.
                if initial_value_type.is_generic():
                    initial_value_type = specialize_from_annotation(
                        self.environment.deepwalk(initial_value_type), type_annotation)

                # TODO Improve the error message when unifying the
                # computed specialization fails.

                self.environment.unify(type_annotation, initial_value_type)

            inferred = initial_value_type

        # If there isn't an initializing value, we should simply use the type
        # annotation.
        else:
            inferred = self.read_type_reference(node.type_annotation)

        # Finally, we should unify the inferred type with the type variable
        # corresponding to the symbol under declaration.
        self.environment.unify(TypeVariable(node), inferred)

        node.__info__['type'] = inferred

    def visit_FunctionDecl(self, node):
        # First, we create (unless we already did) a generic type object for
        # each of the function's generic parameters (if any).
        inner_scope = node.body.__info__['scope']
        for symbol in node.generic_parameters:
            var = TypeVariable(id=(inner_scope, symbol))
            if var not in self.environment:
                self.environment.unify(var, GenericType(symbol))
                inner_scope.typenames.add(symbol)

        # Unlike container declarations, function parameters always have a
        # type annotation, so we can type them directly.
        parameter_types = []
        for parameter in node.signature.parameters:
            type_annotation = self.read_type_reference(parameter.type_annotation)

            # We should unify the type we read from the annotations with the
            # type variable corresponding to the parameter name, so that it'll
            # be typed in the function's body.
            parameter_type = TypeVariable(parameter)
            self.environment.unify(parameter_type, type_annotation)
            parameter_types.append(parameter_type)

            # Function parameters may be associated with a default value. In
            # those instances, we should infer the type of the initializing
            # expression and unify it with that of the parameter's annotation.
            if parameter.default_value:
                default_value_type = self.read_type_instance(parameter.default_value)

                # If the type of default value is generic, we have to first
                # specialize it with the type annotation.
                if default_value_type.is_generic():
                    default_value_type = specialize_from_annotation(
                        self.environment.deepwalk(default_value_type), type_annotation)

                # TODO Improve the error message when unifying the
                # computed specialization fails.
                self.environment.unify(type_annotation, default_value_type)

            parameter.__info__['type'] = type_annotation

        # The return type is simply a type signature we've to evaluate.
        return_type = self.read_type_reference(node.signature.return_type)

        # Once we've computed the function signature, we can create a type
        # for the function itself.
        function_type = FunctionType(
            domain             = parameter_types,
            codomain           = return_type,
            labels             = [param.label for param in node.signature.parameters],
            attributes         = [param.attributes for param in node.signature.parameters],
            generic_parameters = node.generic_parameters)

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

        # We continue the type inference in the function body.
        self.visit(node.body)

        # Finally, we have to unify the type of the return values of the
        # function with its return type.
        return_finder = TypeSolver.ReturnFinder()
        return_finder.visit(node)
        for statement in return_finder.return_statements:
            return_value_type = statement.value.__info__['type']
            self.environment.unify(return_value_type, function_type.codomain)

        node.__info__['scope'][node.name].type = function_type
        node.__info__['type'] = function_type

    def visit_nominal_type(self, node, type_class):
        # First, we create (unless we already did) a generic type object for
        # each of the type's generic parameters (if any).
        inner_scope = node.body.__info__['scope']
        for symbol in node.generic_parameters:
            var = TypeVariable(id=(inner_scope, symbol))
            if var not in self.environment:
                self.environment.unify(var, GenericType(symbol))
                inner_scope.typenames.add(symbol)

        # Then, we create a new nominal type (unless we already did).
        walked = self.environment[TypeVariable(node)]
        if isinstance(walked, TypeVariable):
            type_instance = type_class(
                name               = node.name,
                scope              = node.__info__['scope'],
                inner_scope        = inner_scope,
                generic_parameters = {
                    # We retrieve and store the generic type object we created
                    # for each of the type's generic parameters (if any).
                    symbol: self.environment[TypeVariable((inner_scope, symbol))]
                    for symbol in node.generic_parameters
                },
                members            = {
                    # We create new type variables for each of the symbols the
                    # nominal type defines.
                    name: self.environment[TypeVariable((inner_scope, name))]
                    for name in node.body.__info__['symbols']
                })

            node.__info__['scope'][node.name].type = type_instance
            self.environment.unify(TypeVariable(node), type_instance)

        else:
            type_instance = walked

        # Update `current_self_type` before visiting the type's members to
        # to handle enum case declarations.
        self.current_self_type.append(type_instance)
        self.visit(node.body)
        self.current_self_type.pop()

        node.__info__['type'] = type_instance

    def visit_StructDecl(self, node):
        self.visit_nominal_type(node, StructType)

    def visit_EnumDecl(self, node):
        self.visit_nominal_type(node, EnumType)

    def visit_EnumCaseDecl(self, node):
        # Enum case declaration should always be visited in the context of an
        # enum declaration. Consequently, the top `current_self_type` should
        # refer to that enum type.
        assert self.current_self_type and isinstance(self.current_self_type[-1], EnumType)
        enum_type = self.current_self_type[-1]

        # If the case doesn't have any associated value, we type it as the
        # enum type directly.
        if not node.parameters:
            self.environment.unify(TypeVariable(node), enum_type)
            return

        # If the case does have associated values, we first have to infer
        # their types.
        parameter_types = []
        for parameter in node.parameters:
            type_annotation = self.read_type_reference(parameter.type_annotation)
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
        rvalue_type = self.read_type_instance(node.rvalue)
        self.environment.unify(lvalue_type, rvalue_type)

        # If the lvalue is an identifer, we unify its name with the type of
        # the rvalue.
        if isinstance(node.lvalue, Identifier):
            self.environment.unify(TypeVariable(node.lvalue), rvalue_type)
            return

        # Note that for now, only identifiers are valid lvalues.
        assert False, '%s is not a valid lvalue' % node.target.__class__.__name__

    def visit_Call(self, node):
        # While we don't need to unify the return type of the function, we
        # still need to do it for its arguments.
        self.analyse(node)

    def visit_If(self, node):
        # First, we visit the node's pattern.
        self.visit(node.pattern)

        # Then, we infer the type of the node's pattern expression.
        condition_type = self.read_type_instance(node.pattern.expression)

        # The condition of an if expressions should always be a boolean, so we
        # can unify the type of the node's pattern expression with Bool.
        self.environment.unify(condition_type, Bool)

        # Then we can visit the node's body and else clause (if any).
        self.visit(node.body)
        if node.else_clause:
            self.visit(node.else_clause)

    def visit_Switch(self, node):
        # First, we infer the type of the switch's argument.
        argument_type = self.analyse(node.expression)

        for clause in node.clauses:
            # Then, we visit the pattern of each clause, before unifying their
            # respective expression type with that of the switch's argument.
            self.visit(clause.pattern)
            clause_expression_type = self.read_type_instance(clause.pattern.expression)
            self.environment.unify(clause_expression_type, argument_type)

            # Finally, we can visit each clause's body.
            self.visit(clause.body)

    def visit_Return(self, node):
        self.read_type_instance(node.value)

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

        if isinstance(node, TypeIdentifier):
            # First, we look for the type in the environment.
            result = self.environment[TypeVariable(node)]

            # Then, we handle the optional specialization arguments.
            if node.specialization_arguments:
                # If the result we got isn't a type variable, we return its
                # specialization.
                if not isinstance(result, TypeVariable):
                    # First, we have to create the substitution map for each
                    # type parameter.
                    specializations = {}
                    for argument in node.specialization_arguments:
                        try:
                            generic_type = result.generic_parameters[argument.name]
                        except KeyError:
                            raise InferenceError(
                                "type {} does not have a type parameter named {}".format(
                                    result, argument.name))

                        specializations[id(generic_type)] = self.read_type_reference(
                            argument.type_annotation)

                    # Make sure we got a specialization for all parameters.
                    if len(specializations) < len(result.generic_parameters):
                        raise InferenceError("missing specialization arguments")

                    # Finally, we can specialize the generic type.
                    result = self.environment.deepwalk(result)
                    result = specialize(result, specializations)

                # Otherwise, we have to return a fresh variable, as we don't
                # know what generic type we'll have to specialize yet.
                else:
                    result = TypeVariable()

            node.__info__['type'] = result
            return result

        if isinstance(node, Identifier):
            # We get the identifier's type from the environment.
            result = self.environment[TypeVariable(node)]
            node.__info__['type'] = result

            # We also have to determine the identifier's mutability.
            node.__info__['mutable'] = node.__info__['scope'][node.name].is_mutable

            return result

        if isinstance(node, Select):
            # First, we need to infer the type of the owner.
            owner_types = self.read_type_reference(node.owner)
            if not isinstance(owner_types, TypeUnion):
                owner_types = (owner_types,)

            # We raise a flag if the owner we're visiting isn't a type tag, to
            # decide whether or not we should perform automatic self binding.
            as_instance_member = not self.is_typename(node.owner)

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
                                domain     = member.domain[1:],
                                codomain   = member.codomain,
                                labels     = member.labels[1:],
                                attributes = member.attributes[1:]))
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

        if isinstance(node, ImplicitSelect):
            # If we didn't infer the type of the node's owner yet, we've no
            # choice but to return a type variable.
            if 'owner_type' not in node.__info__:
                node.__info__['owner_type'] = TypeVariable()
                return node.__info__['owner_type']

            # If we already created a variable for the type of the node's
            # owner, but couldn't infer its value yet, we should return the
            # same variable.
            owner_type = self.environment[node.__info__['owner_type']]
            if isinstance(owner_type, TypeVariable):
                return owner_type

            # Once we've inferred the type of the node's owner, we can create
            # a temporary Select node to fallback and the usual analysis.
            select_node = Select(
                owner  = TypeSolver.TypeNode(owner_type, is_typename=True),
                member = Identifier(name=node.member))

            result = self.analyse(select_node)
            node.__info__['type'] = result
            return result

        # If the node is a function signature, we should build a FunctionType.
        if isinstance(node, FunctionSignature):
            domain = []
            for parameter in node.parameters:
                type_annotation = self.read_type_reference(parameter.type_annotation)
                domain.append(type_annotation)

            codomain = self.read_type_reference(node.return_type)

            function_type = FunctionType(
                domain     = domain,
                codomain   = codomain,
                labels     = [parameter.label for parameter in node.parameters],
                attributes = [parameter.attributes for parameter in node.parameters])

            node.__info__['type'] = function_type
            return function_type

        if isinstance(node, PrefixedExpression):
            # First, we have to infer the type of the operand.
            operand_type = self.read_type_instance(node.operand)

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
            node.__info__['function_call_type'] = call_node.__info__['function_call_type']
            return result

        if isinstance(node, BinaryExpression):
            # First, we have to infer the type of the left operand.
            left_type = self.read_type_instance(node.left)

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
            node.__info__['function_call_type'] = call_node.__info__['function_call_type']
            return result

        if isinstance(node, Call):
            # First, we get the possible types of the callee.
            callee_type = self.read_type_reference(node.callee)
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

                if isinstance(signature, TypeVariable):
                    # If the signature is a variable, but the callee is an
                    # implicit select, we know that the codomain is that of
                    # the callee's owner.
                    if isinstance(node.callee, ImplicitSelect):
                        selected_codomains.append(signature)
                        continue

                    # Otherwise, we can't know the codomain yet, and we add a
                    # fresh variable to the set of pre-selected codomains.
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
                            domain             = constructor.domain[1:],
                            codomain           = constructor.codomain,
                            labels             = constructor.labels[1:],
                            attributes         = constructor.attributes[1:],
                            generic_parameters = constructor.generic_parameters))

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
                    selected_candidates = selected_candidates[0]

                node.__info__['type'] = result
                node.__info__['function_call_type'] = callee_type
                return result

            # Then, we have to infer the type of each argument.
            argument_types = []
            for argument in node.arguments:
                argument_type = self.read_type_instance(argument.value)
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

                valid = True
                for i, argument in zip(range(len(node.arguments)), node.arguments):
                    # We check if the argument label match.
                    expected_label = signature.labels[i]
                    if (expected_label == '_') and (argument.name is not None):
                        valid = False
                        break
                    elif expected_label != argument.name:
                        valid = False
                        break

                    # We check if the argument mutability match.
                    expected_mutability = 'mutable' in signature.attributes[i]
                    if expected_mutability and not argument.value.__info__.get('mutable', False):
                        valid = False
                        break

                if not valid:
                    continue

                compatible_candidates.append(signature)

                # NOTE Using profiling, we might determine that it could be
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
                    specialize_with_pattern(
                        self.environment.deepwalk(candidate), specializer, node)
                    for candidate in compatible_candidates))
            print(len(specialized_candidates))

            # Then we filter out the specialized candidates whose signature
            # doesn't match the node's arguments or return type.
            selected_candidates = []
            for signature in specialized_candidates:
                valid = True
                for expected_type, argument_type in zip(signature.domain, argument_types):
                    if not self.environment.matches(expected_type, argument_type):
                        valid = False
                        break
                if not valid:
                    continue

                if not self.environment.matches(signature.codomain, return_type):
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
                        if not argument_type.is_generic():
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

            result = TypeUnion()
            for candidate in selected_candidates:
                result.add(self.environment[candidate.codomain])

            for codomain in selected_codomains:
                result.add(selected_codomains)

            # Avoid creating singletons when there's only one candidate.
            if len(result) == 1:
                result = result.first()
                selected_candidates = selected_candidates[0]

            node.__info__['type'] = result
            node.__info__['function_call_type'] = selected_candidates
            return result

            # TODO Handle variadic arguments
            # A possible approach would be to transform the Call nodes of the
            # AST whenever we visit a variadic parameter, so that we regroup
            # the arguments that can be considered part of the variadic
            # argument. Depending on the definitive syntax and restrictions
            # we'll adopt for variadics parameters, we might have to check
            # multiple configurations of argument groups.

        if isinstance(node, (If, Switch)):
            # First we visit the node as if it was a statement.
            self.visit(node)

            # Then, we have to unify the type of all return values.
            return_finder = TypeSolver.ReturnFinder()
            return_finder.visit(node)

            # Since the node is used as an rvalue, it must have at least one
            # return statement.
            if not return_finder.return_statements:
                raise InferenceError('if expressions used as rvalues must return something')

            result = return_finder.return_statements[0].value.__info__['type']
            for statement in return_finder.return_statements[1:]:
                return_value_type = statement.value.__info__['type']
                self.environment.unify(return_value_type, result)

            node.__info__['type'] = result
            return result

        if isinstance(node, Wildcard):
            if 'type' in node.__info__:
                return node.__info__['type']
            return TypeVariable()

        assert False, "no type inference for node '%s'" % node.__class__.__name__

    def read_type_instance(self, node):
        if isinstance(node, TypeIdentifier):
            return Type
        elif self.is_typename(node):
            raise SyntaxError(
                "cannot use type name as an rvalue; "
                "use '.self' to reference the type object")
        return self.analyse(node)

    def read_type_reference(self, node):
        return self.analyse(node)

    def is_typename(self, node):
        if isinstance(node, Identifier):
            return (('scope' in node.__info__)
                    and (node.name in node.__info__['scope'].typenames))

        if isinstance(node, Select):
            return self.is_typename(node.member)

        if isinstance(node, TypeSolver.TypeNode):
            return node.is_typename

        return False

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


def find_overload_decls(name, scope):
    if scope.parent and (name in scope.parent):
        decl = scope.parent[name].decl
        if not isinstance(decl, list) or not isinstance(decl[0], FunctionDecl):
            return []
        return decl + find_overload_decls(name, scope.parent.parent)
    return []


def specialize(signature, specializations):
    if not signature.is_generic():
        return signature

    if isinstance(signature, NominalType):
        result = signature.__class__(
            name               = signature.name,
            scope              = signature.scope,
            inner_scope        = signature.inner_scope,
            generic_parameters = signature.generic_parameters)

        for generic_id, specialization in specializations.items():
            try:
                parameter_name = next(
                    name for name, param in signature.generic_parameters.items()
                    if id(param) == generic_id)
            except StopIteration:
                continue
            result.specializations[parameter_name] = specialization

        for name, member in signature.members.items():
            result.members[name] = specialize(member, specializations)

        return result

    if isinstance(signature, FunctionType):
        return FunctionType(
            domain     = [
                specialize(original, specializations)
                for original in signature.domain
            ],
            codomain   = specialize(original, specializations),
            labels     = unspecialized.labels,
            attributes = unspecialized.attributes)

    if isinstance(signature, GenericType):
        return specializations.get(id(signature), signature)

    assert False, signature


def specialize_with_pattern(unspecialized, specializer, call_node, specializations=None):
    if not unspecialized.is_generic():
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
                yield t if not t.is_generic() else TypeVariable(id=(id(t), id(call_node)))
        if specializer.is_generic():
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
            if original.is_generic():
                specialized = list(specialize_with_pattern(
                    original, replacement, call_node, specializations))
                specializations[id(original)] = specialized
                specialized_domain.append(specialized)
            else:
                specialized_domain.append((original,))

        if unspecialized.codomain.is_generic():
            specialized = list(specialize_with_pattern(
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
                domain     = specialized[0:-1],
                codomain   = specialized[-1],
                labels     = unspecialized.labels,
                attributes = unspecialized.attributes)

        return

    assert False, 'cannot specialize %s' % unspecialized.__class__.__name__


def specialize_from_annotation(unspecialized, type_annotation):
    # It shouldn't be able to express a generic type annotation, since generic
    # parameters aren't allowed in the syntax of function signatures.
    assert not type_annotation.is_generic()

    # Only function types can specialize a generic type.
    if not isinstance(type_annotation, FunctionType):
        raise InferenceError("type %s does not match %s" % (type_annotation, unspecialized))

    # Filter out functions types that aren't compatible with the given type
    # annotation.
    if not isinstance(unspecialized, TypeUnion):
        unspecialized = (unspecialized,)

    candidates = filter(lambda t: t.is_compatible_with(type_annotation), unspecialized)
    specialized = list(flatten(
        specialize_with_pattern(candidate, type_annotation, None)
        for candidate in candidates))

    if len(specialized) == 0:
        raise InferenceError(
            "no candidate in '%s' is compatible with type annotation '%s'" %
            (','.join(map(str, unspecialized)), type_annotation))

    return TypeUnion(specialized)


def flatten(iterable):
    return (i for it in iterable for i in it)
