from itertools import chain, product

from .ast import *
from .builtin import Bool, Nothing, Type
from .errors import InferenceError
from .scope import Scope
from .types import (
    TypeModifier as TM,
    TypeBase, TypeName, TypeUnion, TypeVariable,
    BuiltinType, NominalType, FunctionType,
    type_factory)


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
            raise InferenceError(
                'could not reach a fixed point after {} iterations'.format(max_iter))

    # TODO: Restrict unions of type variants to the most restricted set of
    # modifiers.

    return (node, type_solver.environment.reified())


class TypesFinder(NodeVisitor):

    def __init__(self, environment):
        self.types = {}
        self.environment = environment

    def visit(self, node):
        if 'type' in node.__meta__:
            # NOTE If we give the type solver the ability to transform the
            # AST, we might have to find another way to "hash" the nodes.
            walked = self.environment.deepwalk(node.__meta__['type'])
            node.__meta__['type'] = walked
            self.types[id(node)] = walked

        self.generic_visit(node)


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
        if (t, u) in memo:
            return
        memo.add((t, u))

        a = self[t]
        b = self[u]

        if a == b:
            return

        elif isinstance(a, TypeUnion) and isinstance(b, TypeUnion):
            results = []
            for ita, itb in product(a, b):
                if self.matches(ita, itb):
                    self.unify(ita, itb, memo)
                    results.append(ita)

            if not results:
                raise InferenceError("no types in '{}' matches a type in '{}'".format(a, b))
            a.types = results
            b.types = results

        elif isinstance(a, TypeUnion) and isinstance(b, TypeBase):
            for it in a:
                if self.matches(it, b):
                    self.unify(it, b, memo)
                    a.types = [it]
                    break
            else:
                raise InferenceError("no type in '{}' matches '{}'".format(a, b))

            # TODO When we'll implement abstract types and/or protocols, we
            # might have to to unify multiple result instead of simply the
            # first type that matches in the union.

        elif isinstance(b, TypeUnion) and isinstance(a, TypeBase):
            self.unify(b, a, memo)

        if isinstance(a, TypeVariable):
            self.storage[a] = b
        elif isinstance(b, TypeVariable):
            self.storage[b] = a

        elif isinstance(a, NominalType) and isinstance(b, NominalType):
            if (a.name != b.name) or (set(a.members.keys()) ^ set(b.members.keys())):
                raise InferenceError("type '{}' does not match '{}'".format(a, b))
            for it in a.members.keys():
                self.unify(a.members[it], b.members[it], memo)

            # FIXME Should we unify defining scopes too?
            # FIXME How to handle type modifiers?

        elif isinstance(a, FunctionType) and isinstance(b, FunctionType):
            for ita, itb in zip(a.domain, b.domain):
                self.unify(ita, itb, memo)
            self.unify(a.codomain, b.codomain, memo)

        elif isinstance(a, TypeBase) and isinstance(b, TypeBase):
            if a != b:
                raise InferenceError("type '{}' does not match '{}'".format(a, b))

        # TODO take conformance into account

        else:
            assert False, 'cannot unify %r and %r' % (a, b)

    def matches(self, t, u):
        a = self[t]
        b = self[u]

        if a == b:
            return True

        if isinstance(a, TypeVariable) or isinstance(b, TypeVariable):
            return (a.modifiers == b.modifiers
                    or a.modifiers == 0
                    or b.modifiers == 0)

        if isinstance(a, TypeUnion):
            return any(self.matches(it, b) for it in a)
        if isinstance(b, TypeUnion):
            return self.matches(u, a)

        if isinstance(a, NominalType) and isinstance(b, NominalType):
            return (a.modifiers == b.modifiers
                    and a.name == b.name
                    and not (set(a.members.keys()) ^ set(b.members.keys()))
                    and all(self.matches(a.members[it], b.members[it]) for it in a.members.keys()))

        # Note that unification of function types is stricter than what `matches`
        # checks, requiring `t` and `u` to be equal under `__eq__`. Relaxing the
        # constraint here is what lets us matching a generic signature to its
        # specialization when visiting Call nodes. However, we normally only unify
        # generic signatures with themselves in FunctionDecl nodes, so having a
        # more relaxed match function shouldn't cause any issue.
        if isinstance(a, FunctionType) and isinstance(b, FunctionType):
            return (a.modifiers == b.modifiers
                    and len(a.domain) == len(b.domain)
                    and all(self.matches(a.domain[i], b.domain[i]) for i in range(len(a.domain)))
                    and self.matches(a.codomain, b.codomain)
                    and self.matches(a.labels, b.labels))

        return False

    def deepwalk(self, t, memo=None):
        if memo is None:
            memo = {}
        if t in memo:
            return memo[t]

        if isinstance(t, TypeUnion):
            result  = TypeUnion()
            memo[t] = result
            for u in t.types:
                result.add(self.deepwalk(u, memo))

            # Avoid creating singletons when there's only one candidate.
            if len(result) == 1:
                result = result.types[0]
            return result

        if isinstance(t, BuiltinType):
            memo[t] = t
            for name, member in t.members:
                t.members[name] = self.deepwalk(member, memo)
            return t

        if isinstance(t, FunctionType):
            return type_factory.make_function(
                modifiers = t.modifiers,
                domain    = [self.deepwalk(d, memo) for d in t.domain],
                labels    = list(t.labels),
                codomain  = self.deepwalk(t.codomain, memo))

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
                print('{:20}{:15}{:}'.format(
                    scope.name + '.' + name,
                    hex(id(inferred_type)),
                    inferred_type))
            else:
                print('{:20}{:15}{:}'.format(
                    '-',
                    hex(id(inferred_type)),
                    inferred_type))


class TypeSolver(NodeVisitor):

    class TypeNode(Node):
        # A dummy AST node whose sole purpose is to specify type information for
        # some internally created nodes.

        def __init__(self, type, is_typename=False):
            super().__init__()
            self.type = type
            self.is_typename = is_typename

    def __init__(self):
        # A simple substitution map: (TypeVariable) -> Type.
        self.environment = Substitution()

        # Enum case declarations typically don't explicitly specify their
        # "return" type, so we need a way to keep track of their type as we
        # visit them.
        self.current_self_type = []

    class ReturnFinder(NodeVisitor):

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

    def visit_ModuleDecl(self, node):
        # As imported symbols are merged into the built-in scope, we have to
        # include them into the environment before we infer the type of its
        # statements.
        module_scope = node.body.__meta__['scope']
        for name in module_scope:
            types = []
            for symbol in module_scope.getlist(name):
                if isinstance(symbol.type, TypeBase):
                    types.append(symbol.type)
                elif isinstance(symbol.type, TypeName):
                    types.append(symbol.type)
            if types:
                self.environment.unify(
                    type_factory.make_variable(id=(module_scope, name)),
                    type_factory.make_union(types) if len(types) > 1 else types[0])

        self.visit(node.body)

    def visit_Block(self, node):
        # We introduce a new type variable for each symbol declared within the
        # current block before visiting its statements, so we can handle cases
        # where a variable refers a type that has yet to be defined.
        scope = node.__meta__['scope']
        for symbol in node.__meta__['symbols']:
            var = type_factory.make_variable(id=(scope, symbol))
            if var not in self.environment:
                self.environment[var] = type_factory.make_variable()

        # We also store a reference to all return statements within the block,
        # so we can later unify their types (e.g. for functions).
        node.__meta__['return_statements'] = []
        for statement in node.statements:
            self.visit(statement)
            if isinstance(statement, Return):
                node.__meta__['return_statements'].append(statement)

    def visit_PropDecl(self, node):
        # If there isn't neither a type annotation, nor an initial value, we
        # can't infer any additional type information.
        if not (node.type_annotation or node.initial_value):
            return

        # First, we need to infer the modifiers of the property's type. If it
        # has a type annotation, we can simply copy them, otherwise we default
        # to `@cst @stk @val`.
        if node.type_annotation:
            type_modifiers = node.type_annotation.modifiers
        else:
            node.type_annotation.modifiers = TM.tm_cst | TM.tm_stk | TM.tm_val

        # If there's an initial value, we shoud infer its type.
        if node.initial_value:
            initial_value_type = self.read_type_instance(node.initial_value)

            # Depending on the binding operator, we may have to override the
            # modifiers of the initial value's type, so they match the return
            # type of the binding operator.
            ts = (initial_value_type.types if isinstance(initial_value_type, TypeUnion)
                  else [initial_value_type])

            # NOTE: If the initial value is a `@ref` type, we could forbid the
            # use of a move binding operator here. But for now we'll let that
            # error be detected and raised by the reference checker.

            # A copy or move binding always produces `@val` types.
            if node.initial_binding in (Operator.o_cpy, Operator.o_mov):
                ts = [
                    type_factory.updating(t, modifiers=t.modifiers & ~TM.tm_ref | TM.tm_val)
                    for t in ts
                ]

            # NOTE: The statement `let x: @ref = y` will fail type inference,
            # because `x` cannot possibly already refer to a valid variable.
            # As a result, contrary to copy assignment statements, initial
            # copy bindings can be assumed to always produce a `@val` type.

            # A reference binding always produces `@ref` types.
            if node.initial_binding == Operator.o_ref:
                ts = [
                    type_factory.updating(t, modifiers=t.modifiers & ~TM.tm_val | TM.tm_ref)
                    for t in ts
                ]

            initial_value_type = TypeUnion(ts)
            if len(initial_value_type) == 1:
                initial_value_type = initial_value_type.types[0]

            # If there's a type annotation as well, we should unify the type
            # it denotes with that of the initial value.
            if node.type_annotation:
                annotation_type = self.read_type_reference(node.type_annotation)
                self.environment.unify(annotation_type, initial_value_type)

            inferred_type = initial_value_type

        # If there isn't an initializing value, we should simply use the type
        # annotation.
        else:
            inferred_type = self.read_type_reference(node.type_annotation)

        # Finally, we should unify the inferred type with the type variable
        # corresponding to the symbol under declaration.
        self.environment.unify(varof(node), inferred_type)
        node.__meta__['type'] = inferred_type

    def visit_FunDecl(self, node):
        # Unlike variables, function parameters are always declared with a
        # type annotation, so we can type them directly.
        parameter_types = []
        parameter_names = []
        for parameter in node.parameters:
            type_annotation = self.read_type_reference(parameter.type_annotation)

            # We should unify the type we read from the annotations with the
            # type variable corresponding to the parameter name, so that it'll
            # be typed in the function's body.
            parameter_type = varof(parameter)
            self.environment.unify(parameter_type, type_annotation)
            parameter.__meta__['type'] = type_annotation

            parameter_types.append(type_annotation)
            parameter_names.append(parameter.name)

        # The type of the codomain is either a type identifier, or Nothing.
        if node.codomain_annotation:
            codomain = self.read_type_reference(node.codomain_annotation)
        else:
            codomain = Nothing

        # Once we've computed the function signature, we can create a type
        # for the function itself.
        function_type = type_factory.make_function(
            modifiers = TM.tm_cst | TM.tm_stk | TM.tm_val,
            domain    = parameter_types,
            labels    = parameter_names,
            codomain  = codomain)

        # As functions may be overloaded, we can't unify the function type
        # we've created with the function's name directly. Instead, we should
        # put it in a TypeUnion to potentially include the signature of the
        # function's overloads, as we find them.
        walked = self.environment[varof(node)]
        if isinstance(walked, TypeVariable):
            overload_set = TypeUnion([function_type])
            self.environment.unify(walked, overload_set)
        elif isinstance(walked, TypeUnion) and isinstance(walked.types[0], FunctionType):
            overload_set = walked
            overload_set.add(function_type)
        else:
            raise InferenceError("cannot overload '{}' with '{}'".format(walked, function_type))

        # If the function name is also associated with function types in the
        # enclosing scopes, we should add the latter as overloads.
        for overload in find_overload_decls(node.name, node.__meta__['scope']):
            overload_set.add(TypeVariable(overload))

        # We continue the type inference in the function body.
        self.visit(node.body)

        # Finally, we have to unify the type of the return values of the
        # function with its return type.
        return_finder = TypeSolver.ReturnFinder()
        return_finder.visit(node)
        for statement in return_finder.return_statements:
            return_value_type = statement.value.__meta__['type']
            self.environment.unify(return_value_type, function_type.codomain)

        node.__meta__['scope'][node.name].type = function_type
        node.__meta__['type'] = function_type

    def visit_nominal_type(self, node, type_class):
        # First, we create (unless we already did) a generic type object for
        # each of the type's generic parameters (if any).
        inner_scope = node.body.__meta__['scope']
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
                scope              = node.__meta__['scope'],
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
                    for name in node.body.__meta__['symbols']
                })

            node.__meta__['scope'][node.name].type = type_instance
            self.environment.unify(TypeVariable(node), type_instance)

        else:
            type_instance = walked

        # Update `current_self_type` before visiting the type's members to
        # to handle enum case declarations.
        self.current_self_type.append(type_instance)
        self.visit(node.body)
        self.current_self_type.pop()

        node.__meta__['type'] = type_instance

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
        assert False, '{} is not a valid lvalue'.format(node.target.__class__.__name__)

    def visit_Call(self, node):
        # While we don't need to unify the return type of the function, we
        # still need to do it for its arguments.
        self.analyse(node)

    def visit_If(self, node):
        # We infer the type of the node's condition.
        condition_type = self.read_type_instance(node.condition)

        # The condition of an if expressions should always be a boolean, so we
        # can unify the type of the node's condition with Bool.
        self.environment.unify(condition_type, Bool)

        # Then we can visit the node's body.
        self.visit(node.body)

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
        # If the node is a dummy TypeNode, we should simply return the types
        # it represents.
        # if isinstance(node, TypeSolver.TypeNode):
        #     return node.type

        # If the node is a simple literal, its type should have been inferred
        # by the parser already.
        if isinstance(node, IntLiteral):
            return node.__meta__['type']

        if isinstance(node, Identifier):
            # We get the identifier's type from the environment.
            result = self.environment[varof(node)]
            node.__meta__['type'] = result
            return result

        # If the node is a function signature, we should build a FunctionType.
        if isinstance(node, FunSign):
            domain = []
            labels = []
            for parameter in node.parameters:
                type_annotation = self.read_type_reference(parameter.type_annotation)
                domain.append(type_annotation)
                labels.append(parameter.label)

            codomain = self.read_type_reference(node.codomain_annotation)

            function_type = type_factory.make_function(
                domain     = domain,
                labels     = labels,
                codomain   = codomain)

            node.__meta__['type'] = function_type
            return function_type

        # if isinstance(node, PrefixExpression):
        #     # First, we have to infer the type of the operand.
        #     operand_type = self.read_type_instance(node.operand)
        #
        #     # Then, we can get the available signatures for the operator.
        #     candidates = self.find_operator_candidates(operand_type, node.operator)
        #     if len(candidates) == 0:
        #         raise InferenceError(
        #             "{} has a no member '{}'".format(operand_type, node.operator))
        #
        #     # Once we've got those operator signatures, we should create a
        #     # temporary Call node to fallback on the usual analysis of
        #     # function call return types.
        #     call_node = Call(
        #         callee = TypeSolver.TypeNode(TypeUnion(candidates)),
        #         arguments = [CallArgument(node.operand)])
        #     result = self.analyse(call_node)
        #
        #     node.__meta__['type'] = result
        #     node.__meta__['function_call_type'] = call_node.__meta__['function_call_type']
        #     return result
        #
        # if isinstance(node, BinaryExpression):
        #     # First, we have to infer the type of the left operand.
        #     left_type = self.read_type_instance(node.left)
        #
        #     # Then, we can get the available signatures for the operator.
        #     candidates = self.find_operator_candidates(left_type, node.operator)
        #     if len(candidates) == 0:
        #         raise InferenceError(
        #             "{} has a no member '{}'".format(left_type, node.operator))
        #
        #     # Once we've got those operator signatures, we should create a
        #     # temporary Call node to fallback on the usual analysis of
        #     # function call return types.
        #     call_node = Call(
        #         callee = TypeSolver.TypeNode(TypeUnion(candidates)),
        #         arguments = [CallArgument(node.left), CallArgument(node.right)])
        #     result = self.analyse(call_node)
        #
        #     node.__meta__['type'] = result
        #     node.__meta__['function_call_type'] = call_node.__meta__['function_call_type']
        #     return result

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
                    # if isinstance(node.callee, ImplicitSelect):
                    #     selected_codomains.append(signature)
                    #     continue

                    # Otherwise, we can't know the codomain yet, and we add a
                    # fresh variable to the set of pre-selected codomains.
                    selected_codomains.append(type_factory.make_variable())
                    continue

                # If the signature is a non-function type, it may designate a
                # call to a type constructor. In that case, all constructors
                # of the type become candidates.
                if isinstance(signature, TypeBase):
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
                        "function call do not match any candidate in '{}'".format(
                        ', '.join(map(str, callee_type))))

                # Avoid creating signletons when there's only one result.
                if len(selected_codomains) == 1:
                    result = selected_codomains[0]
                else:
                    result = TypeUnion(selected_codomains)
                    selected_candidates = selected_candidates[0]

                node.__meta__['type'] = result
                node.__meta__['function_call_type'] = callee_type
                return result

            # Then, we have to infer the type of each argument.
            argument_types = []
            for argument in node.arguments:
                # FIXME read the type of the whole CallArg (for binding policy)
                argument_type = self.read_type_instance(argument.value)
                argument_types.append(argument_type)

            # Either the return type was already inferred in a previous pass,
            # or create a new variable for it.
            return_type = node.__meta__['type'] or type_factory.make_variable()

            # Once we've got signature candidates and argument types, we can
            # filter out signatures that aren't compatible with the argument
            # types we inferred.
            compatible_candidates = []
            for signature in candidates:
                assert(isinstance(signature, FunctionType))

                # Check it the number of parameters match.
                if len(signature.domain) != len(node.arguments):
                    continue

                # Check if the argument labels match.
                valid = True
                for i, argument in zip(range(len(node.arguments)), node.arguments):
                    expected_label = signature.labels[i]
                    if expected_label != argument.label:
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
                specializer = type_factory.make_function(
                    domain   = list(types[0:-1]),
                    labels   = ['' for _ in range(len(types) - 1)],
                    codomain = types[-1])

                specialized_candidates.extend(flatten(
                    specialize_with_pattern(
                        self.environment.deepwalk(candidate), specializer, node)
                    for candidate in compatible_candidates))

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
                        "function call do not match any candidate in '{}'".format(
                        ', '.join(map(str, callee_type))))

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
                    candidate_domains = TypeUnion(candidate_domains.types)
                    self.environment.unify(candidate_domains, argument_type)

            result = TypeUnion()
            for candidate in selected_candidates:
                result.add(self.environment[candidate.codomain])

            for codomain in selected_codomains:
                result.add(selected_codomains)

            # Avoid creating singletons when there's only one candidate.
            if len(result) == 1:
                result = result.types[0]
                selected_candidates = selected_candidates[0]

            node.__meta__['type'] = result
            node.__meta__['function_call_type'] = selected_candidates
            return result

            # TODO Handle variadic arguments
            # A possible approach would be to transform the Call nodes of the
            # AST whenever we visit a variadic parameter, so that we regroup
            # the arguments that can be considered part of the variadic
            # argument. Depending on the definitive syntax and restrictions
            # we'll adopt for variadics parameters, we might have to check
            # multiple configurations of argument groups.

        assert False, "no type inference for node '{}'".format(node.__class__.__name__)

    def read_type_instance(self, node):
        t = self.analyse(node)
        if isinstance(t, TypeName):
            t = Type

        # If the type isn't an union or a variable, and doesn't specify any
        # modifier, we return an union of all possible combinations of type
        # modifiers so that we can infer them later, by elimination.
        if not isinstance(t, (TypeUnion, TypeVariable)) and (t.modifiers == 0):
            return type_factory.make_variants(t)
        return t

    def read_type_reference(self, node):
        # If the node is a type identifier, we first build a type instance
        # from the signature it represents.
        if isinstance(node, TypeIdentifier):
            # If the node doesn't have a signature (i.e. it only specifies
            # type attributes), we create a type variable.
            if node.signature is None:
                return type_factory.make_variable(
                    id        = hex(id(node)),
                    modifiers = node.modifiers)

                # FIXME: Do we need a better variable ID?

            t = self.analyse(node.signature)

            # If provided, the signature of a TypeIdentifier is either a name
            # referring to a nominal type or a structural type (e.g. a
            # function signature). In the former case, we've to use the
            # denoted type rather than that of the symbol.
            if isinstance(t, TypeName):
                t = t.type

            # The signature of a TypeIdentifier should never be a TypeUnion,
            # nor a TypeVariable.
            assert not isinstance(t, (TypeUnion, TypeVariable))

            # Finally we have to apply the type modifiers, as specified by the
            # type identifier.
            return type_factory.updating(t, modifiers=node.modifiers)

        t = self.analyse(node)

        # If the node represents a typename, we've to use the denoted type
        # rather than that of the symbol.
        if isinstance(t, TypeName):
            t = t.type

        # If the type isn't an union or a variable, and doesn't specify any
        # modifier, we return an union of all possible combinations of type
        # modifiers so that we can infer them later, by elimination.
        if not isinstance(t, (TypeUnion, TypeVariable)) and (t.modifiers == 0):
            return type_factory.make_variants(t)
        return t

    def is_typename(self, node):
        if isinstance(node, Identifier):
            return (('scope' in node.__meta__)
                    and (node.name in node.__meta__['scope'].typenames))

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

            elif isinstance(expr_type, TypeBase) and (operator in expr_type.members):
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

    assert False, 'cannot specialize {}'.format(unspecialized.__class__.__name__)


def specialize_from_annotation(unspecialized, type_annotation):
    # It shouldn't be able to express a generic type annotation, since generic
    # parameters aren't allowed in the syntax of function signatures.
    assert not type_annotation.is_generic()

    # Only function types can specialize a generic type.
    if not isinstance(type_annotation, FunctionType):
        raise InferenceError("type {} does not match {}".format(type_annotation, unspecialized))

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
            "no candidate in '{}' is compatible with type annotation '{}'".format(
            (','.join(map(str, unspecialized)), type_annotation)))

    return TypeUnion(specialized)


def flatten(iterable):
    return (i for it in iterable for i in it)


def varof(node):
    return type_factory.make_variable(id=(node.__meta__['scope'], node.name))
