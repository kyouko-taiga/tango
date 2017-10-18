from itertools import chain, product

from .ast import *
from .builtin import Bool, Nothing, Type
from .errors import InferenceError
from .scope import Scope
from .types import (
    TypeModifier as TM, TM_COMBINATIONS,
    TypeBase, TypeName, TypeUnion, TypeVariable,
    PlaceholderType, FunctionType, StructType,
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

    # QUESTION: Is there any reason why we should deepwalk the type variables
    # during the type inference? Right now it's being done by the TypesFinder
    # and the during the specialization of generic functions, but I wonder if
    # that's just legacy from previous versions of the solver.

    # Solve the static dispatching of function calls.
    dispatcher = Dispatcher()
    dispatcher.visit(node)

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

    def flatten_type_set(self, type_set):
        result = set()
        for t in type_set:
            t = self[t]
            if isinstance(t, TypeUnion):
                result |= self.flatten_type_set(t.types)
            else:
                result.add(t)
        return result

    def unify(self, t, u, memo=None):
        assert isinstance(t, TypeBase) and isinstance(u, TypeBase)

        if memo is None:
            memo = set()
        if (t, u) in memo:
            return
        memo.add((t, u))

        a = self[t]
        b = self[u]

        if a == b:
            return

        if isinstance(a, TypeVariable):
            if (a.modifiers == 0) or (a.modifiers == b.modifiers):
                self.storage[a] = b

            elif isinstance(b, TypeUnion):
                matching_types = []
                for itb in (self[t] for t in b):
                    if (itb.modifiers == 0) or (a.modifiers == itb.modifiers):
                        matching_types.append(itb)
                if not matching_types:
                    raise InferenceError(f"cannot unify '{a}' with any candidate in '{b}'")
                self.storage[a] = b
                b.types = matching_types

            else:
                raise InferenceError(f"cannot unify '{a}' with '{b}'")

        elif isinstance(b, TypeVariable):
            self.unify(b, a, memo)

        elif isinstance(a, TypeUnion) and isinstance(b, TypeUnion):
            walked_a = [self[t] for t in a]
            walked_b = [self[t] for t in b]

            # Compute the intersection of a and b.
            types = []
            for ita, itb in product(walked_a, walked_b):
                if self.matches(ita, itb):
                    types.append(ita)
            if not types:
                raise InferenceError(f"cannot '{a}' with '{b}'")

            # Unify all variables of a with b.
            for ita in walked_a:
                if isinstance(ita, TypeVariable):
                    try:
                        self.unify(ita, TypeUnion(walked_b))
                    except InferenceError:
                        continue

            # Unify all variables of b with a.
            for itb in walked_b:
                if isinstance(itb, TypeVariable):
                    try:
                        self.unify(itb, TypeUnion(walked_a))
                    except InferenceError:
                        continue

            # a = b = a & b
            a.types = types
            b.types = types

        elif isinstance(a, TypeUnion):
            result = []
            for ita in a:
                try:
                    self.unify(ita, b, memo)
                    result.append(ita)
                except InferenceError:
                    continue
            a.types = list(self.flatten_type_set(result))

        elif isinstance(b, TypeUnion):
            self.unify(b, a, memo)

        elif isinstance(a, StructType) and isinstance(b, StructType):
            if (a.name != b.name) or (set(a.members.keys()) ^ set(b.members.keys())):
                raise InferenceError(f"cannot unify '{a}' with '{b}'")
            for it in a.members.keys():
                self.unify(a.members[it], b.members[it], memo)

            # FIXME Should we unify defining scopes too?

        elif isinstance(a, FunctionType) and isinstance(b, FunctionType):
            for ita, itb in zip(a.domain, b.domain):
                self.unify(ita, itb, memo)
            self.unify(a.codomain, b.codomain, memo)

        else:
            raise InferenceError(f"cannot unify '{a}' with '{b}'")

    def matches(self, t, u):
        a = self[t]
        b = self[u]

        if a == b:
            return True

        if isinstance(a, TypeVariable) or isinstance(b, TypeVariable):
            return ((a.modifiers == b.modifiers)
                or (a.modifiers == 0)
                or (b.modifiers == 0)
                or isinstance(a, TypeUnion) and any(self.matches(it, b) for it in a)
                or isinstance(b, TypeUnion) and any(self.matches(it, a) for it in b))

        if isinstance(a, PlaceholderType) and (a.specialization is not None):
            return self.matches(a.specialization, b)
        elif isinstance(b, PlaceholderType):
            return self.matches(b, a)

        if isinstance(a, TypeUnion):
            return any(self.matches(it, b) for it in a)
        if isinstance(b, TypeUnion):
            return self.matches(b, a)

        if isinstance(a, StructType) and isinstance(b, StructType):
            return ((a.modifiers == b.modifiers)
                and (a.name == b.name)
                and not (set(a.members.keys()) ^ set(b.members.keys()))
                and all(self.matches(a.members[it], b.members[it]) for it in a.members.keys()))

        # Note that unification of function types is stricter than what
        # `matches` checks because it requires `a` and `b` to be equal under
        # `__eq__`. Relaxing the constraint here is what lets us matching a
        # generic signature to its specialization when visiting Call nodes.
        # However, we normally only unify generic signatures with themselves
        # in FunDecl nodes, so having a more relaxed match function shouldn't
        # cause any issue.
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

        if isinstance(t, StructType):
            # memo[t] = t
            # for name in t.members.keys():
            #     t.members[name] = self.deepwalk(t.members[name], memo)
            # return t

            # FIXME: Updating the members of a nominal type as above would
            # bypass the equality check of the factory, which could make the
            # returned type unequal to the original one under `==`.
            # Unfortunately, we can't call `type_factory.make_struct` without
            # first waling all the struct's members, which means we're faced
            # with a cycle issue.
            pass

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

    def __init__(self):
        # A simple substitution map: (TypeVariable) -> Type.
        self.environment = Substitution()

        # In nominal types declarations (e.g. structs), `Self` should act as a
        # placeholder for the type under declaration. This stack lets us bind
        # such placeholders.
        self.current_self_type = []

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
                    TypeUnion(types) if len(types) > 1 else types[0])

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
        # We assign the type variable associated with the declared symbol to
        # the node's metadata. After unification, it will be reified to the
        # type of the symbol, which we'll use during code generation.
        node.__meta__['type'] = varof(node)

        # If there isn't neither a type annotation, nor an initial value, we
        # can't infer any additional type information.
        if not (node.type_annotation or node.initial_value):
            # IDEA: Maybe we should create a default type annotation while we
            # build the AST with the default modifiers, so that we would fall
            # back to the inference of a annotated variables below. It'd be
            # easier to ensure that properties declared with type annotations
            # are treated the same way properties declared without are.
            return

        # If there's an initial value, we infer its type.
        if node.initial_value:
            rvalue_type = self.analyse(node.initial_value)

            # If there's a type annotation, we read that signature, otherwise
            # we assume a type variable with modifiers `@cst`.
            if node.type_annotation:
                annotation_type = self.analyse_type_expression(node.type_annotation)
            else:
                if 'annotation_type' not in node.__meta__:
                    node.__meta__['annotation_type'] = type_factory.make_variable()
                annotation_type = node.__meta__['annotation_type']

            # Infer the property's type from its binding and propagate the
            # type constraints accordingly.
            (lts, rts) = infer_binding_types(annotation_type, node.initial_binding, rvalue_type)
            self.environment.unify(rvalue_type, rts)
            self.environment.unify(annotation_type, lts)
            inferred_type = lts

        # If there isn't an initializing value, we should simply use the type
        # annotation.
        else:
            inferred_type = self.analyse_type_expression(node.type_annotation)

        # Choose the most restrictive variant of the inferred type.
        if isinstance(inferred_type, TypeUnion):
            candidates = set([
                type_factory.updating(t, modifiers=0) for t in inferred_type
            ])

            result = TypeUnion()
            for candidate in candidates:
                for modifiers in TM_COMBINATIONS:
                    t = type_factory.updating(candidate, modifiers=modifiers)
                    if t in inferred_type:
                        result.add(t)
                        break

            if len(result) == 1:
                inferred_type = result.types[0]
            else:
                inferred_type = result

        # Finally, we should unify the inferred type with the type variable
        # corresponding to the symbol under declaration.
        node.__meta__['scope'][node.name].type = inferred_type
        self.environment.unify(varof(node), inferred_type)

    def visit_FunDecl(self, node):
        # First, we create (unless we already did) a placeholder type object
        # for each of the function's generic placeholders (if any).
        inner_scope = node.body.__meta__['scope']
        for placeholder_name in node.placeholders:
            var = type_factory.make_variable(id=(inner_scope, placeholder_name))
            if var not in self.environment:
                placeholder_type = type_factory.make_placeholder(id=placeholder_name)
                self.environment.unify(
                    var,
                    type_factory.make_name(name=placeholder_name, type=placeholder_type))

        # Unlike variables, function parameters are always declared with a
        # type annotation, so we can type them directly.
        parameter_types  = []
        parameter_labels = []
        for parameter in node.parameters:
            type_annotation = self.analyse_type_expression(parameter.type_annotation)

            # We should unify the type we read from the annotations with the
            # type variable corresponding to the parameter name, so that it'll
            # be typed in the function's body.
            parameter_type = varof(parameter)
            self.environment.unify(parameter_type, type_annotation)
            parameter.__meta__['type'] = type_annotation

            parameter_types.append(type_annotation)
            parameter_labels.append(parameter.label)

        # The type of the codomain is either a type identifier, or Nothing.
        if node.codomain_annotation:
            codomain = self.analyse_type_expression(node.codomain_annotation)
        else:
            codomain = Nothing

        # Once we've computed the function signature, we can create a type
        # for the function itself.
        function_type = type_factory.make_function(
            modifiers = TM.cst | TM.stk | TM.val,
            domain    = parameter_types,
            labels    = parameter_labels,
            codomain  = codomain)

        # As functions may be overloaded, we can't unify the function type
        # we've created with the function's name directly. Instead, we should
        # put it in a TypeUnion to potentially include the signature of the
        # function's overloads, as we find them.
        walked = self.environment[varof(node)]
        if isinstance(walked, TypeVariable):
            overload_set = TypeUnion([function_type])
            self.environment.unify(walked, overload_set)
        elif isinstance(walked, FunctionType):
            overload_set = TypeUnion([walked])
            overload_set.add(function_type)
        elif isinstance(walked, TypeUnion) and isinstance(walked.types[0], FunctionType):
            overload_set = walked
            overload_set.add(function_type)
        else:
            raise InferenceError("cannot overload '{}' with '{}'".format(walked, function_type))

        # TODO: We should also consider the overloads defined in enclosing
        # scopes or defined in other modules.

        # We continue the type inference in the function body.
        self.visit(node.body)

        # Finally, we have to unify the type of the return values of the
        # function with its return type.
        return_finder = TypeSolver.ReturnFinder()
        return_finder.visit(node)
        for statement in return_finder.return_statements:
            return_value_type = statement.value.__meta__['type']
            self.environment.unify(return_value_type, function_type.codomain)

        # NOTE: Checking whether or not the function has a return statement in
        # all its execution paths shouldn't be performed here, but in the pass
        # that analyses the program's CFG.

        for symbol in node.__meta__['scope'].getlist(node.name):
            if symbol.code == node:
                symbol.type = function_type

        # We associate the function type we've created to the AST node, so
        # that code generation may emit the correct type.
        node.__meta__['type'] = function_type

        # We associate the set of overloads to the symbol, so that type
        # inference may consider other candidates.
        self.environment[varof(node)] = overload_set

    def visit_StructDecl(self, node):
        # We create a new nomal type (unless we already did).
        inferred_type = self.environment[varof(node)]
        if isinstance(inferred_type, TypeVariable):
            # Define a helper that retrieves the type of an inner symbol.
            inner_scope = node.body.__meta__['scope']
            def member_type(name):
                return self.environment[type_factory.make_variable(id=(inner_scope, name))]

            # Create the struct type.
            inferred_type = type_factory.make_struct(
                name    = node.name,
                members = {
                    name: member_type(name) for name in node.body.__meta__['symbols']
                })
            inferred_type = type_factory.make_name(name=node.name, type=inferred_type)

            node.__meta__['scope'][node.name].type = inferred_type
            self.environment.unify(varof(node), inferred_type)

            # Bind the `Self` placeholder.
            inner_scope['Self'].type = inferred_type
            self.environment.unify(
                type_factory.make_variable(id=(inner_scope, 'Self')),
                inferred_type)

        self.visit(node.body)
        node.__meta__['type'] = inferred_type

    def visit_Assignment(self, node):
        # We first infer the types of the lvalue and rvalue.
        lvalue_type = self.analyse(node.lvalue)
        rvalue_type = self.analyse(node.rvalue)

        # Infer the expressions' types from the binding operator and propagate
        # the type constraints accordingly.
        (lts, rts) = infer_binding_types(lvalue_type, node.operator, rvalue_type)
        self.environment.unify(lvalue_type, lts)
        self.environment.unify(rvalue_type, rts)

    def visit_Call(self, node):
        # While we don't need to unify the return type of the function, we
        # still need to do it for its arguments.
        self.analyse(node)

    def visit_If(self, node):
        # We infer the type of the node's condition.
        condition_type = self.analyse(node.condition)

        # The condition of an if expressions should always be a boolean, so we
        # can unify the type of the node's condition with Bool.
        expected_type = type_factory.updating(Bool, modifiers=TM.cst | TM.stk | TM.val)
        self.environment.unify(condition_type, expected_type)

        # Then we can visit the node's body.
        self.visit(node.then_block)
        if node.else_block:
            self.visit(node.else_block)

    def visit_Return(self, node):
        self.analyse(node.value)

    def analyse(self, node):
        # If the node is a simple literal, its type should have been inferred
        # by the parser already.
        if isinstance(node, (IntLiteral, DoubleLiteral, StringLiteral, BoolLiteral)):
            return type_factory.make_variants(node.__meta__['type'])

        # If the node is an identifier ...
        if isinstance(node, Identifier):
            # Create a fresh variable, unless we already did.
            if node.__meta__['type'] is None:
                node.__meta__['type'] = self.environment[varof(node)]

            if isinstance(node.__meta__['type'], TypeName):
                # FIXME: If the identifier isn't used a callee or as the owner
                # of a select expression, we should return `Type` here, rather
                # than the referred type. This might be tricky to ensure
                # thought, which is why we may consider adopting Swift's
                # `SomeType.self` syntax.
                node.__meta__['type'] = node.__meta__['type'].type

            return node.__meta__['type']

        # If the node is a select expression ...
        if isinstance(node, Select):
            # First, we need to infer the type of the owner.
            owner_types = self.analyse(node.owner)
            if isinstance(owner_types, TypeUnion):
                prospects = owner_types
            else:
                prospects = (owner_types,)

            candidates = []
            for owner_type in prospects:
                # If the owner's type is a type variable, we have no choice
                # but to return another unrelated type variable.
                if isinstance(owner_type, TypeVariable):
                    candidates.append(type_factory.make_variable())
                    continue

                # If the owner's type isn't a type name, it means the owner is
                # a type instance, rather than a reference to the type itself.
                # If the member is a function, we'll have to perform automatic
                # self binding.
                as_instance_member = not isinstance(owner_type, TypeName)
                if not as_instance_member:
                    owner_type = owner_type.type

                # If the owner's type contains a member named after the
                # select's member, we can add the latter as candidate.
                if (isinstance(owner_type, StructType)
                    and node.member.name in owner_type.members.keys()):

                    member_types = self.environment[owner_type.members[node.member.name]]
                    if not isinstance(member_types, TypeUnion):
                        member_types = (member_types,)

                    for member_type in member_types:
                        if as_instance_member and isinstance(member_type, FunctionType):
                            candidates.append(type_factory.make_function(
                                domain   = list(member_type.domain)[1:],
                                labels   = list(member_type.labels)[1:],
                                codomain = member_type.codomain))
                        else:
                            candidates.append(member_type)

            if len(candidates) == 0:
                raise InferenceError(
                    f"no candidates in {owner_types} has a member '{node.member.name}'")

            # Avoid creating signletons when there's only one result.
            if len(candidates) == 1:
                result = candidates[0]
            else:
                result = TypeUnion(candidates)

            self.unify_node_type(node, result)

            # TODO: Handle genericity.

        # If the node is a call expression ...
        if isinstance(node, Call):
            # First, we get the type of the callee's node.
            callee_type = self.analyse(node.callee)

            if isinstance(callee_type, TypeUnion):
                prospects = callee_type
            else:
                prospects = (callee_type,)

            # For each type the callee can represent, we identify which can be
            # candidate for a function call.
            candidates = []
            selected_codomains = []
            for signature in prospects:
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
                if isinstance(signature, StructType):
                    if '__new__' not in signature.members:
                        continue

                    constructors = self.environment[signature.members['__new__']]
                    if not isinstance(constructors, TypeUnion):
                        constructors = (constructors,)

                    for constructor in constructors:
                        # If the constructor is a variable, we may not know
                        # its signature yet, but we know its codomain anyway,
                        # so we can add it to the pre-selected codomains.
                        if isinstance(constructor, TypeVariable):
                            selected_codomains.append(signature)

                        # Otherwise, we build a new signature `(...) -> T`
                        # that represents the constructor.
                        assert isinstance(constructor, FunctionType)
                        candidates.append(type_factory.make_function(
                            domain   = list(constructor.domain)[1:],
                            labels   = list(constructor.labels)[1:],
                            codomain = signature))

                        # TODO: Generic placeholders.

            # If we can't find any candidate, we return the codomains we've
            # selected so far (if any).
            if not candidates:
                if not selected_codomains:
                    raise InferenceError(
                        "function call do not match any candidate in '{}'".format(
                            ', '.join(map(str, callee_type))))

                # Avoid creating signletons when there's only one result.
                if len(selected_codomains) == 1:
                    return_type = selected_codomains[0]
                else:
                    return_type = TypeUnion(selected_codomains)

                self.unify_node_type(node, return_type)
                return return_type

            # Then, we have to infer the type of each argument.
            argument_types = []
            for argument in node.arguments:
                # FIXME read the type of the whole argument (for binding policy)
                argument_type = self.analyse(argument.value)
                argument_types.append(argument_type)

            # Either the return type was already inferred in a previous pass,
            # or create a new variable for it.
            return_type = node.__meta__['type'] or type_factory.make_variable()

            # Once we've got signature candidates and argument types, we can
            # filter out signatures that aren't compatible with the argument
            # types we inferred.
            compatible_candidates = []
            for signature in candidates:
                assert isinstance(signature, FunctionType)

                # Check if the number of parameters matches.
                if len(signature.domain) != len(node.arguments):
                    continue

                # Check if the argument labels match.
                valid = True
                for i, argument in zip(range(len(node.arguments)), node.arguments):
                    expected_label = signature.labels[i]
                    if (expected_label == '_') and (argument.label is not None):
                        valid = False
                        break
                    elif expected_label != argument.label:
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
                    labels   = ['_' for _ in range(len(types) - 1)],
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

            # If we can't find any candidate, we make sure we at least have
            # some pre-selected codomains.
            if not selected_candidates and not selected_codomains:
                raise InferenceError(
                    f"function call do not match any candidate in '{callee_type}'")

            # NOTE: We can't unify here, because the callee's type might be
            # associated with the symbol of the function (or type) used as
            # callee. This would be problematic if that symbol's type is
            # overloaded and/or generic.
            # So instead we dissociate the symbol's type from that associated
            # with the callee's node.
            selected_candidates = TypeUnion(selected_candidates)
            node.callee.__meta__['type'] = selected_candidates

            # Unify the types of the arguments with the domain of the selected
            # candidates to propagate type constraints.
            for candidate in selected_candidates:
                for i, argument_type in enumerate(argument_types):
                    arg = self.environment[candidate.domain[i]]
                    if isinstance(arg, PlaceholderType):
                        assert arg.specialization is not None, 'unexpected partial specialization'
                        arg = arg.specialization

                    # FIXME: Will this work with generic types as arguments?

            # We unify the argument types of the node with the domain of the
            # selected candidates, to propagate type constraints.
            for i, argument_type in enumerate(argument_types):
                domain_candidates = TypeUnion()
                for candidate in selected_candidates:
                    arg = self.environment[candidate.domain[i]]
                    if isinstance(arg, PlaceholderType):
                        assert arg.specialization is not None, 'unexpected partial specialization'
                        arg = arg.specialization

                    # FIXME: Will this work with generic types as arguments?
                    domain_candidates.add(arg)

                if domain_candidates.types:
                    self.environment.unify(domain_candidates, argument_type)

            return_type = TypeUnion()
            for candidate in selected_candidates:
                codomain = candidate.codomain
                if isinstance(codomain, PlaceholderType):
                    assert codomain.specialization is not None, 'unexpected partial specialization'
                    codomain = codomain.specialization
                return_type.add(self.environment[codomain])

            for codomain in selected_codomains:
                # TODO: Check if we may have placeholder types as selected
                # codomains.
                return_type.add(selected_codomains)

            # Avoid creating singletons when there's only one candidate.
            if len(return_type) == 1:
                return_type = return_type.types[0]
            if len(selected_candidates) == 1:
                selected_candidates = selected_candidates.types[0]

            self.unify_node_type(node, return_type)
            node.__meta__['dispatch_type'] = selected_candidates
            return return_type

            # TODO: Keep track of what specializations of generic functions
            # (and maybe types?) are expected to be generated.

            # TODO: Handle variadic arguments
            # A possible approach would be to transform the Call nodes of the
            # AST whenever we visit a variadic parameter, so that we regroup
            # the arguments that can be considered part of the variadic
            # argument. Depending on the definitive syntax and restrictions
            # we'll adopt for variadics parameters, we might have to check
            # multiple configurations of argument groups.

        assert False, "no type inference for node '{}'".format(node.__class__.__name__)

    def analyse_type_expression(self, node):
        assert isinstance(node, TypeIdentifier)

        # If the node doesn't have a signature (i.e. it only specifies
        # type attributes), we create a type variable.
        if node.signature is None:
            # FIXME: Do we need a better variable ID?
            var_id = hex(id(node))

            result = TypeUnion()
            for modifiers in TM_COMBINATIONS:
                if modifiers & node.modifiers:
                    result.add(type_factory.make_variable(modifiers=modifiers, id=var_id))

        # If the signature is an identifier, we'll check the environment.
        elif isinstance(node.signature, Identifier):
            # Get the type associated with the identifier in its scope.
            t = self.environment[varof(node.signature)]

            # If the type is unkown yet, we can't infer further.
            if isinstance(t, TypeVariable):
                return node.__meta__['type']

            # Otherwise it should be a type name, from which we'll extract the
            # actual referred type.
            assert isinstance(t, TypeName)
            assert isinstance(t.type, (StructType, PlaceholderType,))
            t = t.type

            # Finally apply the type modifiers.
            result = type_factory.updating(
                t, modifiers=node.modifiers or (TM.cst | TM.stk | TM.val))

        # If the signature is a function signature, we'll create the
        # corresponding function type.
        elif isinstance(node.signature, FunSign):
            domain = []
            labels = []
            for parameter in node.signature.parameters:
                type_annotation = self.analyse_type_expression(parameter.type_annotation)
                domain.append(type_annotation)
                labels.append(parameter.label)

            codomain = self.analyse_type_expression(node.signature.codomain_annotation)

            result = type_factory.make_function(
                modifiers  = node.modifiers or (TM.cst | TM.stk | TM.val),
                domain     = domain,
                labels     = labels,
                codomain   = codomain)

        else:
            # TODO: Handle tuple signatures.
            assert False, f"unexpected type signature '{node.__class__}'"

        if isinstance(result, TypeUnion) and len(result) == 1:
            result = result.types[0]
        node.__meta__['type'] = result
        return result

    def unify_node_type(self, node, node_type):
        if node.__meta__['type'] is not None:
            self.environment.unify(node.__meta__['type'], node_type)
        else:
            node.__meta__['type'] = node_type


class Dispatcher(NodeVisitor):

    def visit_Call(self, node):
        # Keep track of the callee's required specializations.
        if isinstance(node.callee, Identifier):
            scope = node.callee.__meta__['scope']

            dispatch_type = node.__meta__['dispatch_type']
            if isinstance(dispatch_type, (TypeUnion, list)):
                raise InferenceError(f"multiple candidates found to call '{node}'")

            found = False
            for symbol in scope.getlist(node.callee.name):
                if isspecialization(dispatch_type, symbol.type):
                    if found:
                        raise InferenceError(f"multiple candidates found to call '{node}'")
                    found = True
                    symbol.specializations.add(dispatch_type)

        # TODO: Handle non-identifier callees (i.e. expressions that return a
        # generic function).

        # TODO: Propagate the specialization requirements to all reachable
        # scopes the callee's symbol is defined in.


def infer_binding_types(lvalue_type, op, rvalue_type):
    if not isinstance(lvalue_type, TypeUnion):
        lvalue_type = TypeUnion([lvalue_type])
    if not isinstance(rvalue_type, TypeUnion):
        rvalue_type = TypeUnion([rvalue_type])

    # A copy binding preserves the rvalue's type, but takes the lvalue's type
    # modifiers. The lvalue's type can have any modifier.
    if op == Operator.cpy:
        # The rvalue's type can be any of the lvalue's type variants.
        rresult = TypeUnion()
        for lty in lvalue_type:
            for t in type_factory.make_variants(lty):
                rresult.add(t)

        # We preserve the modifiers of the lvalue's types.
        lresult = TypeUnion()
        for lty in lvalue_type:
            # If the lvalue's type doesn't have any modifiers, we assume
            # `@cst @stk @val` by default.
            modifiers = lty.modifiers or (TM.cst | TM.stk | TM.val)
            for rty in rvalue_type:
                lresult.add(type_factory.updating(rty, modifiers=modifiers))

    # A move binding always produces `@val` types.
    if op == Operator.mov:
        # The rvalue's type can be any of the lvalue's type `@val` variants.
        rresult = TypeUnion()
        for lty in lvalue_type:
            for t in type_factory.make_variants(lty):
                if t.modifiers & TM.val:
                    rresult.add(t)

        # The same can be said for the lvalue's type.
        lresult = TypeUnion()
        for rty in rvalue_type:
            for t in type_factory.make_variants(rty):
                if t.modifiers & TM.val:
                    lresult.add(t)

        # NOTE: If the rvalue is a `@ref` type, we could forbid the use of a
        # move binding operator here. But we'll leave that error to be handled
        # by the reference checker.

        # NOTE: The statement `x <- y` is illegal if `x` is a shared variable,
        # but we'll let that error to be handled by the reference checker.

    # A reference binding always produces `@ref` types.
    if op == Operator.ref:
        # The rvalue's type can be any of the lvalue's type variants.
        rresult = TypeUnion()
        for lty in lvalue_type:
            for t in type_factory.make_variants(lty):
                rresult.add(t)

        # The lvalue's type can be any `@ref` variant of the rvalue's type.
        lresult = TypeUnion()
        for rty in rvalue_type:
            for t in type_factory.make_variants(rty):
                if t.modifiers & TM.ref:
                    lresult.add(t)

        # NOTE: The statement `x <- y` is illegal if `x` is a shared variable,
        # but we'll let that error to be handled by the reference checker.

    # Avoid singletons.
    if len(lresult) == 1:
        lresult = lresult.types[0]
    if len(rresult) == 1:
        rresult = rresult.types[0]

    return (lresult, rresult)

    assert False


def specialize_with_pattern(unspecialized, specializer, call_node, specializations=None):
    if not unspecialized.is_generic:
        yield unspecialized
        return

    specializations = specializations if (specializations is not None) else {}
    if unspecialized in specializations:
        # FIXME Maybe we should check whether the stored specialization result
        # is compatible with the replacements we otherwise would have yielded.
        for t in specializations[unspecialized]:
            yield t
        return

    if isinstance(unspecialized, PlaceholderType):
        specializers = specializer if isinstance(specializer, TypeUnion) else (specializer,)
        for candidate in specializers:
            if candidate.is_generic:
                yield TypeVariable(id=(id(candidate), id(call_node)))
            elif unspecialized.modifiers == candidate.modifiers:
                yield type_factory.make_placeholder(
                    modifiers      = unspecialized.modifiers,
                    id             = unspecialized.id,
                    specialization = candidate)

            # Note that we don't use the specializer candidate if it doesn't
            # have the same type modifiers as the unspecialized argument.

        return

    if isinstance(unspecialized, FunctionType):
        # If the specializer isn't a function type as well, we can't do
        # anything.
        if not isinstance(specializer, FunctionType):
            return

        specialized_domain = []

        for original, replacement in zip(unspecialized.domain, specializer.domain):
            if original.is_generic:
                specialized = list(specialize_with_pattern(
                    original, replacement, call_node, specializations))
                specializations[original] = specialized
                specialized_domain.append(specialized)
            else:
                specialized_domain.append((original,))

        if unspecialized.codomain.is_generic:
            specialized = list(specialize_with_pattern(
                unspecialized.codomain, specializer.codomain, call_node, specializations))
            specializations[unspecialized.codomain] = specialized
            specialized_codomain = specialized
        else:
            specialized_codomain = (unspecialized.codomain,)

        # FIXME Determine if a replacement can be a type union (or `specialize` returned
        # more than one result for the domain or codomain). If it can't, then we may
        # assume this cartesian product will always be a singleton.
        for specialized in product(*chain(specialized_domain, (specialized_codomain,))):
            yield type_factory.make_function(
                modifiers = unspecialized.modifiers,
                domain    = list(specialized[0:-1]),
                codomain  = specialized[-1],
                labels    = list(unspecialized.labels))

        return

    assert False, 'cannot specialize {}'.format(unspecialized.__class__.__name__)


def isspecialization(left, right):
    if right.is_generic:
        specializations = list(specialize_with_pattern(right, left, object()))
        if not specializations:
            return False
        assert len(specializations) == 1
        right = specializations[0]

    if isinstance(left, FunctionType):
        if not isinstance(right, FunctionType):
            return False
        if len(left.domain) != len(right.domain):
            return False
        for (ltype, rtype) in zip(left.domain, right.domain):
            if not isspecialization(ltype, rtype):
                return False
        return isspecialization(left.codomain, right.codomain)

    if isinstance(left, PlaceholderType):
        return isspecialization(left.specialization, right)
    if isinstance(right, PlaceholderType):
        return isspecialization(left, right.specialization)

    return left == right


def flatten(iterable):
    return (i for it in iterable for i in it)


def varof(node):
    return type_factory.make_variable(id=(node.__meta__['scope'], node.name))
