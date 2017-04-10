from itertools import product
from warnings import warn

from .ast import *
from .builtin import builtin_scope
from .errors import UndefinedSymbol, InferenceError
from .types import BaseType, FunctionType, StructType, TypeUnion


def infer_types(node):
    # We first visit all nodes to infer the type of the expressions.
    type_deducer = TypeSolver()

    # FIXME We should compute a fixed point here.
    type_deducer.visit(node)
    type_deducer.visit(node)

    return type_deducer.environment.reified().storage


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
            return TypeUnion(self.deepwalk(it) for it in t)

        if isinstance(t, FunctionType):
            return FunctionType(
                domain = [self.deepwalk(p) for p in t.domain],
                codomain = self.deepwalk(t.codomain),
                labels = t.labels,
                generic_parameters = [self.deepwalk(g) for g in t.generic_parameters])

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
                print('%s\t(%s)\t:%s' % (symbol, scope.uri + '.' + name, inferred_type))
            else:
                print(symbol, inferred_type)


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


class GenericType(BaseType):

    def __init__(self, name, specialization=None):
        self.name = name
        self.specialization = specialization

    @property
    def is_specialized(self):
        return self.specialization is not None

    def copy(self):
        return GenericType(name=self.name, specialization=self.specialization)

    def __eq__(self, other):
        return ((type(self) == type(other))
                and (self.name == other.name)
                and (self.specialization == other.specialization)
                or  (self.specialization == other))

    def __str__(self):
        if self.specialization:
            return str(self.specialization)
        return '(unspecialized %s)' % self.name


class TypeSolver(Visitor):

    def __init__(self):
        self.environment = Substitution({
            TypeVariable(id=(builtin_scope, symbol)): type
            for symbol, type in builtin_scope.members.items()
        })

        # Methods, properties and enum cases may use `Self` as a placeholder
        # to denote the "final" type they're defined in. This stack will serve
        # us to keep track of the actual type `Self` represents, depending on
        # the context we'll be evaluation typing informations.
        self.current_self_type = []

    def eval_type_signature(self, signature):
        if isinstance(signature, BaseType):
            return signature

        if isinstance(signature, TypeIdentifier):
            if signature.name == 'Self':
                try:
                    return self.current_self_type[-1]
                except IndexError:
                    raise InferenceError("invalid use of 'Self' outside of a type declaration")

            try:
                return self.environment[TypeVariable(signature)]
            except KeyError:
                raise UndefinedSymbol(signature.name)

            # TODO Handle abstract types

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
        # First, we need to create unspecialized generic types for each of the
        # function's generic parameters (if any).
        generic_types = []
        for symbol in node.generic_parameters:
            var = TypeVariable((node.body.__info__['scope'], symbol))
            if var not in self.environment:
                generic_types.append(GenericType(symbol))
                self.environment[var] = generic_types[-1]
            else:
                generic_types.append(self.environment[var])

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
            labels = [parameter.label for parameter in node.signature.parameters],
            generic_parameters = generic_types)

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
            # Make sure the current node isn't child of the given overload to
            # avoid infinite recursions. Note that this implicitly disallows
            # functions of a given scope to be overloaded by the function that
            # defines them.
            if not overload.is_ancestor_of(node):
                self.visit(overload)
                overload_set.add(self.environment[TypeVariable(overload)])
            else:
                warn(
                    "'%(function_name)s' in scope '%(outer_scope)s' will be ignored "
                    "from the overloads of '%(function_name)s' in scope '%(inner_scope)s' "
                    "because its name is shadowed in scope '%(inner_scope)s'." % {
                        'function_name': node.name,
                        'outer_scope': overload.__info__['scope'].uri,
                        'inner_scope': node.__info__['scope'].uri,
                    },
                    category = FutureWarning)

        # Finally, we should continue the type inference in the function body.
        self.visit(node.body)

    def visit_StructDecl(self, node):
        # The body of a struct can be visited as a normal satement block, as
        # long as we push a variable on the `current_self_type` stack before,
        # to properly type references to `Self`.
        struct_name = TypeVariable(node)
        self.current_self_type.append(struct_name)
        self.visit(node.body)
        self.current_self_type.pop()

        # Once we've inferred the type of all its members, we can create the
        # type for the struct itself.
        member_scope = node.body.__info__['scope']
        struct_type = StructType(
            name = node.name,
            members = {
                name: self.environment[TypeVariable((member_scope, name))]
                for name in node.body.__info__['symbols']
            })
        self.environment.unify(struct_name, struct_type)

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
            generic_specializations = {}
            for expected_type, argument_type in zip(signature.domain, argument_types):
                # If the expected type is a generic, make sure it's either
                # still unspecialized, or its specialization matches the
                # inferred argument type.
                if isinstance(expected_type, GenericType):
                    specialization = generic_specializations.get(expected_type.name)
                    if (specialization is not None) and not matches(specialization, argument_type):
                        valid = False
                        break
                    generic_specializations[expected_type.name] = argument_type

                elif not matches(expected_type, argument_type):
                    valid = False
                    break
            if not valid:
                continue

            candidates.append(signature)

        if len(candidates) == 0:
            raise InferenceError(
                "function call do not match any candidate for types '%s'" % callee_signatures)

        # Generate the possible function types to be associated with the node.
        candidates = [instantiate(sig, argument_types) for sig in candidates]

        if len(candidates) == 1:
            result = candidates[0]
        else:
            result = TypeUnion(candidate.codomain for candidate in candidates)

        node.__info__['type'] = result
        return result.codomain

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


def instantiate(generic_signature, specialized_argument_types):
    old_generics = generic_signature.generic_parameters
    new_generics = [g.copy() for g in old_generics]
    generic_names = [g.name for g in old_generics]

    parameter_types = []
    for original, replacement in zip(generic_signature.domain, specialized_argument_types):
        if isinstance(original, GenericType):
            new_generic = new_generics[generic_names.index(original.name)]
            assert new_generic.specialization in (None, replacement)
            new_generic.specialization = replacement
            parameter_types.append(new_generic)
        else:
            parameter_types.append(replacement)

    if isinstance(generic_signature.codomain, GenericType):
        return_type = new_generics[old_generics.index(generic_signature.codomain)]
    else:
        return_type = generic_signature.return_type

    return FunctionType(
        domain = parameter_types,
        codomain = return_type,
        labels = generic_signature.labels,
        generic_parameters = new_generics)

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
                and all(matches(t.members[it], u.members[it]) for it in t))

    return t == u
