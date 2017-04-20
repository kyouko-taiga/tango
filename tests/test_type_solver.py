import unittest

from tango.builtin import Bool, Double, Int, Nothing, String, Type
from tango.errors import InferenceError
from tango.parser import parse
from tango.scope_binder import bind_scopes
from tango.type_checker import TypeVariable, infer_types
from tango.types import EnumType, FunctionType, GenericType, StructType, TypeUnion
from tango.utils import find


class TestTypeSolver(unittest.TestCase):

    def test_container_decl(self):
        module = self.prepare('cst x = 0')
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Int)

        module = self.prepare('cst x: Int')
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Int)

        module = self.prepare('cst x: Int = 0')
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Int)

        module = self.prepare('cst x: String = 0')
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

        module = self.prepare('cst x: (cst _: Int) -> Nothing')
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertIsInstance(x_type, FunctionType)
        self.assertEqual(x_type.domain, [Int])
        self.assertEqual(x_type.codomain, Nothing)
        self.assertEqual(x_type.labels, [None])

        module = self.prepare(
        '''
        mut x
        x = 1.0
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Double)

        module = self.prepare(
        '''
        mut x
        mut y = x
        mut z: Int = y
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        for node in declaration_nodes:
            self.assertEqual(self.type_of(node, environment), Int)

        # TODO Declaring a container wihout any inferrable type should raise
        # an error.

    def test_function_decl(self):
        module = self.prepare('fun f() {}')
        (module, environment) = infer_types(module)
        f_type = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertIsInstance(f_type, FunctionType)
        self.assertFalse(f_type.is_generic)
        self.assertFalse(f_type.domain)
        self.assertFalse(f_type.labels)
        self.assertEqual(f_type.codomain, Nothing)

        module = self.prepare('fun f(cst x: Int, cst _ y: String) -> Double {}')
        (module, environment) = infer_types(module)
        f_type = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertIsInstance(f_type, FunctionType)
        self.assertFalse(f_type.is_generic)
        self.assertEqual(f_type.domain, [Int, String])
        self.assertEqual(f_type.labels, ['x', None])
        self.assertEqual(f_type.codomain, Double)

        module = self.prepare('fun f<T, U>(cst x: T, cst _ y: U) -> T {}')
        (module, environment) = infer_types(module)
        f_type = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertIsInstance(f_type, FunctionType)
        self.assertTrue(f_type.is_generic)
        self.assertEqual(len(f_type.domain), 2)
        self.assertIsInstance(f_type.domain[0], GenericType)
        self.assertEqual(f_type.domain[0].signature, 'T')
        self.assertIsInstance(f_type.domain[1], GenericType)
        self.assertEqual(f_type.domain[1].signature, 'U')
        self.assertEqual(len(f_type.labels), 2)
        self.assertEqual(f_type.labels[0], 'x')
        self.assertIsNone(f_type.labels[1])
        self.assertIsInstance(f_type.codomain, GenericType)
        self.assertEqual(f_type.codomain.signature, 'T')

        module = self.prepare('fun f(cst x: Int = 0) {}')
        (module, environment) = infer_types(module)
        f_type = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertIsInstance(f_type, FunctionType)
        self.assertFalse(f_type.is_generic)
        self.assertEqual(f_type.domain, [Int])
        self.assertEqual(f_type.labels, ['x'])
        self.assertEqual(f_type.codomain, Nothing)

        module = self.prepare('fun f(cst x: Int = 1.0) {}')
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

        module = self.prepare('fun f<T>(cst x: T = 1.0) {}')
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

        module = self.prepare('fun f(cst x: (cst y: Int) -> Int) {}')
        (module, environment) = infer_types(module)
        f_type = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertIsInstance(f_type, FunctionType)
        self.assertFalse(f_type.is_generic)
        self.assertEqual(len(f_type.domain), 1)
        self.assertIsInstance(f_type.domain[0], FunctionType)
        self.assertEqual(f_type.domain[0].domain, [Int])
        self.assertEqual(f_type.domain[0].labels, ['y'])
        self.assertEqual(f_type.domain[0].codomain, Int)
        self.assertEqual(f_type.labels, ['x'])
        self.assertEqual(f_type.codomain, Nothing)

        module = self.prepare('fun f() -> (cst y: Int) -> Int {}')
        (module, environment) = infer_types(module)
        f_type = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertIsInstance(f_type, FunctionType)
        self.assertFalse(f_type.is_generic)
        self.assertFalse(f_type.domain)
        self.assertFalse(f_type.labels)
        self.assertIsInstance(f_type.codomain, FunctionType)
        self.assertEqual(f_type.codomain.domain, [Int])
        self.assertEqual(f_type.codomain.labels, ['y'])
        self.assertEqual(f_type.codomain.codomain, Int)

    def test_function_return_type_unification(self):
        module = self.prepare('fun f() -> Int { return 0 }')
        (module, environment) = infer_types(module)

        module = self.prepare('fun f() -> Int { return 0.0 }')
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

        module = self.prepare('fun f() { return 0.0 }')
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

        module = self.prepare('fun f<T>() -> T { return 0.0 }')
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

        module = self.prepare(
        '''
        fun f() -> Int {
            if true {
                return 0
            } else if false {
                return 1
            } else {
                return 2
            }
        }
        ''')
        (module, environment) = infer_types(module)

        module = self.prepare(
        '''
        fun f() -> Int {
            if true {
                return 0
            } else if false {
                return '1'
            } else {
                return 2
            }
        }
        ''')
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

        module = self.prepare(
        '''
        fun f() -> Int {
            if true {
                return 0
            } else if false {
                return 1
            } else {
                return '2'
            }
        }
        ''')
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

    def test_parameter_overloading(self):
        module = self.prepare(
        '''
        fun f(cst x: Int) {}
        fun f(cst x: String) {}
        ''')
        (module, environment) = infer_types(module)
        f_types = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertIsInstance(f_types, TypeUnion)
        self.assertEqual(len(f_types), 2)
        self.assertIn(Int,    (f.domain[0] for f in f_types.types))
        self.assertIn(String, (f.domain[0] for f in f_types.types))

        self.assertEqual(len(f_types.types[0].domain), 1)
        self.assertEqual(f_types.types[0].codomain, Nothing)

        self.assertEqual(len(f_types.types[1].domain), 1)
        self.assertEqual(f_types.types[1].codomain, Nothing)

        module = self.prepare(
        '''
        fun f(cst x: Int, cst y: Int) {}
        fun f(cst x: Int) {}
        ''')
        (module, environment) = infer_types(module)
        f_types = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertIsInstance(f_types, TypeUnion)
        self.assertEqual(len(f_types), 2)
        self.assertTrue((len(f_types.types[0].domain) == 1) or (len(f_types.types[1].domain) == 1))
        self.assertTrue((len(f_types.types[0].domain) == 2) or (len(f_types.types[1].domain) == 2))

        f0 = f_types.types[0] if len(f_types.types[0].domain) == 1 else f_types.types[1]
        self.assertEqual(f0.domain, [Int])
        self.assertEqual(f0.codomain, Nothing)

        f1 = f_types.types[0] if len(f_types.types[0].domain) == 2 else f_types.types[1]
        self.assertEqual(f1.domain, [Int, Int])
        self.assertEqual(f1.codomain, Nothing)

        # TODO Declaring multiple functions with the same signature should
        # raise an error.

    def test_label_overloading(self):
        module = self.prepare(
        '''
        fun f(cst a x: Int) {}
        fun f(cst b x: Int) {}
        ''')
        (module, environment) = infer_types(module)
        f_types = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertIsInstance(f_types, TypeUnion)
        self.assertEqual(len(f_types), 2)

        self.assertIn('a', (f.labels[0] for f in f_types))
        self.assertIn('b', (f.labels[0] for f in f_types))

        self.assertEqual(len(f_types.types[0].domain), 1)
        self.assertEqual(f_types.types[0].domain, [Int])
        self.assertEqual(f_types.types[0].codomain, Nothing)

        self.assertEqual(len(f_types.types[1].domain), 1)
        self.assertEqual(f_types.types[1].domain, [Int])
        self.assertEqual(f_types.types[1].codomain, Nothing)

        # TODO Declaring multiple functions with the same signature should
        # raise an error.

    def test_return_type_overloading(self):
        module = self.prepare(
        '''
        fun f() -> Int {}
        fun f() -> String {}
        ''')
        (module, environment) = infer_types(module)
        f_types = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertIsInstance(f_types, TypeUnion)
        self.assertEqual(len(f_types), 2)
        self.assertIn(Int,    (f.codomain for f in f_types))
        self.assertIn(String, (f.codomain for f in f_types))

        self.assertFalse(f_types.types[0].domain)
        self.assertFalse(f_types.types[1].domain)

        # TODO Declaring multiple functions with the same signature should
        # raise an error.

    def test_inner_scoped_overloading(self):
        module = self.prepare(
        '''
        fun f() -> Int {
            fun f() -> String {}
        }
        ''')
        (module, environment) = infer_types(module)
        function_nodes = find('FunctionDecl:*', module)

        outer_f = self.type_of(function_nodes[0], environment)
        self.assertIsInstance(outer_f, FunctionType)
        self.assertEqual(outer_f.codomain, Int)

        inner_f = self.type_of(function_nodes[1], environment)
        self.assertIsInstance(inner_f, TypeUnion)
        self.assertEqual(len(inner_f), 2)
        self.assertTrue(any(f == outer_f for f in inner_f))
        self.assertIn(Int,    (f.codomain for f in inner_f))
        self.assertIn(String, (f.codomain for f in inner_f))

        # TODO Declaring multiple functions with the same signature should
        # raise an error.

    def test_shadowing(self):
        module = self.prepare(
        '''
        cst x = 0
        fun f() { cst x = 'Hello, World!' }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        self.assertEqual(self.type_of(declaration_nodes[0], environment), Int)
        self.assertEqual(self.type_of(declaration_nodes[1], environment), String)

        module = self.prepare(
        '''
        cst x = 0
        fun f() { cst x = x }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        self.assertEqual(self.type_of(declaration_nodes[0], environment), Int)
        self.assertEqual(self.type_of(declaration_nodes[1], environment), Int)

    def test_struct_decl(self):
        module = self.prepare('struct S {}')
        (module, environment) = infer_types(module)
        s_type = self.type_of(find('StructDecl:first', module)[0], environment)
        self.assertIsInstance(s_type, StructType)
        self.assertEqual(s_type.name, 'S')

        module = self.prepare(
        '''
        struct S {
            cst foo: Int
            cst bar: String
        }
        '''
        )
        (module, environment) = infer_types(module)
        s_type = self.type_of(find('StructDecl:first', module)[0], environment)
        self.assertIsInstance(s_type, StructType)
        self.assertEqual(s_type.name, 'S')

        # TODO Fix reification of nominal types.
        self.assertEqual(environment[s_type.members['foo']], Int)
        self.assertEqual(environment[s_type.members['bar']], String)

        module = self.prepare(
        '''
        struct S {
            cst foo: Int
            cst bar: String

            fun baz(mut self: Self) {}
        }
        '''
        )
        (module, environment) = infer_types(module)
        s_type = self.type_of(find('StructDecl:first', module)[0], environment)
        self.assertIsInstance(s_type, StructType)
        self.assertEqual(s_type.name, 'S')

        # TODO Fix reification of nominal types.
        self.assertEqual(environment[s_type.members['foo']], Int)
        self.assertEqual(environment[s_type.members['bar']], String)
        self.assertIsInstance(environment[s_type.members['baz']], FunctionType)

        # TODO Declaring a method without Self as its first parameter should
        # raise an error.

    def test_enum_decl(self):
        module = self.prepare('enum E {}')
        (module, environment) = infer_types(module)
        e_type = self.type_of(find('EnumDecl:first', module)[0], environment)
        self.assertIsInstance(e_type, EnumType)
        self.assertEqual(e_type.name, 'E')

        module = self.prepare(
        '''
        enum E {
            case foo
            case bar(x: Int, y: Self)
        }
        '''
        )
        (module, environment) = infer_types(module)
        e_type = self.type_of(find('EnumDecl:first', module)[0], environment)
        self.assertIsInstance(e_type, EnumType)
        self.assertEqual(e_type.name, 'E')

        # TODO Fix reification of nominal types.
        self.assertEqual(environment[e_type.members['foo']], e_type)
        bar_type = environment[e_type.members['bar']]
        self.assertIsInstance(bar_type, FunctionType)
        self.assertEqual(len(bar_type.domain), 2)
        self.assertEqual(bar_type.domain, [Int, e_type])
        self.assertEqual(bar_type.codomain, e_type)

        module = self.prepare(
        '''
        enum E {
            case foo
            case bar(x: Int, y: Self)

            fun baz(mut self: Self) {}
        }
        '''
        )
        (module, environment) = infer_types(module)
        e_type = self.type_of(find('EnumDecl:first', module)[0], environment)
        self.assertIsInstance(e_type, EnumType)
        self.assertEqual(e_type.name, 'E')

        # TODO Fix reification of nominal types.
        self.assertEqual(environment[e_type.members['foo']], e_type)
        self.assertIsInstance(environment[e_type.members['bar']], FunctionType)
        self.assertIsInstance(environment[e_type.members['baz']], FunctionType)

    def test_annotating_custom_nominal_type(self):
        module = self.prepare(
        '''
        struct S {}
        cst x: S
        '''
        )
        (module, environment) = infer_types(module)
        s_type = self.type_of(find('StructDecl:first', module)[0], environment)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, s_type)

        module = self.prepare(
        '''
        cst x: S
        struct S {}
        '''
        )
        (module, environment) = infer_types(module)
        s_type = self.type_of(find('StructDecl:first', module)[0], environment)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, s_type)

        module = self.prepare(
        '''
        enum E {}
        cst x: E
        '''
        )
        (module, environment) = infer_types(module)
        e_type = self.type_of(find('EnumDecl:first', module)[0], environment)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, e_type)

        module = self.prepare(
        '''
        cst x: E
        enum E {}
        '''
        )
        (module, environment) = infer_types(module)
        e_type = self.type_of(find('EnumDecl:first', module)[0], environment)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, e_type)

    def test_nested_types(self):
        module = self.prepare(
        '''
        enum E {
            struct S { cst y: Self }
        }
        cst x: E.S
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        self.assertEqual(self.type_of(declaration_nodes[0], environment).name, 'S')
        self.assertEqual(self.type_of(declaration_nodes[1], environment).name, 'S')

        module = self.prepare(
        '''
        cst x: E.S.F
        enum E {
            struct S {
                enum F {}
            }
        }
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type.name, 'F')

    def test_assignment(self):
        module = self.prepare(
        '''
        cst x
        x = 0
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Int)

        module = self.prepare(
        '''
        cst x: Int
        cst y
        x = y
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        self.assertEqual(self.type_of(declaration_nodes[0], environment), Int)
        self.assertEqual(self.type_of(declaration_nodes[1], environment), Int)

        module = self.prepare(
        '''
        cst x: Int
        cst y: String
        x = y
        '''
        )
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

    def test_functions_as_first_class(self):
        module = self.prepare(
        '''
        fun f(cst x: Int, cst y: Int) -> Int {}
        cst x = f
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        f_type = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertEqual(x_type, f_type)

        module = self.prepare(
        '''
        cst x = f
        fun f(cst x: Int, cst y: Int) -> Int {}
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        f_type = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertEqual(x_type, f_type)

        module = self.prepare(
        '''
        cst x = f
        fun f(cst x: Int, cst y: Int) -> Int {}
        fun f(cst x: String, cst y: String) -> String {}
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        f_type = self.type_of(find('FunctionDecl:first', module)[0], environment)
        self.assertEqual(x_type, f_type)

    def test_nominal_types_as_first_class(self):
        module = self.prepare('cst x = Int')
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Type)

        module = self.prepare(
        '''
        struct S {}
        cst x = S
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Type)

        module = self.prepare(
        '''
        cst x = S
        struct S {}
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Type)

        module = self.prepare(
        '''
        enum E {}
        cst x = E
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Type)

        module = self.prepare(
        '''
        cst x = E
        enum E {}
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Type)

    def test_call_without_overloading(self):
        module = self.prepare(
        '''
        fun f() -> Int {}
        cst x = f()
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Int)

        module = self.prepare(
        '''
        cst x = f(x: 0, y: 0)
        fun f(cst x: Int, cst y: Int) -> Int {}
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Int)

        module = self.prepare(
        '''
        cst x = f(x: g)

        fun f(cst x: (cst y: Int) -> Int) -> Int {}
        fun g(cst y: Int) -> Int {}
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl:first', module)[0], environment)
        self.assertEqual(x_type, Int)

        module = self.prepare(
        '''
        fun f(cst x: Int, cst y: Int) -> Int {}
        cst x = f(x: 0, z: 0)
        '''
        )
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

        module = self.prepare(
        '''
        fun f(cst x: Int, cst y: Int) -> Int {}
        cst x = f(x: 0, 0)
        '''
        )
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

        module = self.prepare(
        '''
        fun f(cst x: Int, cst y: Int) -> Int {}
        cst x = f(x: 0, y: 0.0)
        '''
        )
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

        module = self.prepare(
        '''
        cst x = f(x: g)

        fun f(cst x: (cst y: Int) -> Int) -> Int {}
        fun g(cst x: Int) -> Int {}
        '''
        )
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

    def test_call_with_overloading(self):
        module = self.prepare(
        '''
        fun f(cst x: Int, cst y: Int) -> Int {}
        fun f(cst x: String, cst y: String) -> String {}

        cst x = f(x: 0, y: 0)
        cst y = f(x: 'hello', y: 'world')
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[0], environment)
        self.assertEqual(x_type, Int)
        y_type = self.type_of(declaration_nodes[1], environment)
        self.assertEqual(y_type, String)

        module = self.prepare(
        '''
        cst x = f(x: 0, y: 0)
        cst y = f(x: 'hello', y: 'world')

        fun f(cst x: Int, cst y: Int) -> Int {}
        fun f(cst x: String, cst y: String) -> String {}
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[0], environment)
        self.assertEqual(x_type, Int)
        y_type = self.type_of(declaration_nodes[1], environment)
        self.assertEqual(y_type, String)

        module = self.prepare(
        '''
        fun f(cst x: Int, cst y: Int) -> Int {}
        fun f(cst _ x: String, cst _ y: String) -> String {}

        cst x = f(x: 0, y: 0)
        cst y = f('hello', 'world')
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[0], environment)
        self.assertEqual(x_type, Int)
        y_type = self.type_of(declaration_nodes[1], environment)
        self.assertEqual(y_type, String)

        module = self.prepare(
        '''
        cst x = f()

        fun f() -> Int {}
        fun f() -> String {}
        '''
        )
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl', module)[0], environment)
        self.assertIsInstance(x_type, TypeUnion)
        self.assertIn(Int, x_type)
        self.assertIn(String, x_type)

    def test_call_with_generic_parameters(self):
        module = self.prepare(
        '''
        fun f<T, U>(cst x: T, cst y: U) -> T {}

        cst x = f(x: 0, y: 'hello world')
        cst y = f(x: 'hello world', y: 0)
        cst z = f(x: 1.0, y: 2.0)
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[0], environment)
        self.assertEqual(x_type, Int)
        y_type = self.type_of(declaration_nodes[1], environment)
        self.assertEqual(y_type, String)
        y_type = self.type_of(declaration_nodes[2], environment)
        self.assertEqual(y_type, Double)

        module = self.prepare(
        '''
        cst x = f(x: g)

        fun f<T>(cst x: T) -> T {}
        fun g() {}
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[0], environment)
        self.assertIsInstance(x_type, FunctionType)
        self.assertFalse(x_type.domain)
        self.assertFalse(x_type.labels)
        self.assertEqual(x_type.codomain, Nothing)

        module = self.prepare(
        '''
        cst x = f(x: g)
        cst y = f(x: h)

        fun f<T, U>(cst x: (cst y: T, cst z: U) -> U) -> T {}
        fun g(cst y: Int, cst z: Int) -> Int {}
        fun h(cst y: String, cst z: Double) -> Double {}
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[0], environment)
        self.assertEqual(x_type, Int)
        y_type = self.type_of(declaration_nodes[1], environment)
        self.assertEqual(y_type, String)

        module = self.prepare(
        '''
        cst x = f(x: f(x: g))

        fun f<T>(cst x: T) -> T {}
        fun g(cst x: Int) -> Int {}
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[0], environment)
        self.assertIsInstance(x_type, FunctionType)
        self.assertEqual(x_type.domain, [Int])
        self.assertEqual(x_type.codomain, Int)

    def test_call_constraints_propagation(self):
        module = self.prepare(
        '''
        cst x = f()
        cst y = g(x)
        cst z = h(y)

        fun f() -> Int {}
        fun f() -> String {}
        fun f() -> Double {}

        fun g(cst _ arg: Int) -> Int {}
        fun g(cst _ arg: String) -> String {}
        fun g<T>(cst _ arg: T) -> T {}

        fun h(cst _ arg: Int) -> Int {}
        fun h(cst _ arg: Double) -> Int {}
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[0], environment)
        self.assertIsInstance(x_type, TypeUnion)
        self.assertIn(Int, x_type)
        self.assertIn(Double, x_type)
        y_type = self.type_of(declaration_nodes[1], environment)
        self.assertIsInstance(y_type, TypeUnion)
        self.assertIn(Int, y_type)
        self.assertIn(Double, y_type)
        z_type = self.type_of(declaration_nodes[2], environment)
        self.assertEqual(z_type, Int)

    def test_select(self):
        module = self.prepare(
        '''
        cst s: S
        cst x = s.foo
        cst y = s.bar

        struct S {
            cst foo: Int
            cst bar: String
        }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[1], environment)
        self.assertEqual(x_type, Int)
        y_type = self.type_of(declaration_nodes[2], environment)
        self.assertEqual(y_type, String)

        # TODO Selecting a property statically (e.g. `cst x = S.foo`) should
        # raise an error.

    def test_auto_self_binding(self):
        module = self.prepare(
        '''
        cst s: S
        cst x = s.baz
        cst y = S.baz

        struct S {
            fun baz(mut self: Self) -> Int {}
        }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        s_type = self.type_of(declaration_nodes[0], environment)
        x_type = self.type_of(declaration_nodes[1], environment)
        self.assertIsInstance(x_type, FunctionType)
        self.assertFalse(x_type.domain)
        self.assertFalse(x_type.labels)
        self.assertEqual(x_type.codomain, Int)
        y_type = self.type_of(declaration_nodes[2], environment)
        self.assertIsInstance(y_type, FunctionType)
        self.assertEqual(y_type.domain, [s_type])
        self.assertEqual(y_type.labels, ['self'])
        self.assertEqual(y_type.codomain, Int)

        module = self.prepare(
        '''
        cst x = Point()
        cst y = x.distance(to: x)

        struct Point {
            fun new(cst self: Self) -> Self {}
            fun distance(cst self: Self, cst to other: Self) -> Double {}
        }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[0], environment)
        self.assertIsInstance(x_type, StructType)
        self.assertEqual(x_type.name, 'Point')
        y_type = self.type_of(declaration_nodes[1], environment)
        self.assertEqual(y_type, Double)

    def test_constructor(self):
        module = self.prepare(
        '''
        cst s = S()

        struct S {
            fun new(mut self: Self) -> Self {}
        }
        '''
        )
        (module, environment) = infer_types(module)
        s_type = self.type_of(find('ContainerDecl', module)[0], environment)
        self.assertIsInstance(s_type, StructType)
        self.assertEqual(s_type.name, 'S')

    def test_enum_case_constructor(self):
        module = self.prepare(
        '''
        cst x = E.foo
        cst y = E.bar(x: 0, y: E.foo)

        enum E {
            case foo
            case bar(x: Int, y: Self)
        }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[0], environment)
        self.assertIsInstance(x_type, EnumType)
        self.assertEqual(x_type.name, 'E')
        y_type = self.type_of(declaration_nodes[1], environment)
        self.assertIsInstance(y_type, EnumType)
        self.assertEqual(y_type.name, 'E')

        module = self.prepare(
        '''
        cst x: E = .foo
        cst y: E = .bar(x: 0, y: .foo)

        enum E {
            case foo
            case bar(x: Int, y: Self)
        }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[0], environment)
        self.assertIsInstance(x_type, EnumType)
        self.assertEqual(x_type.name, 'E')
        y_type = self.type_of(declaration_nodes[1], environment)
        self.assertIsInstance(y_type, EnumType)
        self.assertEqual(y_type.name, 'E')

    def test_prefixed_expression(self):
        module = self.prepare('cst x = -0')
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl', module)[0], environment)
        self.assertEqual(x_type, Int)

        module = self.prepare(
        '''
        cst s: S
        cst x = not s

        struct S {
            fun not(cst _ self: Self) -> Self {}
        }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        s_type = self.type_of(declaration_nodes[0], environment)
        x_type = self.type_of(declaration_nodes[1], environment)
        self.assertEqual(x_type, s_type)

        module = self.prepare(
        '''
        cst s: S
        cst x = not s

        struct S {
            fun not(cst _ self: Self) -> Bool {}
        }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[1], environment)
        self.assertEqual(x_type, Bool)

    def test_binary_expression(self):
        module = self.prepare('cst x = 0 + 2')
        (module, environment) = infer_types(module)
        x_type = self.type_of(find('ContainerDecl', module)[0], environment)
        self.assertEqual(x_type, Int)

        module = self.prepare(
        '''
        cst s: S
        cst x = s + s

        struct S {
            fun +(cst _ lhs: Self, cst _ rhs: Self) -> Self {}
        }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        s_type = self.type_of(declaration_nodes[0], environment)
        x_type = self.type_of(declaration_nodes[1], environment)
        self.assertEqual(x_type, s_type)

        module = self.prepare(
        '''
        cst s: S
        cst x = s + 0

        struct S {
            fun +(cst _ lhs: Self, cst _ rhs: Int) -> Int {}
        }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        x_type = self.type_of(declaration_nodes[1], environment)
        self.assertEqual(x_type, Int)

    def test_if_expression(self):
        module = self.prepare('if true {} else {}')
        (module, environment) = infer_types(module)

        module = self.prepare('if false {} else if 1 < 3 {}')
        (module, environment) = infer_types(module)

        module = self.prepare(
        '''
        cst x: Bool
        if x {}
        '''
        )
        (module, environment) = infer_types(module)

        module = self.prepare('if 0 {}')
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

    def test_id_expression_with_pattern(self):
        module = self.prepare(
        '''
        cst e: E = .bar(x: 0, y: .foo)
        if let cst a, cst b in e == .bar(x: a, y: b) {}

        enum E {
            case foo
            case bar(x: Int, y: Self)

            fun == (cst _ lhs: Self, cst _ rhs: Self) -> Bool {}
        }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        a_type = self.type_of(declaration_nodes[1], environment)
        self.assertEqual(a_type, Int)
        b_type = self.type_of(declaration_nodes[2], environment)
        self.assertIsInstance(b_type, EnumType)
        self.assertEqual(b_type.name, 'E')

    def type_of(self, node, environment):
        return environment.storage[TypeVariable(node)]

    def prepare(self, source):
        return bind_scopes(parse(source))
