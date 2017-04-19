import unittest

from tango.builtin import Int, String, Double, Nothing
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
        declaration_node = find('ContainerDecl:first', module)[0]
        self.assertEqual(environment.storage[TypeVariable(declaration_node)], Int)

        module = self.prepare('cst x: Int')
        (module, environment) = infer_types(module)
        declaration_node = find('ContainerDecl:first', module)[0]
        self.assertEqual(environment.storage[TypeVariable(declaration_node)], Int)

        module = self.prepare('cst x: Int = 0')
        (module, environment) = infer_types(module)
        declaration_node = find('ContainerDecl:first', module)[0]
        self.assertEqual(environment.storage[TypeVariable(declaration_node)], Int)

        module = self.prepare('cst x: String = 0')
        with self.assertRaises(InferenceError):
            (module, environment) = infer_types(module)

        module = self.prepare('cst x: (cst _: Int) -> Nothing')
        (module, environment) = infer_types(module)
        declaration_node = find('ContainerDecl:first', module)[0]
        x_type = environment.storage[TypeVariable(declaration_node)]
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
        declaration_node = find('ContainerDecl:first', module)[0]
        self.assertEqual(environment.storage[TypeVariable(declaration_node)], Double)

        module = self.prepare(
        '''
        mut x
        mut y = x
        mut z: Int = y
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        for declaration_node in declaration_nodes:
            self.assertEqual(environment.storage[TypeVariable(declaration_node)], Int)

        # TODO Declaring a container wihout any inferrable type should raise
        # an error.

    def test_function_decl(self):
        module = self.prepare('fun f() {}')
        (module, environment) = infer_types(module)
        declaration_node = find('FunctionDecl:first', module)[0]
        f_type = environment.storage[TypeVariable(declaration_node)]
        self.assertIsInstance(f_type, FunctionType)
        self.assertFalse(f_type.is_generic)
        self.assertFalse(f_type.domain)
        self.assertFalse(f_type.labels)
        self.assertEqual(f_type.codomain, Nothing)

        module = self.prepare('fun f(cst x: Int, cst _ y: String) -> Double {}')
        (module, environment) = infer_types(module)
        declaration_node = find('FunctionDecl:first', module)[0]
        f_type = environment.storage[TypeVariable(declaration_node)]
        self.assertIsInstance(f_type, FunctionType)
        self.assertFalse(f_type.is_generic)
        self.assertEqual(len(f_type.domain), 2)
        self.assertEqual(f_type.domain[0], Int)
        self.assertEqual(f_type.domain[1], String)
        self.assertEqual(len(f_type.labels), 2)
        self.assertEqual(f_type.labels[0], 'x')
        self.assertIsNone(f_type.labels[1])
        self.assertEqual(f_type.codomain, Double)

        module = self.prepare('fun f<T, U>(cst x: T, cst _ y: U) -> T {}')
        (module, environment) = infer_types(module)
        declaration_node = find('FunctionDecl:first', module)[0]
        f_type = environment.storage[TypeVariable(declaration_node)]
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

    def test_parameter_overloading(self):
        module = self.prepare(
        '''
        fun f(cst x: Int) {}
        fun f(cst x: String) {}
        ''')
        (module, environment) = infer_types(module)
        declaration_node = find('FunctionDecl:first', module)[0]
        f_types = environment.storage[TypeVariable(declaration_node)]
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
        declaration_node = find('FunctionDecl:first', module)[0]
        f_types = environment.storage[TypeVariable(declaration_node)]
        self.assertIsInstance(f_types, TypeUnion)
        self.assertEqual(len(f_types), 2)
        self.assertTrue((len(f_types.types[0].domain) == 1) or (len(f_types.types[1].domain) == 1))
        self.assertTrue((len(f_types.types[0].domain) == 2) or (len(f_types.types[1].domain) == 2))

        f0 = f_types.types[0] if len(f_types.types[0].domain) == 1 else f_types.types[1]
        self.assertEqual(f0.domain[0], Int)
        self.assertEqual(f0.codomain, Nothing)

        f1 = f_types.types[0] if len(f_types.types[0].domain) == 2 else f_types.types[1]
        self.assertEqual(f1.domain[0], Int)
        self.assertEqual(f1.domain[1], Int)
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
        declaration_node = find('FunctionDecl:first', module)[0]
        f_types = environment.storage[TypeVariable(declaration_node)]
        self.assertIsInstance(f_types, TypeUnion)
        self.assertEqual(len(f_types), 2)

        self.assertIn('a', (f.labels[0] for f in f_types))
        self.assertIn('b', (f.labels[0] for f in f_types))

        self.assertEqual(len(f_types.types[0].domain), 1)
        self.assertEqual(f_types.types[0].domain[0], Int)
        self.assertEqual(f_types.types[0].codomain, Nothing)

        self.assertEqual(len(f_types.types[1].domain), 1)
        self.assertEqual(f_types.types[1].domain[0], Int)
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
        declaration_node = find('FunctionDecl:first', module)[0]
        f_types = environment.storage[TypeVariable(declaration_node)]
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
        declaration_nodes = find('FunctionDecl:*', module)

        outer_f = environment.storage[TypeVariable(declaration_nodes[0])]
        self.assertIsInstance(outer_f, FunctionType)
        self.assertEqual(outer_f.codomain, Int)

        inner_f = environment.storage[TypeVariable(declaration_nodes[1])]
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
        self.assertEqual(environment.storage[TypeVariable(declaration_nodes[0])], Int)
        self.assertEqual(environment.storage[TypeVariable(declaration_nodes[1])], String)

        module = self.prepare(
        '''
        cst x = 0
        fun f() { cst x = x }
        '''
        )
        (module, environment) = infer_types(module)
        declaration_nodes = find('ContainerDecl', module)
        self.assertEqual(environment.storage[TypeVariable(declaration_nodes[0])], Int)
        self.assertEqual(environment.storage[TypeVariable(declaration_nodes[1])], Int)

    def test_struct_decl(self):
        module = self.prepare('struct S {}')
        (module, environment) = infer_types(module)
        declaration_node = find('StructDecl:first', module)[0]
        s_type = environment.storage[TypeVariable(declaration_node)]
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
        declaration_node = find('StructDecl:first', module)[0]
        s_type = environment.storage[TypeVariable(declaration_node)]
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
        declaration_node = find('StructDecl:first', module)[0]
        s_type = environment.storage[TypeVariable(declaration_node)]
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
        declaration_node = find('EnumDecl:first', module)[0]
        e_type = environment.storage[TypeVariable(declaration_node)]
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
        declaration_node = find('EnumDecl:first', module)[0]
        e_type = environment.storage[TypeVariable(declaration_node)]
        self.assertIsInstance(e_type, EnumType)
        self.assertEqual(e_type.name, 'E')

        # TODO Fix reification of nominal types.
        self.assertEqual(environment[e_type.members['foo']], e_type)
        bar_type = environment[e_type.members['bar']]
        self.assertIsInstance(bar_type, FunctionType)
        self.assertEqual(len(bar_type.domain), 2)
        self.assertEqual(bar_type.domain[0], Int)
        self.assertEqual(bar_type.domain[1], e_type)
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
        declaration_node = find('EnumDecl:first', module)[0]
        e_type = environment.storage[TypeVariable(declaration_node)]
        self.assertIsInstance(e_type, EnumType)
        self.assertEqual(e_type.name, 'E')

        # TODO Fix reification of nominal types.
        self.assertEqual(environment[e_type.members['foo']], e_type)
        self.assertIsInstance(environment[e_type.members['bar']], FunctionType)
        self.assertIsInstance(environment[e_type.members['baz']], FunctionType)

    def test_declaration_order(self):
        module = self.prepare(
        '''
        struct S {}
        cst x: S
        '''
        )
        (module, environment) = infer_types(module)
        s_type = environment.storage[TypeVariable(find('StructDecl:first', module)[0])]
        declaration_node = find('ContainerDecl:first', module)[0]
        self.assertEqual(environment.storage[TypeVariable(declaration_node)], s_type)

        module = self.prepare(
        '''
        cst x: S
        struct S {}
        '''
        )
        (module, environment) = infer_types(module)
        s_type = environment.storage[TypeVariable(find('StructDecl:first', module)[0])]
        declaration_node = find('ContainerDecl:first', module)[0]
        self.assertEqual(environment.storage[TypeVariable(declaration_node)], s_type)

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
        self.assertEqual(environment.storage[TypeVariable(declaration_nodes[0])].name, 'S')
        self.assertEqual(environment.storage[TypeVariable(declaration_nodes[1])].name, 'S')

    def prepare(self, source):
        return bind_scopes(parse(source))
