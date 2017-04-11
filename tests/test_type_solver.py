import unittest

from tango.builtin import Int, String, Double, Nothing
from tango.parser import parse
from tango.scope_binder import bind_scopes
from tango.type_checker import TypeSolver, TypeVariable
from tango.types import FunctionType, TypeUnion
from tango.utils import find


class TestParser(unittest.TestCase):

    def test_constant_decl(self):
        solver = TypeSolver()
        module = self.prepare('cst x = 0')
        solver.visit(module)
        declaration_node = find('ConstantDecl:first', module)[0]
        self.assertEqual(solver.environment[TypeVariable(declaration_node)], Int)

        solver = TypeSolver()
        module = self.prepare('cst x: Int')
        solver.visit(module)
        declaration_node = find('ConstantDecl:first', module)[0]
        self.assertEqual(solver.environment[TypeVariable(declaration_node)], Int)

        solver = TypeSolver()
        module = self.prepare('cst x: Int = 0')
        solver.visit(module)
        declaration_node = find('ConstantDecl:first', module)[0]
        self.assertEqual(solver.environment[TypeVariable(declaration_node)], Int)

        solver = TypeSolver()
        module = self.prepare('cst x: (cst _: Int) -> Nothing')
        solver.visit(module)
        declaration_node = find('ConstantDecl:first', module)[0]
        x_type = solver.environment[TypeVariable(declaration_node)]
        self.assertIsInstance(x_type, FunctionType)

        solver = TypeSolver()
        module = self.prepare(
        '''
        cst x
        cst y = x
        cst z: Int = y
        '''
        )
        solver.visit(module)
        declaration_nodes = find('ConstantDecl', module)
        for declaration_node in declaration_nodes:
            self.assertEqual(solver.environment[TypeVariable(declaration_node)], Int)

    def test_function_decl(self):
        solver = TypeSolver()
        module = self.prepare('fun f() {}')
        solver.visit(module)
        declaration_node = find('FunctionDecl:first', module)[0]
        f_type = solver.environment[TypeVariable(declaration_node)]
        self.assertIsInstance(f_type, TypeUnion)
        self.assertEqual(len(f_type), 1)
        self.assertFalse(f_type.first().generic_parameters)
        self.assertFalse(f_type.first().domain)
        self.assertFalse(f_type.first().labels)
        self.assertEqual(f_type.first().codomain, Nothing)

        solver = TypeSolver()
        module = self.prepare('fun f(cst x: Int, cst _ y: String) -> Double {}')
        solver.visit(module)
        declaration_node = find('FunctionDecl:first', module)[0]
        f_type = solver.environment[TypeVariable(declaration_node)]
        self.assertIsInstance(f_type, TypeUnion)
        self.assertEqual(len(f_type), 1)
        self.assertFalse(f_type.first().generic_parameters)
        self.assertEqual(len(f_type.first().domain), 2)
        self.assertEqual(f_type.first().domain[0], Int)
        self.assertEqual(f_type.first().domain[1], String)
        self.assertEqual(len(f_type.first().labels), 2)
        self.assertEqual(f_type.first().labels[0], 'x')
        self.assertIsNone(f_type.first().labels[1])
        self.assertEqual(f_type.first().codomain, Double)

        solver = TypeSolver()
        module = self.prepare('fun f<T, U>(cst x: T, cst _ y: U) -> T {}')
        solver.visit(module)
        declaration_node = find('FunctionDecl:first', module)[0]
        f_type = solver.environment[TypeVariable(declaration_node)]
        self.assertIsInstance(f_type, TypeUnion)
        self.assertEqual(len(f_type), 1)
        self.assertEqual(len(f_type.first().generic_parameters), 2)
        self.assertEqual(len(f_type.first().domain), 2)
        self.assertEqual(f_type.first().domain[0], f_type.first().generic_parameters['T'])
        self.assertEqual(f_type.first().domain[1], f_type.first().generic_parameters['U'])
        self.assertEqual(len(f_type.first().labels), 2)
        self.assertEqual(f_type.first().labels[0], 'x')
        self.assertIsNone(f_type.first().labels[1])
        self.assertEqual(f_type.first().codomain, f_type.first().generic_parameters['T'])

    def test_shadowing(self):
        solver = TypeSolver()
        module = self.prepare(
        '''
        cst x = 0
        fun f() { cst x = 'Hello, World!' }
        '''
        )
        solver.visit(module)
        declaration_nodes = find('ConstantDecl', module)
        self.assertEqual(solver.environment[TypeVariable(declaration_nodes[0])], Int)
        self.assertEqual(solver.environment[TypeVariable(declaration_nodes[1])], String)

        solver = TypeSolver()
        module = self.prepare(
        '''
        cst x = 0
        fun f() { cst x = x }
        '''
        )
        solver.visit(module)
        declaration_nodes = find('ConstantDecl', module)
        self.assertEqual(solver.environment[TypeVariable(declaration_nodes[0])], Int)
        self.assertEqual(solver.environment[TypeVariable(declaration_nodes[1])], Int)

    def test_parameters_overloading(self):
        solver = TypeSolver()
        module = self.prepare(
        '''
        fun f() {}
        fun f(cst x: Int) {}
        '''
        )
        solver.visit(module)
        declaration_node = find('FunctionDecl:first', module)[0]
        f_type = solver.environment[TypeVariable(declaration_node)]
        self.assertIsInstance(f_type, TypeUnion)
        self.assertEqual(len(f_type), 2)

    def test_return_type_overloading(self):
        solver = TypeSolver()
        module = self.prepare(
        '''
        fun f() -> Int {}
        fun f() -> String {}
        '''
        )
        solver.visit(module)
        declaration_node = find('FunctionDecl:first', module)[0]
        f_type = solver.environment[TypeVariable(declaration_node)]
        self.assertIsInstance(f_type, TypeUnion)
        self.assertEqual(len(f_type), 2)

    def prepare(self, source):
        return bind_scopes(parse(source))
