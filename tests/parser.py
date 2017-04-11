import unittest
import tango.parser as tango
import tango.ast as ast

from funcparserlib.parser import finished, skip


class TestParser(unittest.TestCase):

    def test_identifier(self):
        parser = tango.identifier + skip(finished)

        for s in ['a', '_', 'abc', 'a1', '学生']:
            result = parser.parse(tango.tokenize(s))
            self.assertEqual(result, s)

    def test_operator_identifier(self):
        parser = tango.operator_identifier + skip(finished)

        result = parser.parse(tango.tokenize('+'))
        self.assertEqual(result, '+')

    def test_specialization_parameter(self):
        parser = tango.specialization_parameter + skip(finished)

        result = parser.parse(tango.tokenize('T: Int'))
        self.assertIsInstance(result, ast.SpecializationParameter)
        self.assertEqual(result.name, 'T')
        self.assertEqual(result.type_annotation.name, 'Int')

    def test_type_identifier(self):
        parser = tango.type_identifier + skip(finished)

        result = parser.parse(tango.tokenize('Int'))
        self.assertIsInstance(result, ast.TypeIdentifier)
        self.assertEqual(result.name, 'Int')
        self.assertFalse(result.specialization_parameters)

        result = parser.parse(tango.tokenize('Array[T: Int]'))
        self.assertIsInstance(result, ast.TypeIdentifier)
        self.assertEqual(result.name, 'Array')
        self.assertEqual(result.specialization_parameters[0].name, 'T')
        self.assertEqual(result.specialization_parameters[0].type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize('Dictionary[Key: Int, Value: String]'))
        self.assertIsInstance(result, ast.TypeIdentifier)
        self.assertEqual(result.name, 'Dictionary')
        self.assertEqual(result.specialization_parameters[0].name, 'Key')
        self.assertEqual(result.specialization_parameters[0].type_annotation.name, 'Int')
        self.assertEqual(result.specialization_parameters[1].name, 'Value')
        self.assertEqual(result.specialization_parameters[1].type_annotation.name, 'String')

    def test_function_parameter(self):
        parser = tango.function_parameter + skip(finished)

        result = parser.parse(tango.tokenize('cst x: Int'))
        self.assertIsInstance(result, ast.FunctionParameter)
        self.assertEqual(result.label, 'x')
        self.assertEqual(result.name, 'x')
        self.assertFalse(result.attributes)
        self.assertEqual(result.type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize('mut x: Int'))
        self.assertIn('mutable', result.attributes)

        result = parser.parse(tango.tokenize('cst a x: Int'))
        self.assertIsInstance(result, ast.FunctionParameter)
        self.assertEqual(result.label, 'a')
        self.assertEqual(result.name, 'x')
        self.assertFalse(result.attributes)
        self.assertEqual(result.type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize('cst a x: @attr1 @attr2 Int'))
        self.assertIsInstance(result, ast.FunctionParameter)
        self.assertEqual(result.label, 'a')
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.attributes[0], 'attr1')
        self.assertEqual(result.attributes[1], 'attr2')
        self.assertEqual(result.type_annotation.name, 'Int')

    def test_function_parameter_default_value(self):
        parser = tango.function_parameter + skip(finished)

        result = parser.parse(tango.tokenize('cst x: Int = 0'))
        self.assertIsInstance(result, ast.FunctionParameter)
        self.assertIsInstance(result.default_value, ast.Literal)

    def test_function_signature(self):
        parser = tango.function_signature + skip(finished)

        result = parser.parse(tango.tokenize('() -> Nothing'))
        self.assertIsInstance(result, ast.FunctionSignature)
        self.assertFalse(result.parameters)
        self.assertEqual(result.return_type.name, 'Nothing')

        result = parser.parse(tango.tokenize('(cst x: Int) -> Int'))
        self.assertIsInstance(result, ast.FunctionSignature)
        self.assertEqual(result.parameters[0].name, 'x')
        self.assertEqual(result.return_type.name, 'Int')

        result = parser.parse(tango.tokenize('(cst x: Int, cst y: Int) -> Int'))
        self.assertIsInstance(result, ast.FunctionSignature)
        self.assertEqual(result.parameters[0].name, 'x')
        self.assertEqual(result.parameters[1].name, 'y')
        self.assertEqual(result.return_type.name, 'Int')

        result = parser.parse(tango.tokenize('(cst a x: Int) -> Int'))
        self.assertIsInstance(result, ast.FunctionSignature)
        self.assertEqual(result.parameters[0].name, 'x')
        self.assertEqual(result.parameters[0].label, 'a')
        self.assertEqual(result.return_type.name, 'Int')

    def test_variable_identifier(self):
        parser = tango.variable_identifier + skip(finished)

        for s in ['a', '_', 'abc', 'a1', '学生', '+']:
            result = parser.parse(tango.tokenize(s))
            self.assertIsInstance(result, ast.Identifier)
            self.assertEqual(result.name, s)

    def test_select_expression(self):
        parser = tango.select_expression + skip(finished)

        result = parser.parse(tango.tokenize('a'))
        self.assertIsInstance(result, ast.Identifier)
        self.assertEqual(result.name, 'a')

        result = parser.parse(tango.tokenize('a.b.c'))
        self.assertIsInstance(result, ast.Select)
        self.assertIsInstance(result.owner, ast.Select)
        self.assertEqual(result.owner.owner.name, 'a')
        self.assertEqual(result.owner.member.name, 'b')
        self.assertEqual(result.member.name, 'c')

        result = parser.parse(tango.tokenize('Int.+'))
        self.assertIsInstance(result, ast.Select)
        self.assertEqual(result.owner.name, 'Int')
        self.assertEqual(result.member.name, '+')

    def test_prefixed_expression(self):
        parser = tango.pfx_expr + skip(finished)

        result = parser.parse(tango.tokenize('not x'))
        self.assertIsInstance(result, ast.PrefixedExpression)
        self.assertEqual(result.operator, 'not')
        self.assertEqual(result.operand.name, 'x')

    def test_binary_expression(self):
        parser = tango.bin_expr + skip(finished)

        result = parser.parse(tango.tokenize('0 + 1'))
        self.assertIsInstance(result, ast.BinaryExpression)
        self.assertEqual(result.operator, '+')
        self.assertEqual(result.left.value, '0')
        self.assertEqual(result.right.value, '1')

        result = parser.parse(tango.tokenize('0 + 1 + 2'))
        self.assertIsInstance(result, ast.BinaryExpression)
        self.assertIsInstance(result.right, ast.BinaryExpression)
        self.assertEqual(result.right.left.value, '1')
        self.assertEqual(result.right.right.value, '2')

    def test_binary_precedence(self):
        parser = tango.bin_expr + skip(finished)

        result = parser.parse(tango.tokenize('0 * 1 + 2'))
        self.assertEqual(result.operator, '+')
        self.assertEqual(result.left.operator, '*')
        self.assertEqual(result.left.left.value, '0')
        self.assertEqual(result.left.right.value, '1')
        self.assertEqual(result.right.value, '2')

        result = parser.parse(tango.tokenize('0 + 1 * 2'))
        self.assertEqual(result.operator, '+')
        self.assertEqual(result.left.value, '0')
        self.assertEqual(result.right.operator, '*')
        self.assertEqual(result.right.left.value, '1')
        self.assertEqual(result.right.right.value, '2')

    def test_assignment(self):
        parser = tango.assignment + skip(finished)

        result = parser.parse(tango.tokenize('x = 0'))
        self.assertIsInstance(result, ast.Assignment)
        self.assertEqual(result.lvalue.name, 'x')
        self.assertEqual(result.rvalue.value, '0')

    def test_return_statement(self):
        parser = tango.return_statement + skip(finished)

        result = parser.parse(tango.tokenize('return 0'))
        self.assertIsInstance(result, ast.Return)
        self.assertEqual(result.value.value, '0')

    def test_call_argument(self):
        parser = tango.call_argument + skip(finished)

        result = parser.parse(tango.tokenize('0'))
        self.assertIsInstance(result, ast.CallArgument)
        self.assertEqual(result.value.value, '0')
        self.assertIsNone(result.name)
        self.assertFalse(result.attributes)

        result = parser.parse(tango.tokenize('@attr1 @attr2 0'))
        self.assertIsInstance(result, ast.CallArgument)
        self.assertEqual(result.attributes[0], 'attr1')
        self.assertEqual(result.attributes[1], 'attr2')

        result = parser.parse(tango.tokenize('x: 0'))
        self.assertIsInstance(result, ast.CallArgument)
        self.assertEqual(result.value.value, '0')
        self.assertEqual(result.name, 'x')
        self.assertFalse(result.attributes)

        result = parser.parse(tango.tokenize('x: @attr1 @attr2 0'))
        self.assertIsInstance(result, ast.CallArgument)
        self.assertEqual(result.attributes[0], 'attr1')
        self.assertEqual(result.attributes[1], 'attr2')

    def test_call_expression(self):
        parser = tango.call_expression + skip(finished)

        result = parser.parse(tango.tokenize('f()'))
        self.assertIsInstance(result, ast.Call)
        self.assertEqual(result.callee.name, 'f')
        self.assertFalse(result.arguments)

        result = parser.parse(tango.tokenize('f(0, 1)'))
        self.assertIsInstance(result, ast.Call)
        self.assertEqual(result.arguments[0].value.value, '0')
        self.assertEqual(result.arguments[1].value.value, '1')

        result = parser.parse(tango.tokenize('f(x: 0, 1)'))
        self.assertIsInstance(result, ast.Call)
        self.assertEqual(result.arguments[0].value.value, '0')
        self.assertEqual(result.arguments[1].value.value, '1')

        result = parser.parse(tango.tokenize('f(x: 0, y: 1)'))
        self.assertIsInstance(result, ast.Call)
        self.assertEqual(result.arguments[0].value.value, '0')
        self.assertEqual(result.arguments[1].value.value, '1')

    def test_function_decl(self):
        parser = tango.function_decl + skip(finished)

        result = parser.parse(tango.tokenize('fun f() {}'))
        self.assertIsInstance(result, ast.FunctionDecl)
        self.assertEqual(result.name, 'f')
        self.assertFalse(result.generic_parameters)
        self.assertFalse(result.signature.parameters)
        self.assertEqual(result.signature.return_type.name, 'Nothing')

        result = parser.parse(tango.tokenize('fun f(cst x: Int) -> Int {}'))
        self.assertIsInstance(result, ast.FunctionDecl)
        self.assertEqual(result.name, 'f')
        self.assertFalse(result.generic_parameters)
        self.assertEqual(result.signature.parameters[0].name, 'x')
        self.assertEqual(result.signature.return_type.name, 'Int')

        result = parser.parse(tango.tokenize('fun f<T>(cst x: T) {}'))
        self.assertIsInstance(result, ast.FunctionDecl)
        self.assertEqual(result.name, 'f')
        self.assertEqual(result.generic_parameters[0], 'T')
        self.assertEqual(result.signature.parameters[0].name, 'x')
        self.assertEqual(result.signature.return_type.name, 'Nothing')

    def test_variable_decl(self):
        parser = tango.variable_decl + skip(finished)

        result = parser.parse(tango.tokenize('mut x'))
        self.assertIsInstance(result, ast.VariableDecl)
        self.assertEqual(result.name, 'x')
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initial_value)

        result = parser.parse(tango.tokenize('mut x: Int'))
        self.assertIsInstance(result, ast.VariableDecl)
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.type_annotation.name, 'Int')
        self.assertIsNone(result.initial_value)

    def test_constant_decl(self):
        parser = tango.constant_decl + skip(finished)

        result = parser.parse(tango.tokenize('cst x'))
        self.assertIsInstance(result, ast.ConstantDecl)
        self.assertEqual(result.name, 'x')
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initial_value)

        result = parser.parse(tango.tokenize('cst x: Int'))
        self.assertIsInstance(result, ast.ConstantDecl)
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.type_annotation.name, 'Int')
        self.assertIsNone(result.initial_value)

    def test_enum_case_parameter(self):
        parser = tango.enum_case_parameter + skip(finished)

        result = parser.parse(tango.tokenize('x: Int'))
        self.assertIsInstance(result, ast.EnumCaseParameter)
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.type_annotation.name, 'Int')

    def test_enum_case_decl(self):
        parser = tango.enum_case_decl + skip(finished)

        result = parser.parse(tango.tokenize('case a'))
        self.assertIsInstance(result, ast.EnumCaseDecl)
        self.assertEqual(result.name, 'a')
        self.assertFalse(result.parameters)

        result = parser.parse(tango.tokenize('case a(_: Int, _: Int)'))
        self.assertIsInstance(result, ast.EnumCaseDecl)
        self.assertEqual(result.name, 'a')
        self.assertEqual(result.parameters[0].name, '_')
        self.assertEqual(result.parameters[0].type_annotation.name, 'Int')
        self.assertEqual(result.parameters[1].name, '_')
        self.assertEqual(result.parameters[1].type_annotation.name, 'Int')

    def test_enum_decl(self):
        parser = tango.enum_decl + skip(finished)

        result = parser.parse(tango.tokenize('enum E {}'))
        self.assertIsInstance(result, ast.EnumDecl)
        self.assertEqual(result.name, 'E')
        self.assertFalse(result.import_list)
        self.assertFalse(result.conformance_list)
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('enum E import F, G {}'))
        self.assertEqual(result.import_list[0].name, 'F')
        self.assertEqual(result.import_list[1].name, 'G')

        result = parser.parse(tango.tokenize('enum E import F : P & Q {}'))
        self.assertEqual(result.conformance_list[0].name, 'P')
        self.assertEqual(result.conformance_list[1].name, 'Q')

        result = parser.parse(tango.tokenize(
            '''enum E {
                case a
                case b(x: Int)
            }'''))
        self.assertIsInstance(result, ast.EnumDecl)
        self.assertEqual(result.body.statements[0].name, 'a')
        self.assertEqual(result.body.statements[1].name, 'b')
        self.assertEqual(result.body.statements[1].parameters[0].name, 'x')
        self.assertEqual(result.body.statements[1].parameters[0].type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize(
            '''enum E {
                fun f(cst self: Self) {}
            }'''))
        self.assertIsInstance(result, ast.EnumDecl)
        self.assertIsInstance(result.body.statements[0], ast.FunctionDecl)

    def test_struct_decl(self):
        parser = tango.struct_decl + skip(finished)

        result = parser.parse(tango.tokenize('struct S {}'))
        self.assertIsInstance(result, ast.StructDecl)
        self.assertEqual(result.name, 'S')
        self.assertFalse(result.import_list)
        self.assertFalse(result.conformance_list)
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('struct S import T {}'))
        self.assertEqual(result.import_list[0].name, 'T')

        result = parser.parse(tango.tokenize('struct S import T : P & Q {}'))
        self.assertEqual(result.conformance_list[0].name, 'P')
        self.assertEqual(result.conformance_list[1].name, 'Q')

        result = parser.parse(tango.tokenize(
            '''struct S {
                mut x: Int
            }'''))
        self.assertIsInstance(result, ast.StructDecl)
        self.assertEqual(result.body.statements[0].name, 'x')
        self.assertEqual(result.body.statements[0].type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize(
            '''struct S {
                fun f(cst self: Self) {}
            }'''))
        self.assertIsInstance(result, ast.StructDecl)
        self.assertIsInstance(result.body.statements[0], ast.FunctionDecl)


if __name__ == '__main__':
    unittest.main()
