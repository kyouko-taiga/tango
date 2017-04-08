import unittest
import tango.parser as tango
import tango.ast as ast

from funcparserlib.parser import finished, skip


class TestParser(unittest.TestCase):

    def test_identifier(self):
        parser = tango.identifier + skip(finished)

        for s in ['a', '_', 'abc', 'a1', '学生']:
            result = parser.parse(tango.tokenize(s))
            self.assertIsInstance(result, ast.Identifier)
            self.assertEqual(result.name, s)

    def test_operator_identifier(self):
        parser = tango.operator_identifier + skip(finished)

        result = parser.parse(tango.tokenize('+'))
        self.assertIsInstance(result, ast.OperatorIdentifier)
        self.assertEqual(result.name, '+')

    def test_specialization_parameter(self):
        parser = tango.specialization_parameter + skip(finished)

        result = parser.parse(tango.tokenize('T = Int'))
        self.assertIsInstance(result, ast.SpecializationParameter)
        self.assertEqual(result.name.name, 'T')
        self.assertEqual(result.type_annotation.name.name, 'Int')

    def test_type_identifier(self):
        parser = tango.type_identifier + skip(finished)

        result = parser.parse(tango.tokenize('Int'))
        self.assertIsInstance(result, ast.TypeIdentifier)
        self.assertEqual(result.name.name, 'Int')
        self.assertFalse(result.specialization_parameters)

        result = parser.parse(tango.tokenize('Array[T = Int]'))
        self.assertIsInstance(result, ast.TypeIdentifier)
        self.assertEqual(result.name.name, 'Array')
        self.assertEqual(result.specialization_parameters[0].name.name, 'T')
        self.assertEqual(result.specialization_parameters[0].type_annotation.name.name, 'Int')

        result = parser.parse(tango.tokenize('Dictionary[Key = Int, Value = String]'))
        self.assertIsInstance(result, ast.TypeIdentifier)
        self.assertEqual(result.name.name, 'Dictionary')
        self.assertEqual(result.specialization_parameters[0].name.name, 'Key')
        self.assertEqual(result.specialization_parameters[0].type_annotation.name.name, 'Int')
        self.assertEqual(result.specialization_parameters[1].name.name, 'Value')
        self.assertEqual(result.specialization_parameters[1].type_annotation.name.name, 'String')

    def test_function_parameter(self):
        parser = tango.function_parameter + skip(finished)

        result = parser.parse(tango.tokenize('cst x: Int'))
        self.assertIsInstance(result, ast.FunctionParameter)
        self.assertEqual(result.api_name.name, 'x')
        self.assertEqual(result.name.name, 'x')
        self.assertFalse(result.attributes)
        self.assertEqual(result.type_annotation.name.name, 'Int')

        result = parser.parse(tango.tokenize('mut x: Int'))
        self.assertIn('mutable', result.attributes)

        result = parser.parse(tango.tokenize('cst a x: Int'))
        self.assertIsInstance(result, ast.FunctionParameter)
        self.assertEqual(result.api_name.name, 'a')
        self.assertEqual(result.name.name, 'x')
        self.assertFalse(result.attributes)
        self.assertEqual(result.type_annotation.name.name, 'Int')

        result = parser.parse(tango.tokenize('cst a x: @variadic Int'))
        self.assertIsInstance(result, ast.FunctionParameter)
        self.assertEqual(result.api_name.name, 'a')
        self.assertEqual(result.name.name, 'x')
        self.assertEqual(result.attributes[0].name, 'variadic')
        self.assertEqual(result.type_annotation.name.name, 'Int')

        result = parser.parse(tango.tokenize('cst a x: @variadic @ref Int'))
        self.assertIsInstance(result, ast.FunctionParameter)
        self.assertEqual(result.api_name.name, 'a')
        self.assertEqual(result.name.name, 'x')
        self.assertEqual(result.attributes[0].name, 'variadic')
        self.assertEqual(result.attributes[1].name, 'ref')
        self.assertEqual(result.type_annotation.name.name, 'Int')

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
        self.assertEqual(result.return_type.name.name, 'Nothing')

        result = parser.parse(tango.tokenize('(cst x: Int) -> Int'))
        self.assertIsInstance(result, ast.FunctionSignature)
        self.assertEqual(result.parameters[0].name.name, 'x')
        self.assertEqual(result.return_type.name.name, 'Int')

        result = parser.parse(tango.tokenize('(cst x: Int, cst y: Int) -> Int'))
        self.assertIsInstance(result, ast.FunctionSignature)
        self.assertEqual(result.parameters[0].name.name, 'x')
        self.assertEqual(result.parameters[1].name.name, 'y')
        self.assertEqual(result.return_type.name.name, 'Int')

        result = parser.parse(tango.tokenize('(cst a x: Int) -> Int'))
        self.assertIsInstance(result, ast.FunctionSignature)
        self.assertEqual(result.parameters[0].name.name, 'x')
        self.assertEqual(result.parameters[0].api_name.name, 'a')
        self.assertEqual(result.return_type.name.name, 'Int')

    def test_prefixed_expression(self):
        parser = tango.pfx_expr + skip(finished)

        result = parser.parse(tango.tokenize('not x'))
        self.assertIsInstance(result, ast.PrefixedExpression)
        self.assertEqual(result.operator.name, 'not')
        self.assertEqual(result.operand.name, 'x')

    def test_binary_expression(self):
        parser = tango.bin_expr + skip(finished)

        result = parser.parse(tango.tokenize('0 + 1'))
        self.assertIsInstance(result, ast.BinaryExpression)
        self.assertEqual(result.operator.name, '+')
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
        self.assertEqual(result.operator.name, '+')
        self.assertEqual(result.left.operator.name, '*')
        self.assertEqual(result.left.left.value, '0')
        self.assertEqual(result.left.right.value, '1')
        self.assertEqual(result.right.value, '2')

        result = parser.parse(tango.tokenize('0 + 1 * 2'))
        self.assertEqual(result.operator.name, '+')
        self.assertEqual(result.left.value, '0')
        self.assertEqual(result.right.operator.name, '*')
        self.assertEqual(result.right.left.value, '1')
        self.assertEqual(result.right.right.value, '2')

    def test_assignment(self):
        parser = tango.assignment + skip(finished)

        result = parser.parse(tango.tokenize('x = 0'))
        self.assertIsInstance(result, ast.Assignment)
        self.assertEqual(result.target.name, 'x')
        self.assertEqual(result.value.value, '0')

    def test_function_decl(self):
        parser = tango.function_decl + skip(finished)

        result = parser.parse(tango.tokenize('fun f() {}'))
        self.assertIsInstance(result, ast.FunctionDecl)
        self.assertEqual(result.name.name, 'f')
        self.assertFalse(result.generic_parameters)
        self.assertFalse(result.signature.parameters)
        self.assertEqual(result.signature.return_type.name, 'Nothing')

        result = parser.parse(tango.tokenize('fun f(cst x: Int) -> Int {}'))
        self.assertIsInstance(result, ast.FunctionDecl)
        self.assertEqual(result.name.name, 'f')
        self.assertFalse(result.generic_parameters)
        self.assertEqual(result.signature.parameters[0].name.name, 'x')
        self.assertEqual(result.signature.return_type.name.name, 'Int')

        result = parser.parse(tango.tokenize('fun f<T>(cst x: T) {}'))
        self.assertIsInstance(result, ast.FunctionDecl)
        self.assertEqual(result.name.name, 'f')
        self.assertEqual(result.generic_parameters[0].name.name, 'T')
        self.assertEqual(result.signature.parameters[0].name.name, 'x')
        self.assertEqual(result.signature.return_type.name, 'Nothing')

    def test_variable_decl(self):
        parser = tango.variable_decl + skip(finished)

        result = parser.parse(tango.tokenize('mut x'))
        self.assertIsInstance(result, ast.VariableDecl)
        self.assertEqual(result.name.name, 'x')
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initial_value)

        result = parser.parse(tango.tokenize('mut x: Int'))
        self.assertIsInstance(result, ast.VariableDecl)
        self.assertEqual(result.name.name, 'x')
        self.assertEqual(result.type_annotation.name.name, 'Int')
        self.assertIsNone(result.initial_value)

    def test_constant_decl(self):
        parser = tango.constant_decl + skip(finished)

        result = parser.parse(tango.tokenize('cst x'))
        self.assertIsInstance(result, ast.ConstantDecl)
        self.assertEqual(result.name.name, 'x')
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initial_value)

        result = parser.parse(tango.tokenize('cst x: Int'))
        self.assertIsInstance(result, ast.ConstantDecl)
        self.assertEqual(result.name.name, 'x')
        self.assertEqual(result.type_annotation.name.name, 'Int')
        self.assertIsNone(result.initial_value)

    def test_enum_case_paramter(self):
        parser = tango.enum_case_parameter + skip(finished)

        result = parser.parse(tango.tokenize('x: Int'))
        self.assertIsInstance(result, ast.EnumCaseParameter)
        self.assertEqual(result.name.name, 'x')
        self.assertEqual(result.type_annotation.name.name, 'Int')

    def test_enum_case(self):
        parser = tango.enum_case + skip(finished)

        result = parser.parse(tango.tokenize('case a'))
        self.assertIsInstance(result, ast.EnumCase)
        self.assertEqual(result.name.name, 'a')
        self.assertFalse(result.parameters)

        result = parser.parse(tango.tokenize('case a(_: Int)'))
        self.assertIsInstance(result, ast.EnumCase)
        self.assertEqual(result.name.name, 'a')
        self.assertEqual(result.parameters[0].name.name, '_')
        self.assertEqual(result.parameters[0].type_annotation.name.name, 'Int')

        result = parser.parse(tango.tokenize('case a(_: Int, _: Int)'))
        self.assertIsInstance(result, ast.EnumCase)
        self.assertEqual(result.name.name, 'a')
        self.assertEqual(result.parameters[0].name.name, '_')
        self.assertEqual(result.parameters[0].type_annotation.name.name, 'Int')
        self.assertEqual(result.parameters[1].name.name, '_')
        self.assertEqual(result.parameters[1].type_annotation.name.name, 'Int')

    def test_enum_decl(self):
        parser = tango.enum_decl + skip(finished)

        result = parser.parse(tango.tokenize('enum E {}'))
        self.assertIsInstance(result, ast.EnumDecl)
        self.assertEqual(result.name.name, 'E')
        self.assertFalse(result.import_list)
        self.assertFalse(result.conformance_list)
        self.assertFalse(result.cases)
        self.assertFalse(result.methods)

        result = parser.parse(tango.tokenize('enum E import F {}'))
        self.assertEqual(result.import_list[0].name.name, 'F')

        result = parser.parse(tango.tokenize('enum E import F : P & Q {}'))
        self.assertEqual(result.conformance_list[0].name.name, 'P')
        self.assertEqual(result.conformance_list[1].name.name, 'Q')

        result = parser.parse(tango.tokenize(
            '''enum E {
                case a
                case b(x: Int)
            }'''))
        self.assertIsInstance(result, ast.EnumDecl)
        self.assertEqual(result.cases[0].name.name, 'a')
        self.assertEqual(result.cases[1].name.name, 'b')
        self.assertEqual(result.cases[1].parameters[0].name.name, 'x')
        self.assertEqual(result.cases[1].parameters[0].type_annotation.name.name, 'Int')

        result = parser.parse(tango.tokenize(
            '''enum E {
                fun f(cst self: Self) {}
            }'''))
        self.assertIsInstance(result, ast.EnumDecl)
        self.assertIsInstance(result.methods[0], ast.FunctionDecl)

    def test_struct_decl(self):
        parser = tango.struct_decl + skip(finished)

        result = parser.parse(tango.tokenize('struct S {}'))
        self.assertIsInstance(result, ast.StructDecl)
        self.assertEqual(result.name.name, 'S')
        self.assertFalse(result.import_list)
        self.assertFalse(result.conformance_list)
        self.assertFalse(result.stored_properties)
        self.assertFalse(result.methods)

        result = parser.parse(tango.tokenize('struct S import T {}'))
        self.assertEqual(result.import_list[0].name.name, 'T')

        result = parser.parse(tango.tokenize('struct S import T : P & Q {}'))
        self.assertEqual(result.conformance_list[0].name.name, 'P')
        self.assertEqual(result.conformance_list[1].name.name, 'Q')

        result = parser.parse(tango.tokenize(
            '''struct S {
                mut x: Int
            }'''))
        self.assertIsInstance(result, ast.StructDecl)
        self.assertEqual(result.stored_properties[0].name.name, 'x')
        self.assertEqual(result.stored_properties[0].type_annotation.name.name, 'Int')

        result = parser.parse(tango.tokenize(
            '''struct S {
                fun f(cst self: Self) {}
            }'''))
        self.assertIsInstance(result, ast.StructDecl)
        self.assertIsInstance(result.methods[0], ast.FunctionDecl)


if __name__ == '__main__':
    unittest.main()
