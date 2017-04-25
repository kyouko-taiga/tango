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

        result = parser.parse(tango.tokenize('Array<T: Int>'))
        self.assertIsInstance(result, ast.TypeIdentifier)
        self.assertEqual(result.name, 'Array')
        self.assertEqual(result.specialization_parameters[0].name, 'T')
        self.assertEqual(result.specialization_parameters[0].type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize('Dictionary<Key: Int, Value: String>'))
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

        result = parser.parse(tango.tokenize('cst x: @attr1 @attr2 Int'))
        self.assertIsInstance(result, ast.FunctionParameter)
        self.assertEqual(result.attributes, {'attr1', 'attr2'})
        self.assertEqual(result.type_annotation.name, 'Int')

    def test_function_signature(self):
        parser = tango.function_signature + skip(finished)

        result = parser.parse(tango.tokenize('() -> Nothing'))
        self.assertIsInstance(result, ast.FunctionSignature)
        self.assertFalse(result.parameters)
        self.assertEqual(result.return_type.name, 'Nothing')

        result = parser.parse(tango.tokenize('(cst x: Int, cst y: Int) -> Int'))
        self.assertIsInstance(result, ast.FunctionSignature)
        self.assertEqual(result.parameters[0].name, 'x')
        self.assertEqual(result.parameters[1].name, 'y')
        self.assertEqual(result.return_type.name, 'Int')

    def test_variable_identifier(self):
        parser = tango.variable_identifier + skip(finished)

        for s in ['a', '_', 'abc', 'a1', '学生', '+']:
            result = parser.parse(tango.tokenize(s))
            self.assertIsInstance(result, ast.Identifier)
            self.assertEqual(result.name, s)

    def test_array_literal(self):
        parser = tango.array_literal + skip(finished)

        result = parser.parse(tango.tokenize('[]'))
        self.assertIsInstance(result, ast.ArrayLiteral)
        self.assertFalse(result.items)

        result = parser.parse(tango.tokenize('[a]'))
        self.assertIsInstance(result, ast.ArrayLiteral)
        self.assertEqual(len(result.items), 1)
        self.assertEqual(result.items[0].name, 'a')

        result = parser.parse(tango.tokenize('[a + b, f()]'))
        self.assertIsInstance(result, ast.ArrayLiteral)
        self.assertEqual(len(result.items), 2)
        self.assertIsInstance(result.items[0], ast.BinaryExpression)
        self.assertIsInstance(result.items[1], ast.Call)

    def test_dictionary_item(self):
        parser = tango.dictionary_literal_item + skip(finished)

        result = parser.parse(tango.tokenize('a: b'))
        self.assertIsInstance(result, ast.DictionaryLiteralItem)
        self.assertEqual(result.key.name, 'a')
        self.assertEqual(result.value.name, 'b')

        result = parser.parse(tango.tokenize('f(): 1 + 3'))
        self.assertIsInstance(result, ast.DictionaryLiteralItem)
        self.assertIsInstance(result.key, ast.Call)
        self.assertIsInstance(result.value, ast.BinaryExpression)

    def test_dictionary_literal(self):
        parser = tango.dictionary_literal + skip(finished)

        result = parser.parse(tango.tokenize('[:]'))
        self.assertIsInstance(result, ast.DictionaryLiteral)
        self.assertFalse(result.items)

        result = parser.parse(tango.tokenize('[a: b]'))
        self.assertIsInstance(result, ast.DictionaryLiteral)
        self.assertEqual(len(result.items), 1)
        self.assertEqual(result.items[0].key.name, 'a')
        self.assertEqual(result.items[0].value.name, 'b')

        result = parser.parse(tango.tokenize('[a: b, f(): 1 + 3]'))
        self.assertIsInstance(result, ast.DictionaryLiteral)
        self.assertEqual(len(result.items), 2)
        self.assertEqual(result.items[0].key.name, 'a')
        self.assertEqual(result.items[0].value.name, 'b')
        self.assertIsInstance(result.items[1].key, ast.Call)
        self.assertIsInstance(result.items[1].value, ast.BinaryExpression)

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

    def test_implicit_select_expression(self):
        parser = tango.implicit_select_expression + skip(finished)

        result = parser.parse(tango.tokenize('.a'))
        self.assertIsInstance(result, ast.ImplicitSelect)
        self.assertEqual(result.member, 'a')

    def test_closure(self):
        parser = tango.closure + skip(finished)

        result = parser.parse(tango.tokenize('{ return 0 }'))
        self.assertIsInstance(result, ast.Closure)
        self.assertFalse(result.parameters)
        self.assertEqual(result.statements[0].value.value, '0')

        result = parser.parse(tango.tokenize('{ let cst x in return 0 }'))
        self.assertIsInstance(result, ast.Closure)
        self.assertEqual(result.parameters[0].name, 'x')

        result = parser.parse(tango.tokenize('{ let cst x, cst y in return 0 }'))
        self.assertIsInstance(result, ast.Closure)
        self.assertEqual(result.parameters[0].name, 'x')
        self.assertEqual(result.parameters[1].name, 'y')

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

        for lower, higher in (('or', 'and'), ('and', '=='), ('==', '<'), ('<', '+'), ('+', '*')):
            result = parser.parse(tango.tokenize('a %s b %s c' % (lower, higher)))
            self.assertEqual(result.operator, lower)
            self.assertEqual(result.left.name, 'a')
            self.assertEqual(result.right.operator, higher)
            self.assertEqual(result.right.left.name, 'b')
            self.assertEqual(result.right.right.name, 'c')

            result = parser.parse(tango.tokenize('a %s b %s c' % (higher, lower)))
            self.assertEqual(result.operator, lower)
            self.assertEqual(result.left.operator, higher)
            self.assertEqual(result.left.left.name, 'a')
            self.assertEqual(result.left.right.name, 'b')
            self.assertEqual(result.right.name, 'c')

            result = parser.parse(tango.tokenize('(a %s b) %s c' % (lower, higher)))
            self.assertEqual(result.operator, higher)
            self.assertEqual(result.left.operator, lower)
            self.assertEqual(result.left.left.name, 'a')
            self.assertEqual(result.left.right.name, 'b')
            self.assertEqual(result.right.name, 'c')

    def test_expression(self):
        parser = tango.expression + skip(finished)

        result = parser.parse(tango.tokenize('x'))
        self.assertIsInstance(result, ast.Identifier)

        result = parser.parse(tango.tokenize('0'))
        self.assertIsInstance(result, ast.Literal)

        result = parser.parse(tango.tokenize('(((0)))'))
        self.assertIsInstance(result, ast.Literal)

        result = parser.parse(tango.tokenize('f()'))
        self.assertIsInstance(result, ast.Call)

        result = parser.parse(tango.tokenize('a.b.c'))
        self.assertIsInstance(result, ast.Select)

        result = parser.parse(tango.tokenize('+0'))
        self.assertIsInstance(result, ast.PrefixedExpression)

        result = parser.parse(tango.tokenize('0 + 0'))
        self.assertIsInstance(result, ast.BinaryExpression)

        result = parser.parse(tango.tokenize('Int.+(0 - -9)'))
        self.assertIsInstance(result, ast.Call)
        self.assertIsInstance(result.callee, ast.Select)
        self.assertIsInstance(result.arguments[0].value, ast.BinaryExpression)
        self.assertIsInstance(result.arguments[0].value.right, ast.PrefixedExpression)

    def test_pattern(self):
        parser = tango.pattern + skip(finished)

        result = parser.parse(tango.tokenize('a'))
        self.assertIsInstance(result, ast.Pattern)
        self.assertFalse(result.parameters)
        self.assertEqual(result.expression.name, 'a')

        result = parser.parse(tango.tokenize('_'))
        self.assertIsInstance(result, ast.Pattern)
        self.assertFalse(result.parameters)
        self.assertIsInstance(result.expression, ast.Wildcard)

        result = parser.parse(tango.tokenize('let cst x in a'))
        self.assertIsInstance(result, ast.Pattern)
        self.assertEqual(result.parameters[0].name, 'x')
        self.assertEqual(result.expression.name, 'a')

        result = parser.parse(tango.tokenize('let cst x, mut y in a'))
        self.assertIsInstance(result, ast.Pattern)
        self.assertEqual(result.parameters[0].name, 'x')
        self.assertEqual(result.parameters[1].name, 'y')
        self.assertEqual(result.expression.name, 'a')

    def test_if_expression(self):
        parser = tango.if_expression + skip(finished)

        result = parser.parse(tango.tokenize('if a {}'))
        self.assertIsInstance(result, ast.If)
        self.assertEqual(result.pattern.expression.name, 'a')
        self.assertFalse(result.body.statements)
        self.assertIsNone(result.else_clause)

        result = parser.parse(tango.tokenize('if a {} else {}'))
        self.assertIsInstance(result.else_clause, ast.Block)

        result = parser.parse(tango.tokenize('if a {} else if b {}'))
        self.assertIsInstance(result.else_clause, ast.If)
        self.assertEqual(result.else_clause.pattern.expression.name, 'b')

        result = parser.parse(tango.tokenize('if let cst x in a {}'))
        self.assertIsInstance(result, ast.If)
        self.assertEqual(result.pattern.parameters[0].name, 'x')
        self.assertEqual(result.pattern.expression.name, 'a')
        self.assertFalse(result.body.statements)
        self.assertIsNone(result.else_clause)

        result = parser.parse(tango.tokenize('if let cst x, mut y in a {}'))
        self.assertIsInstance(result, ast.If)
        self.assertEqual(result.pattern.parameters[0].name, 'x')
        self.assertEqual(result.pattern.parameters[1].name, 'y')

    def test_switch_case_clause(self):
        parser = tango.switch_case_clause + skip(finished)

        result = parser.parse(tango.tokenize('case a {}'))
        self.assertIsInstance(result, ast.SwitchCaseClause)
        self.assertEqual(result.pattern.expression.name, 'a')
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('case let cst x in a {}'))
        self.assertIsInstance(result, ast.SwitchCaseClause)
        self.assertEqual(result.pattern.parameters[0].name, 'x')
        self.assertEqual(result.pattern.expression.name, 'a')
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('case let cst x, mut y in a {}'))
        self.assertIsInstance(result, ast.SwitchCaseClause)
        self.assertEqual(result.pattern.parameters[0].name, 'x')
        self.assertEqual(result.pattern.parameters[1].name, 'y')

    def test_switch_expression(self):
        parser = tango.switch_expression + skip(finished)

        result = parser.parse(tango.tokenize('switch a {}'))
        self.assertIsInstance(result, ast.Switch)
        self.assertEqual(result.expression.name, 'a')
        self.assertFalse(result.clauses)

        result = parser.parse(tango.tokenize('switch a { case b {} }'))
        self.assertIsInstance(result, ast.Switch)
        self.assertEqual(result.expression.name, 'a')
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(result.clauses[0].pattern.expression.name, 'b')

        result = parser.parse(tango.tokenize('switch a { case b {} case c {} }'))
        self.assertIsInstance(result, ast.Switch)
        self.assertEqual(result.expression.name, 'a')
        self.assertEqual(len(result.clauses), 2)
        self.assertEqual(result.clauses[0].pattern.expression.name, 'b')
        self.assertEqual(result.clauses[1].pattern.expression.name, 'c')

    def test_break_statement(self):
        parser = tango.break_statement + skip(finished)

        result = parser.parse(tango.tokenize('break'))
        self.assertIsInstance(result, ast.Break)
        self.assertIsNone(result.label)

        result = parser.parse(tango.tokenize('break foo'))
        self.assertIsInstance(result, ast.Break)
        self.assertEqual(result.label, 'foo')

    def test_continue_statement(self):
        parser = tango.continue_statement + skip(finished)

        result = parser.parse(tango.tokenize('continue'))
        self.assertIsInstance(result, ast.Continue)
        self.assertIsNone(result.label)

        result = parser.parse(tango.tokenize('continue foo'))
        self.assertIsInstance(result, ast.Continue)
        self.assertEqual(result.label, 'foo')

    def test_for_loop(self):
        parser = tango.for_loop + skip(finished)

        result = parser.parse(tango.tokenize('for _ in a {}'))
        self.assertIsInstance(result, ast.For)
        self.assertIsNone(result.label)
        self.assertIsNone(result.iterator)
        self.assertEqual(result.sequence.name, 'a')
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('for cst x in a {}'))
        self.assertIsInstance(result, ast.For)
        self.assertIsNone(result.label)
        self.assertEqual(result.iterator.name, 'x')
        self.assertEqual(result.sequence.name, 'a')
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('foo: for cst x in a {}'))
        self.assertIsInstance(result, ast.For)
        self.assertEqual(result.label, 'foo')
        self.assertEqual(result.iterator.name, 'x')
        self.assertEqual(result.sequence.name, 'a')
        self.assertFalse(result.body.statements)

    def test_while_loop(self):
        parser = tango.while_loop + skip(finished)

        result = parser.parse(tango.tokenize('while a {}'))
        self.assertIsInstance(result, ast.While)
        self.assertIsNone(result.label)
        self.assertFalse(result.pattern.parameters)
        self.assertEqual(result.pattern.expression.name, 'a')
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('while let cst x in a {}'))
        self.assertIsInstance(result, ast.While)
        self.assertIsNone(result.label)
        self.assertEqual(result.pattern.parameters[0].name, 'x')
        self.assertEqual(result.pattern.expression.name, 'a')
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('foo: while let cst x in a {}'))
        self.assertIsInstance(result, ast.While)
        self.assertEqual(result.label, 'foo')
        self.assertEqual(result.pattern.parameters[0].name, 'x')
        self.assertEqual(result.pattern.expression.name, 'a')
        self.assertFalse(result.body.statements)

    def test_assignment(self):
        parser = tango.assignment + skip(finished)

        result = parser.parse(tango.tokenize('x = 0'))
        self.assertIsInstance(result, ast.Assignment)
        self.assertEqual(result.lvalue.name, 'x')
        self.assertEqual(result.rvalue.value, '0')

        result = parser.parse(tango.tokenize('x = if a {} else {}'))
        self.assertIsInstance(result, ast.Assignment)
        self.assertEqual(result.lvalue.name, 'x')
        self.assertIsInstance(result.rvalue, ast.If)
        self.assertEqual(result.rvalue.pattern.expression.name, 'a')

        result = parser.parse(tango.tokenize('x = if a {} else if b {} else {}'))
        self.assertIsInstance(result, ast.Assignment)
        self.assertEqual(result.lvalue.name, 'x')
        self.assertIsInstance(result.rvalue, ast.If)
        self.assertEqual(result.rvalue.pattern.expression.name, 'a')
        self.assertEqual(result.rvalue.else_clause.pattern.expression.name, 'b')

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
        self.assertEqual(result.attributes, {'attr1', 'attr2'})

        result = parser.parse(tango.tokenize('x: 0'))
        self.assertIsInstance(result, ast.CallArgument)
        self.assertEqual(result.value.value, '0')
        self.assertEqual(result.name, 'x')
        self.assertFalse(result.attributes)

        result = parser.parse(tango.tokenize('x: @attr1 @attr2 0'))
        self.assertIsInstance(result, ast.CallArgument)
        self.assertEqual(result.attributes, {'attr1', 'attr2'})

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

    def test_function_decl_parameter(self):
        parser = tango.function_decl_parameter + skip(finished)

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
        self.assertEqual(result.attributes, {'attr1', 'attr2'})
        self.assertEqual(result.type_annotation.name, 'Int')

    def test_function_decl_parameter_default_value(self):
        parser = tango.function_decl_parameter + skip(finished)

        result = parser.parse(tango.tokenize('cst x: Int = 0'))
        self.assertIsInstance(result, ast.FunctionParameter)
        self.assertIsInstance(result.default_value, ast.Literal)

    def test_function_decl(self):
        parser = tango.function_decl + skip(finished)

        result = parser.parse(tango.tokenize('fun f() {}'))
        self.assertIsInstance(result, ast.FunctionDecl)
        self.assertEqual(result.name, 'f')
        self.assertFalse(result.generic_parameters)
        self.assertFalse(result.signature.parameters)
        self.assertEqual(result.signature.return_type.name, 'Nothing')

        result = parser.parse(tango.tokenize('fun f(cst x: Int, cst y: String) -> Int {}'))
        self.assertIsInstance(result, ast.FunctionDecl)
        self.assertEqual(result.name, 'f')
        self.assertFalse(result.generic_parameters)
        self.assertEqual(result.signature.parameters[0].name, 'x')
        self.assertEqual(result.signature.parameters[1].name, 'y')
        self.assertEqual(result.signature.return_type.name, 'Int')

        result = parser.parse(tango.tokenize('fun f<T>(cst x: T) {}'))
        self.assertIsInstance(result, ast.FunctionDecl)
        self.assertEqual(result.name, 'f')
        self.assertEqual(result.generic_parameters[0], 'T')
        self.assertEqual(result.signature.parameters[0].name, 'x')
        self.assertEqual(result.signature.return_type.name, 'Nothing')

    def test_container_decl(self):
        parser = tango.container_decl + skip(finished)

        result = parser.parse(tango.tokenize('cst x'))
        self.assertIsInstance(result, ast.ContainerDecl)
        self.assertFalse(result.is_mutable)
        self.assertEqual(result.name, 'x')
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initial_value)

        result = parser.parse(tango.tokenize('cst x: Int'))
        self.assertIsInstance(result, ast.ContainerDecl)
        self.assertFalse(result.is_mutable)
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.type_annotation.name, 'Int')
        self.assertIsNone(result.initial_value)

        result = parser.parse(tango.tokenize('mut x'))
        self.assertIsInstance(result, ast.ContainerDecl)
        self.assertTrue(result.is_mutable)
        self.assertEqual(result.name, 'x')
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initial_value)

        result = parser.parse(tango.tokenize('mut x: Int'))
        self.assertIsInstance(result, ast.ContainerDecl)
        self.assertTrue(result.is_mutable)
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.type_annotation.name, 'Int')
        self.assertIsNone(result.initial_value)

        result = parser.parse(tango.tokenize('cst x = 0'))
        self.assertIsInstance(result, ast.ContainerDecl)
        self.assertFalse(result.is_mutable)
        self.assertEqual(result.name, 'x')
        self.assertIsNone(result.type_annotation)
        self.assertEqual(result.initial_value.value, '0')

        result = parser.parse(tango.tokenize('cst x: Int = 0'))
        self.assertIsInstance(result, ast.ContainerDecl)
        self.assertFalse(result.is_mutable)
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.type_annotation.name, 'Int')
        self.assertEqual(result.initial_value.value, '0')

    def test_enum_case_parameter(self):
        parser = tango.enum_case_parameter + skip(finished)

        result = parser.parse(tango.tokenize('_: Int'))
        self.assertIsInstance(result, ast.EnumCaseParameter)
        self.assertIsNone(result.label)
        self.assertEqual(result.type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize('x: Int'))
        self.assertIsInstance(result, ast.EnumCaseParameter)
        self.assertEqual(result.label, 'x')
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
        self.assertIsNone(result.parameters[0].label)
        self.assertEqual(result.parameters[0].type_annotation.name, 'Int')
        self.assertIsNone(result.parameters[1].label)
        self.assertEqual(result.parameters[1].type_annotation.name, 'Int')

    def test_enum_decl(self):
        parser = tango.enum_decl + skip(finished)

        result = parser.parse(tango.tokenize('enum E {}'))
        self.assertIsInstance(result, ast.EnumDecl)
        self.assertEqual(result.name, 'E')
        self.assertFalse(result.generic_parameters)
        self.assertFalse(result.import_list)
        self.assertFalse(result.conformance_list)
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('enum E<T, U> {}'))
        self.assertEqual(result.generic_parameters[0], 'T')
        self.assertEqual(result.generic_parameters[1], 'U')

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
        self.assertEqual(result.body.statements[1].parameters[0].label, 'x')
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
        self.assertFalse(result.generic_parameters)
        self.assertFalse(result.import_list)
        self.assertFalse(result.conformance_list)
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('struct S<T, U> {}'))
        self.assertEqual(result.generic_parameters[0], 'T')
        self.assertEqual(result.generic_parameters[1], 'U')

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

    def test_protocol_property_decl(self):
        parser = tango.protocol_property_decl + skip(finished)

        result = parser.parse(tango.tokenize('cst x'))
        self.assertIsInstance(result, ast.ContainerDecl)
        self.assertFalse(result.is_mutable)
        self.assertEqual(result.name, 'x')
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initial_value)

        result = parser.parse(tango.tokenize('cst x: Int'))
        self.assertIsInstance(result, ast.ContainerDecl)
        self.assertFalse(result.is_mutable)
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.type_annotation.name, 'Int')
        self.assertIsNone(result.initial_value)

        result = parser.parse(tango.tokenize('mut x'))
        self.assertIsInstance(result, ast.ContainerDecl)
        self.assertTrue(result.is_mutable)
        self.assertEqual(result.name, 'x')
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initial_value)

        result = parser.parse(tango.tokenize('mut x: Int'))
        self.assertIsInstance(result, ast.ContainerDecl)
        self.assertTrue(result.is_mutable)
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.type_annotation.name, 'Int')
        self.assertIsNone(result.initial_value)

    def test_protocol_function_decl(self):
        parser = tango.protocol_function_decl + skip(finished)

        result = parser.parse(tango.tokenize('fun f()'))
        self.assertIsInstance(result, ast.FunctionDecl)
        self.assertEqual(result.name, 'f')
        self.assertFalse(result.generic_parameters)
        self.assertFalse(result.signature.parameters)
        self.assertEqual(result.signature.return_type.name, 'Nothing')

        result = parser.parse(tango.tokenize('fun f(cst x: Int, cst y: String) -> Int'))
        self.assertIsInstance(result, ast.FunctionDecl)
        self.assertEqual(result.name, 'f')
        self.assertFalse(result.generic_parameters)
        self.assertEqual(result.signature.parameters[0].name, 'x')
        self.assertEqual(result.signature.parameters[1].name, 'y')
        self.assertEqual(result.signature.return_type.name, 'Int')

        result = parser.parse(tango.tokenize('fun f<T>(cst x: T)'))
        self.assertIsInstance(result, ast.FunctionDecl)
        self.assertEqual(result.name, 'f')
        self.assertEqual(result.generic_parameters[0], 'T')
        self.assertEqual(result.signature.parameters[0].name, 'x')
        self.assertEqual(result.signature.return_type.name, 'Nothing')

    def test_struct_decl(self):
        parser = tango.protocol_decl + skip(finished)

        result = parser.parse(tango.tokenize('protocol P {}'))
        self.assertIsInstance(result, ast.ProtocolDecl)
        self.assertEqual(result.name, 'P')
        self.assertFalse(result.conformance_list)
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('protocol P : Q & R {}'))
        self.assertEqual(result.conformance_list[0].name, 'Q')
        self.assertEqual(result.conformance_list[1].name, 'R')

        result = parser.parse(tango.tokenize(
            '''protocol P {
                mut x: Int
            }'''))
        self.assertIsInstance(result, ast.ProtocolDecl)
        self.assertEqual(result.body.statements[0].name, 'x')
        self.assertEqual(result.body.statements[0].type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize(
            '''protocol P {
                fun f(cst self: Self)
            }'''))
        self.assertIsInstance(result, ast.ProtocolDecl)
        self.assertIsInstance(result.body.statements[0], ast.FunctionDecl)

if __name__ == '__main__':
    unittest.main()
