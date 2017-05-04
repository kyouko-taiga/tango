import unittest
import tango.parser as tango
import tango.ast as ast

from funcparserlib.parser import finished, skip


class TestParser(unittest.TestCase):

    def test_name(self):
        parser = tango.name + skip(finished)

        for s in ['a', '_', 'abc', 'a1', '学生']:
            result = parser.parse(tango.tokenize(s))
            self.assertEqual(result, s)

    def test_operator_name(self):
        parser = tango.operator_name + skip(finished)

        operators = [
            '>>=', '<<=', '*=' , '/=' , '%=' , '+=' , '-=' , '&=' , '|=' ,
            '+'  , '-'  , '~'  , 'not',
            '?'  , '!'  ,
            '>>' , '<<' , '*'  , '/'  , '%'  ,
            '<'  , '<=' , '>=' , '>'  , 'as?', 'as!', 'is' , '==' , '!=' , '~=' , '===',
            '&'  , '^'  , '|'  , 'and', 'or'
        ]

        for s in operators:
            result = parser.parse(tango.tokenize(s))
            self.assertEqual(result, s)

    def test_signature_param(self):
        parser = tango.signature_param + skip(finished)

        result = parser.parse(tango.tokenize('cst x: Int'))
        self.assertIsInstance(result, ast.SignatureParameter)
        self.assertEqual(result.label, 'x')
        self.assertFalse(result.attributes)
        self.assertEqual(result.type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize('cst _: Int'))
        self.assertIsInstance(result, ast.SignatureParameter)
        self.assertIsNone(result.label)

        result = parser.parse(tango.tokenize('mut x: Int'))
        self.assertIn('mutable', result.attributes)

        result = parser.parse(tango.tokenize('shd x: Int'))
        self.assertIn('shared', result.attributes)

    def test_fun_signature(self):
        parser = tango.fun_signature + skip(finished)

        result = parser.parse(tango.tokenize('() -> Nothing'))
        self.assertIsInstance(result, ast.FunctionSignature)
        self.assertFalse(result.parameters)
        self.assertEqual(result.return_type.name, 'Nothing')

        result = parser.parse(tango.tokenize('(cst x: Int, cst y: Int) -> Int'))
        self.assertIsInstance(result, ast.FunctionSignature)
        self.assertEqual(result.parameters[0].label, 'x')
        self.assertEqual(result.parameters[1].label, 'y')
        self.assertEqual(result.return_type.name, 'Int')

    def test_tuple_signature(self):
        parser = tango.tuple_signature + skip(finished)

        result = parser.parse(tango.tokenize('(cst x: Int)'))
        self.assertIsInstance(result, ast.TupleSignature)
        self.assertEqual(len(result.parameters), 1)
        self.assertEqual(result.parameters[0].label, 'x')

        result = parser.parse(tango.tokenize('(cst x: Int, cst y: Int)'))
        self.assertIsInstance(result, ast.TupleSignature)
        self.assertEqual(result.parameters[0].label, 'x')
        self.assertEqual(result.parameters[1].label, 'y')

    def test_specialization_argument(self):
        parser = tango.specialization_argument + skip(finished)

        result = parser.parse(tango.tokenize('T = Int'))
        self.assertIsInstance(result, ast.SpecializationArgument)
        self.assertEqual(result.name, 'T')
        self.assertEqual(result.value.name, 'Int')

        result = parser.parse(tango.tokenize('T = () -> Nothing'))
        self.assertIsInstance(result, ast.SpecializationArgument)
        self.assertEqual(result.name, 'T')
        self.assertIsInstance(result.value, ast.FunctionSignature)

        result = parser.parse(tango.tokenize('T = (cst _: Int, cst _: String)'))
        self.assertIsInstance(result, ast.SpecializationArgument)
        self.assertEqual(result.name, 'T')
        self.assertIsInstance(result.value, ast.TupleSignature)

    def test_identifier(self):
        parser = tango.identifier + skip(finished)

        result = parser.parse(tango.tokenize('Int'))
        self.assertIsInstance(result, ast.Identifier)
        self.assertEqual(result.name, 'Int')
        self.assertFalse(result.specializations)

        result = parser.parse(tango.tokenize('+'))
        self.assertIsInstance(result, ast.Identifier)
        self.assertEqual(result.name, '+')
        self.assertFalse(result.specializations)

        result = parser.parse(tango.tokenize('Array<T = Int>'))
        self.assertIsInstance(result, ast.Identifier)
        self.assertEqual(result.name, 'Array')
        self.assertEqual(result.specializations[0].name, 'T')
        self.assertEqual(result.specializations[0].value.name, 'Int')

        result = parser.parse(tango.tokenize('Dictionary<Key = Int, Value = String>'))
        self.assertIsInstance(result, ast.Identifier)
        self.assertEqual(result.name, 'Dictionary')
        self.assertEqual(result.specializations[0].name, 'Key')
        self.assertEqual(result.specializations[0].value.name, 'Int')
        self.assertEqual(result.specializations[1].name, 'Value')
        self.assertEqual(result.specializations[1].value.name, 'String')

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

    def test_dict_item(self):
        parser = tango.dict_literal_item + skip(finished)

        result = parser.parse(tango.tokenize('a: b'))
        self.assertIsInstance(result, ast.DictionaryLiteralItem)
        self.assertEqual(result.key.name, 'a')
        self.assertEqual(result.value.name, 'b')

        result = parser.parse(tango.tokenize('f(): 1 + 3'))
        self.assertIsInstance(result, ast.DictionaryLiteralItem)
        self.assertIsInstance(result.key, ast.Call)
        self.assertIsInstance(result.value, ast.BinaryExpression)

    def test_dict_literal(self):
        parser = tango.dict_literal + skip(finished)

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

    def test_select_expr(self):
        parser = tango.select_expr + skip(finished)

        result = parser.parse(tango.tokenize('a.b'))
        self.assertIsInstance(result, ast.Select)
        self.assertEqual(result.owner.name, 'a')
        self.assertEqual(result.member.name, 'b')

        # NOTE Extra parenthesis needed because our LL parser can't handle
        # left recursion.
        result = parser.parse(tango.tokenize('(a.b).c'))
        self.assertIsInstance(result, ast.Select)
        self.assertIsInstance(result.owner, ast.Select)
        self.assertEqual(result.owner.owner.name, 'a')
        self.assertEqual(result.owner.member.name, 'b')
        self.assertEqual(result.member.name, 'c')

        result = parser.parse(tango.tokenize('Int.+'))
        self.assertIsInstance(result, ast.Select)
        self.assertEqual(result.owner.name, 'Int')
        self.assertEqual(result.member.name, '+')

    def test_implicit_select_expr(self):
        parser = tango.implicit_select_expr + skip(finished)

        result = parser.parse(tango.tokenize('.a'))
        self.assertIsInstance(result, ast.ImplicitSelect)
        self.assertEqual(result.member.name, 'a')

    def test_closure_expr(self):
        parser = tango.closure_expr + skip(finished)

        result = parser.parse(tango.tokenize('{ return 0 }'))
        self.assertIsInstance(result, ast.Closure)
        self.assertFalse(result.parameters)
        self.assertEqual(result.statements[0].value.value, '0')

        result = parser.parse(tango.tokenize('{ cst x in return 0 }'))
        self.assertIsInstance(result, ast.Closure)
        self.assertEqual(result.parameters[0].name, 'x')

        result = parser.parse(tango.tokenize('{ cst x, cst y in return 0 }'))
        self.assertIsInstance(result, ast.Closure)
        self.assertEqual(result.parameters[0].name, 'x')
        self.assertEqual(result.parameters[1].name, 'y')

    def test_prefix_expr(self):
        parser = tango.prefix_expr + skip(finished)

        result = parser.parse(tango.tokenize('not x'))
        self.assertIsInstance(result, ast.PrefixExpression)
        self.assertEqual(result.operator, 'not')
        self.assertEqual(result.operand.name, 'x')

    def test_bin_expr(self):
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

        operators = [
            ('or' , 'and'), ('and', '|'  ), ('|'  , '^'  ), ('^'  , '&'  ), ('&'  , '=='  ),
            ('==' , 'as?'), ('as?', '<'  ), ('as?', '<'  ), ('<'  , '+'  ), ('+'  , '*'   ),
            ('+'  , '>>' )
        ]

        for lower, higher in operators:
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

    def test_expr(self):
        parser = tango.expr + skip(finished)

        result = parser.parse(tango.tokenize('x'))
        self.assertIsInstance(result, ast.Identifier)

        result = parser.parse(tango.tokenize('0'))
        self.assertIsInstance(result, ast.Literal)

        result = parser.parse(tango.tokenize('(((0)))'))
        self.assertIsInstance(result, ast.Literal)

        result = parser.parse(tango.tokenize('f()'))
        self.assertIsInstance(result, ast.Call)

        result = parser.parse(tango.tokenize('a.b'))
        self.assertIsInstance(result, ast.Select)

        result = parser.parse(tango.tokenize('+0'))
        self.assertIsInstance(result, ast.PrefixExpression)

        result = parser.parse(tango.tokenize('a?'))
        self.assertIsInstance(result, ast.PostfixExpression)

        result = parser.parse(tango.tokenize('0 + 0'))
        self.assertIsInstance(result, ast.BinaryExpression)

        result = parser.parse(tango.tokenize('Int.+(0 - -9)'))
        self.assertIsInstance(result, ast.Call)
        self.assertIsInstance(result.callee, ast.Select)
        self.assertIsInstance(result.arguments[0].value, ast.BinaryExpression)
        self.assertIsInstance(result.arguments[0].value.right, ast.PrefixExpression)

    def test_wildcard_pattern(self):
        parser = tango.wildcard_pattern + skip(finished)

        result = parser.parse(tango.tokenize('_'))
        self.assertIsInstance(result, ast.WildcardPattern)

    def test_value_binding_pattern(self):
        parser = tango.value_binding_pattern + skip(finished)

        result = parser.parse(tango.tokenize('cst a'))
        self.assertIsInstance(result, ast.ValueBindingPattern)
        self.assertEqual(result.name, 'a')
        self.assertFalse(result.attributes)
        self.assertIsNone(result.type_annotation)

        result = parser.parse(tango.tokenize('cst a: Int'))
        self.assertIsInstance(result, ast.ValueBindingPattern)
        self.assertEqual(result.name, 'a')
        self.assertFalse(result.attributes)
        self.assertEqual(result.type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize('mut a: Int'))
        self.assertIsInstance(result, ast.ValueBindingPattern)
        self.assertEqual(result.name, 'a')
        self.assertIn('mutable', result.attributes)
        self.assertEqual(result.type_annotation.name, 'Int')

    def test_pattern_arg(self):
        parser = tango.pattern_arg + skip(finished)

        result = parser.parse(tango.tokenize('x'))
        self.assertIsInstance(result, ast.PatternArgument)
        self.assertIsInstance(result.value, ast.Pattern)
        self.assertIsInstance(result.value.expression, ast.Identifier)

        result = parser.parse(tango.tokenize('cst x'))
        self.assertIsInstance(result, ast.PatternArgument)
        self.assertIsInstance(result.value, ast.Pattern)
        self.assertIsInstance(result.value.expression, ast.ValueBindingPattern)
        self.assertIsNone(result.label)

        result = parser.parse(tango.tokenize('a = cst x'))
        self.assertIsInstance(result, ast.PatternArgument)
        self.assertIsInstance(result.value, ast.Pattern)
        self.assertIsInstance(result.value.expression, ast.ValueBindingPattern)
        self.assertEqual(result.label, 'a')

    def test_enum_case_pattern(self):
        parser = tango.enum_case_pattern + skip(finished)

        result = parser.parse(tango.tokenize('Nat.succ(cst x)'))
        self.assertIsInstance(result, ast.EnumCasePattern)
        self.assertIsInstance(result.arguments[0], ast.PatternArgument)
        self.assertIsNone(result.arguments[0].label)

        result = parser.parse(tango.tokenize('.succ(cst x)'))
        self.assertIsInstance(result, ast.EnumCasePattern)
        self.assertIsInstance(result.arguments[0], ast.PatternArgument)
        self.assertIsNone(result.arguments[0].label)

        result = parser.parse(tango.tokenize('List.cons(el = cst x, tail = t)'))
        self.assertIsInstance(result, ast.EnumCasePattern)
        self.assertIsInstance(result.arguments[0], ast.PatternArgument)
        self.assertEqual(result.arguments[0].label, 'el')
        self.assertIsInstance(result.arguments[1], ast.PatternArgument)
        self.assertEqual(result.arguments[1].label, 'tail')

        result = parser.parse(tango.tokenize('List.cons(el = e, tail = cst y)'))
        self.assertIsInstance(result, ast.EnumCasePattern)
        self.assertIsInstance(result.arguments[0], ast.PatternArgument)
        self.assertEqual(result.arguments[0].label, 'el')
        self.assertIsInstance(result.arguments[1], ast.PatternArgument)
        self.assertEqual(result.arguments[1].label, 'tail')

        result = parser.parse(tango.tokenize('.cons(el = cst x, tail = cst y)'))
        self.assertIsInstance(result, ast.EnumCasePattern)
        self.assertIsInstance(result.arguments[0], ast.PatternArgument)
        self.assertEqual(result.arguments[0].label, 'el')
        self.assertIsInstance(result.arguments[1], ast.PatternArgument)
        self.assertEqual(result.arguments[1].label, 'tail')

    def test_pattern(self):
        parser = tango.pattern + skip(finished)

        result = parser.parse(tango.tokenize('_'))
        self.assertIsInstance(result, ast.Pattern)
        self.assertIsInstance(result.expression, ast.WildcardPattern)
        self.assertIsNone(result.where_clause)

        result = parser.parse(tango.tokenize('a'))
        self.assertIsInstance(result, ast.Pattern)
        self.assertEqual(result.expression.name, 'a')
        self.assertIsNone(result.where_clause)

        result = parser.parse(tango.tokenize('cst x'))
        self.assertIsInstance(result, ast.Pattern)
        self.assertIsInstance(result.expression, ast.ValueBindingPattern)
        self.assertIsNone(result.where_clause)

        result = parser.parse(tango.tokenize('.succ(cst x)'))
        self.assertIsInstance(result, ast.Pattern)
        self.assertIsInstance(result.expression, ast.EnumCasePattern)
        self.assertIsNone(result.where_clause)

        result = parser.parse(tango.tokenize('.some(cst x) where x > 0'))
        self.assertIsInstance(result, ast.Pattern)
        self.assertIsInstance(result.expression, ast.EnumCasePattern)
        self.assertIsInstance(result.where_clause, ast.BinaryExpression)

    def test_matching_pattern(self):
        parser = tango.matching_pattern + skip(finished)

        result = parser.parse(tango.tokenize('a ~= cst x'))
        self.assertIsInstance(result, ast.MatchingPattern)
        self.assertIsInstance(result.value, ast.Identifier)
        self.assertIsInstance(result.pattern, ast.Pattern)
        self.assertIsInstance(result.pattern.expression, ast.ValueBindingPattern)

    def test_if_expr(self):
        parser = tango.if_expr + skip(finished)

        result = parser.parse(tango.tokenize('if a { }'))
        self.assertIsInstance(result, ast.If)
        self.assertEqual(result.condition.name, 'a')
        self.assertFalse(result.body.statements)
        self.assertIsNone(result.else_clause)

        result = parser.parse(tango.tokenize('if a { } else { }'))
        self.assertIsInstance(result.else_clause, ast.Block)

        result = parser.parse(tango.tokenize('if a { } else if b { }'))
        self.assertIsInstance(result.else_clause, ast.If)
        self.assertEqual(result.else_clause.condition.name, 'b')

    def test_switch_case_clause(self):
        parser = tango.switch_case_clause + skip(finished)

        result = parser.parse(tango.tokenize('case a { }'))
        self.assertIsInstance(result, ast.SwitchCaseClause)
        self.assertEqual(result.pattern.expression.name, 'a')
        self.assertFalse(result.body.statements)

    def test_switch_expr(self):
        parser = tango.switch_expr + skip(finished)

        result = parser.parse(tango.tokenize('switch a { }'))
        self.assertIsInstance(result, ast.Switch)
        self.assertEqual(result.expression.name, 'a')
        self.assertFalse(result.clauses)

        result = parser.parse(tango.tokenize('switch a { case b { } }'))
        self.assertIsInstance(result, ast.Switch)
        self.assertEqual(result.expression.name, 'a')
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(result.clauses[0].pattern.expression.name, 'b')

        result = parser.parse(tango.tokenize('switch a { case b { } case c { } }'))
        self.assertIsInstance(result, ast.Switch)
        self.assertEqual(result.expression.name, 'a')
        self.assertEqual(len(result.clauses), 2)
        self.assertEqual(result.clauses[0].pattern.expression.name, 'b')
        self.assertEqual(result.clauses[1].pattern.expression.name, 'c')

    def test_break_stmt(self):
        parser = tango.break_stmt + skip(finished)

        result = parser.parse(tango.tokenize('break'))
        self.assertIsInstance(result, ast.Break)
        self.assertIsNone(result.label)

        result = parser.parse(tango.tokenize('break foo'))
        self.assertIsInstance(result, ast.Break)
        self.assertEqual(result.label, 'foo')

    def test_continue_stmt(self):
        parser = tango.continue_stmt + skip(finished)

        result = parser.parse(tango.tokenize('continue'))
        self.assertIsInstance(result, ast.Continue)
        self.assertIsNone(result.label)

        result = parser.parse(tango.tokenize('continue foo'))
        self.assertIsInstance(result, ast.Continue)
        self.assertEqual(result.label, 'foo')

    def test_for_loop(self):
        parser = tango.for_loop + skip(finished)

        result = parser.parse(tango.tokenize('for _ in a { }'))
        self.assertIsInstance(result, ast.For)
        self.assertIsNone(result.label)
        self.assertIsInstance(result.iterator, ast.Pattern)
        self.assertEqual(result.sequence.name, 'a')
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('for x in a { }'))
        self.assertIsInstance(result, ast.For)
        self.assertIsNone(result.label)
        self.assertEqual(result.iterator.expression.name, 'x')
        self.assertEqual(result.sequence.name, 'a')
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('foo: for x in a { }'))
        self.assertIsInstance(result, ast.For)
        self.assertEqual(result.label, 'foo')
        self.assertEqual(result.iterator.expression.name, 'x')
        self.assertEqual(result.sequence.name, 'a')
        self.assertFalse(result.body.statements)

    def test_while_loop(self):
        parser = tango.while_loop + skip(finished)

        result = parser.parse(tango.tokenize('while a { }'))
        self.assertIsInstance(result, ast.While)
        self.assertIsNone(result.label)
        self.assertEqual(result.condition.name, 'a')
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('foo: while a { }'))
        self.assertIsInstance(result, ast.While)
        self.assertEqual(result.label, 'foo')
        self.assertEqual(result.condition.name, 'a')
        self.assertFalse(result.body.statements)

    def test_assignment(self):
        parser = tango.assignment + skip(finished)

        result = parser.parse(tango.tokenize('x = 0'))
        self.assertIsInstance(result, ast.Assignment)
        self.assertEqual(result.lvalue.expression.name, 'x')
        self.assertEqual(result.rvalue.value, '0')

        result = parser.parse(tango.tokenize('x = if a { } else { }'))
        self.assertIsInstance(result, ast.Assignment)
        self.assertEqual(result.lvalue.expression.name, 'x')
        self.assertIsInstance(result.rvalue, ast.If)
        self.assertEqual(result.rvalue.condition.name, 'a')

        # TODO Test tuple assignment.

    def test_return_stmt(self):
        parser = tango.return_stmt + skip(finished)

        result = parser.parse(tango.tokenize('return 0'))
        self.assertIsInstance(result, ast.Return)
        self.assertEqual(result.value.value, '0')

    def test_call_arg(self):
        parser = tango.call_arg + skip(finished)

        result = parser.parse(tango.tokenize('0'))
        self.assertIsInstance(result, ast.CallArgument)
        self.assertEqual(result.value.value, '0')
        self.assertIsNone(result.label)
        self.assertFalse(result.attributes)

        result = parser.parse(tango.tokenize('@mut 0'))
        self.assertIsInstance(result, ast.CallArgument)
        self.assertEqual(result.attributes, {'mutable'})

        result = parser.parse(tango.tokenize('x = 0'))
        self.assertIsInstance(result, ast.CallArgument)
        self.assertEqual(result.value.value, '0')
        self.assertEqual(result.label, 'x')
        self.assertFalse(result.attributes)

        result = parser.parse(tango.tokenize('x = @mut 0'))
        self.assertIsInstance(result, ast.CallArgument)
        self.assertEqual(result.attributes, {'mutable'})

    def test_call_expr(self):
        parser = tango.call_expr + skip(finished)

        result = parser.parse(tango.tokenize('f()'))
        self.assertIsInstance(result, ast.Call)
        self.assertEqual(result.callee.name, 'f')
        self.assertFalse(result.arguments)

        result = parser.parse(tango.tokenize('f(0, 1)'))
        self.assertIsInstance(result, ast.Call)
        self.assertEqual(result.arguments[0].value.value, '0')
        self.assertEqual(result.arguments[1].value.value, '1')

        result = parser.parse(tango.tokenize('f(x = 0, 1)'))
        self.assertIsInstance(result, ast.Call)
        self.assertEqual(result.arguments[0].value.value, '0')
        self.assertEqual(result.arguments[1].value.value, '1')

        result = parser.parse(tango.tokenize('f(x = 0, y = 1)'))
        self.assertIsInstance(result, ast.Call)
        self.assertEqual(result.arguments[0].value.value, '0')
        self.assertEqual(result.arguments[1].value.value, '1')

    def test_fun_decl_param_decl(self):
        parser = tango.fun_decl_param + skip(finished)

        result = parser.parse(tango.tokenize('cst x: Int'))
        self.assertIsInstance(result, ast.FunctionParameterDecl)
        self.assertEqual(result.label, 'x')
        self.assertEqual(result.name, 'x')
        self.assertFalse(result.attributes)
        self.assertEqual(result.type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize('mut x: Int'))
        self.assertIn('mutable', result.attributes)

        result = parser.parse(tango.tokenize('cst a x: Int'))
        self.assertIsInstance(result, ast.FunctionParameterDecl)
        self.assertEqual(result.label, 'a')
        self.assertEqual(result.name, 'x')
        self.assertFalse(result.attributes)
        self.assertEqual(result.type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize('cst a x: Int'))
        self.assertIsInstance(result, ast.FunctionParameterDecl)
        self.assertEqual(result.label, 'a')
        self.assertEqual(result.name, 'x')
        self.assertFalse(result.attributes)
        self.assertEqual(result.type_annotation.name, 'Int')

    def test_fun_decl_param_decl_with_default_value(self):
        parser = tango.fun_decl_param + skip(finished)

        result = parser.parse(tango.tokenize('cst x: Int = 0'))
        self.assertIsInstance(result, ast.FunctionParameterDecl)
        self.assertIsInstance(result.default_value, ast.Literal)

    def test_fun_decl(self):
        parser = tango.fun_decl + skip(finished)

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

    def test_prop_decl(self):
        parser = tango.prop_decl + skip(finished)

        result = parser.parse(tango.tokenize('cst x'))
        self.assertIsInstance(result, ast.PropertyDecl)
        self.assertEqual(result.name, 'x')
        self.assertFalse(result.attributes)
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initializer)
        self.assertIsNone(result.getter)
        self.assertIsNone(result.setter)

        result = parser.parse(tango.tokenize('mut x'))
        self.assertIsInstance(result, ast.PropertyDecl)
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.attributes, {'mutable'})
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initializer)
        self.assertIsNone(result.getter)
        self.assertIsNone(result.setter)

        result = parser.parse(tango.tokenize('shd x'))
        self.assertIsInstance(result, ast.PropertyDecl)
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.attributes, {'shared'})
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initializer)
        self.assertIsNone(result.getter)
        self.assertIsNone(result.setter)

        result = parser.parse(tango.tokenize('cst x: Int'))
        self.assertIsInstance(result, ast.PropertyDecl)
        self.assertEqual(result.name, 'x')
        self.assertFalse(result.attributes)
        self.assertEqual(result.type_annotation.name, 'Int')
        self.assertIsNone(result.initializer)
        self.assertIsNone(result.getter)
        self.assertIsNone(result.setter)

    def test_stored_prop_decl(self):
        parser = tango.prop_decl + skip(finished)

        result = parser.parse(tango.tokenize('cst x = 0'))
        self.assertIsInstance(result, ast.PropertyDecl)
        self.assertEqual(result.name, 'x')
        self.assertFalse(result.attributes)
        self.assertIsNone(result.type_annotation)
        self.assertEqual(result.initializer.value, '0')
        self.assertIsNone(result.getter)
        self.assertIsNone(result.setter)

        result = parser.parse(tango.tokenize('cst x: Int = 0'))
        self.assertIsInstance(result, ast.PropertyDecl)
        self.assertEqual(result.name, 'x')
        self.assertFalse(result.attributes)
        self.assertEqual(result.type_annotation.name, 'Int')
        self.assertEqual(result.initializer.value, '0')
        self.assertIsNone(result.getter)
        self.assertIsNone(result.setter)

    def test_computed_prop_decl(self):
        parser = tango.prop_decl + skip(finished)

        result = parser.parse(tango.tokenize('cst x { fun get() -> Int {} }'))
        self.assertIsInstance(result, ast.PropertyDecl)
        self.assertEqual(result.name, 'x')
        self.assertFalse(result.attributes)
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initializer)
        self.assertIsInstance(result.getter, ast.FunctionDecl)
        self.assertEqual(result.getter.name, 'get')
        self.assertIsNone(result.setter)

        result = parser.parse(tango.tokenize('mut x { fun set(cst _ value: Int) { } }'))
        self.assertIsInstance(result, ast.PropertyDecl)
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.attributes, {'mutable'})
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initializer)
        self.assertIsNone(result.getter)
        self.assertIsInstance(result.setter, ast.FunctionDecl)
        self.assertEqual(result.setter.name, 'set')

        result = parser.parse(tango.tokenize(
            '''
            mut x {
                fun get() -> Int { }
                fun set(cst _ value: Int) { }
            }
            '''))
        self.assertIsInstance(result, ast.PropertyDecl)
        self.assertEqual(result.name, 'x')
        self.assertEqual(result.attributes, {'mutable'})
        self.assertIsNone(result.type_annotation)
        self.assertIsNone(result.initializer)
        self.assertIsInstance(result.getter, ast.FunctionDecl)
        self.assertEqual(result.getter.name, 'get')
        self.assertIsInstance(result.setter, ast.FunctionDecl)
        self.assertEqual(result.setter.name, 'set')

    def test_enum_case_param_decl(self):
        parser = tango.enum_case_param + skip(finished)

        result = parser.parse(tango.tokenize('_: Int'))
        self.assertIsInstance(result, ast.EnumCaseParameterDecl)
        self.assertIsNone(result.label)
        self.assertEqual(result.type_annotation.name, 'Int')

        result = parser.parse(tango.tokenize('x: Int'))
        self.assertIsInstance(result, ast.EnumCaseParameterDecl)
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

        result = parser.parse(tango.tokenize('enum E { }'))
        self.assertIsInstance(result, ast.EnumDecl)
        self.assertEqual(result.name, 'E')
        self.assertFalse(result.generic_parameters)
        self.assertFalse(result.import_list)
        self.assertFalse(result.conformance_list)
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('enum E<T, U> { }'))
        self.assertEqual(result.generic_parameters[0], 'T')
        self.assertEqual(result.generic_parameters[1], 'U')

        result = parser.parse(tango.tokenize('enum E import F, G { }'))
        self.assertEqual(result.import_list[0].name, 'F')
        self.assertEqual(result.import_list[1].name, 'G')

        result = parser.parse(tango.tokenize('enum E: P, Q import F { }'))
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

        result = parser.parse(tango.tokenize('struct S: P, Q import T {}'))
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

    def test_protocol_decl(self):
        parser = tango.protocol_decl + skip(finished)

        result = parser.parse(tango.tokenize('protocol P {}'))
        self.assertIsInstance(result, ast.ProtocolDecl)
        self.assertEqual(result.name, 'P')
        self.assertFalse(result.conformance_list)
        self.assertFalse(result.import_list)
        self.assertFalse(result.body.statements)

        result = parser.parse(tango.tokenize('protocol P : Q, R {}'))
        self.assertEqual(result.conformance_list[0].name, 'Q')
        self.assertEqual(result.conformance_list[1].name, 'R')
        self.assertFalse(result.import_list)

        result = parser.parse(tango.tokenize('protocol P: Q, R import T {}'))
        self.assertEqual(result.conformance_list[0].name, 'Q')
        self.assertEqual(result.conformance_list[1].name, 'R')
        self.assertEqual(result.import_list[0].name, 'T')

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
