import unittest

from funcparserlib.parser import finished, skip
from tango.parser import parse
from tango.scope_binder import ScopeBinder, SymbolsExtractor
from tango.utils import find


class TestScopeBinder(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        cls.scope_binder = ScopeBinder()
        cls.symbols_extractor = SymbolsExtractor()

    def prepare(self, source):
        module = parse(source)
        module.name = 'main'
        self.__class__.symbols_extractor.visit(module)
        self.__class__.scope_binder.visit(module)
        return module

    def test_module_decl(self):
        module = self.prepare('')
        self.assertIn('scope', module.body.__info__)
        self.assertIn('symbols', module.body.__info__)

        module = self.prepare('cst x')
        self.assertEqual(module.body.__info__['symbols'], {'x'})

        module = self.prepare(
        '''
        cst x
        fun f() { }
        struct S { }
        '''
        )
        self.assertEqual(module.body.__info__['symbols'], {'x', 'f', 'S'})

    def test_property_decl(self):
        module = self.prepare('cst x')
        decl = find('PropertyDecl:first', module)[0]
        self.assertIn('scope', decl.__info__)
        self.assertEqual(module.body.__info__['scope'], decl.__info__['scope'])

        module = self.prepare('cst x: Int')
        decl = find('PropertyDecl:first', module)[0]
        self.assertIn('scope', decl.__info__)
        self.assertEqual(module.body.__info__['scope'], decl.__info__['scope'])
        self.assertIn('scope', decl.type_annotation.__info__)
        self.assertEqual(module.body.__info__['scope'], decl.type_annotation.__info__['scope'])

        module = self.prepare('cst x = Int')
        decl = find('PropertyDecl:first', module)[0]
        self.assertIn('scope', decl.__info__)
        self.assertEqual(module.body.__info__['scope'], decl.__info__['scope'])
        self.assertIn('scope', decl.initializer.__info__)
        self.assertEqual(module.body.__info__['scope'], decl.initializer.__info__['scope'])


if __name__ == '__main__':
    unittest.main()
