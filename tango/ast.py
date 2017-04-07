class Visitor(object):

    def visit(self, node):
        method_name = 'visit_%s' % node.__class__.__name__
        getattr(self, method_name)(node)

    def visit_ModuleDecl(self, node):
        for statement in node.statements:
            self.visit(statement)

    def visit_ConstantDecl(self, node):
        self.visit(node.name)
        if node.type_annotation:
            self.visit(node.type_annotation)
        if node.initial_value:
            self.visit(node.initial_value)

    def visit_VariableDecl(self, node):
        self.visit(node.name)
        if node.type_annotation:
            self.visit(node.type_annotation)
        if node.initial_value:
            self.visit(node.initial_value)

    def visit_FunctionDecl(self, node):
        self.visit(node.name)
        self.visit(node.signature)
        self.visit(node.body)
        for generic_parameter in node.generic_parameters:
            self.visit(generic_parameter)

    def visit_EnumDecl(self, node):
        self.visit(node.name)
        for case in node.cases:
            self.visit(case)
        for method in node.methods:
            self.visit(method)
        for type_import in node.import_list:
            self.visit(type_import)
        for type_conformance in node.conformance_list:
            self.visit(type_conformance)

    def visit_StructDecl(self, node):
        self.visit(node.name)
        for stored_property in node.stored_properties:
            self.visit(stored_property)
        for method in node.methods:
            self.visit(method)
        for type_import in node.import_list:
            self.visit(type_import)
        for type_conformance in node.conformance_list:
            self.visit(type_conformance)

    def visit_Identifier(self, node):
        pass

    def visit_Literal(self, node):
        pass

    def visit_TypeIdentifier(self, node):
        self.visit(node.name)
        for specialization_parameter in node.specialization_parameters:
            self.visit(specialization_parameter)

    def visit_Assignment(self, node):
        self.visit(node.target)
        self.visit(node.value)

    def visit_BinaryExpression(self, node):
        # self.visit(node.operator)
        self.visit(node.left)
        self.visit(node.right)
