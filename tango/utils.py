from collections.abc import Sized, Iterable

from . import ast


class Selection(Sized, Iterable):

    def __init__(self, nodes):
        self.selection = nodes

    def __iter__(self):
        return iter(self.selection)

    def __len__(self):
        return len(self.selection)

    def __getitem__(self, item):
        return self.selection[item]


class NodeFinder(ast.Visitor):

    def __init__(self, target, modifier):
        self.target = target
        self.modifier = modifier
        self.finished = False

        self.results = []

    def visit(self, node):
        if self.finished:
            return

        if node.__class__.__name__ == self.target:
            self.results.append(node)
            if self.modifier == '*':
                self.generic_visit(node)
            elif self.modifier == 'first':
                self.finished = True
        else:
            self.generic_visit(node)


def find(selector, selection):
    components = selector.split()
    if not components:
        return Selection([])

    next_node_name = components.pop()
    if ':' in next_node_name:
        next_node_name, modifier = next_node_name.split(':')
    else:
        modifier = ''

    if isinstance(selection, ast.Node):
        selection = (selection,)

    results = []
    for node in selection:
        finder = NodeFinder(next_node_name, modifier)
        finder.visit(node)
        results.extend(finder.results)
    results = Selection(results)

    if components:
        return find(' '.join(components), results)
    return results
