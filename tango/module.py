import os
try:
    import cPickle as pickle
except ImportError:
    import pickle


class Module(object):

    def __init__(self, name):
        self.name = name
        self.symbols = {}

    def save(self):
        with open(self.name + '.tngc', 'wb') as f:
            pickle.dump(self, f)


class Symbol(object):

    def __init__(self, name, type=None, code=None, nested=None):
        self.name   = name
        self.type   = type
        self.code   = code
        self.nested = nested or {}
