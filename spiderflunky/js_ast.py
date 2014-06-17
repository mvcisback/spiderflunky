"""Contains code pertaining to the JS AST representation. This is generated
based on the Mozilla Parser API at import time.

"""

from pyquery import PyQuery
from parsimonious.grammar import Grammar
from parsimonious.nodes import NodeVisitor
from parsimonious.exceptions import ParseError
from more_itertools import first
from toposort import toposort_flatten
from itertools import islice
import sys
import pkg_resources
from networkx import DiGraph
import networkx as nx

class JsAst(DiGraph):
    def __init__(self, root):
        DiGraph.__init__(self)
        self.root = root
        self.add_node(root)
        queue = [root]
        while queue:
            parent = queue.pop()
            for node in parent.children():
                self.add_edge(parent, node)
            queue.extend(parent.children())

    def parent(self, node):
        predecessors = self.predecessors(node)
        return predecessors[0] if predecessors else None

    def walk_up(self, start):
        """Yield each node from here to the root of the tree, starting with
        myself."""
        node = start
        while node:
            yield node
            node = self.parent(node)

    def walk_down(self, root=None):
        """Yield each node from here downward, myself included,
        in depth-first pre-order.

        :arg include_self: A flag for including the root in the walk down.

        """
        return nx.traversal.dfs_preorder_nodes(self, root)


class BaseNode(dict):
    """A wrapper around a native Reflect.parse dict providing some convenience
    methods and some caching of expensive computations

    Importing a zillion helper functions into every module is a pain.

    """
    def __init__(self, *args, **kwargs):
        dict.__init__(self, *args, **kwargs)

    def _children(self):
        return []

    def children(self):
        """Return my children, accounting for variations in where children are
        stored in each node type.

        """
        return self._children() or []

    def nearest_scope(self):
        """Return the closest containing scope, constructing and caching it
        first if necessary.

        """
        return self.nearest_scope_holder().scope()

    def scope_chain(self):
        """Yield each scope-defining node from myself upward.

        """
        node = self.nearest_scope_holder()
        while True:
            yield node
            if isinstance(node, Program):
                break
            node = node.parent.nearest_scope_holder()

    def nearest_scope_holder(self):
        """Return the nearest node that can have its own scope, potentially
        including myself.

        This will be either a FunctionDeclaration or a Program (for now).

        """
        return first(n for n in self.walk_up() if
                     isinstance(n, (FunctionDeclaration, Program)))

    def scope_of(self, symbol_name):
        """Return the nearest enclosing AST node (including myself) where the
        variable named ``symbol_name`` is defined.

        """
        for node in self.scope_chain():
            if symbol_name in node.scope():
                return node
        return node  # global

    def scope(self,):
        """Return the set of symbols declared exactly at this node.

        """
        return set()

    def __hash__(self):
        return id(self)


def _clean(text):
    return text.replace(u'\xa0', u' ')


def _get_specs(parser, parser_api_htm):
    specs = (_clean(elem.text) for elem in PyQuery(parser_api_htm)('pre'))
    visitor = SpecVisitor()
    for spec in specs:
        try:
            yield visitor.visit(parser.parse(spec))
        except ParseError:
            pass


def get_specs(*args, **kwargs):
    """Return a list of specs (the results of the SpecVisitor)
    based on Mozilla ParserAPI.

    parser: parser for the grammar
    filename: location of the Mozilla Parser API
    """
    return list(_get_specs(*args, **kwargs))


def dependency_order(specs):
    dep_map = {name: set(parents) for (name, parents, _) in specs}
    return toposort_flatten(dep_map)


def get_class_map(specs):
    """Based on a list of specifications, return a mapping name->cls

    """
    dep_order = dependency_order(specs)
    spec_map = {name: (parents, vals) for name, parents, vals in specs}
    class_map = {}
    for name in dep_order:
        class_map[name] = _node_class_factory(class_map, name, *spec_map[name])
    return class_map


def _flatten(lis):
    """Flattens nonuniform nested iterators.

    """
    # based on http://stackoverflow.com/a/2158532
    for elem in lis:
        if isinstance(elem, list) and not isinstance(elem, basestring):
            for sub in _flatten(elem):
                yield sub
        else:
            yield elem


def function_scope(self):
    """Return the set of symbols declared exactly at this node.

    """
    # We store a set of symbols at each node that can hold a scope, except
    # that we don't bother for the Program (global) scope. It holds
    # everything we couldn't find elsewhere.

    if '_scope' not in self:  # could store this in an instance var
        # Find all the var decls within me, but don't go within any other
        # functions. This implements hoisting.
        self['_scope'] = set(
            node['id']['name'] for node in self.walk_down(
                skip=lambda n: isinstance(n, FunctionDeclaration))
            if isinstance(node, VariableDeclarator)) | \
            set(param['name'] for param in self['params'])
    return self['_scope']


def _node_class_factory(class_map, name, parents, fields):
    """Returns a class representing an AST node.

    """
    # Could perhaps be replaced by an explicit MetaClass
    def _children(self):
        fields_vals = filter(lambda val: isinstance(val, BaseNode),
                             _flatten((self[f] for f in fields)))
        return fields_vals

    __dict__ = {
        '_children': _children,
        '__repr__': lambda self: name}
    if "params" in fields:
        __dict__['scope'] = function_scope

    bases = tuple(map(class_map.get, parents)) if parents else (BaseNode,)
    return type(str(name), bases, __dict__)


def make_graph(root):
    """Sets the parent attribute for all nodes in the tree.
    Root's parent is None

    """



API_GRAMMAR = r"""
start = _ interface _
interface = "interface" __ id _ inherit? _ "{" _ attrs? _ "}"

ops = "\"" op "\"" (_ "|" _ ops)?
op = ~r'([^{}"\s])+'

inherit = "<:" __ parents
parents = id _ ("," _ parents)?

attr = id _ ":" _ vals
attrs = attr _ ";" _ attrs?

vals = val (_ "|" _ vals)?
val = "string" / "null" / "boolean" / dict / list / qid / uint / id
qid = '"' id '"'
list = "[" _ vals _ "]"
dict = "{" _ dict_attrs _ "}"
dict_attrs = attr _ ("," _ dict_attrs)?

uint = "uint32" (_ op _ digit)?

id = ~r"[A-Za-z]+"
digit = ~r"[0-9]+"
_ = ~r"\s*"
__ = ~r"\s+"
"""


class SpecVisitor(NodeVisitor):
    """Implements a NodeVisitor for the Mozilla Parser API
    Returns (name of interface, parents, fields)

    """
    def visit_start(self, node, children):
        return children[1]

    def visit_interface(self, node, children):
        name = children[2]
        inherit = children[4][0] if children[4] else []
        attrs = children[8][0] if children[8] else []
        return (name, inherit, attrs)

    def visit_inherit(self, _, (__, ___, parents)):
        return parents

    def visit_parents(self, _, (name, __, next_parent)):
        return [name]

    def visit_attrs(self, _, (attr, __, ___, ____, attrs)):
        return [attr] + (attrs[0] if attrs else [])

    def visit_attr(self, _, children):
        # task throw away attr if its static like type
        return children[0]

    def visit_id(self, node, _):
        return node.match.group()

    def generic_visit(self, _, visited_children):
        return visited_children

API_PARSER = Grammar(API_GRAMMAR)


def node_hook(json_dict):
    """This is a object hook for the json loads function.

    """
    return CLASS_MAP.get(json_dict.get('type'), BaseNode)(json_dict)

# Inject classes from spec into module
PARSER_API_HTM = pkg_resources.resource_string(__name__, "Parser_API.htm")

CLASS_MAP = get_class_map(get_specs(API_PARSER, PARSER_API_HTM))
THIS_MODULE = sys.modules[__name__]
for cls_name, cls in CLASS_MAP.items():
    setattr(THIS_MODULE, cls_name, cls)
