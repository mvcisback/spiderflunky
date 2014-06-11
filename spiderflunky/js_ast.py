from inspect import isclass
from more_itertools import first

class Node(dict):
    """A wrapper around a native Reflect.parse dict providing some convenience
    methods and some caching of expensive computations

    Importing a zillion helper functions into every module is a pain.

    """
    def walk_up(self):
        """Yield each node from here to the root of the tree, starting with
        myself."""
        node = self
        while node:
            yield node
            node = node.get('_parent')

    def walk_down(self, skip=lambda n: False, include_self=True):
        """Yield each (depth, node) from here downward, myself included, in depth-first
        added more Node types and depth to walking down
        pre-order.

        :arg skip: A predicate decribing nodes to not descend into. We always
            return ourselves, even if the predicate says to skip us.
        :arg include_self: A flag for including the root in the walk down.

        The AST we get from Reflect.parse is somewhat unsatisfying. It's not a
        uniform tree shape; it seems to have already been turned into more
        specialized objects. Thus, we have to traverse into different fields
        depending on node type.

        """
        if include_self:
            yield 0, self

        for child in self.children():
            if not skip(child):
                # Just a "yield from":
                for depth, ret in child.walk_down(skip=skip):
                    yield depth+1, ret

    def _children(self):
        return []

    def children(self):
        """Return my children, accounting for variations in where children are
        stored in each node type."""
        return self._children() or []

    def nearest_scope(self):
        """Return the closest containing scope, constructing and caching it
        first if necessary."""
        return self.nearest_scope_holder().scope()

    def scope_chain(self):
        """Yield each scope-defining node from myself upward."""
        node = self.nearest_scope_holder()
        while True:
            yield node
            if node['type'] == 'Program':
                break
            node = node['_parent'].nearest_scope_holder()

    def nearest_scope_holder(self):
        """Return the nearest node that can have its own scope, potentially
        including myself.

        This will be either a FunctionDeclaration or a Program (for now).

        """
        return first(n for n in self.walk_up() if n['type'] in
                     ['FunctionDeclaration', 'Program'])

    def scope_of(self, symbol_name):
        """Return the nearest enclosing AST node (including myself) where the
        variable named ``symbol_name`` is defined."""
        # TODO: Find formal params, lets, and window.* (and navigator.*?
        # Anything else magic? Ugh, and you can stick refs to window in things.
        # Is that going to be a problem?)

        for node in self.scope_chain():
            if symbol_name in node.scope():
                return node
        return node  # global

    def __str__(self):
        return self['type']


class Program(Node):
    """A Reflect.parse AST with some other handy properties

    A Program is considered to be immutable once finalize() is called, though
    we may continue to make annotations on it for speed.

    """
    def finalize(self):
        """Add parent pointers to my nodes, and assemble a map so we can
        reference nodes by ID."""
        def _add_ids(ast):
            """Add an ``_id`` key to each node in me so we can represent graphs of
            them economically, and build a map of those IDs to the nodes."""
            ret = {}
            for _, node in ast.walk_down():
                identity = node['_id'] = id(node)
                ret[identity] = node
            return ret

        def _add_parent_refs(node):
            """Add parent pointers to each node in me."""
            for child in node.children():
                child['_parent'] = node
                _add_parent_refs(child)

        self.by_id = _add_ids(self)
        _add_parent_refs(self)

    def scope(self):
        # Arguable.
        return set()

    def __str__(self,):
        return "{0}\n".format(super(Program, self).__str__())+"\n".join(
            (" " * i + str(x) for i, x in self.walk_down(include_self=False)))

    def _children(self,):
        return self['body']


class Function(Node):
    def _children(self):
        return [self['id']] + self['params'] + self['defaults'] +\
            [self['rest'], self['body'], self['generator'], self['expression']]

# Statements

class EmptyStatement(Node):
    pass

class BlockStatement(Node):
    def _children(self):
        return self['body']

class ExpressionStatement(Node):
    def _children(self):
        return [self['expression']]

class IfStatement(Node):
    def _children(self):
        ret = [self['test'], self['consequent']]
        if self['alternate']:
            ret.append(self['alternate'])
        return ret

class LabeledStatement(Node):
    def _children(self):
        return [self['body']]

class BreakStatement(Node):
    pass

class ContinueStatement(Node):
    def _children(self):
        return [self['object'], self['body']]

class WithStatement(Node):
    def _children(self):
        return [self['body'], self['object']]

class SwitchStatement(Node):
    def _children(self):
        return [self['discriminant']] + self['cases'] + [self['lexical']]

class ReturnStatement(Node):
    def _children(self):
        return [self['argument']]

class ThrowStatement(Node):
    def _children(self):
        return [self['argument']]

class TryStatement(Node):
    def _children(self):
        return [self['block'], self['handler']] + self['guardedHandlers'] + [self['finalizer']]

class WhileStatement(Node):
    def _children(self):
        return [self['test'], self['body']]

class DoWhileStatement(Node):
    def _children(self):
        return [self['body'], self['test']]

class ForStatement(Node):
    def _children(self):
        return [self['init'], self['test'], self['update'], self['body']]

class ForInStatement(Node):
    def _children(self):
        return [self['left'], self['right'], self['body'], self['each']]

class ForOfStatement(Node):
    def _children(self):
        return [self['left'], self['right'], self['body']]

class LetStatement(Node):
    def _children(self):
        return self['head'] + [self['body']]

class DebuggerStatement(Node):
    pass

# Declarations

class FunctionDeclaration(Node):
    def scope(self):
        """Return the set of symbols declared exactly at this node."""
        # We store a set of symbols at each node that can hold a scope, except
        # that we don't bother for the Program (global) scope. It holds
        # everything we couldn't find elsewhere.

        if '_scope' not in self:  # could store this in an instance var
            # Find all the var decls within me, but don't go within any other
            # functions. This implements hoisting.
            self['_scope'] = set(
                node['id']['name'] for _, node in self.walk_down(
                    skip=lambda n: n['type'] == 'FunctionDeclaration')
                if node['type'] == 'VariableDeclarator') | \
                set(param['name'] for param in self['params'])
        return self['_scope']

    def _children(self):
        raise NotImplementedError(self['type'])

class VariableDeclaration(Node):
    def _children(self):
        if 'declarations' in self:
            return self['declarations'] + [self['kind']]
        else:
            return [self['id'], self['init']]

# Expressions

class ThisExpression(Node):
    pass

class ArrayExpression(Node):
    def _children(self):
        return self['elements']

class ObjectExpression(Node):
    def _children(self):
        return self['properties']

class FunctionExpression(Node):
    def _children(self):
        return [self['id']] + self['params'] + self['defaults'] + \
            [self['rest'], self['body'], self['generator'], self['expression']]

class ArrowExpression(Node):
    def _children(self):
        return self['params'] + self['defaults'] + \
            [self['rest'], self['body'], self['generator'], self['expression']]

class SequenceExpression(Node):
    def _children(self):
        return self['expressions']

class UnaryExpression(Node):
    def _children(self):
        return [self['prefix'], self['argument']]

class BinaryExpression(Node):
    def _children(self):
        return [self['left'], self['right']]

class AssignmentExpression(Node):
    def _children(self):
        return [self['left'], self['right']]

class UpdateExpression(Node):
    def _children(self):
        return [self['argument'], self['prefix']]

class LogicalExpression(Node):
    def _children(self):
        return [self['left'], self['right']]

class ConditionalExpression(Node):
    def _children(self):
        return [self['test'], self['alternate'], self['consequent']]

class NewExpression(Node):
    def _children(self):
        return [self['callee']] + self['arguments']

class CallExpression(Node):
    def _children(self):
        return [self['callee']] + self['arguments']

class MemberExpression(Node):
    def _children(self):
        return [self['object'], self['property'], self['computed']]

class YieldExpression(Node):
    def _children(self):
        return [self['argument']]

class ComprehensionExpression(Node):
    def _children(self):
        return [self['body']] + self['blocks'] + [self['filter']]

class GeneratorExpression(Node):
    def _children(self):
        return [self['body']] + self['blocks'] + [self['filter']]

class GraphExpression(Node):
    def _children(self):
        return [self['index'], self['expression']]

class GraphIndexExpression(Node):
    def _children(self):
        return [self['index']]

class LetExpression(Node):
    def _children(self):
        return self['head'] + [self['body']]

# Patterns

class ObjectPattern(Node):
    def _children(self):
        return self['properties']

class ArrayPattern(Node):
    def _children(self):
        return self['elements']

# Clauses

class SwitchCase(Node):
    def _children(self):
        return [self['test']] + self['consequent']

class CatchClause(Node):
    def _children(self):
        return [self['param'], self['guard'], self['body']]

class ComprehensionBlock(Node):
    def _children(self):
        return [self['left'], self['right'], self['each']]

# Miscellaneous

class Identifier(Node):
    def _children(self):
        return [self['name']]

class Literal(Node):
    pass

# E4X
class XMLDefaultDeclaration(Node):
    def _children(self):
        return [self['namespace']]

class XMLAnyName(Node):
    pass

class XMLQualifiedIdentifier(Node):
    def _children(self):
        return [self['left'], self['right'], self['computed']]

class XMLFunctionQualifiedIdentifier(Node):
    def _children(self):
        return [self['right'], self['computed']]

class XMLFilterExpression(Node):
    def _children(self):
        return [self['left'], self['right']]

class XMLElement(Node):
    def _children(self):
        return self['contents']

class XMLList(Node):
    def _children(self):
        return self['contents']

class XMLEscape(Node):
    def _children(self):
        return [self['expression']]

class XMLText(Node):
    def _children(self):
        return [self['text']]

class XMLStartTag(Node):
    def _children(self):
        return self['contents']

class XMLEndTag(Node):
    def _children(self):
        return self['contents']

class XMLPointTag(Node):
    def _children(self):
        if type(self['contents']) is list:
            return self['contents']
        else:
            return [self['contents']]

class XMLName(Node):
    def _children(self):
        return self['contents']

class XMLComment(Node):
    def _children(self):
        return self['contents']

class XMLProcessingInstruction(Node):
    def _children(self):
        return [self['target'], self['contents']]


NODE_TYPES = {cls.__name__:cls for cls in globals().values() if
              isclass(cls) and issubclass(cls, dict)}

def make_node(d):
    """Construct the right kind of Node for a raw Reflect.parse node."""
    try:
        return NODE_TYPES[d['type'], Node](d)
    except KeyError:
        raise NotImplementedError(d.get('type', "Unknown Type"))
