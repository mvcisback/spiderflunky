from parsimonious.grammar import Grammar
from parsimonious.nodes import NodeVisitor


API_GRAMMAR = Grammar(r"""
sigs = (sig (_ ";")?)+
sig = ("static"? _ ident _)? lambda
lambda = lambda

declare = "declare" __ kind __ ident _ "{" _ defines _ "}"
interface = "export"? __ "interface" __ ident __ "{" _ sigs _ "}"
defines = "TODO"

args = arg+
arg = ident _ sep _ type (_ "," _)?
ident = ~r"[\w\.\?]+"
type = lambda / (ident type_vars? "[]"?)
type_vars = "<" _ type_var+ _ ">"
type_var = ~r"\w+" (_ "," _) ?
kind = "module"
comment = line_comment / block_comment
line_comment = "TODO"
block_comment = "TODO"
sep = ":" / "=>"

_ = ~r"\s*"
__ = ~r"\s+"

""")

def parse(lines):
    raise NotImplementedError
