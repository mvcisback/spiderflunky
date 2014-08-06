from parsimonious.grammar import Grammar
from parsimonious.nodes import NodeVisitor


TYPESCRIPT_GRAMMAR = Grammar(r"""
    comment = block_comment / line_comment
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

    line_comment = "//" line_comment_char+
    line_comment_char = ~r"."
    block_comment = "/*" block_comment_char+ "*/"
    block_comment_char = !("*/") ~r"."s

    sep = ":" / "=>"

    _ = ~r"\s*"
    __ = ~r"\s+"
""")


def parse(lines):
    raise NotImplementedError
