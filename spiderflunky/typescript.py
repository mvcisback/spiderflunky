from parsimonious.grammar import Grammar
from parsimonious.nodes import NodeVisitor


TYPESCRIPT_GRAMMAR = Grammar(r"""
    start = func_type
    ident = ~r"[\w\.\?]+"
    str_lit = "TODO"
    num_lit = "TODO"

    comment = block_comment / line_comment
    line_comment = "//" line_comment_char+
    line_comment_char = ~r"."
    block_comment = "/*" block_comment_char+ "*/"
    block_comment_char = !("*/") ~r"."s

    type = predefined_type /  type_ref / type_query / type_lit
    predefined_type = "any" / "number" / "boolean" / "string" / "void"
    type_ref = type_name space type_args?
    type_name = ident ("." ident)*
    type_lit = obj_type / array_type / func_type / constr_type
    obj_type = "{" _ type_body _ "}"
    array_type = elem_type space* "[" _ "]"
    elem_type = predefined_type / type_ref /  type_query / obj_type / array_type    
    constr_type = "new" func_type
    type_body = type_member_list (_ ";")?
    type_member_list = type_member+
    type_member = (prop_sig / call_sig / constr_sig/ index_sig / method_sig) (_ ";")?
    prop_sig = prop_name "?"? type_annotation?
    prop_name = ident / str_lit / num_lit
    
    param_list = required_parm_list
                 / optional_param_list
                 / rest_param
                 / (required_parm_list _ "," _ optional_param_list)
                 / (required_parm_list _ "," _ rest_param)
                 / (optional_param_list _ "," _ "rest_param")
                 / (required_parm_list _ "," _ optional_param_list _ "," _ rest_param)


    required_parm_list = "TODO"
    optional_param_list = "TODO"
    rest_param = "TODO"


    call_sig = type_params? "(" param_list ")" type_annotation?
    func_type = type_params _ "(" _ param_list _ ")" _ "=>" type
    constr_sig = "new" _ type_params? _ "(" _ param_list? _ ")" _ type_annotation?

    index_sig = "[" _ ident _ ":" _ ("string" / "number") _ "]" _ type_annotation
    method_sig = prop_name "?"? call_sig

    type_query = "typeof" _ type_query_expr
    type_query_expr = ident ("." ident)*
    type_args = type_arg+
    type_arg = type _ (_ "," _)?
    type_params = "<" _ type_param+ _ ">"
    type_param = ident _ (constraint)? (_ "," _)?
    type_annotation = "TODO"
    constraint = "extends" type


    declaration_source_file = (declaration_element+)?
    declaration_element = export_assignment / ("export"? decls)
    decls = interface_decl / import_decl / external_import_decl / ambient_decl

    external_import_decl = "import" _ ident _ "=" _ external_module_ref _ ";"
    external_module_ref = "require" _ "(" _ str_lit _ ")"

    import_decl = "import" _ ident _ "=" _ entity_name _ ";"
    export_assignment = "export" _ "=" ident _ ";"
    import_decl = "TODO"
    ambient_decl = "TODO"
    interface_decl = "TODO"
   

    sep = ":" / "=>"

    _ = ~r"\s*"
    __ = ~r"\s+"
    space = !(~r"[\n\r]") ~r"\s"
""")


def parse(lines):
    raise NotImplementedError
