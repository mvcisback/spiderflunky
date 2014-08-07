from parsimonious.grammar import Grammar
from parsimonious.nodes import NodeVisitor


TYPESCRIPT_GRAMMAR = Grammar(r"""
    ident = ~r"[\w\.\?]+" # TODO really implement

    str_lit = ('"' double_str_char* '"') / ("'" single_str_char* "'")
        double_str_char = (!('"') !break_char source_char)
                         / str_char_common
        single_str_char = (!('"') !break_char source_char)
                        / str_char_common
        str_char_common = ("\\" escape_seq)
                        / line_continuation
        break_char = "\\" / line_terminator
        line_continuation = "\\" line_terminator_sequence
        escape_seq = char_escape_sequence
                   / "TODO lookahead" 
                   / hex_escape_seq
                   / unicode_escape_seq
        char_escape_seq = single_escape_char
                        / non_escape_char
        single_escape_char = ~r"['\"\\bfnrtv]"
        non_escape_char = !escape_char !line_terminator source_char
        escape_char = single_escape_char
                    / decimal_digit
                    / "x" / "u"
        hex_escape_seq = "x" hex_digit hex_digit
        unicode_escape_seq = "u" hex_digit hex_digit hex_digit hex_digit
        source_char = ~r"."
        
        line_terminator = "\n" / "\r" / LS / PS
        line_terminator_sequence =  "\r\n" / "\r" / "\n" / LS / PS
        LS = "\u2028" # TODO can't prefix with u
        PS = "\u2029"
        char_escape_sequence = single_escape_char / non_escape_char

    hex_digit = ~r"[0-9a-eA-F]"
    decimal_digit = ~r"[0-9]"
    num_lit = decimal_lit / hex_int_lit
    decimal_lit = (decimal_int_lit "." decimal_digit* exponent_part?)
                / ("." decimal_digit+ exponent_part?)
                / (decimal_int_lit exponent_part?)
        decimal_int_lit = "0" / (~r"[1-9]" decimal_digit*)
        decimal_digit = ~r"[0-9]"
        exponent_part = ("e" / "E") signed_int
        signed_int = ("+" / "-")? decimal_digit+
        hex_int_lit = "0x" hex_digit

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
    public_or_private = "public" / "private"
    
    param_list = required_param_list
               / optional_param_list
               / rest_param
               / (required_param_list _ "," _ optional_param_list)
               / (required_param_list _ "," _ rest_param)
               / (optional_param_list _ "," _ "rest_param")
               / (required_param_list _ "," _ optional_param_list _ "," _ rest_param)

    required_param_list = required_param (_ "," _ required_param)*
    required_param = (public_or_private? _ ident _ type_annotation?)
                   / (ident _ ":" _ str_lit)
    optional_param_list = optional_param (_ "," _ optional_param)*
    optional_param = public_or_private? _ ident _ (("?" _ type_annotation?) / (type_annotation? _ initialiser))
        initialiser = "TODO"
    rest_param = "..." _ ident type_annotation?

    call_sig = type_params? "(" param_list ")" type_annotation?
    func_type = type_params _ "(" _ param_list _ ")" _ "=>" type
    constr_sig = "new" _ type_params? _ "(" _ param_list? _ ")" _ type_annotation?
        type_params = "<" _ type_param+ _ ">"

    index_sig = "[" _ ident _ ":" _ ("string" / "number") _ "]" _ type_annotation
    method_sig = prop_name "?"? call_sig

    type_query = "typeof" _ type_query_expr
    type_query_expr = ident ("." ident)*
    type_args = type_arg+
    type_arg = type _ (_ "," _)?
    
    type_param = ident _ (constraint)? (_ "," _)?
    type_annotation = ":" _ type
    constraint = "extends" type

    declaration_source_file = (declaration_element+)?
    declaration_element = export_assignment / ("export"? decls)
    decls = interface_decl / import_decl / external_import_decl / ambient_decl

    external_import_decl = "import" _ ident _ "=" _ external_module_ref _ ";"
    external_module_ref = "require" _ "(" _ str_lit _ ")"

    import_decl = "import" _ ident _ "=" _ entity_name _ ";"
        entity_name = ident (( "." ident)?)+
    export_assignment = "export" _ "=" ident _ ";"
    ambient_decl = "declare" _ ambient_decls
        ambient_decls = ambient_var_decl
                      / ambient_func_decl
                      / ambient_class_decl
                      / ambient_enum_decl
                      / ambient_module_decl
                      / ambient_extern_module_decl
        ambient_var_decl = "var" _ ident _ type_annotation? _ ";"
        ambient_func_decl = "function" _ ident _ call_sig _ ";"
        ambient_class_decl = "class" _ ident _ type_params? class_heritage "{" _ ambient_class_body _ "}"
            ambient_class_body = ambient_class_body_elem*
            ambient_class_body_elem = ambient_constr_decl / ambient_prop_member_decl / index_sig
                 ambient_constr_decl = "constructor" _ "(" _ param_list? _ ")" _ ";"
                 ambient_prop_member_decl = public_or_private? "static"? prop_name ((type_annotation?)
                                                                                   / call_sig) ";"
                 
       ambient_enum_decl = "enum" _ ident _ "{" _ ambient_enum_body _ "}"
           ambient_enum_body = ambient_enum_member_list _ ","?
           ambient_enum_member_list = ambient_enum_member (_ "," _ ambient_enum_member)*
           ambient_enum_member = prop_name (_ "=" _ num_lit)?
       ambient_module_decl = "module" _ ident_path _ "{" _ ambient_module_body _ "}"
           ambient_module_body = ambient_module_elem*
           _ambient_module_decl = ambient_var_decl
                                / ambient_func_decl
                                / ambient_class_decl
                                / interface_decl
                                / ambient_enum_decl
                                / ambient_module_decl
                                / import_decl

       ambient_module_elem = "export"? _ambient_module_decl
       ambient_extern_module_decl = "module" _ str_lit _ "{" ambient_extern_module_body "}"
           ambient_extern_module_body = ambient_extern_module_elem*
           ambient_extern_module_elem = ambient_module_elem
                                      / export_assignment
                                      / ("export"? external_import_decl)

    class_heritage = class_extends_clause? implements_clause?
        class_extends_clause = "extends" _ class_type
            class_type = type_ref
        implements_clause = "implements" _ class_or_interface_type_list
        class_or_interface_type_list = type_ref (_ ", " _ type_ref)*

    interface_decl = "interface" ident type_params? interface_extends_clause? obj_type
        interface_extends_clause = "extends" (_ type_ref _ (",")?)*

    ident_path = "ident" ("." ident)*

    sep = ":" / "=>"

    _ = ~r"\s*"
    __ = ~r"\s+"
    space = !(~r"[\n\r]") ~r"\s"
""", "declaration_source_file")


def parse(lines):
    TYPESCRIPT_GRAMMAR.parse(lines)

