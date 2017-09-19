// Tango-light grammar for Lark

module          : (_NEWLINE | stmt)*

block          : "{" _stmt_end* stmt* "}"

fun_decl        : "function" NAME [generic_decls] "(" [param_decls] ")" ["->" type_ident] block

param_decls     : param_decl ("," param_decl)* [","]
param_decl      : NAME [NAME] [":" type_ident] ["=" expr]

generic_decls   : "<" NAME ("," NAME)* [","] ">"

prop_decl       : "let" NAME [":" type_ident] [_assign_op expr] [block]

struct_decl     : "struct" NAME [generic_decls] ["conforms" conformances] block

enum_decl       : "enum" NAME [generic_decls] ["conforms" conformances] block

case_decl       : "case" NAME ["(" param_decls ")"]

conformances    : NAME ("," NAME)*

?stmt           : stmt_ _stmt_end
?stmt_          : block
                | fun_decl
                | prop_decl
                | struct_decl
                | enum_decl
                | case_decl
                | expr _assign_op expr              -> assign_stmt
                | ("when" expr | "else") block      -> when_stmt
                | "return" [expr] ["from" NAME]     -> return_stmt
                | "continue" ["from" NAME]          -> continue_stmt
                | "break" ["from" NAME]             -> break_stmt
                | expr

_stmt_end       : _NEWLINE | ";"

?expr           : range_expr
?range_expr     : or_test [".." or_test]
?or_test        : and_test ("or" and_test)*
?and_test       : prefix_expr ("and" prefix_expr)*
?prefix_expr    : _prefix_op suffix_expr | suffix_expr
?suffix_expr    : eq_expr _suffix_op | eq_expr
?eq_expr        : cast_expr (_eq_op   cast_expr)*
?cast_expr      : cmp_expr  (_cast_op cmp_expr)*
?cmp_expr       : shf_expr  (_cmp_op  shf_expr)*
?shf_expr       : add_expr  (_shf_op  add_expr)*
?add_expr       : mul_expr  (_add_op  mul_expr)*
?mul_expr       : atom_expr (_mul_op  atom_expr)*

!_assign_op     : "="   | "&-"  | "<-"  | "?-"
!_prefix_op     : "+"   | "-"   | "not"
!_suffix_op     : "!"   | "?"
!_eq_op         : "=="  | "!="  | "~="  | "===" | "is"
!_cast_op       : "as?" | "as!"
!_cmp_op        : "<"   | "<="  | ">="  | ">"
!_shf_op        : "<<"  | ">>"
!_add_op        : "+"   | "-"
!_mul_op        : "*"   | "/"   | "%"

?atom_expr      : atom_expr "(" [arguments] ")"     -> call_expr
                | atom_expr "[" [arguments] "]"     -> subscript_expr
                | atom_expr "." NAME                -> select_expr
                | "." atom_expr                     -> select_expr
                | "(" [arguments] ")"               -> tuple_literal
                | "[" [array_items] "]"             -> array_literal
                | "[" map_items "]"                 -> map_literal
                | "[" set_items "]"                 -> set_literal
                | if_expr
                | switch_expr
                | for_loop
                | while_loop
                | closure
                | ident
                | literal

arguments       : argval | argument ("," argument)* [","]
argument        : NAME _assign_op argval
?argval         : "let" NAME [":" type_modifier+]   -> value_binding
                | expr

array_items     : expr ("," expr)* [","]

map_items       : map_item ("," map_item)* [","]
map_item        : expr ":" expr

set_items       : set_item ("," set_item)* [","]
set_item        : expr ":"

if_expr         : "if" expr block ["else" (if_expr | block)]

switch_expr     : "switch" expr block

for_loop        : "for" [":" NAME] expr "in" expr block

while_loop      : "while" [":" NAME] expr block

closure         : "function" "(" [param_decls] ")" ["->" type_ident] block

type_ident      : type_modifier* type_sign | type_modifier+
type_modifier   : "@" (CST | MUT | STK | SHD | VAL | REF | OWN | WEAK)
?type_sign      : ident
                | fun_sign
                | tuple_sign
                | "(" type_sign ")"

fun_sign        : "(" [sign_params] ")" "->" type_ident

tuple_sign      : "struct" "{" sign_params "}"

sign_params     : sign_param ("," sign_param)* [","]
sign_param      : NAME ":" type_ident

ident           : NAME [specializations]

specializations : "<" (type_sign | specialization ("," specialization)* [","]) ">"
specialization  : NAME "=" type_sign

?literal        : NUMBER                            -> int_literal
                | STRING                            -> string_literal

CST             : "cst"
MUT             : "mut"
STK             : "stk"
SHD             : "shd"
VAL             : "val"
REF             : "ref"
OWN             : "own"
WEAK            : "weak"

NAME            : /[a-zA-Z_]\w*/
NUMBER          : /(0|([1-9][0-9]*))(\.[0-9]+)?([Ee][+-]?[0-9]+)?/
STRING          : /'[^']*'/

COMMENT         : /\#[^\n]*/
_NEWLINE        : (/\r?\n[\t ]*/ | COMMENT)+

%ignore /[\t \f]+/              // Ignore whitespaces
%ignore /\\\\[\t \f]*\r?\n/     // Ignore line continuation
%ignore COMMENT
