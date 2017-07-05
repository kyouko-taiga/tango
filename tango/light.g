// Tango-light grammar for Lark

module         : stmt_list

!block         : "{" stmt_list "}"

stmt_list      : _stmt*

_stmt          : _simple_stmt

_simple_stmt   : prop_decl
               | fun_decl
               | assign_stmt
               | if_stmt
               | return_stmt
               | call_expr

prop_decl      : VAR NAME (":" type_ident)? (assign_op _expr)?

fun_decl       : FUN NAME "(" param_decls? ")" "->" type_ident block

param_decls    : param_decl ("," param_decl)*

param_decl     : NAME ":" type_ident

assign_stmt    : ident assign_op _expr

if_stmt        : IF _expr block

return_stmt    : RETURN _expr

_expr          : or_test

?or_test       : and_test    (lor_op  and_test)*    -> binary_expr
?and_test      : eq_expr     (land_op eq_expr)*     -> binary_expr
?eq_expr       : cmp_expr    (eq_op   cmp_expr)*    -> binary_expr
?cmp_expr      : add_expr    (cmp_op  add_expr)*    -> binary_expr
?add_expr      : mul_expr    (add_op  mul_expr)*    -> binary_expr
?mul_expr      : prefix_expr (mul_op  prefix_expr)* -> binary_expr

prefix_expr    : prefix_op? _primary

!lor_op        : "or"
!land_op       : "and"
!eq_op         : "==" | "!="
!cmp_op        : "<"  | "<=" | ">=" | ">"
!add_op        : "+"  | "-"
!mul_op        : "*"  | "/"  | "%"
!prefix_op     : "+"  | "-"
!assign_op     : "=" | "&-" | "<-"

_primary       : call_expr
               | ident
               | literal
               | "(" _expr ")"

!call_expr     : _expr "(" call_arg ")"

call_arg       : NAME assign_op _expr

!fun_sign      : "(" sign_param ")" "->" type_ident

sign_param     : NAME ":" type_ident

type_ident     : type_modifier* _type_sign

type_modifier  : "@" (CST | MUT | STK | SHD | VAL | REF | OWN)

_type_sign     : ident | fun_sign | "(" _type_sign ")"

ident          : NAME

literal        : NUMBER
               | STRING

VAR            : "var"
CST            : "cst"
MUT            : "mut"
STK            : "stk"
SHD            : "shd"
VAL            : "val"
REF            : "ref"
OWN            : "own"
FUN            : "fun"
IF             : "if"
RETURN         : "return"

COMMENT        : /\#[^\n]*/
_NEWLINE       : ( /\r?\n[\t ]*/ | COMMENT )+

NAME           : /[^\W\d][\w]*/
NUMBER         : /(0|([1-9][0-9]*))(\.[0-9]+)?([Ee][+-]?[0-9]+)?/
STRING         : /'[^']*'/

%ignore /[\t \f]+/
%ignore COMMENT
%ignore _NEWLINE
