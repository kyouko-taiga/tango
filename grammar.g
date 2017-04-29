start: module_decl;

module_decl             : stmt_list;

@stmt_list              : NEWLINE* ( stmt NEWLINE )* NEWLINE*;

@stmt                   : block
                        | computed_prop_decl
                        | stored_prop_decl
                        | fun_decl
                        | struct_decl
                        | enum_decl
                        | protocol_decl
                        | extension_decl
                        | assignment
                        | self_assignment
                        | for_loop
                        | while_loop
                        | return_statement
                        | break_statement
                        | continue_statement
                        | call_expr
                        | if_expr
                        | switch_expr;

block                   : LBRACE NEWLINE* ( stmt NEWLINE )* RBRACE;

computed_prop_decl      : attribute*
                          mutability_modifier NAME
                          type_annotation?
                          LBRACE fun_decl fun_decl? RBRACE;

stored_prop_decl        : attribute*
                          mutability_modifier NAME
                          type_annotation?
                          initializer?;

mutability_modifier     : CST | MUT | SHD;

type_annotation         : COLON attribute* expr;

initializer             : EQ expr;

fun_decl                : attribute*
                          FUN ( NAME | operator )
                          generic_clause?
                          LPAR fun_decl_param_list? RPAR
                          (TO expr)?
                          where_clause?
                          block;

generic_clause          : LESSER NAME ( COMMA NAME )* GREATER;

where_clause            : WHERE expr;

@fun_decl_param_list    : fun_decl_param ( COMMA fun_decl_param )*;

fun_decl_param          : mutability_modifier NAME NAME?
                          type_annotation
                          initializer?;

attribute               : '@override' | '@mut' | '@shd';

struct_decl             : attribute*
                          STRUCT NAME
                          generic_clause?
                          conformance_clause?
                          import_clause?
                          where_clause?
                          struct_block;

conformance_clause      : COLON expr ( COMMA expr )*;

import_clause           : IMPORT expr ( COMMA expr )*;

struct_block            : LBRACE NEWLINE* ( struct_member NEWLINE )* RBRACE;

@struct_member          : struct_decl
                        | enum_decl
                        | protocol_decl
                        | fun_decl
                        | computed_prop_decl
                        | stored_prop_decl
                        | abstract_type_decl;

enum_decl               : attribute*
                          ENUM NAME
                          generic_clause?
                          conformance_clause?
                          import_clause?
                          where_clause?
                          enum_block;

enum_block              : LBRACE NEWLINE* ( enum_member NEWLINE )* RBRACE;

enum_member             : struct_decl
                        | enum_decl
                        | protocol_decl
                        | enum_case_decl
                        | fun_decl
                        | computed_prop_decl
                        | stored_prop_decl
                        | abstract_type_decl;

enum_case_decl          : attribute*
                          CASE NAME
                          ( LPAR enum_case_param ( COMMA enum_case_param )* RPAR )?;

enum_case_param         : NAME COLON expr;

protocol_decl           : attribute*
                          PROTOCOL NAME
                          conformance_clause?
                          import_clause?
                          protocol_block;

protocol_block          : LBRACE NEWLINE* ( protocol_member NEWLINE )* RBRACE;

protocol_member         : fun_decl
                        | fun_requirement_decl
                        | computed_prop_decl
                        | prop_requirement_decl
                        | abstract_type_decl;

fun_requirement_decl    : attribute*
                          FUN ( NAME | operator )
                          generic_clause?
                          LPAR fun_decl_param_list? RPAR
                          (TO expr)?
                          where_clause?;

prop_requirement_decl   : attribute*
                          mutability_modifier NAME
                          type_annotation?;

abstract_type_decl      : attribute*
                          ABS NAME
                          conformance_clause?
                          initializer?;

extension_decl          : EXTENSION expr
                          where_clause?
                          TO ( struct_decl | enum_decl );

assignment              : pattern EQ pattern;

self_assignment         : expr self_assignment_op expr;

self_assignment_op      : PIPEEQ
                        | AMPEQ
                        | CIRCUMFLEXEQ
                        | RSHIFTEQ
                        | LSHIFTEQ
                        | PLUSEQ
                        | MINUSEQ
                        | STAREQ
                        | SLASHEQ
                        | PERCENTEQ;

for_loop                : ( NAME EQ )? FOR pattern IN expr block;

while_loop              : ( NAME EQ )? WHILE pattern block;

return_statement        : RETURN expr;

break_statement         : BREAK NAME?;

continue_statement      : CONTINUE NAME?;

@expr                   : binary_expr;

?binary_expr            : or_test;

?or_test                 : and_test    ( OR         and_test    )*;
?and_test                : or_expr     ( AND        or_expr     )*;
?or_expr                 : xor_expr    ( PIPE       xor_expr    )*;
?xor_expr                : and_expr    ( CIRCUMFLEX and_expr    )*;
?and_expr                : eq_expr     ( AMP        eq_expr     )*;
?eq_expr                 : cmp_expr    ( eq_op      cmp_expr    )*;
?cmp_expr                : cast_expr   ( cmp_op     cast_expr   )*;
?cast_expr               : add_expr    ( cast_op    add_expr    )*;
?add_expr                : mul_expr    ( add_op     mul_expr    )*;
?mul_expr                : shft_expr   ( mul_op     shft_expr   )*;
?shft_expr               : prefix_expr ( shft_op    prefix_expr )*;

eq_op                   : IS | EQEQ | EQEQEQ | NOTEQ | TILDEEQ;
cmp_op                  : LESSEREQ | LESSER | GREATEREQ | GREATER;
cast_op                 : (AS INTERROGATION) | (AS EXCLAMATION);
add_op                  : PLUS | MINUS;
mul_op                  : STAR | SLASH | PERCENT;
shft_op                 : RSHIFT | LSHIFT;

?prefix_expr            : prefix_op? postfix_expr;

prefix_op               : NOT | PLUS | MINUS | TILDE;

?postfix_expr           : primary_expr postfix_op?;

postfix_op              : INTERROGATION | EXCLAMATION;

@primary_expr           : call_expr
                        | subscript_expr
                        | select_expr
                        | closure_expr
                        | if_expr
                        | switch_expr
                        | type_signature
                        | identifier
                        | literal
                        | LPAR expr RPAR;

call_expr               : expr LPAR call_arg_list? RPAR;

@call_arg_list          : call_arg ( COMMA call_arg )*;

call_arg                : ( NAME EQ )? attribute* expr;

subscript_expr          : expr LSQB call_arg_list? RSQB;

select_expr             : expr DOT identifier;

closure_expr            : LBRACE ( closure_param_list IN )? NEWLINE* stmt_list;

@closure_param_list     : value_binding_pattern ( COMMA value_binding_pattern )*;

if_expr                 : IF pattern block else_clause?;

else_clause             : ( ELSE if_expr ) | ( ELSE block );

switch_expr             : SWITCH expr switch_block;

switch_block            : LBRACE NEWLINE* ( switch_clause NEWLINE )* NEWLINE* RBRACE;

switch_clause           : CASE pattern block;

@type_signature         : function_signature | tuple_signature;

function_signature      : LPAR signature_param_list? RPAR
                          TO expr;

@signature_param_list   : signature_param ( COMMA signature_param )*;

signature_param         : mutability_modifier NAME COLON attribute* expr;

tuple_signature         : LPAR signature_param_list? RPAR;

identifier              : ( NAME | operator ) ( LESSER specialization_list GREATER )?;

operator                : LESSER | LESSEREQ | EQEQ | TILDEEQ | EQEQEQ | NOTEQ | GREATEREQ | GREATER
                        | PIPEEQ | AMPEQ | CIRCUMFLEXEQ | RSHIFTEQ | LSHIFTEQ | PLUSEQ | MINUSEQ
                        | STAREQ | SLASHEQ | PERCENTEQ | PIPE | AMP | CIRCUMFLEX | TILDE | RSHIFT
                        | LSHIFT | PLUS | MINUS | STAR | SLASH | PERCENT;

@specialization_list    : specialization ( COMMA specialization )*;

specialization          : NAME EQ expr;

@literal                : number_literal | string_literal | dict_literal | array_literal;

number_literal          : NUMBER;
string_literal          : STRING;
array_literal           : LSQB ( expr ( COMMA expr )* )? RSQB;
dict_literal            : LSQB ( dict_literal_item_list | COLON ) RSQB;

@dict_literal_item_list : dict_literal_item ( COMMA dict_literal_item )*;

dict_literal_item       : expr COLON expr;

@pattern                : (value_binding_pattern | tuple_pattern | enum_case_pattern | expr)
                          where_clause?;

value_binding_pattern   : mutability_modifier NAME
                          type_annotation?;

tuple_pattern           : LPAR pattern_arg_list RPAR;

@pattern_arg_list       : ( call_arg COMMA )? fresh_arg ( COMMA ( call_arg | fresh_arg ) )*;

fresh_arg               : ( NAME EQ )? attribute* value_binding_pattern;

enum_case_pattern       : expr ( LPAR pattern_arg_list RPAR )?;

NUMBER                  : '(0|([1-9][0-9]*))(\.[0-9]+)?([Ee][+-]?[0-9]+)?';

COMMENT                 : '#.*\n' (%ignore);
WHITESPACE              : '[ \t\v]+' (%ignore);
NEWLINE                 : '[\r\n]+' (%newline);
STRING                  : '\'[^\']*\'';
NAME                    : '[^\W\d][\w]*'
                        (%unless
                            // Mutability modifiers.
                            CST       : 'cst';
                            MUT       : 'mut';
                            SHD       : 'shd';

                            // Declaration keywords.
                            FUN       : 'fun';
                            ABS       : 'abs';
                            STRUCT    : 'struct';
                            ENUM      : 'struct';
                            CASE      : 'case';
                            PROTOCOL  : 'protocol';
                            EXTENSION : 'extension';

                            // Operators.
                            OR        : 'or';
                            AND       : 'and';
                            NOT       : 'not';
                            IN        : 'in';
                            IS        : 'is';
                            AS        : 'as';

                            // Control structures.
                            IF        : 'if';
                            ELSE      : 'else';
                            SWITCH    : 'switch';
                            FOR       : 'for';
                            WHILE     : 'while';
                            RETURN    : 'return';
                            BREAK     : 'break';
                            CONTINUE  : 'continue';

                            // Other keywords.
                            WHERE     : 'where';
                            IMPORT    : 'import';
                        );

LBRACE                  : '{';
RBRACE                  : '}';
LSQB                    : '\[';
RSQB                    : '\]';
LPAR                    : '\(';
RPAR                    : '\)';
COLON                   : ':';
COMMA                   : ',';
DOT                     : '.';

EQ                      : '=';

LESSER                  : '<';
LESSEREQ                : '<=';
EQEQ                    : '==';
TILDEEQ                 : '~=';
EQEQEQ                  : '===';
NOTEQ                   : '!=';
GREATEREQ               : '>=';
GREATER                 : '>';

PIPEEQ                  : '\|=';
AMPEQ                   : '&=';
CIRCUMFLEXEQ            : '\^=';
RSHIFTEQ                : '>>=';
LSHIFTEQ                : '<<=';
PLUSEQ                  : '\+=';
MINUSEQ                 : '\-=';
STAREQ                  : '\*=';
SLASHEQ                 : '\/=';
PERCENTEQ               : '%=';

PIPE                    : '\|';
AMP                     : '&';
CIRCUMFLEX              : '\^';
TILDE                   : '~';
RSHIFT                  : '>>';
LSHIFT                  : '<<';
PLUS                    : '\+';
MINUS                   : '\-';
STAR                    : '\*';
SLASH                   : '\/';
PERCENT                 : '%';
EXCLAMATION             : '!';
INTERROGATION           : '\?';

TO                      : '->';
