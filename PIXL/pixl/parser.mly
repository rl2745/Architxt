/* Authors: Maxwell Hu and Jacob Gold */
/* Ocamlyacc parser for Pixl */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT INCREMENT DECREMENT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL VOID STRING
%token LBRAC RBRAC COLON CHAR LANGLE RANGLE BAR TILDA
%token EXP PIXEL DOT ROWS COLS RED BLUE GREEN ALPHA MAT
%token <int> LITERAL
%token <string> ID
%token <string> STR_LIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc RED GREEN BLUE ALPHA
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG TILDA BAR

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT                             { Int }
  | BOOL                            { Bool }
  | VOID                            { Void }
  | STRING                          { String }
  | PIXEL                           { Pixel }
  | typ MAT                         { Matrix($1) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                                               { Expr $1 }
  | RETURN SEMI                                             { Return Noexpr }
  | RETURN expr SEMI                                        { Return $2 }
  | LBRACE stmt_list RBRACE                                 { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE                 { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt                    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt                           { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL                                                { Literal($1) }
  | TRUE                                                   { BoolLit(true) }
  | FALSE                                                  { BoolLit(false) }
  | ID                                                     { Id($1) }
  | STR_LIT                                                { StringLit($1) }
  | expr PLUS   expr                                       { Binop($1, Add,   $3) }
  | expr MINUS  expr                                       { Binop($1, Sub,   $3) }
  | expr TIMES  expr                                       { Binop($1, Mult,  $3) }
  | expr DIVIDE expr                                       { Binop($1, Div,   $3) }
  | expr EQ     expr                                       { Binop($1, Equal, $3) }
  | expr NEQ    expr                                       { Binop($1, Neq,   $3) }
  | expr LT     expr                                       { Binop($1, Less,  $3) }
  | expr LEQ    expr                                       { Binop($1, Leq,   $3) }
  | expr GT     expr                                       { Binop($1, Greater, $3) }
  | expr GEQ    expr                                       { Binop($1, Geq,   $3) }
  | expr AND    expr                                       { Binop($1, And,   $3) }
  | expr OR     expr                                       { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG                                   { Unop(Neg, $2) }
  | NOT expr                                               { Unop(Not, $2) }
  | ID ASSIGN expr                                         { Assign($1, $3) }
  | ID LBRAC expr RBRAC LBRAC expr RBRAC ASSIGN expr       { Assignm($1, $3, $6, $9) }
  | ID DOT RED ASSIGN expr                                 { Assignp($1, Red, $5) }
  | ID DOT GREEN ASSIGN expr                               { Assignp($1, Green, $5) }
  | ID DOT BLUE ASSIGN expr                                { Assignp($1, Blue, $5) }
  | ID DOT ALPHA ASSIGN expr                               { Assignp($1, Alpha , $5) }
  | ID LPAREN actuals_opt RPAREN                           { Call($1, $3) }
  | LPAREN expr RPAREN                                     { $2 }
  | LBRAC mat_lit RBRAC                                    { MatrixLit(List.rev($2)) }
  | pixel_lit                                              { $1 }
  | ID DOT RED                                             { Access($1, Red) }
  | ID DOT GREEN                                           { Access($1, Green) }
  | ID DOT BLUE                                            { Access($1, Blue) }
  | ID DOT ALPHA                                           { Access($1, Alpha) }
  | ID LBRAC expr RBRAC LBRAC expr RBRAC                   { MatrixAccess($1, $3, $6)}
  | ID LANGLE expr COLON expr COMMA expr COLON expr RANGLE { Crop($1, $3, $5, $7, $9) }
  | ID DOT ROWS                                            { Rows($1) }
  | ID DOT COLS                                            { Cols($1) }
  | BAR expr                                               { VFlip($2) }
  | TILDA expr                                             { HFlip($2) }
  | expr PLUS PLUS                                         { Unop(Increment, $1) }
  | expr MINUS MINUS                                       { Unop(Decrement, $1) }
  | MAT LPAREN expr COMMA expr COMMA typ RPAREN            { EMatrix($3, $5, $7) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

mat_lit:
    row_lit                        { [(List.rev $1)] }
  | mat_lit SEMI row_lit         { (List.rev $3) :: $1 }

row_lit:
    expr                             { [$1] }
  | row_lit COMMA expr            { $3 :: $1 }

pixel_lit:
    LPAREN expr COMMA expr COMMA expr COMMA expr RPAREN { PixelLit($2, $4, $6, $8) }
