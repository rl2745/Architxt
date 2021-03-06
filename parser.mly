/* Ocamlyacc parser for Architxt */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA PLUS MINUS TIMES DIVIDE ASSIGN DELETE 
%token NOT INCR DECR EQ NEQ LT LEQ GT GEQ AND OR /* added incr and decr*/
%token RETURN IF ELSE FOR WHILE INT BOOL FLOAT VOID NEW
%token STRING PERIOD CHAR POINT ARRAY MAP SURFACE NAME /*added String and period, char, point*/
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT
%token <string> STRLIT /*added ID and STRLIT*/
%token EOF
/*need to add map*/

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc SURFACE NAME
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG


%%

program:
  decls EOF { let (fst, snd) = $1 in (List.rev fst, List.rev snd) }

decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

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
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

primitive:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | VOID  { Void  }
  | STRING { String }
  | POINT { Point }
  | MAP  { Map }
  /*added String and point; need to add map*/

typ:
  primitive { $1 }
  | primitive LBRACKET RBRACKET { ArrayType($1) }





vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1)            }
  | FLIT	     { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr %prec NEG { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }
  | STRLIT            { StringLit($1) }
  | point_lit { $1 }
  | NEW primitive LBRACKET expr RBRACKET  { ArrayInit($2, $4) }
  | DELETE ID                             { ArrayDelete($2) }
  | ID LBRACKET expr RBRACKET ASSIGN expr { ArrayAssign($1, $3, $6) }
  | ID LBRACKET expr RBRACKET             { ArrayAccess($1, $3) }
  | NEW primitive LBRACKET expr COMMA expr RBRACKET { MapInit($4, $6)}
  /*| map_init { $1 }*/
  | ID PERIOD SURFACE ASSIGN expr            { PointAssign($1, Surface, $5)}
  | ID PERIOD SURFACE                        { PointAccess($1, Surface)}
  | ID PERIOD NAME ASSIGN expr               { PointAssign($1, Name, $5)}
  | ID PERIOD NAME                           { PointAccess($1, Name)}
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET     { MapAccess($1, $3, $6)}
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET ASSIGN expr    { MapAssign($1, $3, $6, $9)}
  /*added string and point, need to add map*/

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }


point_lit:
    LPAREN expr COMMA expr RPAREN    { PointLit($2, $4)}

/*map_init:
    NEW primitive LBRACKET expr COMMA expr RBRACKET { MapInit($2, $4)}*/

  /*need to add point and map specific stuff*/
