(* AST based on class MicroC example *)
(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or
(* point types = surface, name *)
type ptyp = Surface | Name

type uop = Neg | Not 

type typ = Int | Bool | Float | Void | String | Char | Point | Map | ArrayType of typ (*added String, char, point; need to add map*)

type bind = typ * string

type expr =
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr
  | StringLit of string
  | PointLit of expr * expr
  | ArrayInit of typ * expr
  | ArrayDelete of string
  | ArrayAssign of string * expr * expr
  | ArrayAccess of string * expr
  | MapInit of expr * expr
  | PointAssign of string * ptyp * expr
  | PointAccess of string * ptyp
  | MapAccess of string * expr * expr
  | MapAssign of string * expr * expr * expr
  (* added String, point; need to add map, assign map, assign point, access...*)

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | String -> "string"
  | Char -> "char"
  | Point -> "point"
  | ArrayType(t) -> "ArrayType:" ^ string_of_typ t
  | Map -> "map"
  (*added String, char, and point; need to add map*)

let string_of_ptyp = function
    Surface -> "surface"
  | Name -> "name"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | StringLit(s) -> s
  | PointLit(s, c) -> "( "^ string_of_expr s ^ "," ^ string_of_expr c ^ " )" 
  | ArrayAccess(s, e) -> s ^ "[" ^ string_of_expr e ^ "]"
  | ArrayAssign(s, e1, e2) ->
      s ^ "[" ^string_of_expr e1 ^"] ="^ string_of_expr e2
  | ArrayInit(typ, len) -> string_of_typ typ ^ "[" ^ string_of_expr len ^ "]"
  | ArrayDelete(s) -> "delete " ^ s
  | MapInit(e1, e2) -> "Map" ^ "[" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ "]"
  | PointAssign(id, t, e) -> id ^ "." ^ string_of_ptyp t ^ "=" ^ string_of_expr e
  | PointAccess(id, t) -> id ^ "." ^ string_of_ptyp t
  | MapAccess(id, e1, e2) -> id ^ "[" ^ string_of_expr e1 ^ "]" ^ "[" ^ string_of_expr e2 ^ "]"
  | MapAssign(id, e1, e2, e3) -> id ^ "[" ^ string_of_expr e1 ^ "]" ^ "[" ^ string_of_expr e2 ^ "] =" ^ string_of_expr e3
  (* could change how its represented, just used this for now*)
  (*added String and point; need to add map*)

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^ "OKAY"
