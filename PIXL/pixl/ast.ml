(* Authors: Maxwell Hu and Marco Starger*)
(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or
type field = Red | Blue | Green | Alpha

type uop = Neg | Not | Increment | Decrement
type flip = Bar | Underscore

type typ = Int | Bool | Void | String | Pixel | Char | Matrix of typ

and expr =
    Literal of int
  | StringLit of string
  | BoolLit of bool
  | MatrixLit of expr list list
  | PixelLit of expr * expr * expr * expr
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Assignp of string * field * expr
  | Assignm of string * expr * expr * expr
  | Call of string * expr list
  | Access of string * field
  | Crop of string * expr * expr * expr * expr
  | HFlip of expr
  | VFlip of expr
  | Noexpr
  | MatrixAccess of string * expr * expr
  | Rows of string
  | Cols of string
  | EMatrix of expr * expr * typ

type bind = typ * string

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
  | Increment -> "++"
  | Decrement -> "--"

let string_of_field = function
     Red -> "R"
  |  Blue -> "B"
  | Green -> "G"
  | Alpha -> "A"

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | String -> "string"
  | Pixel -> "pixel"
  | Char -> "char"
  | Matrix(typ) -> string_of_typ typ ^ " matrix"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | MatrixLit(ll) -> "[" ^ String.concat "," (List.map string_of_expr (List.concat ll)) ^ "]"
  | PixelLit(v1,v2,v3,v4) -> "(" ^ string_of_expr v1 ^ "," ^ string_of_expr v2 ^ "," ^ string_of_expr v3 ^ "," ^ string_of_expr v4 ^ ")"
  | StringLit(s) -> s
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Access(id, field) -> id ^ "." ^ string_of_field field
  | Crop(v, e1, e2, e3, e4) -> v ^ "<" ^ string_of_expr e1 ^ ":" ^ string_of_expr e2 ^ ", " ^ string_of_expr e3 ^ ":" ^ string_of_expr e4 ^ ">"
  | Noexpr -> ""
  | MatrixAccess(v, e1, e2) -> v ^ "[" ^ string_of_expr e1 ^ "]" ^ "[" ^ string_of_expr e2 ^ "]"
  | Rows(id) -> id ^ ".rows"
  | Cols(id) -> id ^ ".cols"
  | Assignp(id, f, e1) -> id ^ "." ^ string_of_field f ^ "=" ^ string_of_expr e1
  | Assignm(id, e1, e2, value) -> id ^ "[" ^ string_of_expr e1 ^ "]" ^ "[" ^ string_of_expr e2 ^ "]" ^ "=" ^ string_of_expr value
  | EMatrix(e1,e2,tp) -> "matrix(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ "," ^ string_of_typ tp ^ ")"
  | HFlip(e) -> "hflip " ^ string_of_expr e
  | VFlip(e) -> "vflip " ^ string_of_expr e


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
  String.concat "\n" (List.map string_of_fdecl funcs)
