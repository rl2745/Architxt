(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SBoolLit of bool
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SNoexpr
  | SStringLit of string
  | SPointLit of sexpr * sexpr
  | SArrayInit of typ * sexpr
  | SArrayDelete of string
  | SArrayAssign of string * sexpr * sexpr
  | SArrayAccess of string * sexpr
  | SMapInit of sexpr * sexpr
  | SPointAssign of string * ptyp * sexpr
  | SPointAccess of string * ptyp
  | SMapAccess of string * sexpr * sexpr
  | SMapAssign of string * sexpr * sexpr * sexpr
  (*add String, Point, ArrayInit, Delete, Access, Assign, MapInit*)

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    slocals : bind list;
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SFliteral(l) -> l
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SStringLit(s) -> s
  | SPointLit(s, c) -> "( "^ string_of_sexpr s ^ "," ^ string_of_sexpr c ^ " )" 
  | SArrayAccess(s, e) -> s ^ "[" ^ string_of_sexpr e ^ "]"
  | SArrayAssign(s, e1, e2) ->
      s ^ "[" ^string_of_sexpr e1 ^"] ="^ string_of_sexpr e2
  | SArrayInit(typ, len) -> string_of_typ typ ^ "[" ^ string_of_sexpr len ^ "]"
  | SArrayDelete(s) -> "delete " ^ s
  | SMapInit(e1, e2) -> "Map" ^ "[" ^ string_of_sexpr e1 ^ "," ^ string_of_sexpr e2 ^ "]"       
        (* add string, point, arrayaccess, assign, init, delete, mapinit*)  
  | SPointAssign(id, t, e) -> id ^ "." ^ string_of_ptyp t ^ "=" ^ string_of_sexpr e
  | SPointAccess(id, t) -> id ^ "." ^ string_of_ptyp t  
  | SMapAccess(id, e1, e2) -> id ^ "[" ^ string_of_sexpr e1 ^ "]" ^ "[" ^ string_of_sexpr e2 ^ "]"
  | SMapAssign(id, e1, e2, e3) -> id ^ "[" ^ string_of_sexpr e1 ^ "]" ^ "[" ^ string_of_sexpr e2 ^ "] =" ^ string_of_sexpr e3 
  | SNoexpr -> ""
				  ) ^ ")"
  
let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
