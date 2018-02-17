(* Authors: Jacob Gold and Shiv Sakhuja *)
open Ast

type sexpr =
  SLiteral of int * typ
  | SStringLit of string * typ
  | SBoolLit of bool * typ
  | SMatrixLit of sexpr list list * typ
  | SPixelLit of sexpr * sexpr * sexpr * sexpr * typ
  | SId of string * typ
  | SBinop of sexpr * op * sexpr * typ
  | SUnop of uop * sexpr * typ
  | SAssign of string * sexpr * typ
  | SAssignp of string * field * sexpr * typ
  | SAssignm of string * sexpr * sexpr * sexpr * typ
  | SCall of string * sexpr list * typ
  | SAccess of string * field * typ
  | SMatrixAccess of string * sexpr * sexpr * typ
  | SNoexpr
  | SRows of string
  | SCols of string
  | SEMatrix of sexpr * sexpr * typ

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr * typ
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
