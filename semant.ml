(*semantic checking for architxt compiler based on microc compiler
returns SAST if successful, throws an exception if something goes wrong
implemented without variable declarations wherever*)

open Ast
open Sast

module StringMap = Map.Make(String)

let check (globals, functions) = 
	(*check binds used for globals, formals, and locals as well*)
	let check_binds (kind: string) (to_check: bind list) = 
		let check_it checked binding = 
			let void_err = "illegal void " ^kind ^ " " ^snd binding
			and dup_err = "duplicate " ^ kind ^ " " ^snd binding
			in match binding with
        		(* No void bindings *)
        		(Void, _) -> raise (Failure void_err)
      		| (_, n1) -> match checked with
                    (* No duplicate bindings *)
                      ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
                    | _ -> binding :: checked
    in let _ = List.fold_left check_it [] (List.sort compare to_check) 
       in to_check
  in 

  (**** Checking Global Variables ****)

  let globals' = check_binds "global" globals in

  (* Collect function declarations for built-in functions: no bodies 
  print (for string and int), fill, propagate, display*)
  let built_in_decls = 
  	StringMap.add "print_i" { typ = Void; fname = "print_i"; formals = [(Int, "x")];
      locals = []; body = [] } 
    ( StringMap.add "print" { typ = Void; fname = "print"; formals = [(String, "x")];
      locals = []; body = [] }
    ( StringMap.add "fill" { typ = Void; fname = "fill"; formals = [(Int, "x"); (Int, "y"); (Point, "p")];
      locals = []; body = [] }
    ( StringMap.add "propagate" { typ = Void; fname = "propagate"; formals = [(Point, "p")];
      locals = []; body = [] }
    ( StringMap.add "display" { typ = Void; fname = "display"; formals = [(Map, "m")];
      locals = []; body = [] }))))
  in

  (*add_func checks to make sure things aren't built in and things aren't duplicated*)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in
  (*add function name to symbol table*)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  (*checkbinds on formals and locals to make sure formals or locals are not void/dup*)
  let check_function func =
    let formals' = check_binds "formal" func.formals in
    let locals' = check_binds "local" func.locals in
  (*makes check to make sure type assignments are correct*)
  let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   
  (*build local symbol table of variables for function*)
  let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty (globals' @ formals' @ locals' )
    in
  (*return local variable from local symbol table*)
  let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
  (*return semantically checked expression with a type -- NEED TO ADD VARIABLE DECLARATION WHEREVER*)
      let rec expr = function
        Literal  l -> (Int, SLiteral l)
      | Fliteral l -> (Float, SFliteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
        Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in
  (*return semantically checked statement with sexprs*)
    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
    SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') 
        else raise (
    Failure ("return gives " ^ string_of_typ t ^ " expected " ^
       string_of_typ func.typ ^ " in " ^ string_of_expr e))
      
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      slocals  = locals';
      sbody = match check_stmt (Block func.body) with
  SBlock(sl) -> sl
      | _ -> let err = "internal error: block didn't become a block?"
      in raise (Failure err)
    }
  in (globals', List.map check_function functions)