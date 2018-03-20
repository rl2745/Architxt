(*semantic checking for architxt compiler based on microc compiler
returns SAST if successful, throws an exception if something goes wrong*)

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
      locals = []; body = [] })
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
  (*build local symbol table of variables for function*)
  (*return local variable from local symbol table*)
  (*return semantically checked expression with a type *)
  (*return semantically checked statement with sexprs*)
