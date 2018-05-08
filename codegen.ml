(*codegen*)

(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(* We'll refer to Llvm and Ast constructs with module names *)
module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (globals, functions) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let the_module = L.create_module context "Architxt"
  and i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context 
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
(*   and pointer_t    = L.pointer_type context *)
  and void_t     = L.void_type   context in
  let pointer_t = L.pointer_type in
  let str_t      = L.pointer_type i8_t in

  let point_st    = L.named_struct_type context "point_t" 
    in let _ = L.struct_set_body point_st [|i1_t; str_t|] true in
    let point_t = L.pointer_type point_st in

  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  (* Convert Architxt types to LLVM types *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Void  -> void_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.String -> str_t
    | A.Point -> point_t
    | A.ArrayType(t) -> pointer_t (ltype_of_typ t)
    | t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " not implemented yet"))
  in

  (* Declare each global variable; remember its value in a map *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.ArrayType(_) -> L.const_pointer_null (ltype_of_typ t)
        | A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare a "printf" function to implement Architxt's "print". *)
  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| str_t |] in
  let printf_func : L.llvalue = 
     L.declare_function "printf" printf_t the_module in 

  let to_imp str = raise (Failure ("Not yet implemented: " ^ str)) in

(* Define each function (arguments and return type) so we can 
   * define it's body and call it later *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        let () = L.set_value_name n p in
  let local = L.build_alloca (ltype_of_typ t) n builder in
        let _  = L.build_store p local builder in
  StringMap.add n local m 
      in

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      let add_local m (t, n) =
  let local_var = L.build_alloca (ltype_of_typ t) n builder
  in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals 
    in

    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    let get_array_element name i builder =
      let arr = L.build_load (lookup name) "" builder in
      let ptr = L.build_gep arr [|i|] "" builder in
      L.build_load ptr "" builder

    in

    let set_array_element name i v builder =
      let arr = L.build_load (lookup name) "" builder in
      let ptr = L.build_gep arr [| i |] "" builder in
      L.build_store v ptr builder

    in

    let init_array typ len builder = 
      L.build_array_malloc (ltype_of_typ typ) len "" builder
    in
    
    (* Generate LLVM code for a call to Architxt's "print" *)
    let rec expr builder ((_, e) : sexpr) = match e with
	      SLiteral i -> L.const_int i32_t i (* Generate a constant integer *)
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l -> L.const_float_of_string float_t l
      | SNoexpr -> L.const_int i32_t 0
      | SId s -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = expr builder e in
            let _  = L.build_store e' (lookup s) builder in e'
      | SStringLit s -> L.build_global_stringptr(s) "str" builder
      | SArrayAccess(s, e) -> get_array_element s (expr builder e) builder
      | SArrayInit(typ, e) -> let len = (expr builder e) in init_array typ len builder
      | SArrayDelete(s) -> L.build_free (L.build_load (lookup s) "" builder) builder
      | SArrayAssign(s, lhs, rhs) -> set_array_element s (expr builder lhs) (expr builder rhs) builder
      | SBinop (e1, op, e2) ->
          let (t, _) = e1
          and e1' = expr builder e1
          and e2' = expr builder e2 in
          if t = A.Float then (match op with 
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mult    -> L.build_fmul
          | A.Div     -> L.build_fdiv 
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | A.And | A.Or ->
              raise (Failure "internal error: semant should have rejected and/or on float")
          ) e1' e2' "tmp" builder 
          else (match op with
          | A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          ) e1' e2' "tmp" builder
      | SUnop(op, e) -> let (t, _) = e in
          let e' = expr builder e in (match op with
                A.Neg when t = A.Float -> L.build_fneg e' "tmp" builder
              | A.Neg                  -> L.build_neg e' "tmp" builder
              | A.Not                  -> L.build_not e' "tmp" builder)
      | SCall ("print", [e]) ->
         L.build_call printf_func [| str_format_str ; (expr builder e) |]
             "printf" builder
      | SCall ("print_i", [e]) -> (* Generate a call instruction *)
	       L.build_call printf_func [| int_format_str ; (expr builder e) |]
	           "printf" builder 
      | SCall ("print_f", [e]) -> (* Generate a call instruction *)
         L.build_call printf_func [| float_format_str ; (expr builder e) |]
             "printf" builder 
      | SCall (f, act) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (expr builder) (List.rev act)) in
          let result = (match fdecl.styp with A.Void -> ""
        | _ -> f ^ "_result") in
          L.build_call fdef (Array.of_list actuals) result builder
      | SPointLit(e1, e2) -> let e1' = expr builder e1 and e2' = expr builder e2 in
        let point = L.build_alloca point_st "point" builder in
        let ptr1 = L.build_struct_gep point 0 "pointer1" builder in
        ignore(L.build_store e1' ptr1 builder);
        let ptr2 = L.build_struct_gep point 1 "pointer2" builder in
        ignore(L.build_store e2' ptr2 builder); point
        (*L.const_named_struct point_t [|e1'; e2'|]*)
      | SPointAccess(s, p) -> let name = L.build_load (lookup s) s builder in
        (match p with
          A.Surface -> let p1 = L.build_struct_gep name 0 "pointer1" builder in
              L.build_load p1 "surface" builder
          | A.Name -> let p2 = L.build_struct_gep name 1 "pointer2" builder in
              L.build_load p2 "name" builder)
      | SPointAssign(s, p, e) -> let name = L.build_load (lookup s) s builder and e' = expr builder e 
        in (match p with
            A.Surface -> let p1 = L.build_struct_gep name 0 "pointer1" builder in
              ignore(L.build_store e' p1 builder)
          | A.Name -> let p2 = L.build_struct_gep name 1 "pointer2" builder in
              ignore(L.build_store e' p2 builder)); name
      | _ -> to_imp (string_of_sexpr (A.Int,e))  
    in



    (* Each basic block in a program ends with a "terminator" instruction i.e.
    one that ends the basic block. By definition, these instructions must
    indicate which basic block comes next -- they typically yield "void" value
    and produce control flow, not values *)
    (* Invoke "instr builder" if the current block doesn't already
       have a terminator (e.g., a branch). *)
    let add_terminal builder instr =
                           (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
  Some _ -> ()
      | None -> ignore (instr builder) in
  
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    (* Imperative nature of statement processing entails imperative OCaml *)
    let rec stmt builder = function
  SBlock sl -> List.fold_left stmt builder sl
        (* Generate code for this expression, return resulting builder *)
      | SExpr e -> let _ = expr builder e in builder 
      | SReturn e -> let _ = match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder 
                     in builder
      (* The order that we create and add the basic blocks for an If statement
      doesnt 'really' matter (seemingly). What hooks them up in the right order
      are the build_br functions used at the end of the then and else blocks (if
      they don't already have a terminator) and the build_cond_br function at
      the end, which adds jump instructions to the "then" and "else" basic blocks *)
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
         (* Add "merge" basic block to our function's list of blocks *)
   let merge_bb = L.append_block context "merge" the_function in
         (* Partial function used to generate branch to merge block *) 
         let branch_instr = L.build_br merge_bb in

         (* Same for "then" basic block *)
   let then_bb = L.append_block context "then" the_function in
         (* Position builder in "then" block and build the statement *)
         let then_builder = stmt (L.builder_at_end context then_bb) then_stmt in
         (* Add a branch to the "then" block (to the merge block) 
           if a terminator doesn't already exist for the "then" block *)
   let () = add_terminal then_builder branch_instr in

         (* Identical to stuff we did for "then" *)
   let else_bb = L.append_block context "else" the_function in
         let else_builder = stmt (L.builder_at_end context else_bb) else_stmt in
   let () = add_terminal else_builder branch_instr in

         (* Generate initial branch instruction perform the selection of "then"
         or "else". Note we're using the builder we had access to at the start
         of this alternative. *)
   let _ = L.build_cond_br bool_val then_bb else_bb builder in
         (* Move to the merge block for further instruction building *)
   L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
          (* First create basic block for condition instructions -- this will
          serve as destination in the case of a loop *)
    let pred_bb = L.append_block context "while" the_function in
          (* In current block, branch to predicate to execute the condition *)
    let _ = L.build_br pred_bb builder in

          (* Create the body's block, generate the code for it, and add a branch
          back to the predicate block (we always jump back at the end of a while
          loop's body, unless we returned or something) *)
    let body_bb = L.append_block context "while_body" the_function in
          let while_builder = stmt (L.builder_at_end context body_bb) body in
    let () = add_terminal while_builder (L.build_br pred_bb) in

          (* Generate the predicate code in the predicate block *)
    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = expr pred_builder predicate in

          (* Hook everything up *)
    let merge_bb = L.append_block context "merge" the_function in
    let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
    L.builder_at_end context merge_bb

      (* Implement for loops as while loops! *)
      | SFor (e1, e2, e3, body) -> stmt builder
      ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
