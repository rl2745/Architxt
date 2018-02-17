(* Authors: Justin Borczuk and Marco Starger *)
(* Code generation: translate takes a SAST and produces LLVM IR *)

module Semant = Semant
module L = Llvm
module A = Ast
module S = Sast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Pixl"
  and i64_t  = L.i64_type  context
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context
  and str_t  = L.pointer_type (L.i8_type context) in

  let funcn = ref (List.hd functions) in

  let ltype_of_typ = function
      A.Int -> i64_t
    | A.Bool -> i1_t
    | A.Char -> i8_t
    | A.String -> str_t
    | A.Pixel -> L.pointer_type(L.i64_type context)
    | A.Matrix(_) -> L.pointer_type(L.i64_type context)
    | A.Void -> void_t in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let return_type = L.pointer_type(L.i64_type context) in
  let read_img = L.var_arg_function_type return_type [| str_t |] in
  let read_img_func = L.declare_function "read_img" read_img the_module in

  let arg1 = L.pointer_type(L.i64_type context) in
  let write_img = L.var_arg_function_type i64_t [|arg1; str_t; str_t |] in
  let write_img_func = L.declare_function "write_img" write_img the_module in

  let str_of_int = L.var_arg_function_type (L.pointer_type i8_t) [| i64_t |] in
  let str_of_int_func = L.declare_function "str_of_int" str_of_int the_module in

  let str_con = L.var_arg_function_type (L.pointer_type i8_t) [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let str_con_func = L.declare_function "str_con" str_con the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.S.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.S.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.S.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.S.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let _ = L.set_gc (Some "shadow-stack") the_function in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m in

      let add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.S.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.S.slocals in


    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) in

    (* Sast stmt builder and return the builder for each of the statement's successor *)
    let rec expr builder = function
      S.SLiteral (i, _) -> L.const_int i64_t i
      | S.SBoolLit (b, _) -> L.const_int i1_t (if b then 1 else 0)
      | S.SStringLit (s, _) -> L.build_global_stringptr(s) "strptr" builder
      | S.SNoexpr -> L.const_int i64_t 0
      | S.SEMatrix(rows,cols,typ) -> let rows = expr builder rows
                                     and cols = expr builder cols in
                                     (match typ with
                                     A.Matrix(A.Int) -> let left = L.build_mul cols rows "left" builder in
                                                        let mat = L.build_add left (L.const_int i64_t 2) "mat" builder in
                                                        let size = mat in
                                                        let typ = L.pointer_type i64_t in
                                                        let arr = L.build_array_malloc typ size "matrix1" builder in
                                                        let arr = L.build_pointercast arr typ "matrix2" builder in
                                                        let arr_ptr = L.build_gep arr [|L.const_int i64_t 0|] "pixel3" builder in ignore(L.build_store (rows) arr_ptr builder);
                                                        let arr_ptr = L.build_gep arr [|L.const_int i64_t 1|] "pixel3" builder in ignore(L.build_store (cols) arr_ptr builder);
                                                        arr

                                     | A.Matrix (A.Pixel) -> let left = L.build_mul cols rows "left" builder in
                                                        let left = L.build_mul left (L.const_int i64_t 4) "left" builder in
                                                        let mat = L.build_add left (L.const_int i64_t 2) "mat" builder in
                                                        let size = mat in
                                                        let typ = L.pointer_type i64_t in
                                                        let arr = L.build_array_malloc typ size "matrix1" builder in
                                                        let arr = L.build_pointercast arr typ "matrix2" builder in
                                                        let arr_ptr = L.build_gep arr [|L.const_int i64_t 0|] "pixel3" builder in ignore(L.build_store (rows) arr_ptr builder);
                                                        let arr_ptr = L.build_gep arr [|L.const_int i64_t 1|] "pixel3" builder in ignore(L.build_store (cols) arr_ptr builder);
                                                        arr
                                     | _ -> raise(Failure("You shouldn't be seeing this")))

      | S.SAssignm(id, exp1, exp2, value, typ) -> let arr =  L.build_load (lookup id) id builder
                                             and value = expr builder value in
                                             (match typ with
                                             A.Pixel -> let pointer = L.build_gep arr [|L.const_int i64_t 1|] "matrix7" builder in
                                                        let cols = L.build_load pointer "Access2" builder in
                                                        let exp1 = expr builder exp1 in
                                                        let exp2 = expr builder exp2 in
                                                        let left = L.build_mul cols exp1 "left" builder in
                                                        let left = L.build_add left exp2 "left2" builder in
                                                        let left = L.build_mul left (L.const_int i64_t 4) "left3" builder in
                                                        let loc = L.build_add left (L.const_int i64_t 2) "right" builder in
                                                        let pointer1 = L.build_gep value [|L.const_int i64_t 0|] "matrix8" builder in
                                                        let num1 = L.build_load pointer1 "Access3" builder in
                                                        let pointer2 = L.build_gep value [|L.const_int i64_t 1|] "matrix8" builder in
                                                        let num2 = L.build_load pointer2 "Access3" builder in
                                                        let pointer3 = L.build_gep value [|L.const_int i64_t 2|] "matrix8" builder in
                                                        let num3 = L.build_load pointer3 "Access3" builder in
                                                        let pointer4 = L.build_gep value [|L.const_int i64_t 3|] "matrix8" builder in
                                                        let num4 = L.build_load pointer4 "Access3" builder in
                                                        let arr_ptr = L.build_gep arr [|loc|] "pixel3" builder in ignore(L.build_store (num1) arr_ptr builder);
                                                        let arr_ptr = L.build_gep arr [|L.build_add loc (L.const_int i64_t 1) "add1" builder|] "pixel4" builder in ignore(L.build_store (num2) arr_ptr builder);
                                                        let arr_ptr = L.build_gep arr [|L.build_add loc (L.const_int i64_t 2) "add2" builder|] "pixel5" builder in ignore(L.build_store (num3) arr_ptr builder);
                                                        let arr_ptr = L.build_gep arr [|L.build_add loc (L.const_int i64_t 3) "add3" builder|] "pixel6" builder in ignore(L.build_store (num4) arr_ptr builder);
                                                        arr


                                             | A.Int ->  let pointer = L.build_gep arr [|L.const_int i64_t 1|] "matrix7" builder in
                                   		       let cols = L.build_load pointer "Access2" builder in
                                   		       let exp1 = expr builder exp1 in
                                   		       let exp2 = expr builder exp2 in
                                                       let left = L.build_mul cols exp1 "left" builder in
                                                       let right = L.build_add exp2 (L.const_int i64_t 2) "right" builder in
                                                       let loc = L.build_add left right "add" builder in
                                                       let pointer = L.build_gep arr [|loc|] "matrix8" builder in
                                                       ignore(L.build_store (value) pointer builder);
                                                       arr
                                             | _ -> raise(Failure("You shouldn't be seeing this")))


      | S.SAssignp(id,field,e1,_) -> let arr = L.build_load (lookup id) id builder
                                        and value = expr builder e1 in
					(match field with
                                          A.Red ->   let arr_ptr = L.build_gep arr [|L.const_int i64_t 0|] "pixel3" builder in ignore(L.build_store (value) arr_ptr builder)

                                        | A.Green ->  let arr_ptr = L.build_gep arr [|L.const_int i64_t 1|] "pixel3" builder in ignore(L.build_store (value) arr_ptr builder)

                                        | A.Blue ->  let arr_ptr = L.build_gep arr [|L.const_int i64_t 2|] "pixel3" builder in ignore(L.build_store (value) arr_ptr builder)

                                        | A.Alpha ->  let arr_ptr = L.build_gep arr [|L.const_int i64_t 3|] "pixel3" builder in ignore(L.build_store (value) arr_ptr builder));

					arr

      | S.SRows(id) ->    let arr = L.build_load (lookup id) id builder in
                         let pointer = L.build_gep arr [|L.const_int i64_t 0|] "pixel7" builder in
                         L.build_load pointer "Access1" builder
      | S.SCols(id) ->    let arr = L.build_load (lookup id) id builder in
                         let pointer = L.build_gep arr [|L.const_int i64_t 1|] "pixel7" builder in
                         L.build_load pointer "Access1" builder

      | S.SId (s, _) -> L.build_load (lookup s) s builder
      | S.SPixelLit(e1, e2, e3, e4, _) -> let size = L.const_int i64_t 4 in
          				  let typ = L.pointer_type i64_t in
          				  let arr = L.build_array_malloc typ size "pixel1" builder in
                                          let arr = L.build_pointercast arr typ "pixel2" builder in
          				  let e1 = expr builder e1 in
          				  let e2 = expr builder e2 in
          				  let e3 = expr builder e3 in
          				  let e4 = expr builder e4 in
         				  let arr_ptr = L.build_gep arr [|L.const_int i64_t 0|] "pixel3" builder in ignore(L.build_store (e1) arr_ptr builder);
          				  let arr_ptr = L.build_gep arr [|L.const_int i64_t 1|] "pixel4" builder in ignore(L.build_store (e2) arr_ptr builder);
          				  let arr_ptr = L.build_gep arr [|L.const_int i64_t 2|] "pixel5" builder in ignore(L.build_store (e3) arr_ptr builder);
          				  let arr_ptr = L.build_gep arr [|L.const_int i64_t 3|] "pixel6" builder in ignore(L.build_store (e4) arr_ptr builder);
          				  arr

      | S.SMatrixLit(li, typ) -> (match typ with
                           A.Matrix(A.Pixel) -> let rows = List.length li in
                           let columns = List.length (List.hd li) in
                           let mat = (4 * rows * columns) + 2 in
                           let size = L.const_int i64_t mat in
                           let typ = L.pointer_type i64_t in
                           let arr = L.build_array_malloc typ size "matrix1" builder in
                           let arr = L.build_pointercast arr typ "matrix2" builder in
                           let arr_ptr = L.build_gep arr [|L.const_int i64_t 0|] "pixel3" builder in ignore(L.build_store (L.const_int i64_t rows) arr_ptr builder);
                           let arr_ptr = L.build_gep arr [|L.const_int i64_t 1|] "pixel3" builder in ignore(L.build_store (L.const_int i64_t (columns)) arr_ptr builder);
                           for r=0 to rows-1 do
                                for c=0 to columns-1 do
                                let element = List.nth (List.nth li r) c in
                                let element = expr builder element in
                                let loc = (r * columns + c) * 4 + 2 in
                                let arr_ptr = L.build_gep element [|L.const_int i64_t 0|] "matrix1" builder in
                                let num1 = L.build_load arr_ptr "num1" builder in
                                let arr_ptr = L.build_gep element [|L.const_int i64_t 1|] "matrix2" builder in
                                let num2 = L.build_load arr_ptr "num2" builder in
                                let arr_ptr = L.build_gep element [|L.const_int i64_t 2|] "matrix3" builder in
                                let num3 = L.build_load arr_ptr "num3" builder in
                                let arr_ptr = L.build_gep element [|L.const_int i64_t 3|] "matrix4" builder in
                                let num4 = L.build_load arr_ptr "num4" builder in
                                let arr_ptr = L.build_gep arr [|L.const_int i64_t loc|] "matrix5" builder in ignore(L.build_store (num1) arr_ptr builder);
                                let arr_ptr = L.build_gep arr [|L.const_int i64_t (loc + 1)|] "matrix6" builder in ignore(L.build_store (num2) arr_ptr builder);
                                let arr_ptr = L.build_gep arr [|L.const_int i64_t (loc + 2)|] "matrix7" builder in ignore(L.build_store (num3) arr_ptr builder);
                                let arr_ptr = L.build_gep arr [|L.const_int i64_t (loc + 3)|] "matrix8" builder in ignore(L.build_store (num4) arr_ptr builder);
                                done
                           done;
                           arr
                          | A.Matrix(A.Int) -> let rows = List.length li in
                           let columns = List.length (List.hd li) in
                           let mat = rows * columns + 2 in
                           let size = L.const_int i64_t mat in
                           let typ = L.pointer_type i64_t in
                           let arr = L.build_array_malloc typ size "matrix1" builder in
                           let arr = L.build_pointercast arr typ "matrix2" builder in
                           let arr_ptr = L.build_gep arr [|L.const_int i64_t 0|] "pixel3" builder in ignore(L.build_store (L.const_int i64_t rows) arr_ptr builder);
                           let arr_ptr = L.build_gep arr [|L.const_int i64_t 1|] "pixel3" builder in ignore(L.build_store (L.const_int i64_t columns) arr_ptr builder);                               for r=0 to rows-1 do
                                for c=0 to columns-1 do
                                let loc = r * columns + c + 2 in
                                let element = List.nth (List.nth li r) c in
                                let element = expr builder element in
                                let arr_ptr = L.build_gep arr [|L.const_int i64_t loc|] "matrix3" builder in
                                ignore(L.build_store (element) arr_ptr builder);
                                done
                           done;
                           arr
                         | _ -> raise(Failure("You shouldn't be seeing this")))


      | S.SMatrixAccess(v,e1,e2,typ) -> (match typ with
                                   A.Pixel -> let arr1 = L.build_load (lookup v) v builder in
                                   let pointer = L.build_gep arr1 [|L.const_int i64_t 1|] "matrix7" builder in
                                   let cols = L.build_load pointer "Access2" builder in
                                   let exp1 = expr builder e1 in
                                   let exp2 = expr builder e2 in
                                   let left = L.build_mul cols exp1 "left" builder in
                                   let left = L.build_add left exp2 "left2" builder in
                                   let left = L.build_mul left (L.const_int i64_t 4) "left3" builder in
                                   let loc = L.build_add left (L.const_int i64_t 2) "right" builder in
                                   let size = L.const_int i64_t 4 in
                                   let typ = L.pointer_type i64_t in
                                   let arr2 = L.build_array_malloc typ size "pixel1" builder in
                                   let arr2 = L.build_pointercast arr2 typ "pixel2" builder in
                                   let pointer1 = L.build_gep arr1 [|loc|] "matrix8" builder in
                                   let num1 = L.build_load pointer1 "Access3" builder in
                                   let pointer2 = L.build_gep arr1 [|L.build_add loc (L.const_int i64_t 1) "add1" builder|] "matrix8" builder in
                                   let num2 = L.build_load pointer2 "Access3" builder in
                                   let pointer3 = L.build_gep arr1 [|L.build_add loc (L.const_int i64_t 2) "add2" builder|] "matrix8" builder in
                                   let num3 = L.build_load pointer3 "Access3" builder in
                                   let pointer4 = L.build_gep arr1 [|L.build_add loc (L.const_int i64_t 3) "add3" builder|] "matrix8" builder in
                                   let num4 = L.build_load pointer4 "Access3" builder in
                                   let arr_ptr = L.build_gep arr2 [|L.const_int i64_t 0|] "pixel3" builder in ignore(L.build_store (num1) arr_ptr builder);
                                   let arr_ptr = L.build_gep arr2 [|L.const_int i64_t 1|] "pixel4" builder in ignore(L.build_store (num2) arr_ptr builder);
                                   let arr_ptr = L.build_gep arr2 [|L.const_int i64_t 2|] "pixel5" builder in ignore(L.build_store (num3) arr_ptr builder);
                                   let arr_ptr = L.build_gep arr2 [|L.const_int i64_t 3|] "pixel6" builder in ignore(L.build_store (num4) arr_ptr builder);
                                   arr2

                                   | A.Int -> let arr = L.build_load (lookup v) v builder in
                                   let pointer = L.build_gep arr [|L.const_int i64_t 1|] "matrix7" builder in
                                   let cols = L.build_load pointer "Access2" builder in
                                   let exp1 = expr builder e1 in
                                   let exp2 = expr builder e2 in
                                   let left = L.build_mul cols exp1 "left" builder in
                                   let right = L.build_add exp2 (L.const_int i64_t 2) "right" builder in
                                   let loc = L.build_add left right "add" builder in
                                   let pointer = L.build_gep arr [|loc|] "matrix8" builder in
                                   L.build_load pointer "Access3" builder
                                   | _ -> raise(Failure("You shouldn't be seeing this")))
      | S.SAccess(s, e, _) ->
          let arr = L.build_load (lookup s) s builder in
                           (match e with
                             A.Red  -> let pointer = L.build_gep arr [|L.const_int i64_t 0|] "pixelRed" builder in
                                       L.build_load pointer "Access1" builder

                           | A.Green -> let pointer = L.build_gep arr [|L.const_int i64_t 1|] "pixelGreen" builder in
                                        L.build_load pointer "Access1" builder

                           | A.Blue ->  let pointer = L.build_gep arr [|L.const_int i64_t 2|] "pixelBlue" builder in
                                        L.build_load pointer "Access1" builder

                           | A.Alpha -> let pointer = L.build_gep arr [|L.const_int i64_t 3|] "pixelAlpha" builder in
                                        L.build_load pointer "Access1" builder)


      | S.SBinop(e1, op, e2, _) ->
          let e1' = expr builder e1
          and e2' = expr builder e2 in
              (match op with
               A.Add     ->   L.build_add
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

      | S.SUnop(op, e, _) ->
          let e' = expr builder e in
          (match op with
            A.Neg       -> L.build_neg e' "tmp" builder
            | A.Not     -> L.build_not e' "tmp" builder
            | _ -> raise(Failure("You shouldn't be seeing this")))

      | S.SAssign (s, e, _) -> let e' = expr builder e in
                          ignore (L.build_store e' (lookup s) builder); e'

      | S.SCall ("print", [e], _) | S.SCall ("printb", [e], _) ->
          L.build_call printf_func [| int_format_str ; (expr builder e) |]
            "printf" builder
      | S.SCall ("prints", [e], _) ->
          L.build_call printf_func [| str_format_str ; (expr builder e) |]
            "printf" builder
      | S.SCall ("read", [e], _) ->
          L.build_call read_img_func [| (expr builder e) |] "read_img" builder
      | S.SCall ("write", [e1;e2;e3], _)->
          L.build_call write_img_func [|(expr builder e1); (expr builder e2); (expr builder e3)|] "write_img" builder
      | S.SCall ("str_of_int", [e], _) ->
          L.build_call str_of_int_func [| (expr builder e) |] "str_of_int" builder
      | S.SCall ("str_con", [e1;e2], _) ->
          L.build_call str_con_func [| (expr builder e1); (expr builder e2) |] "str_con" builder
      | S.SCall(f, act, _) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (expr builder) (List.rev act)) in
          let result = (match fdecl.S.styp with
                                 A.Void -> ""
                                 | _ -> f ^ "_result") in
            L.build_call fdef (Array.of_list actuals) result builder

      in

    let rec stmt builder = function
      S.SBlock sl -> List.fold_left stmt builder sl
      |S.SExpr (e, _) -> ignore (expr builder e); builder
      |S.SReturn (e) -> ignore (match !funcn.S.styp with
          A.Void -> L.build_ret_void builder
          | _ -> L.build_ret (expr builder e) builder); builder
      |S.SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
         let merge_bb = L.append_block context "merge" the_function in

         let then_bb = L.append_block context "then" the_function in
         add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
           (L.build_br merge_bb);

         let else_bb = L.append_block context "else" the_function in
         add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
           (L.build_br merge_bb);

         ignore (L.build_cond_br bool_val then_bb else_bb builder);
         L.builder_at_end context merge_bb
      |S.SWhile (predicate, body) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore (L.build_br pred_bb builder);

          let body_bb = L.append_block context "while_body" the_function in
          add_terminal (stmt (L.builder_at_end context body_bb) body)
            (L.build_br pred_bb);

          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr pred_builder predicate in

          let merge_bb = L.append_block context "merge" the_function in
          ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
          L.builder_at_end context merge_bb
     | S.SFor (e1, e2, e3, body) -> stmt builder
            ( S.SBlock [S.SExpr(e1,A.Int); S.SWhile (e2, S.SBlock [body ; S.SExpr(e3,A.Int)]) ] )

    in
    (* Build the code for each statement in the function *)
    let builder = stmt builder (S.SBlock fdecl.S.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.S.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
