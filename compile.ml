open Syntax
open Llvm
open Llvm_executionengine

exception Compile_error of string
let compiler_error msg = raise (Compile_error msg)

let context = global_context ()
let the_module = create_module context "lambda module"
let builder = builder context
let named_values: (ident, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context
let bool_type = i1_type context
let void_type = void_type context

let lookup x =
  try
    Hashtbl.find named_values x
  with
  | Not_found -> compiler_error ("Variable " ^ x ^ " not found")

(* Create an alloca instruction in the entry block of the function.
 *  This is used for mutable variables, etc. *)
let create_entry_block_alloca the_function var_name =
  let builder = builder_at context (instr_begin (entry_block the_function)) in
  build_alloca double_type var_name builder

let rec compile_expr = function
  | Var x -> let v = lookup x in
             (* Load the value *)
             build_load v x builder
  | Number f -> const_float double_type f
  | Bool b -> let v = match b with
                | true -> 1
                | false -> 0
              in
              const_int bool_type v
  | Mult (e1, e2) -> build_fmul (compile_expr e1) (compile_expr e2) "multmp" builder
  | Div (e1, e2) -> build_fdiv (compile_expr e1) (compile_expr e2) "divtmp" builder
  | Plus (e1, e2) -> build_fadd (compile_expr e1) (compile_expr e2) "addtmp" builder
  | Minus (e1, e2) -> build_fsub (compile_expr e1) (compile_expr e2) "subtmp" builder
  | Equal (e1, e2) -> build_fcmp Fcmp.Ueq (compile_expr e1) (compile_expr e2) "eqtmp" builder
  | Less (e1, e2) -> build_fcmp Fcmp.Ult (compile_expr e1) (compile_expr e2) "letmp" builder
  | If (pe, ce, ae) ->
     let pred = compile_expr pe in

     (* Grab the first block so that we might later add the
      * conditional branch to it at the end of the function *)
     let start_bb = insertion_block builder in
     let the_function = block_parent start_bb in

     let then_bb = append_block context "then" the_function in

     (* Emit 'then' value *)

     position_at_end then_bb builder;
     let then_val = compile_expr ce in

     (* Compilation of 'then' can change the current block, update then_bb
      * for the phi. We create a new because one is used for the phi node
      * and the other is used for the conditional branch *)
     let new_then_bb = insertion_block builder in

     (* Emit 'else' value *)
     let else_bb = append_block context "else" the_function in
     position_at_end else_bb builder;
     let else_val = compile_expr ae; in

     (* Compilation of 'else' can change the current block, update else_bb
      * for the phi. *)

     let new_else_bb = insertion_block builder in

     (* Emit the merge block *)
     let merge_bb = append_block context "ifcont" the_function in
     position_at_end merge_bb builder;
     let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
     let phi = build_phi incoming "iftmp" builder in

     (* Return to the start block to add the conditional branch *)
     position_at_end start_bb builder;
     ignore (build_cond_br pred then_bb else_bb builder);

     (* Set an unconditional branch at the end of the 'then' block and the
      * 'else' block to the merge 'block' *)
     position_at_end new_then_bb builder;
     ignore (build_br merge_bb builder);
     position_at_end new_else_bb builder;
     ignore (build_br merge_bb builder);

     (* Finally, set the builder to the end of the merge block *)
     position_at_end merge_bb builder;

     phi
  | For (i, s, f, st, body) ->
     (* Output this as:
      * var = alloca double
      * ...
      * start = startexpr
      * store start -> var
      * goto loop
      * loop:
      *   ...
      *   bodyexpr
      *   ...
      * loopend:
      *   step = stepexpr
      *   endcond = endexpr
      *
      * curvar = load var
      * nextvar = curvar + step
      * store nextvar -> var
      * br endcond, loop, endloop
      *
      * endloop: *)

     let the_function = block_parent (insertion_block builder) in

    (* Create an alloca for the variable in the entry block *)
     let alloca = create_entry_block_alloca the_function i in

    (* Emit the start code first, without 'variable' in scope *)
     let start_val = compile_expr s in

    (* Store the value into the alloca *)
     ignore (build_store start_val alloca builder);

    (* Make the new basic block for the loop header, inserting after the current block *)
     let loop_bb = append_block context "loop" the_function in

    (* Insert an explict fall through from the current block to the loop_bb *)
     ignore (build_br loop_bb builder);

    (* Start insertion into loop_bb *)
     position_at_end loop_bb builder;

    (* Within the loop, the variable is defined equal to the alloca. If it shadows
     * an existing variable, we have to restore it, so save it now *)
     let old_val =
       try Some (Hashtbl.find named_values i) with Not_found -> None
     in
     Hashtbl.add named_values i alloca;

    (* Emit the body of the loop. This, like any other expr, can change the current BB.
     * Note that we ignore the value computed by the body, but we don't allow an error *)
     ignore (compile_expr body);

    (* Emit the step value *)
     let step_val =
       match st with
       | Some step -> compile_expr step
       | None      -> const_float double_type 1.0
     in

    (* Compute the end condition *)
     let end_cond = compile_expr f in

    (* Reload, increment, and restore the alloca. This handles the case where
     * the body of the loop mutates the variable *)
     let cur_var = build_load alloca i builder in
     let next_var = build_fadd cur_var step_val "nextvar" builder in
     ignore(build_store next_var alloca builder);

    (* Create the "after loop" block and insert it *)
     let after_bb  = append_block  context "afterloop" the_function in

    (* Insert the conditional branch into the end of the loop end_bb *)
     ignore (build_cond_br end_cond loop_bb after_bb builder);

    (* Any new code will be inserted in after_bb *)
     position_at_end after_bb builder;

    (* Restore the unshadowed variable *)
     begin match old_val with
           | Some old_val -> Hashtbl.add named_values i old_val
           | None -> ()
     end;

    (* for expr always returns 0.0 *)
     const_null double_type

  | Apply (f, elist) ->  let callee =
                       match lookup_function f the_module with
                       | Some func -> func
                       | None -> compiler_error ("Function " ^ f ^ " not found")
                     in
                     let params = params callee in

                     if Array.length params == List.length elist then () else compiler_error "Incorrect # of args passed";
                     let args = Array.map compile_expr (Array.of_list elist) in
                     build_call callee args "calltmp" builder

(* Create an alloca for each argument and register the argument in the
   symbol table so that references to it will succeed *)
let create_argument_allocas the_function args =
  Array.iteri (fun i ai ->
               let var_name = args.(i) in
               (* Create an alloca for this variable *)
               let alloca = create_entry_block_alloca the_function var_name in

               (* Store the initial value into the alloca *)
               ignore (build_store ai alloca builder);

               (* Add arguments to the variable symbol table *)
               Hashtbl.add named_values var_name alloca;
              )
              (params the_function)

let compile_prototype name args =
  (* Make the function type: double(double, double) etc. *)
  let doubles = Array.make (Array.length args) double_type in
  let ft = function_type double_type doubles in
  let f =
    match lookup_function name the_module with
    | None -> declare_function name ft the_module
    | Some f -> compiler_error ("Redefinition of function " ^ name )
  in

  (* Set names for all arguments *)
  Array.iteri (fun i a ->
               let n = args.(i) in
               set_value_name n a;
               Hashtbl.add named_values n a;
              ) (params f);
  f

let compile_func the_fpm f args e =
  Hashtbl.clear named_values;
  let the_function = compile_prototype f args in

  (* Create a new basic block to start insertion into *)
  let bb = append_block context "entry" the_function in
  position_at_end bb builder;

  try
    (* Add all arguments to the symbol table and create their allocas *)
    create_argument_allocas the_function args;

    let ret_val = compile_expr e in

    (* Finish off the function *)
    let _ = build_ret ret_val builder in

    (* Validate the generate code, checking for consistency *)
    Llvm_analysis.assert_valid_function the_function;

    (* Optimize the function *)
    let _ = PassManager.run_function the_function the_fpm in

    the_function
  with e ->
    delete_function the_function;
    raise e



let compile_function the_fpm = function
  | Extern (f, args) -> (f, compile_prototype f (Array.of_list args))
  | Fun (f, args, e) -> (f, compile_func the_fpm f (Array.of_list args) e)
  | _ -> compiler_error "Function expected"

let compile_defn = function
  | Def(x, e) -> let v = compile_expr e in
                 Hashtbl.add named_values x v
  | _ -> compiler_error "Definition expected"

let rec compile_topexprs = function
  | []     ->  const_null double_type
  | [Expr e] -> compile_expr e
  | (Expr e) :: t -> ignore (compile_expr e);
                     compile_topexprs t
  | _ -> compiler_error "List of expressions expected"

let is_fundef = function
  | Fun _ -> true
  | Extern _ -> true
  | _ -> false

let is_def = function
  | Def _ -> true
  | _ -> false

let is_expr = function
  | Expr _ -> true
  | _ -> false

let compile_program the_fpm program =
  let funs = List.filter is_fundef program in
  let defs = List.filter is_def program in
  let exprs = List.filter is_expr program in

  let protos = List.map (fun e -> compile_function the_fpm e) funs in

  (* Create an entry point function (lambda_main) *)
  let ft = function_type double_type [| |] in
  let lambda_main =  declare_function "lambda_main" ft the_module in

  (* Create a new basic block to start insertion into *)
  let bb = append_block context "entry" lambda_main in
  position_at_end bb builder;

  (* Clear previous names *)
  Hashtbl.clear named_values;

  (* Add prototypes back in *)
  List.iter (fun (name, p) -> Hashtbl.add named_values name p) protos;

  (* Compile toplevel defintions *)
  List.iter compile_defn defs;

  (* Compile expr and return it *)
  let ret_val = compile_topexprs exprs in

  (* Finish off the function *)
  let _ = build_ret ret_val builder in

  (* Validate the generate code, checking for consistency *)
  Llvm_analysis.assert_valid_function lambda_main;

  (* Optimize the function *)
  let _ = PassManager.run_function lambda_main the_fpm in ()
