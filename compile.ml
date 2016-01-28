open Syntax
open Llvm


exception Compile_error of string
let compiler_error msg = raise (Compile_error msg)

let context = global_context ()
let the_module = create_module context "lambda jit"
let builder = builder context
let named_values: (ident, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context
let bool_type = i1_type context

let lookup x =
  try
    Hashtbl.find named_values x
  with
  | Not_found -> compiler_error ("Variable " ^ x ^ " not found")

let rec compile_expr = function
  | Var x -> lookup x
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
  | Less (e1, e2) -> build_fcmp Fcmp.Ule (compile_expr e1) (compile_expr e2) "letmp" builder
  | Apply (f, e) ->  let callee =
                       match lookup_function f the_module with
                       | Some func -> func
                       | None -> compiler_error ("Function " ^ f ^ " not found")
                     in
                     let params = params callee in

                     if Array.length params == 1 then () else compiler_error "Incorrect # of args passed";
                     let args = Array.map compile_expr [| e |] in
                     build_call callee args "calltmp" builder

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

let compile_toplevel = function
  | Expr e -> compile_expr e
  | Def (x, e) -> let v = compile_expr e in
                  Hashtbl.add named_values x v;
                  v
  | Fun (f, x, e) -> Hashtbl.clear named_values;
                     let the_function = compile_prototype f [| x |] in

                     (* Create a new basic block to start insertion into *)
                     let bb = append_block context "entry" the_function in
                     position_at_end bb builder;

                     try
                       let ret_val = compile_expr e in

                       (* Finish off the function *)
                       let _ = build_ret ret_val builder in

                       (* Validate the generate code, checking for consistency *)
                       Llvm_analysis.assert_valid_function the_function;

                       the_function
                     with e ->
                       delete_function the_function;
                       raise e
