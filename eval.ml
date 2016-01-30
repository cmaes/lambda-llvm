open Syntax

exception Runtime_error of string

let runtime_error msg = raise (Runtime_error msg)

exception Return_val of value
let return_value v = raise (Return_val v)

let lookup env x =
  try
    List.assoc x env
  with
  | Not_found -> runtime_error ("Variable " ^ x ^ " not found")

let lookup_func ctx f =
  try
    List.assoc f ctx
  with
  | Not_found -> runtime_error ("Function " ^ f ^ " not found")

let float_of_value = function
  | VFloat f -> f
  | _ -> runtime_error "float expected"

let rec eval ctx env = function
  | Var x -> lookup env x
  | Number f -> VFloat f
  | Bool   b -> VBool b
  | Mult   (e1, e2) -> (match (eval ctx env e1), (eval ctx env e2) with
                        | VFloat f1, VFloat f2 -> VFloat (f1 *. f2)
                        | _  -> runtime_error "Numbers expected in *")
  | Div (e1, e2) -> (match (eval ctx env e1), (eval ctx env e2) with
                     | VFloat f1, VFloat f2 -> VFloat (f1 /. f2)
                     | _ -> runtime_error "Numbers expected in /")
  | Plus (e1, e2) -> (match (eval ctx env e1), (eval ctx env e2) with
                      | VFloat f1, VFloat f2 -> VFloat (f1 +. f2)
                      | _ -> runtime_error "Numbers expected in +")
  | Minus (e1, e2) -> (match (eval ctx env e1), (eval ctx env e2) with
                       | VFloat f1, VFloat f2 -> VFloat (f1 -. f2)
                       | _ -> runtime_error "Numbers expected in -")
  | Equal (e1, e2) -> (match (eval ctx env e1), (eval ctx env e2) with
                       | VFloat f1, VFloat f2 -> VBool (f1 = f2)
                       | _ -> runtime_error "Numbers expected in =")
  | Less (e1, e2) -> (match (eval ctx env e1), (eval ctx env e2) with
                      | VFloat f1, VFloat f2 -> VBool (f1 < f2)
                      | _ -> runtime_error "Numbers expected in <")
  | If (pe, ce, ae) -> (match (eval ctx env pe) with
                        | VBool b -> if b then (eval ctx env ce) else (eval ctx env ae)
                        | _ -> runtime_error "Boolean expected in if predicate")
  | Apply (f, elist) -> let params, func_stmts = lookup_func ctx f in
                        let args = List.map (fun e -> eval ctx env e) elist in
                        let zipped = List.combine params args in
                        let extended_env = zipped @ env in
                        try
                          ignore (eval_stmts ctx extended_env func_stmts);
                          VFloat 0.0
                        with
                          Return_val v -> v

  and
    eval_stmt ctx env = function
    | Expr e -> (eval ctx env e, ctx, env)
    | Let  (x, e) -> let v = (eval ctx env e) in
                     (VNull, ctx, ((x, v) :: env))
    | For (i, s, f, st, b) -> let start = float_of_value (eval ctx env s) in
                              let final = float_of_value (eval ctx env f) in
                              let step  = float_of_value (match st with
                                                          | Some e -> eval ctx env e
                                                          | None   -> VFloat 1.0) in
                              let loopvar  = ref start in
                              while !loopvar < final do
                                ignore (eval_stmts ctx ((i, (VFloat !loopvar))::env) b);
                                loopvar := !loopvar +. step
                              done;
                              (VNull, ctx, env)
    | Assign (x, e) -> let v = (eval ctx env e) in
                       (VNull, ctx, ((x, v) :: env))
    | Return e -> return_value (eval ctx env e)
  and
    eval_stmts ctx base_env stmts = let env = ref base_env in
                                    let last_val = ref VNull in
                                    List.iter (fun s -> let (v, _, env') = eval_stmt ctx !env s in
                                                        env := env';
                                                        last_val := v)
                                              stmts;
                                    !last_val

let eval_toplevel ctx env = function
  | Stmt s -> eval_stmt ctx env s
  | Extern (f, args) -> let pair = (args, []) in
                        let ctx' = ((f, pair) :: ctx) in
                        (VNull, ctx', env)
  | Fun (f, args, body) -> (VNull, (f, (args, body)) :: ctx, env)
