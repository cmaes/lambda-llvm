open Syntax

exception Runtime_error of string

let runtime_error msg = raise (Runtime_error msg)

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
  | Apply (f, elist) -> let params, func_expr = lookup_func ctx f in
                        let args = List.map (fun e -> eval ctx env e) elist in
                        let zipped = List.combine params args in
                        eval ctx (zipped @ env) func_expr

let eval_toplevel ctx env = function
  | Expr e -> (eval ctx env e, ctx, env)
  | Def  (x, e) -> let v = (eval ctx env e) in
                   (VNull, ctx, ((x, v) :: env))
  | Fun (f, args, e) -> (VNull, (f, (args, e)) :: ctx, env)
