type ident = string


type expr =
  | Var of ident                    (* Variable *)
  | Number of float                 (* Floating point number *)
  | Bool   of bool                  (* Boolean constant *)
  | Mult   of expr * expr           (* Product [e1 * e2 ] *)
  | Div    of expr * expr           (* Divison [e1 / e2 ] *)
  | Plus   of expr * expr           (* Sum [e1 + e2] *)
  | Minus  of expr * expr           (* Difference [e1 - e2] *)
  | Equal  of expr * expr           (* Floating point comparison [ e1 == e2 ] *)
  | Less   of expr * expr           (* Floating point comparison [ e1 < e2 ] *)
  | Apply  of ident * expr          (* Application  [ f(e2) ] *)

type context = (ident * expr) list

type toplevel_cmd =
  | Expr of expr                    (* Expression *)
  | Def of ident * expr              (* Value definition [ let x = e ] *)
  | Fun of ident * ident * expr     (* Function [ fun f(x) { e } ] *)

type value =
  | VNull
  | VBool of bool
  | VFloat of float

type environment = (ident * value) list
