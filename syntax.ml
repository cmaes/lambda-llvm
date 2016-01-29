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
  | If     of expr * expr * expr    (* Conditional [ if pred then cons else altern ] *)
  | For    of
      ident * expr * expr * expr option * expr
                                    (* For loop [ for i=e, cmp, [step] in e *)
  | Apply  of ident * (expr list)   (* Application  [ f(e1, ..., en) ] *)

type context = (ident list * expr) list

type toplevel_cmd =
  | Expr of expr                               (* Expression *)
  | Extern of ident * (ident list)             (* External declaration [ extern f(x1, ..., xn) ] *)
  | Def of ident * expr                        (* Value definition [ let x = e ] *)
  | Fun of ident * (ident list) * expr  (* Function [ fun f(x1, ..., xn) { e } ] *)

type value =
  | VNull
  | VBool of bool
  | VFloat of float

type environment = (ident * value) list
