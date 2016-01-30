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
  | Apply  of ident * (expr list)   (* Application  [ f(e1, ..., en) ] *)
 and stmt =
  | Expr of expr                                           (* Expression *)
  | For of ident * expr * expr * expr option * (stmt list) (* For loop [ for i=e, cmp, [step] in e *)
  | Let of ident * expr                                    (* Value declaration [ let x = e ] *)
  | Assign of ident * expr                                 (* Mutable assignment [ x = e ] *)
  | Return of expr                                         (* Return [ return e ] *)

type context = ((ident list) * (stmt list)) list

type toplevel_cmd =
  | Stmt of stmt                               (* Expression *)
  | Extern of ident * (ident list)             (* External declaration [ extern f(x1, ..., xn) ] *)
  | Fun of ident * (ident list) * (stmt list)  (* Function [ fun f(x1, ..., xn) { e1; ...; en } ] *)

type value =
  | VNull
  | VBool of bool
  | VFloat of float

type environment = (ident * value) list
