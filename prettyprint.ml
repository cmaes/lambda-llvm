open Syntax

(* prec  | operator  | operator
   level | character | name
   ==================================
       4 | * /       | multiplication, division
       3 | + -       | addition, subtraction
       2 | <         | comparison operators
       1 | ==        | equality operators
       0 | fun       | function
There are 5 levels of precedence.
*)

let rec string_of_expr e = to_str (-1) e
  and
    to_str n e =
    let (m, str) = match e with
      | Var  v -> (5, v)
      | Bool b -> (5, string_of_bool b)
      | Number n -> (5, Printf.sprintf "%g" n)
      | Apply (f, e) -> (5, f ^ "(" ^ (to_str (-1) e) ^ ")")
      | Mult  (e1, e2) -> (4, (to_str 4 e1) ^ " * " ^ (to_str 5 e2))
      | Div   (e1, e2) -> (4, (to_str 4 e1) ^ " / " ^ (to_str 5 e2))
      | Plus  (e1, e2) -> (3, "(" ^ (to_str 3 e1) ^ " + " ^ (to_str 4 e2) ^ ")")
      | Minus (e1, e2) -> (3, (to_str 3 e1) ^ " - " ^ (to_str 4 e2))
      | Less      (e1, e2) -> (2, (to_str 2 e1) ^ " < " ^ (to_str 3 e2))
      | Equal     (e1, e2) -> (1, (to_str 2 e1) ^ " == " ^ (to_str 3 e2))
    in
    if m < n then "(" ^ str ^ ")" else str


let print_value = function
  | VNull -> print_endline ""
  | VBool b  -> print_endline (string_of_bool b)
  | VFloat f -> print_endline (string_of_float f)
