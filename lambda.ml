open Syntax
open Prettyprint
open Eval


let repl base_ctx base_env =
  print_endline "Lambda. Press Ctrl-D to exit.";
  let ctx = ref base_ctx in
  let env = ref base_env in
  try
    while true do
      try
        print_string "Lambda> ";
        let str = read_line () in
        let ast = Parser.toplevel Lexer.token (Lexing.from_string str) in
        let value, ctx', env' = (eval_toplevel !ctx !env ast) in
        Prettyprint.print_value value;
        ctx := ctx';
        env := env';
      with
      | Parser.Error -> print_endline "Syntax error"
      | Eval.Runtime_error msg -> print_endline ("Runtime error: " ^ msg)
    done
  with
    End_of_file -> print_endline "\nGoodbye."



let main =
  repl [] []
