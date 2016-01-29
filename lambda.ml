open Syntax
open Prettyprint
open Eval

open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts


let repl base_ctx base_env =
  print_endline "Lambda. Press Ctrl-D to exit.";

  let ctx = ref base_ctx in
  let env = ref base_env in

  try
    while true do
      try
        print_string "Lambda> "; flush stdout;
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
    End_of_file -> (Llvm.dump_module Compile.the_module;
                    print_endline "\nGoodbye.")

exception Badfilename of string
let bad_filename msg = raise (Badfilename msg)

let get_last_period str =
  let last_period = ref (-1) in
  for i = 0 to (String.length str) - 1 do
    if str.[i] == '.' then
      last_period := i
    else
      ()
  done;
  if !last_period >= 0 then
    !last_period
  else
    bad_filename "No period found in filename"

let basename fname =
  let last_period = get_last_period fname in
  String.sub fname 0 last_period

let compile fname =
  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  let ast    = Parser.program Lexer.token lexbuf in
  let outfname = basename fname ^ ".ll" in
  let oc = open_out outfname in
  close_in ic;

  ignore (initialize ());

  let the_fpm = PassManager.create_function Compile.the_module in

  (* Promote allocas to registers *)
  add_memory_to_register_promotion the_fpm;

  (* Do simple "peephole" and bit-twiddling optimizations *)
  add_instruction_combination the_fpm;

  (* reassociate expressions *)
  add_reassociation the_fpm;

  (* Eliminate common sub-expressions *)
  add_gvn the_fpm;

  (* Simplify the control flow graph (delete unreachable blocks, etc.) *)
  add_cfg_simplification the_fpm;

  ignore (PassManager.initialize the_fpm);

  Compile.compile_program the_fpm ast;
  Printf.fprintf oc "%s\n" (Llvm.string_of_llmodule Compile.the_module);
  close_out oc

let main =
  if Array.length Sys.argv > 1 then
    compile Sys.argv.(1)
  else
    repl [] []
