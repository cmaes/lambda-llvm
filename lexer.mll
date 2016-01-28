{
  open Lexing
  open Parser
}

let ident = ['a'-'z' 'A'-'Z']+
let digit = ['0'-'9']
let frac = '.' digit*
let exp  = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

rule token = parse
   [' ' '\t' '\r' '\n' ] { token lexbuf }
  | float                { FLOAT (float_of_string (lexeme lexbuf)) }
  | "true"               { BOOL (true) }
  | "false"              { BOOL (false) }
  | "fun"                { FUN }
  | "let"                { LET }
  | "="                  { EQUAL }
  | "=="                 { EQUALEQUAL }
  | "<"                  { LESS }
  | "("                  { LPAREN }
  | ")"                  { RPAREN }
  | "{"                  { LBRACE }
  | "}"                  { RBRACE }
  | "+"                  { PLUS }
  | "-"                  { MINUS }
  | "*"                  { MULT }
  | "/"                  { DIV }
  | ","                  { COMMA }
  | ";"                  { SEMI }
  | "if"                 { IF }
  | "then"               { THEN }
  | "else"               { ELSE }
  | ident                { IDENT (lexeme lexbuf) }
  | eof                  { EOF }
