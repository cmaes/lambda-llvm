{
  open Lexing
  open Parser
}

let ident = ['a'-'z' 'A'-'Z']+
let digit = ['0'-'9']
let frac = '.' digit*
let exp  = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let comment = '#' [^ '\n' '\r']*

rule token = parse
   [' ' '\t' '\r' '\n' ] { token lexbuf }
  | comment              { token lexbuf }
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
  | "for"                { FOR }
  | "extern"             { EXTERN }
  | "return"             { RETURN }
  | ident                { IDENT (lexeme lexbuf) }
  | eof                  { EOF }
