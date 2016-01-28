%{
    open Syntax
%}

%token <Syntax.ident> IDENT
%token <bool> BOOL
%token <float> FLOAT
%token PLUS
%token MINUS
%token MULT
%token DIV
%token LESS
%token EQUALEQUAL
%token FUN
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LET
%token EQUAL
%token COMMA
%token SEMI
%token IF
%token THEN
%token ELSE
%token EOF

%start program
%type <Syntax.toplevel_cmd list> program

%start toplevel
%type <Syntax.toplevel_cmd> toplevel

(* the order of the following is important to define precedence *)
%nonassoc  ELSE
%nonassoc EQUALEQUAL
%nonassoc LESS
%left PLUS MINUS
%left MULT DIV
%%

program:
    | EOF                                      { [] }
    | t = toplevel; l = program;               { t :: l }

toplevel:
 | e = expr; SEMI; SEMI                        { Expr e }
 | LET; x = IDENT; EQUAL; e = expr; SEMI; SEMI { Def (x, e) }
 | f = func_def;                               { f }


expr:
  | s = simple_expr { s }
  | ap = app_expr   { ap }
  | a  = arith_expr { a }
  | b = bool_expr   { b }
  | c = cond_expr   { c }

simple_expr:
  | x = IDENT                 { Var x }
  | b = BOOL                  { Bool b }
  | f = FLOAT                 { Number f }
  | LPAREN; e = expr; RPAREN  { e }

app_expr:
  | f = IDENT; LPAREN; args = expr_list; RPAREN    { Apply (f, args) }


arith_expr:
  | e1 = expr; PLUS; e2 = expr  { Plus (e1, e2)  }
  | e1 = expr; MINUS; e2 = expr { Minus (e1, e2) }
  | e1 = expr; MULT; e2 = expr  { Mult (e1, e2) }
  | e1 = expr; DIV; e2 = expr   { Div  (e1, e2) }

bool_expr:
  | e1 = expr; LESS; e2 = expr       { Less (e1, e2) }
  | e1 = expr; EQUALEQUAL; e2 = expr { Equal (e1, e2) }

cond_expr:
  | IF; pred = expr; THEN; conseq = expr; ELSE altern = expr
  { If (pred, conseq, altern) }


func_def:
  | FUN; f = IDENT; LPAREN; args = ident_list; RPAREN; LBRACE; e = expr; RBRACE
   { Fun(f, args, e) }

expr_list:
  |                                  { []    }
  | e = expr;                        { [e]   }
  | e = expr; COMMA; el = expr_list  { e::el }

ident_list:
  |                                   { []    }
  | i = IDENT;                        { [i]   }
  | i = IDENT; COMMA; il = ident_list { i::il }
%%
