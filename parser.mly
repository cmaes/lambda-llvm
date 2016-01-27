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
%token EOF

%start toplevel
%type <Syntax.toplevel_cmd> toplevel

(* the order of the following is important to define precedence *)
%nonassoc EQUALEQUAL
%nonassoc LESS
%left PLUS MINUS
%left MULT DIV
%%

toplevel:
 | e = expr; EOF                        { Expr e }
 | LET; x = IDENT; EQUAL; e = expr; EOF { Def (x, e) }
 | f = func_def; EOF                    { f }


expr:
  | s = simple_expr { s }
  | ap = app_expr   { ap }
  | a  = arith_expr { a }
  | b = bool_expr   { b }



simple_expr:
  | x = IDENT                 { Var x }
  | b = BOOL                  { Bool b }
  | f = FLOAT                 { Number f }
  | LPAREN; e = expr; RPAREN  { e }

app_expr:
  | f = IDENT; LPAREN; e = expr; RPAREN    { Apply (f, e) }


arith_expr:
  | e1 = expr; PLUS; e2 = expr  { Plus (e1, e2)  }
  | e1 = expr; MINUS; e2 = expr { Minus (e1, e2) }
  | e1 = expr; MULT; e2 = expr  { Mult (e1, e2) }
  | e1 = expr; DIV; e2 = expr   { Div  (e1, e2) }

bool_expr:
  | e1 = expr; LESS; e2 = expr       { Less (e1, e2) }
  | e1 = expr; EQUALEQUAL; e2 = expr { Equal (e1, e2) }


func_def:
  | FUN; f = IDENT; LPAREN; x = IDENT; RPAREN; LBRACE; e = expr; RBRACE { Fun(f, x, e) }


%%
