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
%token FOR
%token EXTERN
%token RETURN
%token EOF

%start program
%type <Syntax.toplevel_cmd list> program

%start toplevel
%type <Syntax.toplevel_cmd> toplevel

(* the order of the following is important to define precedence *)
%nonassoc ELSE
%nonassoc EQUALEQUAL
%nonassoc LESS
%left PLUS MINUS
%left MULT DIV
%%

program:
 | EOF                                      { [] }
 | t = toplevel; l = program;               { t :: l }

toplevel:
 | s = stmt;                                   { Stmt s }
 | f = func_def;                               { f }
 | e = extern_def; SEMI                        { e }

stmt:
 | e = expr; SEMI;                             { Expr e }
 | LET; x = IDENT; EQUAL; e = expr; SEMI;      { Let (x, e) }
 | f = for_expr                                { f }
 | x = IDENT; EQUAL; e = expr; SEMI            { Assign(x, e) }
 | RETURN; e = expr; SEMI                      { Return e }

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

for_expr:
  | FOR; i = IDENT; EQUAL; start = expr; COMMA; final = expr; LBRACE; body = stmt_seq; RBRACE
    { For(i, start, final, None, body) }
  | FOR; i = IDENT; EQUAL; start = expr; COMMA; final = expr; COMMA; step = expr; LBRACE; body = stmt_seq; RBRACE
    { For(i, start, final, Some step, body) }

func_def:
  | FUN; f = IDENT; LPAREN; args = ident_list; RPAREN; LBRACE; body = stmt_seq; RBRACE
    { Fun(f, args, body) }

extern_def:
  | EXTERN; f = IDENT; LPAREN; args = ident_list; RPAREN;
    { Extern(f, args) }

stmt_seq:
  | s = stmt;                        { [s] }
  | s = stmt; t = stmt_seq           { s :: t }

expr_list:
  |                                  { []    }
  | e = expr;                        { [e]   }
  | e = expr; COMMA; el = expr_list  { e::el }

ident_list:
  |                                   { []    }
  | i = IDENT;                        { [i]   }
  | i = IDENT; COMMA; il = ident_list { i::il }
%%
