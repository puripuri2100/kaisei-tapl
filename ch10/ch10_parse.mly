%{
  open Ch10_sub
%}

%token <string>STR
%token TRUE FALSE TY_BOOL
%token LAMBDA DOT TERM COLON ARROW IF ELSE LBRACE RBRACE
%token LPAREN RPAREN
%token EOF

%start program
%type <Ch10_sub.typed_term> program

%%


program :
  | term EOF { $1 }
term :
  | var {$1}
  | app {$1}
  | LAMBDA STR COLON ty DOT term {TypedAbs($2,$4,$6)}
  | TRUE {TypedTrue}
  | FALSE {TypedFalse}
  | if_ {$1}
  | pterm {$1}
pterm :
  | LPAREN term RPAREN {$2}
var :
  | STR {TypedVar($1)}
septerm :
  | pterm {$1}
  | var {$1}
app :
  | septerm septerm {TypedApp($1,$2)}
  | app septerm {TypedApp($1,$2)}
if_ :
  | IF pterm LBRACE term RBRACE ELSE LBRACE term RBRACE {TypedIf($2,$4,$8)}
ty :
  | TY_BOOL {TyBool}
  | LPAREN ty RPAREN {$2}
  | LPAREN ty ARROW ty RPAREN {TyArr($2,$4)}

%%