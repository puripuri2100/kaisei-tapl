%{
  open Ch7_sub
%}

%token VAR
%token <string>STR
%token LAMBDA DOT TERM
%token LPAREN RPAREN
%token EOF

%start parse
%type <Ch7_sub.labeledterm> parse

%%


parse :
  | term EOF { $1 }
term :
  | var {$1}
  | app {$1}
  | LAMBDA STR DOT term {LtmAbs($2, $4)}
pterm :
  | LPAREN term RPAREN {$2}
var :
  | STR {LtmVar($1)}
septerm :
  | pterm {$1}
  | var {$1}
app :
  | septerm septerm {LtmApp($1,$2)}
  | app septerm {LtmApp($1,$2)}

%%
