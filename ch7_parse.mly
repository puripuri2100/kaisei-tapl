%{
  open Ch7_sub
%}

%token VAR
%token <string>STR
%token LAMBDA DOT TERM
%token LPAREN RPAREN
%token EOF

%start parse
%type <Ch7_sub.term> parse

%%


parse :
  | term EOF { $1 }
term :
  | STR {TmVar(0, 1)}
  | term term {TmApp($1, $2)}
  | LAMBDA STR DOT term {TmAbs($2, $4)}
  | LPAREN term RPAREN {$2}

%%
