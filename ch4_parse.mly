%{
  open Ch4_sub
%}

%token True False Zero Succ Pred IsZero End
%token If Then Else
%token LPAREN RPAREN
%token EOF

%start parse
%type <Ch4_sub.term> parse

%%

parse :
  | term EOF { $1 }
term :
  | True { TmTrue }
  | False { TmFalse }
  | If term Then term Else term { TmIf($2, $4, $6) }
  | Zero { TmZero }
  | Succ term { TmSucc($2) }
  | Pred term { TmPred($2) }
  | IsZero term { TmIsZero($2) }
  | LPAREN term RPAREN {$2}

%%
