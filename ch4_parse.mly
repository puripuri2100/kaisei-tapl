%{
  open Ch4_sub
%}

%token TmlTrue TmlFalse TmlZero TmlSucc TmlPred TmlIsZero TmlEnd
%token TmlIf Then Else
%token LPAREN RPAREN
%token EOF

%start parse
%type <Ch4_sub.term> parse

%%

parse :
  | term EOF { $1 }
term :
  | TmlTrue { TmTrue }
  | TmlFalse { TmFalse }
  | TmlIf term Then term Else term { TmIf($2, $4, $6) }
  | TmlZero { TmZero }
  | TmlSucc term { TmSucc($2) }
  | TmlPred term { TmPred($2) }
  | TmlIsZero term { TmIsZero($2) }
  | LPAREN term RPAREN {$2}

%%
