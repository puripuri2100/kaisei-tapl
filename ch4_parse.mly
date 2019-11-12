%{
  open Ch4_sub
%}

%token TmlTrue TmlFalse TmlIf TmlZero TmlSucc TmlPred TmlIsZero TmlEnd
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
  | TmlIf term term term { TmIf($2, $3, $4) }
  | TmlZero { TmZero }
  | TmlSucc term { TmSucc($2) }
  | TmlPred term { TmPred($2) }
  | TmlIsZero term { TmIsZero($2) }
  | LPAREN term RPAREN {$2}

%%
