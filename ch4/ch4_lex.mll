{
  open Ch4_parse
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum = digit | alpha

rule lex = parse
  | space   { lex lexbuf }
  | "if"  {If}
  | "then" {Then}
  | "else" {Else}
  | "true"  {True}
  | "false" {False}
  | "succ"  {Succ}
  | "pred"  {Pred}
  | "zero"  {Zero}
  | "is_zero" {IsZero}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | eof {EOF}
