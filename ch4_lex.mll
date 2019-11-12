{
  open Ch4_parse
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum = digit | alpha

rule lex = parse
  | space   { lex lexbuf }
  | "if"  {TmlIf}
  | "true"  {TmlTrue}
  | "false" {TmlFalse}
  | "succ"  {TmlSucc}
  | "pred"  {TmlPred}
  | "zero"  {TmlZero}
  | "is_zero" {TmlIsZero}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | eof {EOF}
