{
  open Ch7_parse
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum = digit | alpha
let str = [^ ' ' '\t' '\n' '\r' '@' '`' '\\' '{' '}' '<' '>' '%' '|' '*' '$' '#' ';']

rule lex = parse
  | space   { lex lexbuf }
  | "\\" {LAMBDA}
  | "." {DOT}
  | alpha+ as s {STR(s)}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | eof {EOF}
