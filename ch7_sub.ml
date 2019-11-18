type term =
  | TmVar of int * int
  | TmAbs of string * term
  | TmApp of term * term
  | TmWrong
