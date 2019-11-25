type term =
  | TmVar of int
  | TmAbs of term
  | TmApp of term * term
  | TmWrong

type labeledterm =
  | LtmVar of string
  | LtmAbs of string * labeledterm
  | LtmApp of labeledterm * labeledterm

type binding = NameBind

type context = (string * binding) list
