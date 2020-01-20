type term =
        | TmVar of string * ty
        | TmAbs of string * ty * term
        | TmApp of term * term

type ty =
        | TyArr of ty * ty
        | TyBool

type context = (string * ty) list
