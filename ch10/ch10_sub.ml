type ty =
        | TyArr of ty * ty
        | TyBool


type typed_term =
        | TypedVar of string
        | TypedAbs of string * ty * typed_term
        | TypedApp of typed_term * typed_term
        | TypedTrue
        | TypedFalse
        | TypedIf of typed_term * typed_term * typed_term
        

    
type term =
        | TmVar of string
        | TmAbs of string * term
        | TmApp of term * term
        | TmTrue
        | TmFalse
        | TmIf of term * term * term
        




type context = (string * ty) list
