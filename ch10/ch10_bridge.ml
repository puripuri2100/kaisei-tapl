open Ch10_sub
open Ch10_base

let rec erase_type typed_term =
    match typed_term with
        | TypedVar(s,ty) -> TmVar(s)
        | TypedAbs(s,ty,t) -> TmAbs(s,(erase_type t))
        | TypedApp(typed1,typed2) -> TmApp((erase_type typed1),(erase_type typed2))
        | TypedTrue -> TmTrue
        | TypedFalse -> TmFalse
