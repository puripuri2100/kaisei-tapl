open Tapl_base

open Ch10_sub
open Ch10_typechecking
open Ch10_bridge

let rec is_val term =
    match term with
        | TmVar(s) -> true
        | TmAbs(_1,_2) -> true 
        | TmTrue -> true
        | TmFalse -> true


let term_subst term dist value =
    match term with
        | TmVar(s) when (=) s dist -> value
        | TmAbs(s,t) -> TmAbs(s,term_subst t dist value)
        | TmApp(t1,t2) -> TmApp(term_subst t1 dist value,term_subst t2 dist value)
        | x -> x

let rec eval1 term = //型情報なし
    match term with
        | TmApp(TmAbs(s,t),v) when is_val v -> term_subst t s v
        | TmApp(v,t) when is_val v -> TmApp(v,eval1 t)
        | TmApp(t1,t2) -> TmApp(eval1 t1,t2)
        | x -> x

let term_to_string term =
    match term with
        | TmVar(s) -> s
        | TmAbs(s,t) -> "λ. " ^ term_to_string t
        | TmApp(t1,t2) -> "( " ^ term_to_string t1 ^ " ) ( " ^ term_to_string t2 ^ " )"
        | TmTrue -> "TRUE"
        | TmFalse -> "FALSE"


let show term = 
    Printf.printf "%s\n" (term_to_string term)

let rec show_step_by_step term = 
    match term with
        | v when is_val v -> { Printf.printf "%s\n" (term_to_string v)}
        | term -> { Printf.printf "%s\n" (term_to_string term); show_step_by_step (eval1 term)}

let main =
    let typed_term = TypedTrue in
    let () = typeof [] typed_term in
    let term = erase_type typed_term in
    show_step_by_step term;
     


