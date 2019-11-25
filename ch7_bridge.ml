open Ch7_sub

exception UndefinedName

let serch_str ctx str =
    let rec serch ctx' cnt =
        match ctx' with
            | (s,_)::tail when s=str -> cnt
            | (s,_)::tail -> (serch tail cnt+1) 
            | [] -> raise UndefinedName
    in
    (serch ctx 0)

let rec remove_name ctx term =
    match term with
        | LtmVar(s) -> TmVar((serch_str ctx s))
        | LtmAbs(s,t) -> TmAbs((remove_name ((s,NameBind)::ctx) t))
        | LtmApp(t1,t2) -> TmApp((remove_name ctx t1),(remove_name ctx t2))