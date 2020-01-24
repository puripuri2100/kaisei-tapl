open Ch10_base
open Ch10_sub

let rec serch_str_from_ctx ctx str =
    match ctx with
        | (s,ty)::tail when str = s -> true
        | (s,ty)::tail -> serch_str_from_ctx tail str
        | [] -> false

let add_ctx ctx s ty =
    match s with 
        | s when serch_str_from_ctx ctx s -> make_error "Name Conflict";
        | s -> (s,ty)::ctx

let rec get_type_from_ctx (ctx : context) str =
    match ctx with
        |((s,t)::tail) when (=) str s -> t
        |(s,_)::tail -> get_type_from_ctx tail str
        | [] -> make_error (str^" does not exist in the context.")


let rec typeof ctx term =
    match term with
        | TypedVar(id) -> (get_type_from_ctx ctx id)
        | TypedAbs(x,tyT1,t2) ->
            let tyT2 = typeof (add_ctx ctx x  tyT1) t2 in
            TyArr(tyT1,tyT2)
        | TypedApp(t1,t2) ->
            let tyT1 = typeof ctx t1 in
            let tyT2 = typeof ctx t2 in
            (match tyT1 with
                TyArr(tyT11,tyT12) ->
                    if (=) tyT2 tyT11 then tyT12
                    else make_error "wrong type argument";
                | _ -> make_error "LHS is not function";
            )
        | TypedTrue -> TyBool
        | TypedFalse -> TyBool
        | TypedIf(c,t1,t2) ->
            if (<>) (typeof ctx c) TyBool then make_error "Condition should be Boolean";
            match (typeof ctx t1) with
                | tyT1 when (=) tyT1 (typeof ctx t2) -> tyT1
                | _ -> make_error "Type of then-expr should be equal to else-expr";
