open Ch10_base
open Ch10_sub

let serch_str_from_ctx ctx str =
    match ctx with
        | (s,ty)::tail when str = s -> true
        | (s,ty)::tail -> serch_str_from_ctx tail str
        | [] -> false

let add_ctx ctx bind =
    match bind with 
        | (s,ty) when serch_str_from_ctx ctx s -> make_error "Name Conflict"
        | b -> b::ctx

let rec get_type_from_ctx ctx str =
    match ctx with
        |(s,t)::tail when str=s -> t
        |(s,_)::tail -> get_type_from_ctx tail str
        | [] -> make_error str^" does not exist in the context."


let typeof ctx term =
    match t with
        | TypedVar(id) -> 
            get_type_from_ctx ctx id
        | TypedAbs(x,tyT1,t2) ->
            let ctx' = (add_cxt (x,tyT1)) in
            let tyT2 = typeof ctx' t2 in
            TyArr(tyT1,tyT2)
        | TypedApp(t1,t2) ->
            let tyT1 = typeof ctx t1 in
            let tyT2 = typeof ctx t2 in
            (match tyT1 with
                TyArr(tyT11,tyT12) ->
                    if (=) tyT2 tyT11 then ty T12
                    else make_error "%s\n" "error";
                | _ -> make_error "%s\n" "error";
            )
        | TypedTrue -> TyBool
        | TypedFalse -> TyBool
