exception NotInContext

let rec get_type_from_ctx ctx str =
    match ctx with
        |(s,t)::tail when std=s -> t
        |(s,_)::tail -> get_type_from_ctx tail str
        | _ -> raise NotInContext




exception ParameterMissMatch
exception ArrowTyExpected

let typeof ctx term =
    match t with
        TmVar(id,_) -> 
            get_type_from_ctx ctx id
        | TmAbs(x,tyT1,t2) ->
            let ctx' = (x,tyT1)::ctx in
            let tyT2 = typeof ctx' t2 in
            TyArr(tyT1,tyT2)
        | TmApp(t1,t2) ->
            let tyT1 = typeof ctx t1 in
            let tyT2 = typeof ctx t2 in
            (match tyT1 with
                TyArr(tyT11,tyT12) ->
                    if (=) tyT2 tyT11 then ty T12
                    else Printf.printf "%s\n" "error";raise ParameterMissMatch
                | _ -> Printf.printf "%s\n" "error";raise ArrowTyExpected
            )
        | TmTrue -> TyBool
        | TmFalse -> TyBool
