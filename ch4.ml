type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term


exception NoRuleApplies


let rec isnumericval t =
  match t with
  | TmZero -> true
  | TmSucc(t1) -> isnumericval t1
  | _ -> false


let rec isval t =
  match t with
  | TmTrue -> true
  | TmFalse -> true
  | t when isnumericval t -> true (*数値であればtrue*)
  | _ -> false 


let rec eval1 t =
  match t with
  | TmIf (TmTrue, t2, _) -> t2
  | TmIf (TmFalse, _, t3) -> t3
  | TmIf (t1, t2, t3) ->
    let t1' = eval1 t1 in
    TmIf(t1', t2, t3)
  | TmSucc (t1) ->
    let t1' = eval1 t1 in
    TmSucc(t1')
  | TmPred(TmZero) -> TmZero
  | TmPred(TmSucc(nv1)) when (isnumericval nv1) -> nv1
  | TmPred(t1) ->
    let t1' = eval1 t1 in
    TmPred(t1')
  | TmIsZero(TmZero) -> TmTrue
  | TmIsZero(TmSucc(nv1)) when (isnumericval nv1) -> TmFalse
  | TmIsZero(t1) ->
    let t1' = eval1 t1 in
    TmIsZero(t1')
  | _ -> raise NoRuleApplies

(*
let rec eval t =
  try let t' = eval1 t in eval t'
  with NoRuleApplies -> t
*)

let rec eval t =
  let t'opt =
    try let t' = eval1 t in Some(t')
    with NoRuleApplies -> None
  in
  match t'opt with
  | Some(t') -> eval t'
  | None -> t


let rec show t =
  match t with
  | TmTrue -> "TmTrue"
  | TmFalse -> "TmFalse"
  | TmIf(t1, t2, t3) -> "TmIf(" ^ show t1 ^ ", " ^ show t2 ^ ", " ^ show t3 ^ ")"
  | TmZero -> "TmZero"
  | TmSucc(t1) -> "TmSucc(" ^ show t1 ^ ")"
  | TmPred(t1) -> "TmPred(" ^ show t1 ^ ")"
  | TmIsZero(t1) -> "TmIsZero(" ^ show t1 ^ ")"


let main =
  let t = TmIf(TmIsZero(TmSucc(TmZero)), TmSucc(TmZero), TmZero) in
  Printf.printf "%s\n%s\n%s\n%s\n"
    (t |> show)
    (t |> eval1 |> show)
    (t |> eval1 |> eval1 |> show)
    (t |> eval |> show)

