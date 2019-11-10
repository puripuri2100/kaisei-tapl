type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

type rule =
  | E_IfTrue
  | E_IfFalse
  | E_If
  | E_Succ
  | E_PredSucc
  | E_PredZero
  | E_Pred
  | E_IsZeroZero
  | E_IsZeroSucc
  | E_IsZero


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
  let rec to_string t_ = match t_ with
  | TmTrue -> "TmTrue"
  | TmFalse -> "TmFalse"
  | TmIf(t1, t2, t3) -> "TmIf(" ^ to_string t1 ^ ", " ^ to_string t2 ^ ", " ^ to_string t3 ^ ")"
  | TmZero -> "TmZero"
  | TmSucc(t1) -> "TmSucc(" ^ to_string t1 ^ ")"
  | TmPred(t1) -> "TmPred(" ^ to_string t1 ^ ")"
  | TmIsZero(t1) -> "TmIsZero(" ^ to_string t1 ^ ")"
  in
  Printf.printf "%s\n" (to_string t)


let rec show_step_by_step t =
  let t'opt =
    try let t' = eval1 t in Some(t')
    with NoRuleApplies -> None
  in
  match t'opt with
  | Some(t') -> show t; let _ = read_line () in show_step_by_step t'
  | None -> show t


let main =
  let t = TmIf(TmIsZero(TmSucc(TmZero)), TmSucc(TmZero), TmZero) in
  (* t |> show;
  t |> eval1 |> show;
  t |> eval1 |> eval1 |> show;
  t |> eval |> show; *)
  t |> show_step_by_step