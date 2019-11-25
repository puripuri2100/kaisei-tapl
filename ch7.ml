type term =
  | TmVar of int * int
  | TmAbs of string * term
  | TmApp of term * term


type binding = NameBind


type context = (string * binding) list


let rec pickfrechname lst str =
  let new_str = str ^ "'" in
  let check (s, _) = (s = new_str) in
    if List.exists check lst then
      pickfrechname lst new_str
    else
      let new_ctx = (new_str, NameBind) :: lst in
      (new_ctx, new_str)


let index2name ctx t =
  if List.length ctx <= t then
    None
  else
    Some(List.nth ctx t |> (fun (s, _) -> s))


let get_option a opt =
  match opt with
  | Some(a') -> a'
  | None -> a


let ctxlength ctx = List.length ctx


let rec term_to_string ctx t =
  match t with
  | TmAbs(x, t1) ->
    let (ctx', x') = pickfrechname ctx x in
    "(λ " ^  x' ^ ". " ^ term_to_string ctx' t1 ^ ")"
  | TmApp(t1, t2) ->
    "(" ^ term_to_string ctx t1 ^ ". " ^ term_to_string ctx t2 ^ ")"
  | TmVar(x, n) ->
    if ctxlength ctx = n then
      index2name ctx x |> get_option "[bad index]"
    else
      "[bad index]"


let termShift d t =
  let rec walk c t =
    match t with
    | TmVar(x, n) when (x >= c) -> TmVar(x + d, n + d)
    | TmVar(x, n) -> TmVar(x, n + d)
    | TmAbs(x, t1) -> TmAbs(x, walk(c + 1) t1)
    | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
  in
    walk 0 t


let termSubst j s t =
  let rec walk c t =
    match t with
    | TmVar(x, n) when (x = j + c) -> termShift c s
    | TmVar(x, n) -> TmVar(x, n)
    | TmAbs(x, t1) -> TmAbs(x, walk (c + 1) t1)
    | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
  in
    walk 0 t


let termSubstTop s t =
    termShift (-1) (termSubst 0 (termShift 1 s) t)


let rec isval ctx t =
  match t with
  | TmAbs(_, _) -> true
  | _ -> false


let rec eval1 ctx t =
  match t with
  | TmApp(TmAbs(x, t12), v2) when isval ctx v2 ->
    termSubstTop v2 t12
  | TmApp(v1, t2) when isval ctx v1 ->
    let t2' = eval1 ctx t2 in
    TmApp(v1, t2')
  | TmApp(t1, t2) ->
    let t1' = eval1 ctx t1 in
    TmApp(t1', t2)
  | _ -> TmWrong


let main =
  let t = TmApp(TmAbs("x", TmVar(0, 1)), TmAbs("y", TmVar(0, 1))) in
  let ctx = [] in
    eval1 ctx t |> term_to_string ctx |> Printf.printf "%s\n"