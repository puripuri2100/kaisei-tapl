open Tapl_base

open Ch7_sub
open Ch7_parse
open Ch7_lex

type binding = NameBind


type context = (string * binding) list


let rec pickfrechname lst str =
  let new_str = str ^ "'" in
  let check (s, _) = (s = str) in
    if List.exists check lst then
      pickfrechname lst new_str
    else
      let new_ctx = (str, NameBind) :: lst in
      (new_ctx, str)


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
      index2name ctx x |> get_option "[bad index1]"
    else
      "[bad index2]"
  | TmWrong -> "TmWrong"


let termShift d t =
  let rec walk c t =
    match t with
    | TmVar(x, n) when (x >= c) -> TmVar(x + d, n + d)
    | TmVar(x, n) -> TmVar(x, n + d)
    | TmAbs(x, t1) -> TmAbs(x, walk(c + 1) t1)
    | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
    | TmWrong -> TmWrong
  in
    walk 0 t


let termSubst j s t =
  let rec walk c t =
    match t with
    | TmVar(x, n) when (x = j + c) -> termShift c s
    | TmVar(x, n) -> TmVar(x, n)
    | TmAbs(x, t1) -> TmAbs(x, walk (c + 1) t1)
    | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
    | TmWrong -> TmWrong
  in
    walk 0 t


let termSubstTop s t =
    termShift (-1) (termSubst 0 (termShift 1 s) t)


let rec isval ctx t =
  match t with
  | TmAbs(_, _) -> true
  | TmWrong -> true
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


let show ctx t =
  Printf.printf "%s\n" (term_to_string ctx t)


let rec show_step_by_step ctx t =
  match t with
  | v when isval ctx v -> show ctx t
  | term ->
    let t' = eval1 ctx t in
    let _ = show ctx t in show_step_by_step ctx t'



let main_of_file file_name =
  let channel = open_in file_name in
  let t = channel |> Lexing.from_channel |> parse lex in
  let ctx = [] in
    try  t |> show ctx with
      | Sys_error _ -> Printf.printf "%s\n" (make_error (NoSuchFile(file_name)))
      | Parsing.Parse_error ->  Printf.printf "%s\n" (make_error ParserError)
      | Failure _ -> Printf.printf "%s\n" (make_error LexerError)


let main_of_string str =
  let t = str |> Lexing.from_string |> parse lex in
  let ctx = [] in
    try  t |> show_step_by_step ctx with
      | Parsing.Parse_error ->  Printf.printf "%s\n" (make_error ParserError)
      | Failure _ -> Printf.printf "%s\n" (make_error LexerError)


let arg_spec =
  [
    ("-f",     Arg.String main_of_file,   "input text file");
    ("--file", Arg.String main_of_file,   "input text file");
    ("-t",     Arg.String main_of_string, "input text"     );
    ("--text", Arg.String main_of_string, "input text"     );
  ]


let main =
(*
  (λ x. λ x'. x) (λ x. x) => λ x'. λ x. x
*)
(*
  let t = TmApp(TmAbs("x", TmApp(TmVar(0, 1), TmVar(0, 1))), TmAbs("x", TmApp(TmVar(0, 1), TmVar(0, 1)))) in
  let ctx = [] in
    show ctx t 
*)
  Arg.parse arg_spec (fun file_name -> main_of_file file_name) ""

