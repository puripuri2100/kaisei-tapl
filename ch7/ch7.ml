
open Ch7_base

open Ch7_sub
open Ch7_parse
open Ch7_lex
open Ch7_bridge

let rec term_to_string term =
  match term with
  | TmAbs(t) ->
    "λ. " ^ term_to_string t
  | TmApp(t1, t2) ->
    "( " ^ term_to_string t1 ^ " )  ( " ^ term_to_string t2 ^ " )"
  | TmVar(n) ->
    string_of_int n
  | TmWrong -> "TmWrong"


let rec termShift d c term =
    match term with
    | TmVar(x) when (x >= c) -> TmVar(x + d)
    | TmVar(x) -> TmVar(x)
    | TmAbs(t) -> TmAbs(termShift d (c+1) t)
    | TmApp(t1, t2) -> TmApp(termShift d c t1,termShift d c t2)
    | _ -> TmWrong


let rec termSubst j s term =
    match term with
    | TmVar(x) when (x = j) -> s
    | TmVar(x) -> TmVar(x)
    | TmAbs(t1) -> TmAbs(termSubst (j+1) (termShift 1 0 s) term)
    | TmApp(t1, t2) -> TmApp(termSubst j s t1, termSubst j s t2)
    | _ -> TmWrong


let termSubstTop s t =
    termShift (-1) 0 (termSubst 0 (termShift 1 0 s) t)


let isval t =
  match t with
  | TmAbs(_) -> true
  | TmWrong -> true
  | _ -> false


let rec eval1 t =
  match t with
  | TmApp(TmAbs(t12), v2) when isval v2 ->
    termSubstTop v2 t12
  | TmApp(v1, t2) when isval v1 ->
    let t2' = eval1 t2 in
    TmApp(v1, t2')
  | TmApp(t1, t2) ->
    let t1' = eval1 t1 in
    TmApp(t1', t2)
  | _ -> TmWrong


let show t =
  Printf.printf "%s\n" (term_to_string t)


let rec show_step_by_step t =
  match t with
  | v when isval v -> show t
  | term ->
    let t' = eval1 t in
      let _ = show t in show_step_by_step t'



let main_of_file file_name =
  let channel = open_in file_name in
  let t = channel |> Lexing.from_channel |> parse lex in
  let ctx = [] in
    try  t |> (remove_name ctx) |> show_step_by_step with
      | Sys_error _ -> Printf.printf "%s\n" (make_error (NoSuchFile(file_name)))
      | Parsing.Parse_error ->  Printf.printf "%s\n" (make_error ParserError)
      | Failure _ -> Printf.printf "%s\n" (make_error LexerError)
      | UndefinedName -> Printf.printf "%s\n" "UndefinedName"


let main_of_string str =
  let t = str |> Lexing.from_string |> parse lex  in
  let ctx = [] in
    try  t |> remove_name ctx|> show_step_by_step with
      | Parsing.Parse_error ->  Printf.printf "%s\n" (make_error ParserError)
      | Failure _ -> Printf.printf "%s\n" (make_error LexerError)


let arg_spec =
  [
    ("-f",     Arg.String main_of_file,   "input text file");
    ("--file", Arg.String main_of_file,   "input text file");
    ("-t",     Arg.String main_of_string, "input text"     );
    ("--text", Arg.String main_of_string, "input text"     );
  ]

let usagemsg =
  "Kaisei-Taplにusageはありません。残念です。"

let main =
  Arg.parse arg_spec (fun file_name -> main_of_file file_name) usagemsg

