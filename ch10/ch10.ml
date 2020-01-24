open Ch10_sub

open Ch10_typechecking
open Ch10_parse
open Ch10_lex
open Ch10_bridge
open Ch10_base

let rec is_val term =
    match term with
        | TmVar(s) -> true
        | TmAbs(_1,_2) -> true 
        | TmTrue -> true
        | TmFalse -> true
        | _ -> false


let rec term_subst term dist value =
    match term with
        | TmVar(s) when (=) s dist -> value
        | TmAbs(s,t) -> TmAbs(s,term_subst t dist value)
        | TmApp(t1,t2) -> TmApp(term_subst t1 dist value,term_subst t2 dist value)
        | x -> x

let rec eval1 term =
    match term with
        | TmApp(TmAbs(s,t),v) when is_val v -> term_subst t s v
        | TmApp(v,t) when is_val v -> TmApp(v,eval1 t)
        | TmApp(t1,t2) -> TmApp(eval1 t1,t2)
        | x -> x

let rec term_to_string term =
    match term with
        | TmVar(s) -> s
        | TmAbs(s,t) -> "λ " ^ s ^ ". " ^ term_to_string t
        | TmApp(t1,t2) -> "( " ^ term_to_string t1 ^ " ) ( " ^ term_to_string t2 ^ " )"
        | TmTrue -> "TRUE"
        | TmFalse -> "FALSE"


let show term = 
    Printf.printf "%s\n" (term_to_string term)

let rec show_step_by_step term = 
    match term with
        | v when (is_val v) -> Printf.printf "%s\n" (term_to_string v)
        | term ->  Printf.printf "%s\n" (term_to_string term); show_step_by_step (eval1 term)



let main_of_file file_name =
  let channel = open_in file_name in
  let t = channel |> Lexing.from_channel |> program lex in
  let ctx = [] in
  let _ = typeof ctx t in
  try erase_type t |> show_step_by_step with
      | Sys_error _ -> make_error "NO SUCH FILE"
      | Parsing.Parse_error ->  make_error "PARSE ERROR"
      | Failure _ -> make_error "LEX ERROR"

let main_of_string str =
  let t = str |> Lexing.from_string |> program lex  in
  let ctx = [] in
  let _ = typeof ctx t in
  try erase_type t |> show_step_by_step with
      | Parsing.Parse_error ->  make_error "PARSE ERROR"
      | Failure _ -> make_error "LEX ERROR"

let speclist =
  [
     ("-f",     Arg.String main_of_file,   "input text file");
     ("-t",     Arg.String main_of_string, "input text"     );
  ]
      


let usagemsg =
  "Kaisei-Taplにusageはありません。残念です。"

let main =
  Arg.parse speclist (fun file_name -> main_of_file file_name) usagemsg