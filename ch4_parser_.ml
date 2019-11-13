open Ch4_token_def

type symbol =
  | NtmlTerm
  | TmlTrue
  | TmlFalse
  | TmlIf
  | TmlThen
  | TmlElse
  | TmlZero
  | TmlSucc
  | TmlPred
  | TmlIsZero
  | TmlEnd
  | TmlStr of string

let string_to_stack s =
  let rec exp i st =
    if i < 0 then st else exp (i - 1) (Stack.push s.[i] st;st) in
  exp (String.length s - 1) (Stack.create())
  (* 最初に読んだものをtopに持ってくるには最後からstringを読む *)



let is_alpha :char -> bool = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let string_from_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf
  (* サイトから拾ってきたのでよくわからない *)

let try_pop st =
  try let item = (Stack.pop st) in Some(item)
  with Stack.Empty -> None

let rec read_till_nalpha (st : char Stack.t) (chars : char list) =
  match try_pop st with
    | Some(c) when is_alpha c -> (read_till_nalpha st (c :: chars))
    | _ -> List.rev chars
  (* 読んだものを右につけていくので最後に反転させる *)

let list_to_stack l =
  let rec exp i st =
    if i <= List.length l then exp (i+1) (Stack.push (List.nth l i) st;st) else st in
  exp 1 (Stack.create()) 
  (* lex_analyzeのlexは読んだトークンをそのまま先頭につけてくので結果的に本来と逆順の列が出てくるので
  スタックに整形するときは逆にそのまま積んでいけばtopに一番最初に読んだトークンが来る。これが望ましい。 *)

let lex_analyze (s : string) =
  let chsta = string_to_stack s in
  let rec lex l =
    match (try_pop chsta) with
      | Some(c) when is_alpha c -> 
        let str = string_from_chars (read_till_nalpha chsta [c]) in
        (match str with
          | "if" -> lex (TmlIf::l)
          | "then" -> lex (TmlThen::l)
          | "else" -> lex (TmlElse::l)
          | "true" -> lex (TmlTrue::l)
          | "false" -> lex (TmlFalse::l)
          | "succ" -> lex (TmlSucc::l)
          | "pred" -> lex (TmlPred::l)
          | "zero" -> lex (TmlZero::l)
          | "is_zero" -> lex (TmlIsZero::l)
          | str -> lex (TmlStr(str)::l))
      | Some(_) -> lex l
      | None -> list_to_stack (TmlEnd::l) (*読み終わったらスタックにして返す*)
  in
  lex []


exception SyntaxError

let rec read_term sym_stack input_stack =
  match try_pop input_stack with
    | Some(TmlTrue) -> TmTrue
    | Some(TmlFlase) -> TmFlase
    | Some(TmlIf) -> 
      let t1 = read_till_then sym_stack input_stack in
      let t2 = read_till_else sym_stack input_stack in
      let t3 = read_term sym_stack input_stack in
      TmIf(
        t1,t2,t3
      )
    | Some(TmlZero) -> TmZero
    | Some(TmlSucc) -> TmSucc(read_term sym_stack input_stack)
    | Some(TmlPred) -> TmPred(read_term sym_stack input_stack)
    | Some(TmlIsZero) -> TmIsZero(read_term sym_stack input_stack)
    | _ -> 
and read_till_then sym_stack input_stack =
  let t = read_term sym_stack input_stack in
  match try_pop input_stack with
    | Some(TmlThen) -> t
    | _ -> raise SyntaxError
and read_till_else sym_stack input_stack =
  let t = read_term sym_stack input_stack in
  match try_pop input_stack with
    | Some(TmlElse) -> t
    | _ -> raise SyntaxError
    

let parse s =
  let input_stack = lex_analyze s in
  let sym_stack : symbol Stack.t = Stack.create() in
  Stack.push TmlEnd stack;
  Stack.push NtmlTerm stack;
  try let t = read_term sym_stack input_stack in Some(t)
  with SyntaxError -> print_string "gomi!" ; None