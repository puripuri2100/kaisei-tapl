type symbol =
  | NtmlTerm
  | TmlTrue
  | TmlFalse
  | TmlIf
  | TmlZero
  | TmlSucc
  | TmlPred
  | TmlIisZero
  | TmlEnd

let string_to_stack s =
  let rec exp i st =
    if i < 0 then st else exp (i - 1) (Stack.push s.[i] st) in
  exp (String.length s - 1) Stack.create()
  (* 最初に読んだものをtopに持ってくるには最後からstringを読む *)



let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let string_from_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf
  (* サイトから拾ってきたのでよくわからない *)

let try_pop st =
  let ch'opt =
    try let item = Stack.pop st in Some(item)
    with Stack.Empty -> None

let rec read_till_nalpha st chars =
  match try_pop st with
    | Some(c) when is_alpha c -> read_till_nalpha st c::chars
    | _ -> List.rev chars
  (* 読んだものを右につけていくので最後に反転させる *)

let list_to_stack l =
  let rec exp i st =
    if i < List.length l then exp (i+1) (Stack.push s.[i] st) else st in
  exp 0 Stack.create() 
  (* lex_analyzeのlexは読んだトークンをそのまま先頭につけてくので結果的に本来と逆順の列が出てくるので
  スタックに整形するときは逆にそのまま積んでいけばtopに一番最初に読んだトークンが来る。これが望ましい。 *)

let lex_analyze s =
  let chsta = string_to_stack ch in
  let rec lex l =
    let cch try let ch = Stack.pop chsta in Some
    match try_pop chsta with
      | Some(c) when is_alpha c -> 
        let str = string_from_chars (read_till_nalpha chsta [c]) in
        match str with
          | "if" -> lex TmlIf::l
          | "true" -> lex TmTrue::l
          | "false" -> lex TmlFalse::l
          | "succ" -> lex TmlSucc::l
          | "pred" -> lex TmlPred::l
          | "zero" -> lex TmlZero::l
          | "is_zero" -> lex TmlIsZero::l
      | Some(_) -> lex l
      | None -> list_to_stack l (*読み終わったらスタックにして返す*)
  in
  lex []

(* let read_term sym_stack input_stack =
  match try_pop input_stack with
    | TmlTrue -> Tm *)

(* let parse s =
  let input_stack = lex_analyze s in
  let sym_stack : symbol Stack.t = Stack.create() in
  Stack.push TmlEnd stack;
  Stack.push NtmlTerm stack;
  let read input_ch =
    match (Stack.pop stack) with
    | SymChar(ch) when (ch==input_ch) -> ()
    | SymTrue -> 
  List.iter  *)