type error_type =
  | NoSuchFile of string
  | LexerError
  | ParserError
  | WrongNumberOfArguments


let make_error error =
  let message =
    match error with
    | NoSuchFile(file_name) -> "No such file: " ^ file_name
    | LexerError -> "Syntax Error at Lexer"
    | ParserError -> "Syntax Error at Parser"
    | WrongNumberOfArguments -> "Wrong number of arguments"
  in
  ("![Error]  " ^ message)