type error_type =
  | NoSuchFile of string
  | WrongNumberOfArguments


let make_error error =
  let message =
    match error with
    | NoSuchFile(file_name) -> "No such file: " ^ file_name
    | WrongNumberOfArguments -> "Wrong number of arguments"
  in
  let main = "![Error]  " ^ message in
    main
