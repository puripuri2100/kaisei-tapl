let read_chan chan = 
  let str = ref "" in
  try
    while true do
      str := !str ^ (input_line chan) ^ "\n"
    done ; !str 
  with End_of_file ->
      !str

let main =
  for i = 1 to 7 do
    Printf.printf "\n%s\n" ("--- TEST"^(string_of_int i)^" BEGIN ---");
    let tmp_file = Filename.temp_file "" ".txt" in
    let _ = Sys.command ("../../_build/ch10 -f test" ^ (string_of_int i) ^  ".txt > " ^ tmp_file) in
    let chan = open_in tmp_file in
    let s = read_chan chan in
    let () = close_in chan in
    Printf.printf "%s" s;
    Printf.printf "%s\n" ("--- TEST"^(string_of_int i)^" END   ---");
  done