let main =
  for i = 1 to 6 do
    Printf.printf "\n%s\n" ("--- TEST"^(string_of_int i)^" BEGIN ---");
    let tmp_file = Filename.temp_file "" ".txt" in
    let _ = Sys.command ("../../_build/ch10 -f test" ^ (string_of_int i) ^  ".txt > " ^ tmp_file) in
    let chan = open_in tmp_file in
    let s = input_line chan in
    let () = close_in chan in
    Printf.printf "%s\n" s;
    Printf.printf "%s\n" ("--- TEST"^(string_of_int i)^" END   ---");
  done