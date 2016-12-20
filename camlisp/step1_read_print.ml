let line_breaked str = str ^ "\n"

let read str = Reader.read_str str 

let eval str = str

let print exp = Printer.pr_str exp

let rep str =
  match read str with
  | Ok form -> print (eval form)
  | Error msg -> msg

let () =
  while true do
    print_string "user> ";
    let line = try
        read_line ()
      with 
        End_of_file -> print_string "See you...\n"; exit 0
    in
    print_string (line_breaked (rep line))
  done
