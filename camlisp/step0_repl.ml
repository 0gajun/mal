let line_breaked str = str ^ "\n"

let read str = str

let eval str = str

let print exp = exp

let rep str =
  read str
  |> eval
  |> print

let () =
  try
    while true do
      print_string "user> ";
      read_line ()
      |> rep
      |> line_breaked
      |> print_string
    done
  with
    End_of_file -> print_string "See you...\n"
