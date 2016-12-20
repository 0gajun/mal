let enclose pre post str = pre ^ str ^ post

let rec pr_str = function
  | Types.List x ->
    (String.concat " " (List.map pr_str x))
    |> enclose "(" ")"
  | Types.Integer x ->
    string_of_int x
  | Types.Symbol x -> x
  | Types.Nil -> "nil"
  | Types.Bool x -> string_of_bool x
  | Types.Vector x ->
    String.concat " " (List.map pr_str x)
    |> enclose "[" "]"
  | Types.Keyword x -> ":" ^ x
  | Types.String x ->
    Str.full_split (Str.regexp "[\n|\\\\|\\\\\"]") x
    |> List.map (function | Str.Delim x -> "\\" ^ x | Str.Text x -> x)
    |> String.concat ""
    |> enclose "\"" "\""
