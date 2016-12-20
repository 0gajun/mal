module T = Types.Types

let enclose pre post str = pre ^ str ^ post

let rec pr_str = function
  | T.List x ->
    String.concat " " (List.map pr_str x)
    |> enclose "(" ")"
  | T.Integer x ->
    string_of_int x
  | T.Symbol x -> x
  | T.Nil -> "nil"
  | T.Bool x -> string_of_bool x
  | T.Vector x -> 
    String.concat " " (List.map pr_str x)
    |> enclose "[" "]"
  | T.Keyword x -> ":" ^ x
  | T.String x ->
    Str.full_split (Str.regexp "[\n|\\\\|\\\\\"]") x
    |> List.map (function | Str.Delim x -> "\\" ^ x | Str.Text x -> x)
    |> String.concat ""
    |> enclose "\"" "\""
  | T.Map x ->
    Types.MalMap.fold (fun key v acc -> acc ^ (pr_str key) ^ " " ^ (pr_str v)) x ""
    |> enclose "{" "}"
