let rec pr_str = function
  | Types.List x ->
    "(" ^ (String.concat " " (List.map pr_str x)) ^ ")"
  | Types.Integer x ->
    string_of_int x
  | Types.Symbol x -> 
    x
