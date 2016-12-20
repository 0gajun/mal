type mal_type =
  | Integer of int
  | Symbol of string
  | List of mal_type list
  | Bool of bool
  | Nil
  | Keyword of string
  | String of string
  | Vector of mal_type list
