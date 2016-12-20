module rec Types
  : sig
    type t = 
    | Integer of int
    | Symbol of string
    | List of t list
    | Bool of bool
    | Nil
    | Keyword of string
    | String of string
    | Vector of t list
    | Map of t MalMap.t
  end = Types

and MalValue
  : sig
    type t = Types.t
    val compare : t -> t -> int
  end
  = struct
    type t = Types.t
    let compare = Pervasives.compare
  end

and MalMap
  : Map.S with type key = MalValue.t
  = Map.Make(MalValue)

type mal_type = MalValue.t
