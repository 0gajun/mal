type reader = {
  form: Types.mal_type;
  tokens: string list;
}

type list_reader = {
  forms: Types.mal_type list;
  tokens: string list;
}

let tokenizer_regexp =
  (Pcre.regexp "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)")

let tokenizer str =
  Pcre.split ~rex:tokenizer_regexp str
  |> List.filter (fun x -> String.length x > 0)

let gsub re f str = 
  Str.full_split re str
  |> List.map (function | Str.Delim x -> f x | Str.Text x -> x)
  |> String.concat ""

let read_atom token =
  match token with
  | "nil" -> Types.Nil
  | "true" -> Types.Bool true
  | "false" -> Types.Bool false
  | _ ->
    match token.[0] with
    | '0'..'9' -> Types.Integer (int_of_string token)
    | '"' -> Types.String (gsub (Str.regexp "\\\\.") 
                             (function | "\\n" -> "\n" | x -> String.sub x 1 1)
                             (String.sub token 1 ((String.length token) - 2))
                          )
    | ':' -> Types.Keyword (Str.replace_first (Str.regexp "^:") "" token)
    | _ -> Types.Symbol token

let rec read_list eol list_reader = 
  match list_reader.tokens with
  | [] -> raise End_of_file
  | token :: tokens ->
    if String.equal token eol then
      { forms = List.rev list_reader.forms; tokens = tokens }
    else 
      let reader = read_form list_reader.tokens
      in
      read_list eol { forms = reader.form :: list_reader.forms; tokens = reader.tokens}

and read_form all_tokens = 
  match all_tokens with
  | [] -> raise End_of_file
  | token :: tokens ->
    match token with
    | "(" -> let list_reader = read_list ")" { forms = []; tokens = tokens } in
      { form = Types.List list_reader.forms; tokens = list_reader.tokens }
    | "[" -> let list_reader = read_list "]" { forms = []; tokens = tokens } in
      { form = Types.Vector list_reader.forms; tokens = list_reader.tokens }
    | "\"" -> raise End_of_file (* Handling imcomplete string literal *)
    | _ -> let form = read_atom token in
      { form = form; tokens = tokens }


let read_str str =
  try
    let reader = read_form (tokenizer str)
    in
    Ok reader.form
  with
    End_of_file -> Error "expected ')', got EOF\r\n"
