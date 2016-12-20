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

let read_atom token =
  let first_char = token.[0] in
  match first_char with
  | '0'..'9' -> Types.Integer (int_of_string token)
  | _ -> Types.Symbol token

let rec read_list list_reader = 
  match list_reader.tokens with
  | [] -> raise End_of_file
  | token :: tokens ->
    match token with
    | ")" -> { forms = List.rev list_reader.forms; tokens = tokens }
    | _ -> let reader = read_form list_reader.tokens in
      read_list { forms = reader.form :: list_reader.forms; tokens = reader.tokens}

and read_form all_tokens = 
  match all_tokens with
  | [] -> raise End_of_file
  | token :: tokens ->
    match token with
    | "(" -> let list_reader = read_list { forms = []; tokens = tokens } in
      { form = Types.List list_reader.forms; tokens = list_reader.tokens }
    | _ -> let form = read_atom token in
      { form = form; tokens = tokens }


let read_str str =
  try
    let reader = read_form (tokenizer str) in
    Ok reader.form
  with
    End_of_file -> Error "expected ')', got EOF\r\n"
