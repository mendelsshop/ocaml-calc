type op = Add | Sub | Mul | Div

let first_char input =
  try String.get input 0 |> Option.some with Invalid_argument _ -> Option.None

let rest_string (input : string) =
  try String.sub input 1 (String.length input - 1) |> Option.some
  with Invalid_argument _ -> Option.none

let rec string_to_chars input =
  match first_char input with
  | None -> []
  | Some f -> (
      match rest_string input with
      | Some rest -> f :: string_to_chars rest
      | None -> f :: [])

let rec eat_space input =
  match input with (' ' | '\n') :: rest -> eat_space rest | _ -> input

let char_to_op input =
  match input with
  | '+' -> Option.some Add
  | '-' -> Option.some Sub
  | '*' -> Option.some Mul
  | '/' -> Option.some Div
  | _ -> Option.none


let op_to_math op = match op with
  | Add -> Float.add
  | Sub -> Float.sub
  | Mul -> Float.mul
  | Div -> Float.div
let get_op input =
  match input |> eat_space with
  | [] -> Result.error []
  | op :: rest -> (
      match Option.map op_to_math (char_to_op op) with
      | Some op -> Result.ok (op, rest)
      | None -> Result.error rest)


let is_number input = match input with '0' .. '9' -> true | _ -> false

let rec parse_int input =
  match input with
  | number :: rest when is_number number ->
      let number = String.make 1 number in
      Result.fold
        ~ok:(fun (value, rest) -> (number ^ value, rest))
        ~error:(fun _ -> (number, rest))
        (parse_int rest)
      |> Result.ok
  | _ -> Result.error input

let parse_decimal input =
  match input with
  | '.' :: rest ->
      Result.fold
        ~ok:(fun (value, rest) -> ("." ^ value, rest))
        ~error:(fun _ -> (".", rest))
        (parse_int rest)
      |> Result.ok
  | _ -> Result.error input

let parse_number input =
  let number, rest =
    match parse_int input with
    | Ok (number, rest) -> (Some number, rest)
    | Error rest -> (None, rest)
  in
  let decimal_part = parse_decimal rest in
  let number =
    match (number, decimal_part) with
    | Some number, Ok (".", rest) -> Ok (number, rest)
    | Some number, Ok (decimal, rest) -> Ok (number ^ decimal, rest)
    | Some number, Error rest -> Ok (number, rest)
    | None, Ok (".", _) -> Error input
    | None, Ok (decimal, rest) -> Ok ("0" ^ decimal, rest)
    | None, Error _ -> Error input
  in
  Result.map (fun (number, rest) -> (number |> Float.of_string, rest)) number

let get_number input = input |> eat_space |> parse_number

