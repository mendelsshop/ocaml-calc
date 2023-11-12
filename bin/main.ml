let (>>=) result f =
  Result.bind result f

let () = print_string "type your math expression [number] [+|-|*|/] [number]\n"

let () =
  let line =  read_line () in let input
  =  line |> Calc.string_to_chars in 
  let result =
    (Calc.get_number input) >>= fun (n1, rest1) ->
    (Calc.get_op rest1) >>= fun (op, rest2) ->
    (Calc.get_number rest2) >>= fun (n2, r) ->
    Ok (op n1 n2, r)
  in
  match result with
  | Ok (number,   []) -> Printf.printf "%s = %.2f\n" line number
  | _ -> Printf.printf "not a valid expression %s\n" line 
