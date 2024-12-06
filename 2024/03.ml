let explode s = List.init (String.length s) (String.get s)

let parse_number_until suffix s : (int * char list) option =
  let is_digit = function
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
    | _ -> false
  in
  let rec aux current s =
    match s with
    | [] -> (None, s)
    | x :: xs when is_digit x -> aux (current ^ String.make 1 x) xs
    | x :: xs
      when x == suffix
           && String.length current >= 0
           && String.length current <= 3 ->
        (Some (int_of_string current), xs)
    | _ -> (None, s)
  in
  let digit, rest = aux "" s in
  match digit with None -> None | Some digit -> Some (digit, rest)

let parse_expect pat s : char list option =
  let rec aux current s =
    if current = pat then (true, s)
    else
      match s with
      | [] -> (false, s)
      | x :: xs ->
          let next_current = current ^ String.make 1 x in
          if String.starts_with ~prefix:next_current pat then
            aux next_current xs
          else (false, s)
  in
  let found, rest = aux "" s in
  if found then Some rest else None

type token = Mul | Number of int | Do | Dont

let tl_or_empty l = try List.tl l with _ -> []

let evaluate parse_do_and_dont expr : int =
  let rec aux last_token expr =
    if List.length expr = 0 then 0
    else
      let res = parse_expect "don't()" expr in
      if parse_do_and_dont && Option.is_some res then
        aux Dont (tl_or_empty expr)
      else
        match last_token with
        | Do -> (
            (* parse mul( *)
            let res = parse_expect "mul(" expr in
            match res with
            | None -> aux Do (tl_or_empty expr)
            | Some rest -> aux Mul rest)
        | Mul -> (
            (* parse first number *)
            let res = parse_number_until ',' expr in
            match res with
            | None -> aux Do (tl_or_empty expr)
            | Some (num, rest) -> aux (Number num) rest)
        | Number num1 -> (
            (* parse second number *)
            let res = parse_number_until ')' expr in
            match res with
            | None -> aux Do (tl_or_empty expr)
            | Some (num2, rest) -> (num1 * num2) + aux Do rest)
        | Dont -> (
            let res = parse_expect "do()" expr in
            match res with
            | None -> aux Dont (tl_or_empty expr)
            | Some rest -> aux Do rest)
  in
  aux Do (explode expr)

let () =
  In_channel.with_open_text "03.txt" (fun ic ->
      let lines = In_channel.input_lines ic in
      let res = lines |> List.map (evaluate false) |> List.fold_left ( + ) 0 in
      Printf.printf "%d\n" @@ res;
      let res = lines |> List.fold_left ( ^ ) "" |> evaluate true in
      Printf.printf "%d\n" @@ res)
