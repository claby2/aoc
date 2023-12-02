open Format

type color = Red | Green | Blue

let rec explode (s : string) =
  match s with
  | "" -> []
  | s -> [ String.get s 0 ] @ explode (String.sub s 1 (String.length s - 1))

let consume_number (s : string) : int option * string =
  let rec consume (processing : bool) (s : char list) : string * char list =
    match s with
    | [] -> ("", [])
    | c :: rest ->
        if (c >= '1' && c <= '9') || (processing && c == '0') then
          let num_string, rest = consume true rest in
          (String.make 1 c ^ num_string, rest)
        else if processing then ("", s)
        else consume false rest
  in

  let num_string, rest = consume false (explode s) in
  (int_of_string_opt num_string, String.of_seq (List.to_seq rest))

let consume_color (s : string) : color option * string =
  let rec consume (s : char list) : color option * char list =
    match s with
    | [] -> (None, [])
    | 'r' :: 'e' :: 'd' :: rest -> (Some Red, rest)
    | 'g' :: 'r' :: 'e' :: 'e' :: 'n' :: rest -> (Some Green, rest)
    | 'b' :: 'l' :: 'u' :: 'e' :: rest -> (Some Blue, rest)
    | _ :: rest -> consume rest
  in
  let color, rest = consume (explode s) in
  (color, String.of_seq (List.to_seq rest))

let part1 (ch : in_channel) =
  let rec is_valid_set (set : string) : bool =
    match consume_number set with
    | None, _ -> true
    | Some num, rest ->
        let color, rest = consume_color rest in
        (match color with
        | None -> raise (Failure "no color found")
        | Some Red -> num <= 12
        | Some Blue -> num <= 14
        | Some Green -> num <= 13)
        && is_valid_set rest
  in

  let rec is_valid_game (game : string list) : bool =
    match game with
    | [] -> true
    | set :: rest -> is_valid_set set && is_valid_game rest
  in

  let rec loop (total : int) =
    try
      let s = input_line ch in
      let id, s = consume_number s in
      let sets = String.split_on_char ';' s in
      if is_valid_game sets then Option.get id + loop total else loop total
    with End_of_file -> total
  in
  printf "%d\n" (loop 0)

let part2 (ch : in_channel) =
  let rec maximum_color (c : color) (sets : string list) : int =
    let rec process_set (set : string) : int =
      match consume_number set with
      | None, _ -> 0
      | Some num, set ->
          let color, set = consume_color set in
          if color = Some c then num else process_set set
    in
    match sets with
    | [] -> 0
    | set :: rest -> max (process_set set) (maximum_color c rest)
  in

  let rec calculate_power (sets : string list) : int =
    maximum_color Red sets * maximum_color Green sets * maximum_color Blue sets
  in

  let rec loop (total : int) =
    try
      let s = input_line ch in
      let _, s = consume_number s in
      let sets = String.split_on_char ';' s in
      calculate_power sets + loop total
    with End_of_file -> total
  in
  printf "%d\n" (loop 0)

let () =
  let ch = open_in "02.txt" in
  part1 ch;
  let ch = open_in "02.txt" in
  part2 ch
