open Format

let rec explode (s : string) =
  match s with
  | "" -> []
  | s -> [ String.get s 0 ] @ explode (String.sub s 1 (String.length s - 1))

let part1 (ch : in_channel) =
  let rec is_number (c : char) =
    match c with
    | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' -> true
    | _ -> false
  in

  let rec first_number (l : char list) =
    match l with
    | [] -> raise (Failure "no number")
    | c :: rest when is_number c -> c
    | c :: rest -> first_number rest
  in

  let rec loop (total : int) =
    try
      let s = input_line ch in
      match s with
      | "" -> 0
      | s ->
          let l = explode s in
          let a = first_number l in
          let b = first_number (List.rev l) in
          let num =
            ((int_of_char a - int_of_char '0') * 10)
            + (int_of_char b - int_of_char '0')
          in
          num + loop total
    with End_of_file -> total
  in
  printf "%d\n" (loop 0)

let part2 (ch : in_channel) =
  let rec numbers (l : char list) =
    match l with
    | [] -> []
    | 'o' :: 'n' :: 'e' :: _ | '1' :: _ -> 1 :: numbers (List.tl l)
    | 't' :: 'w' :: 'o' :: _ | '2' :: _ -> 2 :: numbers (List.tl l)
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ | '3' :: _ ->
        3 :: numbers (List.tl l)
    | 'f' :: 'o' :: 'u' :: 'r' :: _ | '4' :: _ -> 4 :: numbers (List.tl l)
    | 'f' :: 'i' :: 'v' :: 'e' :: _ | '5' :: _ -> 5 :: numbers (List.tl l)
    | 's' :: 'i' :: 'x' :: _ | '6' :: _ -> 6 :: numbers (List.tl l)
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ | '7' :: _ ->
        7 :: numbers (List.tl l)
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ | '8' :: _ ->
        8 :: numbers (List.tl l)
    | 'n' :: 'i' :: 'n' :: 'e' :: _ | '9' :: _ -> 9 :: numbers (List.tl l)
    | 'z' :: 'e' :: 'r' :: 'o' :: _ | '0' :: _ -> 0 :: numbers (List.tl l)
    | _ :: rest -> numbers rest
  in
  let first_number (s : string) = List.hd (numbers (explode s)) in
  let last_number (s : string) = List.hd (List.rev (numbers (explode s))) in

  let rec loop (total : int) =
    try
      let s = input_line ch in
      match s with
      | "" -> 0
      | s ->
          let a = first_number s in
          let b = last_number s in
          let num = (a * 10) + b in
          num + loop total
    with End_of_file -> total
  in
  printf "%d\n" (loop 0)

let () =
  let ch = open_in "01.txt" in
  part1 ch;
  let ch = open_in "01.txt" in
  part2 ch
